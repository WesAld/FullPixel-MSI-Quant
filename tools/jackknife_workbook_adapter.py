#!/usr/bin/env python3
"""Add the optional V3 Jackknife_Log2FC sheet to a validated workbook."""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
from pathlib import Path

import pandas as pd
from openpyxl import load_workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter

from jackknife_analysis import (calculate_jackknife, calculate_log2fc,
                                matched_replicate_pairs, unique_tokens)
from normalization_routing import (resolve_method, workbook_sheet_names,
                                   workbook_intensity_column_suffix)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest().upper()


def style_sheet(worksheet) -> None:
    worksheet.freeze_panes = "A2"
    worksheet.auto_filter.ref = worksheet.dimensions
    for cell in worksheet[1]:
        cell.font = Font(bold=True)
        cell.fill = PatternFill("solid", fgColor="E2F0D9")
        cell.alignment = Alignment(wrap_text=True)
    for index, column in enumerate(worksheet.columns, 1):
        cells = list(column)
        width = min(max((len(str(cell.value)) if cell.value is not None else 0)
                        for cell in cells[:200]) + 2, 45)
        worksheet.column_dimensions[get_column_letter(index)].width = max(width, 10)


def append_readme(workbook, rows: list[tuple[str, object]]) -> None:
    worksheet = workbook["README"]
    for field, value in rows:
        worksheet.append([field, value])


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()
    request = json.loads(Path(args.request).read_text(encoding="utf-8"))
    report = {"schema_version": "1.0.0", "status": "FAIL",
              "workflow_id": request.get("workflow_id")}
    try:
        method = resolve_method(request)
        source_item = request["source_workbook"]
        source = Path(source_item["path"])
        if not source.is_file() or sha256(source) != source_item["sha256"].upper():
            raise RuntimeError("Source workbook identity failed.")
        output_directory = Path(request["output_working_directory"])
        output_directory.mkdir(parents=True, exist_ok=False)
        output = output_directory / request.get("output_filename", source.name)
        shutil.copy2(source, output)
        workbook = load_workbook(output)
        enabled = request.get("jackknife", {}).get("enabled") is True
        if not enabled:
            append_readme(workbook, [("Jackknife analysis", "disabled")])
            workbook.save(output)
            report.update(status="PASS", enabled=False, output_workbook={
                "path": str(output.resolve()), "sha256": sha256(output)})
        else:
            mappings = request.get("display_metadata", {}).get("sample_mappings", [])
            pairs = matched_replicate_pairs(mappings)
            _, all_features_sheet = workbook_sheet_names(method)
            frame = pd.read_excel(output, sheet_name=all_features_sheet)
            suffix = workbook_intensity_column_suffix(method)
            unique_mappings = {}
            for item in mappings:
                internal = str(item.get("internal_value", "")).strip()
                if internal and internal not in unique_mappings:
                    unique_mappings[internal] = item
            internals = [p["reference_internal"] for p in pairs] + [
                p["comparison_internal"] for p in pairs]
            labels = [str(unique_mappings[i].get("user_facing_label") or
                          unique_mappings[i].get("user_facing_value") or i)
                      for i in internals]
            tokens = unique_tokens(labels, internals)
            column_by_internal = {internal: token + suffix
                                  for internal, token in zip(internals, tokens)}
            missing = [column for column in column_by_internal.values()
                       if column not in frame.columns]
            if missing:
                raise RuntimeError("Primary-intensity columns are missing: " +
                                   ", ".join(missing))
            log2_columns = [column for column in frame.columns
                            if str(column).startswith("log2FC_")]
            if len(log2_columns) != 1:
                raise RuntimeError("Expected exactly one full-data log2FC column.")
            reference_columns = [column_by_internal[p["reference_internal"]]
                                 for p in pairs]
            comparison_columns = [column_by_internal[p["comparison_internal"]]
                                  for p in pairs]
            audited_full = calculate_log2fc(
                frame, reference_columns, comparison_columns)
            recorded_full = pd.to_numeric(frame[log2_columns[0]], errors="coerce")
            if int((audited_full.isna() != recorded_full.isna()).sum()):
                raise RuntimeError("Full-data log2FC missing-value audit failed.")
            both = audited_full.notna() & recorded_full.notna()
            maximum_difference = float((audited_full[both] -
                                        recorded_full[both]).abs().max()) if both.any() else 0.0
            if maximum_difference > 1e-10:
                raise RuntimeError("Full-data log2FC numerical audit failed.")
            calculated = calculate_jackknife(
                frame, pairs, column_by_internal, frame[log2_columns[0]])
            identity_candidates = ["dataset_id", "ion_mode", "target_id",
                                   "mzmine_row_id", "target_mz", "annotation_status",
                                   "annotation_name", "annotation_formula",
                                   "annotation_adduct", "annotation_score",
                                   "annotation_database", "GraphPad_label"]
            identity = [column for column in identity_candidates if column in frame]
            output_frame = pd.concat([frame[identity], calculated], axis=1)
            if "Jackknife_Log2FC" in workbook.sheetnames:
                del workbook["Jackknife_Log2FC"]
            insert_at = workbook.sheetnames.index("Mean_Log2FC") + 1
            worksheet = workbook.create_sheet("Jackknife_Log2FC", insert_at)
            worksheet.append(list(output_frame.columns))
            for row in output_frame.itertuples(index=False, name=None):
                worksheet.append([None if pd.isna(value) else value for value in row])
            style_sheet(worksheet)
            replicate_ids = [pair["replicate_id"] for pair in pairs]
            append_readme(workbook, [
                ("Jackknife analysis", "enabled"),
                ("Jackknife resampling unit", "matched biological replicate pairs"),
                ("Jackknife matched pairs", len(pairs)),
                ("Jackknife replicate IDs", ", ".join(replicate_ids)),
                ("Jackknife primary normalization method", method),
                ("Jackknife SE method", "sqrt(((n-1)/n) * sum((theta_i - mean(theta))^2))"),
            ])
            workbook.save(output)
            report.update(status="PASS", enabled=True,
                          primary_normalization_method=method,
                          matched_replicate_ids=replicate_ids,
                          resampling_unit="matched biological replicate pairs",
                          full_log2fc_audit_max_abs=maximum_difference,
                          rows=len(output_frame), output_workbook={
                              "path": str(output.resolve()), "sha256": sha256(output)})
    except Exception as exc:
        report["error"] = str(exc)
    Path(args.report).parent.mkdir(parents=True, exist_ok=True)
    Path(args.report).write_text(json.dumps(report, indent=2), encoding="utf-8")
    return 0 if report["status"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
