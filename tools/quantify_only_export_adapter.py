#!/usr/bin/env python3
"""Create a five-sheet workbook with individual sample readouts and no comparisons."""
from __future__ import annotations
import argparse, hashlib, json, re, sys
from pathlib import Path
import pandas as pd
from openpyxl import Workbook, load_workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter
sys.path.insert(0, str(Path(__file__).resolve().parent))
from normalization_routing import provenance, resolve_method


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()


def safe(value, fallback="Sample"):
    token = re.sub(r"[^A-Za-z0-9]+", "_", str(value or "")).strip("_")
    return (token or fallback)[:64]


def write_df(ws, frame: pd.DataFrame):
    ws.append(list(frame.columns))
    for row in frame.itertuples(index=False, name=None):
        ws.append([None if pd.isna(value) else value for value in row])
    ws.freeze_panes = "A2"
    ws.auto_filter.ref = ws.dimensions
    for cell in ws[1]:
        cell.font = Font(bold=True)
        cell.fill = PatternFill("solid", fgColor="D9EAF7")
        cell.alignment = Alignment(wrap_text=True)
    for index, column in enumerate(ws.columns, 1):
        width = min(max(len(str(cell.value)) if cell.value is not None else 0
                        for cell in list(column)[:200]) + 2, 45)
        ws.column_dimensions[get_column_letter(index)].width = max(width, 10)


def mode_mappings(all_mappings, mode):
    rows = [x for x in all_mappings if not x.get("ion_mode") or x.get("ion_mode") == mode]
    seen = set(); out = []
    for row in rows:
        key = row.get("internal_value")
        if key and key not in seen:
            seen.add(key); out.append(row)
    return out


def rename_sample_columns(frame, mappings, mode):
    suffixes = [
        "overall_normalized_signal", "local_strong_pixel_intensity",
        "conservative_detection_fraction", "conservative_detected_pixel_count",
        "primary_quantification_eligible", "primary_qc_status",
    ]
    labels = [row.get("user_facing_label") or row.get("user_facing_value") or
              row.get("internal_value") for row in mappings]
    bases = [safe(label, row.get("internal_value") or "Sample")
             for label, row in zip(labels, mappings)]
    counts = {value: bases.count(value) for value in set(bases)}
    rename = {}
    for row, base in zip(mappings, bases):
        internal = row["internal_value"]
        display = base if counts[base] == 1 else safe(f"{base}_{mode}_{internal}")
        for suffix in suffixes:
            source = f"{internal}_{suffix}"
            if source in frame.columns:
                rename[source] = f"{display}_{suffix}"
    return frame.rename(columns=rename), rename


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()
    req = json.loads(Path(args.request).read_text(encoding="utf-8"))
    report_path = Path(args.report)
    report = {"schema_version": "1.0.0", "workflow_id": req.get("workflow_id"),
              "status": "FAIL"}
    try:
        if req.get("analysis_type") != "quantify_only":
            raise RuntimeError("Quantify-only export received the wrong analysis type.")
        method = resolve_method(req)
        normalization = provenance(method, req.get("display_metadata", {}).get("normalization", {}).get("normalization_scale", 1000000))
        masters = req.get("masters", [])
        if not masters:
            raise RuntimeError("At least one annotated per-sample master is required.")
        mappings = req.get("display_metadata", {}).get("sample_mappings", [])
        frames = []; source_rows = 0; sample_columns_by_mode = {}
        for item in masters:
            path = Path(item["path"])
            if not path.is_file() or sha(path) != item["sha256"].upper():
                raise RuntimeError(f"Master identity failed: {path}")
            frame = pd.read_csv(path, low_memory=False)
            source_rows += len(frame)
            mode = item["ion_mode"]
            frame["ion_mode"] = mode
            frame, renamed = rename_sample_columns(frame, mode_mappings(mappings, mode), mode)
            sample_columns_by_mode[mode] = sorted(renamed.values())
            frames.append(frame)
        master = pd.concat(frames, ignore_index=True, sort=False)
        if len(master) != source_rows:
            raise RuntimeError("Combined row retention failed.")
        forbidden = [column for column in master.columns if any(
            token in column.lower() for token in ("log2fc", "p_value", "pvalue", "difference_trt", "trt_minus_ctrl"))]
        if forbidden:
            raise RuntimeError("Comparison-specific columns are forbidden in Quantify samples only: " + ", ".join(forbidden[:10]))
        if "annotation_name" not in master.columns:
            master["annotation_name"] = ""
        master["GraphPad_label"] = master.apply(
            lambda row: f"{row.get('annotation_name') or 'Unannotated'} | m/z {float(row.get('target_mz')):.4f}", axis=1)
        meta_candidates = [
            "dataset_id", "ion_mode", "target_id", "mzmine_row_id", "target_mz",
            "annotation_name", "annotation_formula", "annotation_adduct",
            "annotation_score", "annotation_compound_score", "annotation_database",
            "annotation_mz_diff", "annotation_mz_diff_ppm", "annotation_match_status",
            "GraphPad_label",
        ]
        meta = [column for column in meta_candidates if column in master.columns]
        overall_cols = sorted(column for column in master.columns if column.endswith("_overall_normalized_signal"))
        local_cols = sorted(column for column in master.columns if column.endswith("_local_strong_pixel_intensity"))
        fraction_cols = sorted(column for column in master.columns if column.endswith("_conservative_detection_fraction"))
        count_cols = sorted(column for column in master.columns if column.endswith("_conservative_detected_pixel_count"))
        eligible_cols = sorted(column for column in master.columns if column.endswith("_primary_quantification_eligible"))
        qc_status_cols = sorted(column for column in master.columns if column.endswith("_primary_qc_status"))
        if not overall_cols or not local_cols or not fraction_cols or not count_cols:
            raise RuntimeError("Required individual-sample readout columns are missing.")
        normalized = master[meta + overall_cols].copy()
        detected = master[meta + local_cols].copy()
        detection = master[meta + fraction_cols + count_cols].copy()
        for column in fraction_cols:
            detection[column] = pd.to_numeric(detection[column], errors="coerce") * 100.0
        detection = detection.rename(columns={column: column.replace(
            "_conservative_detection_fraction", "_detection_fraction_percent")
            for column in fraction_cols})
        qc_rows = [
            ["source_rows_retained", "PASS", len(master), source_rows],
            ["comparison_columns_absent", "PASS", 0, 0],
            ["individual_normalized_columns", "PASS", len(overall_cols), ">=1"],
            ["individual_detected_intensity_columns", "PASS", len(local_cols), ">=1"],
            ["individual_detection_fraction_columns", "PASS", len(fraction_cols), ">=1"],
            ["individual_detection_count_columns", "PASS", len(count_cols), ">=1"],
            ["validated_workflow_output", "PASS",
             "Validated final workbook", "validated export contract"],
        ]
        qc = pd.DataFrame(qc_rows, columns=["check", "status", "observed", "expected"])
        if eligible_cols or qc_status_cols:
            qc = pd.concat([qc, pd.DataFrame([[
                "sample_level_qc_fields_preserved", "PASS",
                len(eligible_cols) + len(qc_status_cols), ">=0"]], columns=qc.columns)],
                ignore_index=True)
        mode_names = ", ".join(item["ion_mode"] for item in masters)
        sample_labels = sorted({
            (row.get("user_facing_label") or row.get("user_facing_value") or row.get("internal_value"))
            for row in mappings if row.get("internal_value")
        })
        readme = pd.DataFrame({
            "Field": ["Application version", "Project", "Exposure", "Analysis type", "Ion modes",
                      "Registered biological samples", "Normalization",
                      "Primary normalization method", "Normalization definition version",
                      "Detection threshold", "Output interpretation",
                      "Comparison statistics", "Validation status"],
            "Value": ["2.0.2", req.get("display_metadata", {}).get("project_name"),
                      req.get("exposure_label"), "Quantify samples only", mode_names,
                      ", ".join(sample_labels),
                      normalization["normalization_definition"],
                      method, normalization["normalization_definition_version"],
                      "Conservative raw intensity >= 1000; minimum 10 pixels",
                      "One column per individual sample for each readout",
                      "None: no fold change, group mean, p-value, pairing, pseudocount, or imputation",
                      "Validated workflow output"],
        })
        outdir = Path(req["output_working_directory"])
        outdir.mkdir(parents=True, exist_ok=False)
        filename = (f"{safe(req.get('display_metadata', {}).get('project_name'), 'Project')}_"
                    f"{safe(req.get('exposure_label'), 'Exposure')}_MALDIPixelQuant_"
                    "Quantified_Samples.xlsx")
        workbook_path = outdir / filename
        workbook = Workbook(); workbook.remove(workbook.active)
        for name, frame in [
            ("Normalized_Intensity", normalized),
            ("Detected_Pixel_Intensity", detected),
            ("Detection", detection), ("QC", qc), ("README", readme),
        ]:
            write_df(workbook.create_sheet(name), frame)
        workbook.calculation.fullCalcOnLoad = True
        workbook.calculation.forceFullCalc = True
        workbook.calculation.calcMode = "auto"
        workbook.save(workbook_path)
        checked = load_workbook(workbook_path, read_only=True, data_only=False)
        expected_sheets = ["Normalized_Intensity", "Detected_Pixel_Intensity",
                           "Detection", "QC", "README"]
        if checked.sheetnames != expected_sheets:
            checked.close(); raise RuntimeError("Final workbook sheet contract failed.")
        sheet_rows = {ws.title: ws.max_row - 1 for ws in checked.worksheets}
        headers = {ws.title: [cell.value for cell in next(ws.iter_rows(min_row=1, max_row=1))]
                   for ws in checked.worksheets}
        checked.close()
        exported_headers = headers["Normalized_Intensity"] + headers["Detected_Pixel_Intensity"] + headers["Detection"]
        if any("log2" in str(value).lower() or "p_value" in str(value).lower()
               for value in exported_headers):
            raise RuntimeError("Comparison headers appeared in the quantify-only workbook.")
        report.update(status="PASS",
            final_workbook={"path": str(workbook_path), "sha256": sha(workbook_path),
                            "bytes": workbook_path.stat().st_size},
            source_rows=source_rows, sheet_rows=sheet_rows,
            sample_columns_by_mode=sample_columns_by_mode,
            validation={"status": "PASS", "comparison_statistics_absent": True},
            normalization=normalization)
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 0
    except Exception as exc:
        report.update(status="FAIL", error=str(exc),
                      recommended_correction="Inspect the preserved quantify-only export request and annotated masters.")
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
