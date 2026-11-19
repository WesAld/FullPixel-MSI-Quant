#!/usr/bin/env python3
"""Add a method-aware all-registered-features comparison sheet.

The adapter copies an already validated two-group workbook, leaves its existing
worksheets unchanged, and adds one sheet built directly from the hash-verified
annotated three-readout masters. The masters retain all registered quantitative
rows, including rows without a compound annotation.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import shutil
import tempfile
import zipfile
import posixpath
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Any, Iterable

import pandas as pd
from openpyxl import load_workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.workbook.properties import CalcProperties
from openpyxl.utils import get_column_letter

from normalization_routing import (resolve_method,
                                   workbook_intensity_column_suffix,
                                   workbook_sheet_names)



def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest().upper()


def safe_token(value: Any, fallback: str) -> str:
    token = re.sub(r"[^A-Za-z0-9]+", "_", str(value or "")).strip("_")
    return (token or fallback)[:64]


def natural_key(value: str) -> list[Any]:
    return [int(part) if part.isdigit() else part.lower()
            for part in re.split(r"(\d+)", value)]


def first_nonblank(row: pd.Series, candidates: Iterable[str]) -> str:
    for column in candidates:
        if column in row.index:
            value = row[column]
            if value is not None and str(value).strip() and str(value).lower() != "nan":
                return str(value).strip()
    return ""



def sanitize_missing_drawing_references(source: Path, destination: Path) -> bool:
    """Remove only dangling drawing/VML relationships from a malformed XLSX."""
    relationship_ns = "http://schemas.openxmlformats.org/package/2006/relationships"
    office_rel_ns = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    content_ns = "http://schemas.openxmlformats.org/package/2006/content-types"
    changed = False
    with zipfile.ZipFile(source, "r") as archive:
        names = set(archive.namelist())
        replacements: dict[str, bytes] = {}
        removed_ids_by_owner: dict[str, set[str]] = {}

        for rel_path in [name for name in names if "/_rels/" in name and name.endswith(".rels")]:
            root = ET.fromstring(archive.read(rel_path))
            owner = rel_path.replace("/_rels/", "/").removesuffix(".rels")
            owner_directory = posixpath.dirname(owner)
            removed: set[str] = set()
            for relationship in list(root):
                target = relationship.attrib.get("Target", "")
                rel_type = relationship.attrib.get("Type", "")
                if not target or "://" in target:
                    continue
                resolved = posixpath.normpath(posixpath.join(owner_directory, target))
                missing_drawing = (resolved not in names and
                    (rel_type.endswith("/drawing") or rel_type.endswith("/vmlDrawing")))
                if missing_drawing:
                    removed.add(relationship.attrib.get("Id", ""))
                    root.remove(relationship)
                    changed = True
            if removed:
                removed_ids_by_owner[owner] = removed
                replacements[rel_path] = ET.tostring(root, encoding="utf-8", xml_declaration=True)

        for owner, removed_ids in removed_ids_by_owner.items():
            if owner not in names:
                continue
            root = ET.fromstring(archive.read(owner))
            for element in list(root):
                rel_id = element.attrib.get(f"{{{office_rel_ns}}}id", "")
                local_name = element.tag.rsplit("}", 1)[-1]
                if rel_id in removed_ids and local_name in {"drawing", "legacyDrawing"}:
                    root.remove(element)
                    changed = True
            replacements[owner] = ET.tostring(root, encoding="utf-8", xml_declaration=True)

        content_path = "[Content_Types].xml"
        if content_path in names:
            root = ET.fromstring(archive.read(content_path))
            for override in list(root):
                part_name = override.attrib.get("PartName", "").lstrip("/")
                content_type = override.attrib.get("ContentType", "")
                if (part_name and part_name not in names and
                    ("drawing" in content_type.lower() or part_name.startswith("xl/drawings/"))):
                    root.remove(override)
                    changed = True
            replacements[content_path] = ET.tostring(root, encoding="utf-8", xml_declaration=True)

        with zipfile.ZipFile(destination, "w", zipfile.ZIP_DEFLATED) as output:
            for name in archive.namelist():
                output.writestr(name, replacements.get(name, archive.read(name)))
    return changed


def openpyxl_compatible_source(source: Path, temporary_directory: Path) -> tuple[Path, bool]:
    try:
        workbook = load_workbook(source, read_only=False, data_only=False)
        workbook.close()
        return source, False
    except KeyError as exception:
        if "drawing" not in str(exception).lower() and "vml" not in str(exception).lower():
            raise
        cleaned = temporary_directory / "source_without_dangling_drawings.xlsx"
        changed = sanitize_missing_drawing_references(source, cleaned)
        if not changed:
            raise
        workbook = load_workbook(cleaned, read_only=False, data_only=False)
        workbook.close()
        return cleaned, True

def workbook_content_digest(path: Path, excluded: set[str] | None = None) -> dict[str, str]:
    """Hash worksheet cell values/formulas, not ZIP metadata."""
    excluded = excluded or set()
    workbook = load_workbook(path, read_only=True, data_only=False)
    result: dict[str, str] = {}
    try:
        for worksheet in workbook.worksheets:
            if worksheet.title in excluded:
                continue
            digest = hashlib.sha256()
            digest.update(worksheet.title.encode("utf-8"))
            for row in worksheet.iter_rows():
                values = [cell.value for cell in row]
                digest.update(json.dumps(values, default=str, ensure_ascii=False,
                                         separators=(",", ":")).encode("utf-8"))
                digest.update(b"\n")
            result[worksheet.title] = digest.hexdigest().upper()
    finally:
        workbook.close()
    return result


def read_readme_labels(path: Path) -> tuple[str | None, str | None]:
    try:
        frame = pd.read_excel(path, sheet_name="README", dtype=str,
                              keep_default_na=False)
    except Exception:
        return None, None
    if frame.shape[1] < 2:
        return None, None
    lookup = {
        str(key).strip().lower(): str(value).strip()
        for key, value in zip(frame.iloc[:, 0], frame.iloc[:, 1])
    }
    reference = lookup.get("reference condition")
    comparison = lookup.get("comparison condition")
    if not reference or not comparison:
        groups = lookup.get("sample groups", "")
        match = re.search(
            r"reference\s*:\s*([^;]+);\s*comparison\s*:\s*([^;(]+)",
            groups, flags=re.IGNORECASE)
        if match:
            reference = reference or match.group(1).strip()
            comparison = comparison or match.group(2).strip()
    return reference, comparison


def source_workbook_sample_labels(path: Path) -> list[str]:
    try:
        headers = list(pd.read_excel(path, sheet_name="Mean_Log2FC", nrows=0).columns)
    except Exception:
        return []
    generic = [column.removesuffix("_overall_normalized_signal")
               for column in headers
               if str(column).endswith("_overall_normalized_signal")]
    if generic:
        return generic
    legacy = [column.removesuffix("_TIC_normalized_signal")
              for column in headers
              if str(column).endswith("_TIC_normalized_signal")]
    return legacy


def read_master(item: dict[str, Any]) -> pd.DataFrame:
    path = Path(item["path"])
    if not path.is_file():
        raise RuntimeError(f"Annotated master is missing: {path}")
    expected = str(item.get("sha256", "")).upper()
    actual = sha256(path)
    if expected and actual != expected:
        raise RuntimeError(f"Annotated-master SHA-256 mismatch: {path}")

    header = list(pd.read_csv(path, nrows=0).columns)
    text_columns = {
        "dataset_id", "target_id", "mzmine_row_id", "annotation_name",
        "annotation_formula", "annotation_adduct", "annotation_score",
        "annotation_compound_annotation_score", "annotation_compound_score",
        "annotation_database", "annotation_mz_diff", "annotation_mz_diff_ppm",
    }
    dtypes = {column: "string" for column in header if column in text_columns}
    frame = pd.read_csv(path, dtype=dtypes, keep_default_na=False,
                        low_memory=False)
    if "ion_mode" not in frame.columns:
        frame["ion_mode"] = item.get("ion_mode", "")
    else:
        frame["ion_mode"] = frame["ion_mode"].replace("", item.get("ion_mode", ""))
    return frame


def infer_sample_ids(master: pd.DataFrame) -> tuple[list[str], list[str]]:
    sample_ids = sorted({
        column.removesuffix("_overall_normalized_signal")
        for column in master.columns
        if column.endswith("_overall_normalized_signal")
    }, key=natural_key)
    reference = [sample for sample in sample_ids if sample.startswith("CTRL_")]
    comparison = [sample for sample in sample_ids if sample.startswith("TRT_")]
    if len(reference) < 2 or len(reference) != len(comparison):
        raise RuntimeError(
            "All-features export requires equal CTRL_ and TRT_ internal sample sets "
            "with at least two replicates per condition."
        )
    return reference, comparison


def mapping_lookup(request: dict[str, Any]) -> dict[str, str]:
    mappings = request.get("display_metadata", {}).get("sample_mappings", []) or []
    result: dict[str, str] = {}
    for mapping in mappings:
        internal = str(mapping.get("internal_value", "")).strip()
        display = (mapping.get("user_facing_label") or
                   mapping.get("user_facing_value") or internal)
        if internal and internal not in result:
            result[internal] = str(display).strip() or internal
    return result


def unique_display_tokens(labels: list[str], internal_ids: list[str]) -> list[str]:
    base = [safe_token(label, internal) for label, internal in zip(labels, internal_ids)]
    counts = {token: base.count(token) for token in set(base)}
    return [token if counts[token] == 1 else safe_token(f"{token}_{internal}", internal)
            for token, internal in zip(base, internal_ids)]


def finite_mean(frame: pd.DataFrame, columns: list[str]) -> pd.Series:
    values = frame[columns].apply(pd.to_numeric, errors="coerce")
    # Script 03 requires all replicates for condition statistics.
    return values.mean(axis=1, skipna=False)


def fold_change(comparison: pd.Series, reference: pd.Series) -> pd.Series:
    result = pd.Series(float("nan"), index=reference.index, dtype="float64")
    valid = (reference.notna() & comparison.notna() &
             (reference > 0) & (comparison > 0))
    result.loc[valid] = comparison.loc[valid] / reference.loc[valid]
    return result


def log2_values(values: pd.Series) -> pd.Series:
    result = pd.Series(float("nan"), index=values.index, dtype="float64")
    valid = values.notna() & (values > 0)
    result.loc[valid] = values.loc[valid].map(math.log2)
    return result


def max_numeric_difference(left: pd.Series, right: pd.Series) -> tuple[float, int]:
    a = pd.to_numeric(left, errors="coerce")
    b = pd.to_numeric(right, errors="coerce")
    mismatch = int((a.isna() != b.isna()).sum())
    both = a.notna() & b.notna()
    difference = (a[both] - b[both]).abs()
    return (float(difference.max()) if len(difference) else 0.0, mismatch)


def display_metadata(request: dict[str, Any], source: Path,
                     reference_ids: list[str], comparison_ids: list[str]) -> tuple[
                         str, str, list[str], list[str]
                     ]:
    metadata = request.get("display_metadata", {}) or {}
    readme_reference, readme_comparison = read_readme_labels(source)
    reference_label = (metadata.get("reference_condition") or
                       readme_reference or "CTRL")
    comparison_label = (metadata.get("comparison_condition") or
                        readme_comparison or "TRT")

    lookup = mapping_lookup(request)
    all_internal = reference_ids + comparison_ids
    source_labels = source_workbook_sample_labels(source)
    if len(source_labels) == len(all_internal):
        source_lookup = dict(zip(all_internal, source_labels))
    else:
        source_lookup = {}
    labels = [lookup.get(sample) or source_lookup.get(sample) or sample
              for sample in all_internal]
    tokens = unique_display_tokens(labels, all_internal)
    return (str(reference_label), str(comparison_label),
            tokens[:len(reference_ids)], tokens[len(reference_ids):])


def build_all_features(request: dict[str, Any], source: Path,
                       method: str) -> tuple[pd.DataFrame, dict[str, Any]]:
    masters = request.get("masters", []) or []
    if not masters:
        raise RuntimeError("At least one hash-verified annotated master is required.")
    frames = [read_master(item) for item in masters]
    master = pd.concat(frames, ignore_index=True, sort=False)
    if master.empty:
        raise RuntimeError("Annotated masters contain no quantitative rows.")

    required_identity = ["dataset_id", "mzmine_row_id", "target_mz"]
    missing_identity = [column for column in required_identity
                        if column not in master.columns]
    if missing_identity:
        raise RuntimeError("Annotated masters are missing identity columns: " +
                           ", ".join(missing_identity))

    reference_ids, comparison_ids = infer_sample_ids(master)
    reference_label, comparison_label, reference_tokens, comparison_tokens = \
        display_metadata(request, source, reference_ids, comparison_ids)

    reference_columns = [f"{sample}_overall_normalized_signal"
                         for sample in reference_ids]
    comparison_columns = [f"{sample}_overall_normalized_signal"
                          for sample in comparison_ids]
    missing_quantitative = [column for column in
                            reference_columns + comparison_columns
                            if column not in master.columns]
    if missing_quantitative:
        raise RuntimeError("Annotated masters are missing normalized-intensity columns: " +
                           ", ".join(missing_quantitative))

    reference_mean = finite_mean(master, reference_columns)
    comparison_mean = finite_mean(master, comparison_columns)
    calculated_fc = fold_change(comparison_mean, reference_mean)
    calculated_log2 = log2_values(calculated_fc)

    audit_difference = 0.0
    audit_missing = 0
    if "log2FC_overall_TRT_vs_CTRL" in master.columns:
        audit_difference, audit_missing = max_numeric_difference(
            calculated_log2, master["log2FC_overall_TRT_vs_CTRL"])
        if audit_difference > 1e-10 or audit_missing:
            raise RuntimeError(
                "All-features log2 fold-change audit disagrees with the "
                "validated replicate summary."
            )

    annotation_name = master.get("annotation_name", pd.Series("", index=master.index))
    annotation_name = annotation_name.fillna("").astype(str).str.strip()

    result = pd.DataFrame()
    result["dataset_id"] = master["dataset_id"].astype(str)
    result["ion_mode"] = master["ion_mode"].astype(str)
    if "target_id" in master.columns:
        result["target_id"] = master["target_id"].astype(str)
    result["mzmine_row_id"] = master["mzmine_row_id"].astype(str)
    result["target_mz"] = pd.to_numeric(master["target_mz"], errors="coerce")
    result["annotation_status"] = annotation_name.map(
        lambda value: "Annotated" if value else "Unannotated")
    result["annotation_name"] = annotation_name

    direct_fields = [
        ("annotation_formula", ["annotation_formula"]),
        ("annotation_adduct", ["annotation_adduct"]),
        ("annotation_score", ["annotation_score"]),
        ("annotation_compound_score", [
            "annotation_compound_annotation_score", "annotation_compound_score"]),
        ("annotation_database", ["annotation_database"]),
        ("annotation_mz_diff", ["annotation_mz_diff"]),
        ("annotation_mz_diff_ppm", ["annotation_mz_diff_ppm"]),
    ]
    for output_column, candidates in direct_fields:
        source_column = next((column for column in candidates
                              if column in master.columns), None)
        result[output_column] = (master[source_column].astype(str)
                                 if source_column else "")

    intensity_suffix = workbook_intensity_column_suffix(method)
    display_columns: list[str] = []
    for internal, token in zip(reference_ids + comparison_ids,
                               reference_tokens + comparison_tokens):
        output_column = f"{token}{intensity_suffix}"
        result[output_column] = pd.to_numeric(
            master[f"{internal}_overall_normalized_signal"], errors="coerce")
        display_columns.append(output_column)

    reference_token = safe_token(reference_label, "Reference")
    comparison_token = safe_token(comparison_label, "Comparison")
    reference_mean_column = f"mean_{reference_token}{intensity_suffix}"
    comparison_mean_column = f"mean_{comparison_token}{intensity_suffix}"
    fold_column = f"fold_change_{comparison_token}_vs_{reference_token}"
    log2_column = f"log2FC_{comparison_token}_vs_{reference_token}"
    result[reference_mean_column] = reference_mean
    result[comparison_mean_column] = comparison_mean
    result[fold_column] = calculated_fc
    result[log2_column] = calculated_log2
    result["GraphPad_label"] = [
        f"{name if name else 'Unannotated'} | m/z {mz:.4f}"
        if pd.notna(mz) else (name if name else "Unannotated")
        for name, mz in zip(annotation_name, result["target_mz"])
    ]

    result["__mode_order"] = pd.Categorical(
        result["ion_mode"].str.upper(), categories=["POS", "POSITIVE", "NEG", "NEGATIVE"],
        ordered=True)
    result["__row_numeric"] = pd.to_numeric(result["mzmine_row_id"], errors="coerce")
    result = result.sort_values(
        ["__mode_order", "__row_numeric", "target_mz"], kind="stable",
        na_position="last").drop(columns=["__mode_order", "__row_numeric"])
    result = result.reset_index(drop=True)

    duplicate_key = result[["dataset_id", "ion_mode", "mzmine_row_id"]].astype(str)
    if duplicate_key.duplicated().any():
        raise RuntimeError("All-features output contains duplicated feature identities.")

    report = {
        "source_rows": int(len(master)),
        "output_rows": int(len(result)),
        "annotated_rows": int((result["annotation_status"] == "Annotated").sum()),
        "unannotated_rows": int((result["annotation_status"] == "Unannotated").sum()),
        "nonblank_mzmine_scores": int(result["annotation_score"].astype(str).str.strip().ne("").sum()),
        "replicates_per_condition": len(reference_ids),
        "reference_condition": reference_label,
        "comparison_condition": comparison_label,
        "sample_columns": display_columns,
        "reference_mean_column": reference_mean_column,
        "comparison_mean_column": comparison_mean_column,
        "fold_change_column": fold_column,
        "log2fc_column": log2_column,
        "log2fc_audit_max_abs": audit_difference,
        "log2fc_audit_missing_mismatches": audit_missing,
    }
    return result, report


def style_sheet(worksheet) -> None:
    worksheet.freeze_panes = "A2"
    worksheet.auto_filter.ref = worksheet.dimensions
    for cell in worksheet[1]:
        cell.font = Font(bold=True)
        cell.fill = PatternFill("solid", fgColor="D9EAF7")
        cell.alignment = Alignment(wrap_text=True)
    for index, column in enumerate(worksheet.columns, 1):
        cells = list(column)
        width = min(max((len(str(cell.value)) if cell.value is not None else 0)
                        for cell in cells[:200]) + 2, 45)
        worksheet.column_dimensions[get_column_letter(index)].width = max(width, 10)


def append_dataframe(worksheet, frame: pd.DataFrame) -> None:
    worksheet.append(list(frame.columns))
    for row in frame.itertuples(index=False, name=None):
        worksheet.append([None if pd.isna(value) else value for value in row])
    style_sheet(worksheet)
    headers = {cell.value: cell.column for cell in worksheet[1]}
    if "target_mz" in headers:
        for cell in worksheet.iter_cols(min_col=headers["target_mz"],
                                        max_col=headers["target_mz"], min_row=2):
            for item in cell:
                item.number_format = "0.0000"
    for name, column in headers.items():
        if (str(name).endswith(workbook_intensity_column_suffix(
                "spectrum_sum")) or
                str(name).endswith(workbook_intensity_column_suffix("raw")) or
                str(name).endswith(workbook_intensity_column_suffix("rms")) or
                str(name).endswith(workbook_intensity_column_suffix("median")) or
                str(name).startswith("fold_change_") or
                str(name).startswith("log2FC_")):
            for cells in worksheet.iter_cols(min_col=column, max_col=column, min_row=2):
                for item in cells:
                    item.number_format = "0.000000"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    arguments = parser.parse_args()

    request_path = Path(arguments.request)
    report_path = Path(arguments.report)
    request = json.loads(request_path.read_text(encoding="utf-8"))
    report: dict[str, Any] = {
        "schema_version": "1.0.0",
        "workflow_id": request.get("workflow_id"),
        "status": "FAIL",
        "sheet_name": None,
    }
    try:
        if request.get("analysis_type") not in (None, "compare_two_groups"):
            raise RuntimeError("The all-features sheet applies only to two-group comparison workflows.")
        method = resolve_method(request)
        intensity_sheet, all_features_sheet = workbook_sheet_names(method)
        report.update(primary_normalization_method=method,
                      expected_primary_intensity_sheet=intensity_sheet,
                      sheet_name=all_features_sheet)
        source_item = request.get("source_workbook", {})
        source = Path(source_item.get("path", ""))
        if not source.is_file():
            raise RuntimeError("Validated source workbook is missing.")
        expected_source_hash = str(source_item.get("sha256", "")).upper()
        actual_source_hash = sha256(source)
        if expected_source_hash and actual_source_hash != expected_source_hash:
            raise RuntimeError("Source workbook SHA-256 identity failed.")

        output_directory = Path(request["output_working_directory"])
        output_directory.mkdir(parents=True, exist_ok=False)
        compatible_source, repaired_source = openpyxl_compatible_source(
            source, output_directory)
        original_digest = workbook_content_digest(
            compatible_source, excluded={all_features_sheet})
        all_features, details = build_all_features(
            request, compatible_source, method)

        filename = request.get("output_filename") or source.name
        output = output_directory / filename
        shutil.copy2(compatible_source, output)

        workbook = load_workbook(output)
        if all_features_sheet in workbook.sheetnames:
            del workbook[all_features_sheet]
        if intensity_sheet not in workbook.sheetnames:
            raise RuntimeError(
                f"Primary intensity sheet is missing for normalization method "
                f"'{method}': expected '{intensity_sheet}'.")
        insert_at = workbook.sheetnames.index(intensity_sheet) + 1
        worksheet = workbook.create_sheet(all_features_sheet, insert_at)
        append_dataframe(worksheet, all_features)
        if workbook.calculation is None:
            workbook.calculation = CalcProperties()
        workbook.calculation.fullCalcOnLoad = True
        workbook.calculation.forceFullCalc = True
        workbook.calculation.calcMode = "auto"
        workbook.save(output)

        updated_digest = workbook_content_digest(
            output, excluded={all_features_sheet})
        if original_digest != updated_digest:
            raise RuntimeError("An existing workbook sheet changed while adding the all-features sheet.")

        checked = load_workbook(output, read_only=True, data_only=False)
        expected_sheets = [
            "Mean_Log2FC", "All_Pixels", intensity_sheet,
            all_features_sheet, "QC", "README",
        ]
        try:
            if checked.sheetnames != expected_sheets:
                raise RuntimeError(
                    "Extended comparison workbook sheet order failed validation: " +
                    ", ".join(checked.sheetnames)
                )
            worksheet = checked[all_features_sheet]
            if worksheet.max_row - 1 != len(all_features):
                raise RuntimeError("All-features worksheet row retention failed.")
            headers = [cell.value for cell in next(worksheet.iter_rows(min_row=1, max_row=1))]
            required_headers = {
                "dataset_id", "ion_mode", "mzmine_row_id", "target_mz",
                "annotation_status", "annotation_name", "annotation_score",
                details["reference_mean_column"], details["comparison_mean_column"],
                details["fold_change_column"], details["log2fc_column"],
            }
            missing = sorted(required_headers - set(headers))
            if missing:
                raise RuntimeError("All-features worksheet is missing columns: " +
                                   ", ".join(missing))
        finally:
            checked.close()

        report.update(
            status="PASS",
            final_workbook={
                "path": str(output),
                "sha256": sha256(output),
                "bytes": output.stat().st_size,
            },
            source_workbook={
                "path": str(source),
                "sha256": actual_source_hash,
                "existing_sheet_content_preserved": True,
                "dangling_drawing_references_removed": repaired_source,
            },
            validation={"status": "PASS", **details},
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 0
    except Exception as exception:
        report.update(
            status="FAIL",
            error=str(exception),
            recommended_correction=(
                "Inspect the preserved all-features export request, source workbook, "
                "and annotated-master artifacts."
            ),
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
