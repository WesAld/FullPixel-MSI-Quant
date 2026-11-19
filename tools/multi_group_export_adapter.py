#!/usr/bin/env python3
"""Create a validated six-sheet workbook for one-reference multi-group MSI comparisons.

Every comparison is descriptive and is calculated upstream by the unchanged
frozen Script 03. This adapter independently audits those values, keeps the
annotation-focused publication sheets, and includes every registered feature
in All_Features_TIC_Normalized with the original MZmine annotation score.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import sys
from pathlib import Path
from typing import Any, Iterable

import pandas as pd
from openpyxl import Workbook, load_workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter
sys.path.insert(0, str(Path(__file__).resolve().parent))
from normalization_routing import (provenance, resolve_method,
                                   workbook_intensity_column_suffix,
                                   workbook_sheet_names)

SHEETS = [
    "Mean_Log2FC",
    "All_Pixels",
    "TIC_normalized_intensity",
    "All_Features_TIC_Normalized",
    "QC",
    "README",
]

ANNOTATION_COLUMNS = [
    "annotation_status",
    "annotation_name",
    "annotation_formula",
    "annotation_adduct",
    "annotation_score",
    "annotation_compound_score",
    "annotation_database",
    "annotation_method",
    "annotation_comment",
    "annotation_mz",
    "annotation_mz_diff",
    "annotation_mz_diff_ppm",
    "annotation_inchi",
    "annotation_inchi_key",
    "annotation_smiles",
    "annotation_isomeric_smiles",
    "annotation_classyfire_superclass",
    "annotation_classyfire_class",
    "annotation_classyfire_subclass",
    "annotation_classyfire_parent",
    "annotation_pubchem_cid",
]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest().upper()


def safe_token(value: Any, fallback: str = "Value") -> str:
    token = re.sub(r"[^A-Za-z0-9]+", "_", str(value or "")).strip("_")
    return (token or fallback)[:64]


def nonblank(value: Any) -> str:
    if value is None:
        return ""
    text = str(value).strip()
    if not text or text.lower() in {"nan", "none", "na"}:
        return ""
    return text


def numeric(frame: pd.DataFrame, columns: Iterable[str]) -> pd.DataFrame:
    return frame[list(columns)].apply(pd.to_numeric, errors="coerce")


def finite_mean(frame: pd.DataFrame, columns: list[str]) -> pd.Series:
    values = numeric(frame, columns)
    # Frozen Script 03 requires all configured biological replicates for
    # condition statistics. min_count reproduces that no-imputation rule.
    return values.mean(axis=1, skipna=False)


def ratio(numerator: pd.Series, denominator: pd.Series) -> pd.Series:
    numerator = pd.to_numeric(numerator, errors="coerce")
    denominator = pd.to_numeric(denominator, errors="coerce")
    result = pd.Series(float("nan"), index=numerator.index, dtype="float64")
    valid = (
        numerator.notna()
        & denominator.notna()
        & (numerator > 0)
        & (denominator > 0)
    )
    result.loc[valid] = numerator.loc[valid] / denominator.loc[valid]
    return result


def log2_ratio(numerator: pd.Series, denominator: pd.Series) -> pd.Series:
    values = ratio(numerator, denominator)
    valid = values.notna() & (values > 0)
    result = pd.Series(float("nan"), index=values.index, dtype="float64")
    result.loc[valid] = values.loc[valid].map(math.log2)
    return result


def max_difference(observed: pd.Series, expected: pd.Series) -> tuple[float, int]:
    left = pd.to_numeric(observed, errors="coerce")
    right = pd.to_numeric(expected, errors="coerce")
    both = left.notna() & right.notna()
    missing_disagreement = int((left.isna() != right.isna()).sum())
    differences = (left[both] - right[both]).abs()
    return (
        float(differences.max()) if len(differences) else 0.0,
        missing_disagreement,
    )


def require_columns(frame: pd.DataFrame, columns: Iterable[str], context: str) -> None:
    missing = [column for column in columns if column not in frame.columns]
    if missing:
        raise RuntimeError(
            f"{context} is missing required columns: "
            + ", ".join(missing[:30])
        )


def select_columns(frame: pd.DataFrame, predicate) -> list[str]:
    return [column for column in frame.columns if predicate(str(column))]


def unique_order(values: Iterable[str]) -> list[str]:
    result: list[str] = []
    seen: set[str] = set()
    for value in values:
        if value not in seen:
            seen.add(value)
            result.append(value)
    return result


def style_sheet(worksheet) -> None:
    worksheet.freeze_panes = "A2"
    worksheet.auto_filter.ref = worksheet.dimensions
    for cell in worksheet[1]:
        cell.font = Font(bold=True)
        cell.fill = PatternFill("solid", fgColor="D9EAF7")
        cell.alignment = Alignment(wrap_text=True)
    for index, column in enumerate(worksheet.columns, 1):
        sampled = list(column)[:200]
        width = max(
            [len(str(cell.value)) if cell.value is not None else 0
             for cell in sampled] + [8]
        )
        worksheet.column_dimensions[get_column_letter(index)].width = min(
            max(width + 2, 10),
            45,
        )


def write_frame(worksheet, frame: pd.DataFrame) -> None:
    worksheet.append(list(frame.columns))
    for row in frame.itertuples(index=False, name=None):
        worksheet.append([
            None if pd.isna(value) else value
            for value in row
        ])
    style_sheet(worksheet)


def read_masters(request: dict) -> tuple[pd.DataFrame, list[dict], int]:
    masters = request.get("masters", [])
    if not masters:
        raise RuntimeError(
            "At least one validated multi-group annotated master is required."
        )
    frames: list[pd.DataFrame] = []
    source_rows = 0
    seen_modes: set[str] = set()
    for item in masters:
        mode = str(item.get("ion_mode", "")).upper().strip()
        path = Path(str(item.get("path", "")))
        expected_hash = str(item.get("sha256", "")).upper().strip()
        if mode not in {"POS", "NEG"}:
            raise RuntimeError(f"Unsupported ion mode in master: {mode}")
        if mode in seen_modes:
            raise RuntimeError(f"Duplicate {mode} annotated master.")
        seen_modes.add(mode)
        if not path.is_file():
            raise RuntimeError(f"Annotated master is missing: {path}")
        if not expected_hash or sha256(path) != expected_hash:
            raise RuntimeError(f"Annotated master identity failed: {path}")
        frame = pd.read_csv(path, low_memory=False)
        if "ion_mode" not in frame.columns:
            frame.insert(min(1, len(frame.columns)), "ion_mode", mode)
        else:
            frame["ion_mode"] = mode
        frames.append(frame)
        source_rows += len(frame)
    combined = pd.concat(frames, ignore_index=True, sort=False)
    if len(combined) != source_rows:
        raise RuntimeError("Combined annotated-master row retention failed.")
    return combined, masters, source_rows


def prepare_metadata(master: pd.DataFrame) -> tuple[pd.DataFrame, list[str]]:
    master = master.copy()
    required_base = [
        "dataset_id",
        "ion_mode",
        "mzmine_row_id",
        "target_id",
        "target_mz",
    ]
    require_columns(master, required_base, "Multi-group annotated master")
    for column in ANNOTATION_COLUMNS:
        if column not in master.columns:
            master[column] = ""
    master["annotation_name"] = (
        master["annotation_name"].fillna("").astype(str)
    )
    master["annotation_status"] = master["annotation_name"].map(
        lambda value: "Annotated" if nonblank(value) else "Unannotated"
    )

    def label(row: pd.Series) -> str:
        name = nonblank(row.get("annotation_name")) or "Unannotated"
        mz = pd.to_numeric(
            pd.Series([row.get("target_mz")]),
            errors="coerce",
        ).iloc[0]
        mz_text = f"{float(mz):.4f}" if pd.notna(mz) else nonblank(
            row.get("target_mz")
        )
        return f"{name} | m/z {mz_text}"

    master["GraphPad_label"] = master.apply(label, axis=1)
    metadata = required_base + ANNOTATION_COLUMNS + ["GraphPad_label"]
    return master, metadata


def comparison_definitions(metadata: dict) -> list[dict]:
    comparisons = metadata.get("comparisons", [])
    if not isinstance(comparisons, list) or len(comparisons) < 2:
        raise RuntimeError(
            "Multi-group export requires at least two configured comparisons."
        )
    result: list[dict] = []
    reference_ids: set[str] = set()
    comparison_ids: set[str] = set()
    for index, item in enumerate(comparisons, start=1):
        reference_id = nonblank(item.get("reference_condition_id"))
        comparison_id = nonblank(item.get("comparison_condition_id"))
        reference_label = (
            nonblank(item.get("reference_display_label"))
            or nonblank(item.get("reference_display_id"))
            or reference_id
        )
        comparison_label = (
            nonblank(item.get("comparison_display_label"))
            or nonblank(item.get("comparison_display_id"))
            or comparison_id
        )
        reference_token = safe_token(
            item.get("reference_token") or reference_label,
            "Reference",
        )
        comparison_token = safe_token(
            item.get("comparison_token") or comparison_label,
            f"Treatment_{index}",
        )
        if not reference_id or not comparison_id or reference_id == comparison_id:
            raise RuntimeError("Comparison condition identifiers are invalid.")
        reference_ids.add(reference_id)
        if comparison_id in comparison_ids:
            raise RuntimeError("A comparison condition was selected more than once.")
        comparison_ids.add(comparison_id)
        result.append({
            "comparison_id": nonblank(item.get("comparison_id"))
                or f"comparison_{index}",
            "reference_condition_id": reference_id,
            "comparison_condition_id": comparison_id,
            "reference_label": reference_label,
            "comparison_label": comparison_label,
            "reference_token": reference_token,
            "comparison_token": comparison_token,
            "token": f"{comparison_token}_vs_{reference_token}",
        })
    if len(reference_ids) != 1:
        raise RuntimeError(
            "All multi-group comparisons must use the same reference condition."
        )
    return result


def configured_sample_tokens(metadata: dict) -> list[str]:
    """Return the exact sample tokens used by the summary adapter."""
    mappings = metadata.get("sample_mappings", [])
    by_internal: dict[str, str] = {}

    for mapping in mappings:
        internal = nonblank(mapping.get("internal_value"))
        if not internal:
            continue

        label = (
            nonblank(mapping.get("user_facing_label"))
            or nonblank(mapping.get("user_facing_value"))
            or internal
        )
        token = safe_token(label, internal)

        # POS and NEG mappings may repeat the same biological sample.
        # Retain one canonical token per internal biological sample.
        if internal not in by_internal:
            by_internal[internal] = token
        elif by_internal[internal] != token:
            raise RuntimeError(
                "A biological sample has inconsistent display tokens "
                "across ion modes."
            )

    if not by_internal:
        return []

    token_counts: dict[str, int] = {}
    for token in by_internal.values():
        token_counts[token] = token_counts.get(token, 0) + 1

    return [
        (
            token
            if token_counts[token] == 1
            else safe_token(f"{token}_{internal}", internal)
        )
        for internal, token in by_internal.items()
    ]


def exact_sample_columns(
    master: pd.DataFrame,
    metadata: dict,
    suffix: str,
) -> list[str]:
    """Select registered sample columns, excluding Script 03 aggregates."""
    tokens = configured_sample_tokens(metadata)

    if tokens:
        expected = [f"{token}{suffix}" for token in tokens]
        missing = [
            column for column in expected
            if column not in master.columns
        ]
        if missing:
            raise RuntimeError(
                "Registered individual sample columns are missing for "
                f"{suffix}: " + ", ".join(missing[:20])
            )
        return expected

    # Backward-compatible fallback for older metadata without mappings.
    aggregate_prefixes = (
        "mean_",
        "median_",
        "sd_",
        "min_",
        "max_",
    )
    return [
        column
        for column in master.columns
        if str(column).endswith(suffix)
        and not str(column).startswith(aggregate_prefixes)
    ]


def sample_and_group_columns(
    master: pd.DataFrame,
    metadata: dict,
    comparisons: list[dict],
) -> dict[str, list[str]]:
    columns = list(master.columns)
    groups = unique_order(
        [comparisons[0]["reference_token"]]
        + [item["comparison_token"] for item in comparisons]
    )

    overall_samples = exact_sample_columns(
        master,
        metadata,
        "_overall_normalized_signal",
    )
    local_samples = exact_sample_columns(
        master,
        metadata,
        "_local_strong_pixel_intensity",
    )
    detection_samples = exact_sample_columns(
        master,
        metadata,
        "_conservative_detection_fraction",
    )
    count_samples = exact_sample_columns(
        master,
        metadata,
        "_conservative_detected_pixel_count",
    )
    if min(
        len(overall_samples),
        len(local_samples),
        len(detection_samples),
        len(count_samples),
    ) < 6:
        raise RuntimeError(
            "At least six individual sample columns are required."
        )
    counts = {
        len(overall_samples),
        len(local_samples),
        len(detection_samples),
        len(count_samples),
    }
    if len(counts) != 1:
        raise RuntimeError(
            "Individual sample-column counts disagree across readouts: "
            f"overall={len(overall_samples)}, "
            f"local={len(local_samples)}, "
            f"detection_fraction={len(detection_samples)}, "
            f"detection_count={len(count_samples)}."
        )

    expected = {
        "overall_means": [
            f"mean_{group}_overall_signal" for group in groups
        ],
        "local_means": [
            f"mean_{group}_local_strong_pixel_intensity" for group in groups
        ],
        "detection_means": [
            f"mean_{group}_detection_fraction" for group in groups
        ],
        "overall_fold": [
            f"fold_change_overall_{item['comparison_token']}"
            f"_vs_{item['reference_token']}"
            for item in comparisons
        ],
        "overall_log2": [
            f"log2FC_overall_{item['comparison_token']}"
            f"_vs_{item['reference_token']}"
            for item in comparisons
        ],
        "local_fold": [
            f"local_intensity_fold_change_{item['comparison_token']}"
            f"_vs_{item['reference_token']}"
            for item in comparisons
        ],
        "local_log2": [
            f"local_intensity_log2FC_{item['comparison_token']}"
            f"_vs_{item['reference_token']}"
            for item in comparisons
        ],
        "detection_difference": [
            f"detection_fraction_difference_{item['comparison_token']}"
            f"_minus_{item['reference_token']}"
            for item in comparisons
        ],
        "detection_ratio": [
            f"detection_fraction_ratio_{item['comparison_token']}"
            f"_vs_{item['reference_token']}"
            for item in comparisons
        ],
    }
    for label, required in expected.items():
        require_columns(master, required, label)

    return {
        "groups": groups,
        "overall_samples": overall_samples,
        "local_samples": local_samples,
        "detection_samples": detection_samples,
        "count_samples": count_samples,
        **expected,
    }


def audit_values(
    master: pd.DataFrame,
    comparisons: list[dict],
    columns: dict[str, list[str]],
) -> tuple[list[dict], float]:
    audits: list[dict] = []
    maximum = 0.0
    for index, comparison in enumerate(comparisons):
        reference = comparison["reference_token"]
        treatment = comparison["comparison_token"]

        observed_overall_fold = master[columns["overall_fold"][index]]
        observed_overall_log2 = master[columns["overall_log2"][index]]
        observed_local_fold = master[columns["local_fold"][index]]
        observed_local_log2 = master[columns["local_log2"][index]]
        observed_detection = master[
            columns["detection_difference"][index]
        ]

        expected_overall_fold = ratio(
            master[f"mean_{treatment}_overall_signal"],
            master[f"mean_{reference}_overall_signal"],
        )
        expected_overall_log2 = log2_ratio(
            master[f"mean_{treatment}_overall_signal"],
            master[f"mean_{reference}_overall_signal"],
        )
        expected_local_fold = ratio(
            master[f"mean_{treatment}_local_strong_pixel_intensity"],
            master[f"mean_{reference}_local_strong_pixel_intensity"],
        )
        expected_local_log2 = log2_ratio(
            master[f"mean_{treatment}_local_strong_pixel_intensity"],
            master[f"mean_{reference}_local_strong_pixel_intensity"],
        )
        expected_detection = (
            pd.to_numeric(
                master[f"mean_{treatment}_detection_fraction"],
                errors="coerce",
            )
            - pd.to_numeric(
                master[f"mean_{reference}_detection_fraction"],
                errors="coerce",
            )
        )

        checks = [
            ("overall_fold", observed_overall_fold, expected_overall_fold),
            ("overall_log2", observed_overall_log2, expected_overall_log2),
            ("local_fold", observed_local_fold, expected_local_fold),
            ("local_log2", observed_local_log2, expected_local_log2),
            ("detection_difference", observed_detection, expected_detection),
        ]
        for metric, observed, expected in checks:
            difference, missing = max_difference(observed, expected)
            maximum = max(maximum, difference)
            status = (
                "PASS"
                if difference <= 1e-10 and missing == 0
                else "FAIL"
            )
            audits.append({
                "comparison": comparison["token"],
                "metric": metric,
                "status": status,
                "maximum_absolute_difference": difference,
                "missingness_disagreements": missing,
            })
    failures = [item for item in audits if item["status"] != "PASS"]
    if failures:
        raise RuntimeError(
            "Independent multi-group numerical audit failed: "
            + "; ".join(
                f"{item['comparison']} {item['metric']}"
                for item in failures[:10]
            )
        )
    return audits, maximum


def condition_sample_counts(metadata: dict) -> dict[str, int]:
    mappings = metadata.get("sample_mappings", [])
    seen: set[tuple[str, str]] = set()
    counts: dict[str, int] = {}
    labels: dict[str, str] = {}
    for item in mappings:
        internal_sample = nonblank(item.get("internal_value"))
        condition = (
            nonblank(item.get("internal_condition_id"))
            or nonblank(item.get("original_condition_id"))
        )
        mode = nonblank(item.get("ion_mode")).upper()
        if not internal_sample or not condition:
            continue
        # The same biological sample is registered once in each selected ion
        # mode. Count it once for the README replicate summary.
        key = (condition, internal_sample)
        if key in seen:
            continue
        seen.add(key)
        counts[condition] = counts.get(condition, 0) + 1
        labels[condition] = (
            nonblank(item.get("original_condition_id"))
            or condition
        )
    return {
        labels.get(condition, condition): count
        for condition, count in counts.items()
    }


def build_workbook_tables(
    master: pd.DataFrame,
    metadata_columns: list[str],
    comparisons: list[dict],
    columns: dict[str, list[str]],
    method: str = "spectrum_sum",
) -> tuple[dict[str, pd.DataFrame], list[dict]]:
    annotated = master[
        master["annotation_status"].eq("Annotated")
    ].copy()

    overall_source = (
        metadata_columns
        + columns["overall_samples"]
        + columns["overall_means"]
        + columns["overall_fold"]
        + columns["overall_log2"]
    )
    overall = annotated[overall_source].copy()
    overall_rename: dict[str, str] = {}
    for index, comparison in enumerate(comparisons):
        overall_rename[columns["overall_fold"][index]] = (
            f"fold_change_{comparison['comparison_token']}"
            f"_vs_{comparison['reference_token']}"
        )
        overall_rename[columns["overall_log2"][index]] = (
            f"log2FC_{comparison['comparison_token']}"
            f"_vs_{comparison['reference_token']}"
        )
    overall.rename(columns=overall_rename, inplace=True)

    all_pixels_source = (
        metadata_columns
        + columns["detection_samples"]
        + columns["count_samples"]
        + columns["detection_means"]
        + columns["detection_difference"]
        + columns["detection_ratio"]
    )
    all_pixels = annotated[all_pixels_source].copy()
    for column in (
        columns["detection_samples"]
        + columns["detection_means"]
        + columns["detection_difference"]
    ):
        all_pixels[column] = (
            pd.to_numeric(all_pixels[column], errors="coerce") * 100.0
        )
    all_pixels_rename: dict[str, str] = {}
    for index, comparison in enumerate(comparisons):
        all_pixels_rename[columns["detection_difference"][index]] = (
            f"ion_positive_pixel_change_"
            f"{comparison['comparison_token']}_minus_"
            f"{comparison['reference_token']}_pp"
        )
    for group, column in zip(columns["groups"], columns["detection_means"]):
        all_pixels_rename[column] = (
            f"mean_{group}_detection_fraction_percent"
        )
    all_pixels.rename(columns=all_pixels_rename, inplace=True)

    local_source = (
        metadata_columns
        + columns["local_samples"]
        + columns["local_means"]
        + columns["local_fold"]
        + columns["local_log2"]
    )
    local = annotated[local_source].copy()
    local_rename: dict[str, str] = {}
    for group, column in zip(columns["groups"], columns["local_means"]):
        local_rename[column] = f"mean_{group}_local_intensity"
    for index, comparison in enumerate(comparisons):
        local_rename[columns["local_fold"][index]] = (
            f"local_intensity_fold_change_"
            f"{comparison['comparison_token']}_vs_"
            f"{comparison['reference_token']}"
        )
        local_rename[columns["local_log2"][index]] = (
            f"local_intensity_log2FC_"
            f"{comparison['comparison_token']}_vs_"
            f"{comparison['reference_token']}"
        )
    local.rename(columns=local_rename, inplace=True)

    all_features_source = (
        metadata_columns
        + columns["overall_samples"]
        + columns["overall_means"]
        + columns["overall_fold"]
        + columns["overall_log2"]
    )
    all_features = master[all_features_source].copy()
    all_features_rename: dict[str, str] = {}
    intensity_suffix = workbook_intensity_column_suffix(method)
    for column in columns["overall_samples"]:
        all_features_rename[column] = (
            column.removesuffix("_overall_normalized_signal")
            + intensity_suffix
        )
    for group, column in zip(columns["groups"], columns["overall_means"]):
        all_features_rename[column] = (
            f"mean_{group}{intensity_suffix}"
        )
    for index, comparison in enumerate(comparisons):
        all_features_rename[columns["overall_fold"][index]] = (
            f"fold_change_{comparison['comparison_token']}"
            f"_vs_{comparison['reference_token']}"
        )
        all_features_rename[columns["overall_log2"][index]] = (
            f"log2FC_{comparison['comparison_token']}"
            f"_vs_{comparison['reference_token']}"
        )
    all_features.rename(columns=all_features_rename, inplace=True)

    intensity_sheet, all_features_sheet = workbook_sheet_names(method)
    return {
        "Mean_Log2FC": overall,
        "All_Pixels": all_pixels,
        intensity_sheet: local,
        all_features_sheet: all_features,
    }, []


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()

    request_path = Path(args.request)
    report_path = Path(args.report)
    request = json.loads(request_path.read_text(encoding="utf-8"))
    report: dict = {
        "schema_version": "1.0.0",
        "workflow_id": request.get("workflow_id"),
        "status": "FAIL",
    }

    try:
        if request.get("analysis_type") != "compare_multiple_groups":
            raise RuntimeError(
                "Expected compare_multiple_groups analysis type."
            )

        method = resolve_method(request)
        normalization = provenance(method, request.get("display_metadata", {}).get("normalization", {}).get("normalization_scale", 1000000))
        intensity_sheet, all_features_sheet = workbook_sheet_names(method)
        sheets = ["Mean_Log2FC", "All_Pixels", intensity_sheet, all_features_sheet, "QC", "README"]
        master, masters, source_rows = read_masters(request)
        master, metadata_columns = prepare_metadata(master)
        display_metadata = request.get("display_metadata", {})
        comparisons = comparison_definitions(display_metadata)
        columns = sample_and_group_columns(
            master,
            display_metadata,
            comparisons,
        )
        audits, maximum_difference_value = audit_values(
            master,
            comparisons,
            columns,
            method,
        )
        tables, _ = build_workbook_tables(
            master,
            metadata_columns,
            comparisons,
            columns,
            method,
        )

        annotation_status = master["annotation_status"]
        annotated_rows = int(annotation_status.eq("Annotated").sum())
        unannotated_rows = int(annotation_status.eq("Unannotated").sum())
        score_rows = int(
            master["annotation_score"]
            .fillna("")
            .astype(str)
            .str.strip()
            .ne("")
            .sum()
        )

        replicate_counts = condition_sample_counts(display_metadata)
        treatment_labels = [
            item["comparison_label"] for item in comparisons
        ]
        comparison_text = "; ".join(
            f"{item['comparison_label']} vs {item['reference_label']}"
            for item in comparisons
        )
        ion_modes = ", ".join(
            unique_order(str(item.get("ion_mode", "")).upper()
                         for item in masters)
        )
        qc_rows: list[list[Any]] = [
            ["source_rows_retained", "PASS", len(master), source_rows],
            [
                "multi_group_comparison_count",
                "PASS",
                len(comparisons),
                "at least 2",
            ],
            [
                "individual_sample_column_count",
                "PASS",
                len(columns["overall_samples"]),
                "at least 6",
            ],
            [
                "condition_count",
                "PASS",
                len(columns["groups"]),
                "at least 3",
            ],
            [
                "all_features_retained",
                "PASS",
                len(tables[all_features_sheet]),
                source_rows,
            ],
            [
                "annotated_features",
                "PASS",
                annotated_rows,
                "reported without filtering all-features sheet",
            ],
            [
                "unannotated_features",
                "PASS",
                unannotated_rows,
                "retained",
            ],
            [
                "mzmine_score_rows_retained",
                "PASS",
                score_rows,
                "all supplied scores retained",
            ],
            [
                "maximum_numerical_audit_difference",
                "PASS",
                maximum_difference_value,
                "<= 1e-10",
            ],
            [
                "validated_workflow_output",
                "PASS",
                "Validated final workbook",
                "validated export contract",
            ],
        ]
        for audit in audits:
            qc_rows.append([
                f"{audit['comparison']}__{audit['metric']}",
                audit["status"],
                audit["maximum_absolute_difference"],
                "<= 1e-10 and identical missingness",
            ])
        qc = pd.DataFrame(
            qc_rows,
            columns=["check", "status", "observed", "expected"],
        )

        reference_label = comparisons[0]["reference_label"]
        readme = pd.DataFrame({
            "Field": [
                "Project",
                "Exposure",
                "Analysis type",
                "Reference condition",
                "Comparison conditions",
                "Comparisons",
                "Ion modes",
                "Replicates per condition",
                "Application version",
                "Primary normalization method",
                "Normalization definition version",
                "Normalization",
                "Detection threshold",
                "Statistical interpretation",
                "Annotation handling",
                "MZmine score handling",
                "Validation status",
            ],
            "Value": [
                display_metadata.get("project_name", ""),
                request.get("exposure_label", ""),
                "Compare multiple groups",
                reference_label,
                ", ".join(treatment_labels),
                comparison_text,
                ion_modes,
                "; ".join(
                    f"{label}={count}"
                    for label, count in replicate_counts.items()
                ),
                "2.0.2",
                method,
                normalization["normalization_definition_version"],
                normalization["normalization_definition"],
                (
                    "Conservative raw intensity >= 1000; "
                    "minimum 10 pixels"
                ),
                (
                    "Descriptive reference-versus-treatment readouts; "
                    "no p-values, pairing, pseudocount, or imputation"
                ),
                (
                    "Annotation-focused sheets retain annotated rows; "
                    f"{all_features_sheet} retains every feature"
                ),
                (
                    "Original canonical MZmine annotation scores and "
                    "additional annotation fields are retained"
                ),
                "Validated workflow output",
            ],
        })
        tables["QC"] = qc
        tables["README"] = readme

        output_directory = Path(request["output_working_directory"])
        if output_directory.exists() and any(output_directory.iterdir()):
            raise RuntimeError(
                "Multi-group export workspace must be absent or empty."
            )
        output_directory.mkdir(parents=True, exist_ok=False)

        project_token = safe_token(
            display_metadata.get("project_name"),
            "Project",
        )
        exposure_token = safe_token(
            request.get("exposure_label"),
            "Exposure",
        )
        filename = (
            f"{project_token}_{exposure_token}_MALDIPixelQuant_"
            "Multi_Group_Final_Results.xlsx"
        )
        workbook_path = output_directory / filename

        workbook = Workbook()
        workbook.remove(workbook.active)
        for sheet in sheets:
            worksheet = workbook.create_sheet(sheet)
            write_frame(worksheet, tables[sheet])
        workbook.calculation.fullCalcOnLoad = True
        workbook.calculation.forceFullCalc = True
        workbook.calculation.calcMode = "auto"
        workbook.save(workbook_path)

        checked = load_workbook(
            workbook_path,
            read_only=True,
            data_only=False,
        )
        checked_sheets = checked.sheetnames
        sheet_rows = {
            worksheet.title: max(0, worksheet.max_row - 1)
            for worksheet in checked.worksheets
        }
        checked.close()
        if checked_sheets != sheets:
            raise RuntimeError("Final workbook sheet contract failed.")
        if any(
            sheet_rows.get(sheet, 0) < 1
            for sheet in (
                all_features_sheet,
                "QC",
                "README",
            )
        ):
            raise RuntimeError(
                "Final workbook is missing required all-feature or audit rows."
            )
        if sheet_rows[all_features_sheet] != source_rows:
            raise RuntimeError(
                f"{all_features_sheet} row retention failed."
            )
        if sheet_rows["Mean_Log2FC"] != annotated_rows:
            raise RuntimeError(
                "Annotation-focused row retention failed."
            )

        report.update(
            status="PASS",
            final_workbook={
                "path": str(workbook_path),
                "sha256": sha256(workbook_path),
                "bytes": workbook_path.stat().st_size,
            },
            source_rows=source_rows,
            annotated_rows=annotated_rows,
            unannotated_rows=unannotated_rows,
            mzmine_score_rows=score_rows,
            condition_count=len(columns["groups"]),
            comparison_count=len(comparisons),
            sample_count=len(columns["overall_samples"]),
            sheet_rows=sheet_rows,
            numerical_audit={
                "status": "PASS",
                "maximum_absolute_difference":
                    maximum_difference_value,
                "records": audits,
            },
            workbook_contract={
                "status": "PASS",
                "sheet_names": sheets,
                "all_features_retained": True,
                "annotation_scores_retained": True,
                "inferential_statistics": False,
            },
            normalization=normalization,
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(
            json.dumps(report, indent=2),
            encoding="utf-8",
        )
        return 0
    except Exception as exception:
        report.update(
            status="FAIL",
            error=str(exception),
            recommended_correction=(
                "Inspect the preserved multi-group export request and "
                "hash-verified annotated masters."
            ),
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(
            json.dumps(report, indent=2),
            encoding="utf-8",
        )
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
