#!/usr/bin/env python3
"""Merge canonical MZmine annotation metadata into a multi-group quantitative summary."""
from __future__ import annotations

import argparse
import hashlib
import json
import math
from datetime import datetime
from pathlib import Path

import pandas as pd

REQUIRED_ANNOTATION_COLUMNS = [
    "id",
    "mz",
    "preferred_annotation:compound_name",
    "preferred_annotation:mol_formula",
    "preferred_annotation:adduct",
    "preferred_annotation:score",
    "preferred_annotation:precursor_mz",
    "preferred_annotation:annotation_method",
    "preferred_annotation:comment",
    "preferred_annotation:neutral_mass",
    "preferred_annotation:inchi",
    "preferred_annotation:inchi_key",
    "preferred_annotation:isomeric_smiles",
    "preferred_annotation:smiles",
    "preferred_annotation:classyfire_superclass",
    "preferred_annotation:classyfire_class",
    "preferred_annotation:classyfire_subclass",
    "preferred_annotation:classyfire_parent",
    "preferred_annotation:pubchem_cid",
    "preferred_annotation:compound_annotation_score",
    "preferred_annotation:database_name",
    "preferred_annotation:mz_diff",
    "preferred_annotation:mz_diff_ppm",
]


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()


def canonical_id(value: object) -> str:
    text = str(value if value is not None else "").strip()
    if not text or text.lower() == "nan":
        return ""
    try:
        number = float(text)
        if math.isfinite(number) and number.is_integer():
            return str(int(number))
    except (TypeError, ValueError):
        pass
    return text


def equal_series(left: pd.Series, right: pd.Series) -> tuple[bool, float]:
    """Compare aligned values without subtracting Boolean columns."""
    if len(left) != len(right):
        return False, 0.0

    lvalue = left.reset_index(drop=True)
    rvalue = right.reset_index(drop=True)

    left_missing = lvalue.isna()
    right_missing = rvalue.isna()
    if bool((left_missing ^ right_missing).any()):
        return False, 0.0

    present = ~(left_missing | right_missing)
    if not bool(present.any()):
        return True, 0.0

    if (
        pd.api.types.is_bool_dtype(lvalue.dtype)
        or pd.api.types.is_bool_dtype(rvalue.dtype)
    ):
        left_bool = (
            lvalue[present].astype(str).str.strip().str.lower()
        )
        right_bool = (
            rvalue[present].astype(str).str.strip().str.lower()
        )
        return bool((left_bool == right_bool).all()), 0.0

    lnum = pd.to_numeric(lvalue[present], errors="coerce")
    rnum = pd.to_numeric(rvalue[present], errors="coerce")
    numeric = lnum.notna() & rnum.notna()
    max_abs = 0.0

    if bool(numeric.any()):
        left_numeric = lnum[numeric].astype(float)
        right_numeric = rnum[numeric].astype(float)
        differences = (left_numeric - right_numeric).abs()
        if len(differences):
            max_abs = float(differences.max())
        close = (
            differences
            <= 1e-12 + 1e-12 * right_numeric.abs()
        )
        if not bool(close.all()):
            return False, max_abs

    remaining = ~numeric
    if bool(remaining.any()):
        left_text = (
            lvalue[present][remaining]
            .fillna("")
            .astype(str)
        )
        right_text = (
            rvalue[present][remaining]
            .fillna("")
            .astype(str)
        )
        if not bool((left_text == right_text).all()):
            return False, max_abs

    return True, max_abs


def compare_quantitative(before: pd.DataFrame, after: pd.DataFrame) -> dict:
    if len(before) != len(after):
        return {"status": "FAIL", "reason": "row count mismatch"}
    if not set(before.columns).issubset(after.columns):
        return {"status": "FAIL", "reason": "quantitative columns missing"}

    changed: list[dict] = []
    max_abs = 0.0

    for column in before.columns:
        equal, column_max = equal_series(
            before[column],
            after[column],
        )
        max_abs = max(max_abs, column_max)
        if not equal:
            changed.append({
                "column": column,
                "reason": "value mismatch",
            })
            break

    return {
        "status": "PASS" if not changed else "FAIL",
        "changed_cells": changed,
        "maximum_absolute_difference": max_abs,
        "fields_tested": len(before.columns),
    }


def prepare_retry_workspace(workspace: Path) -> Path | None:
    """Archive a failed annotation attempt and create a clean workspace."""
    archived: Path | None = None

    if workspace.exists():
        if not workspace.is_dir():
            raise RuntimeError(
                f"Annotation workspace is not a directory: {workspace}"
            )

        if any(workspace.iterdir()):
            timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
            archived = workspace.with_name(
                f"{workspace.name}.failed-attempt-{timestamp}"
            )
            counter = 2
            while archived.exists():
                archived = workspace.with_name(
                    f"{workspace.name}.failed-attempt-{timestamp}-{counter}"
                )
                counter += 1
            workspace.rename(archived)
        else:
            workspace.rmdir()

    workspace.mkdir(parents=True, exist_ok=False)
    return archived


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()

    request_path = Path(args.request)
    report_path = Path(args.report)
    req = json.loads(request_path.read_text(encoding="utf-8"))
    report: dict = {
        "schema_version": "1.0.0",
        "run_id": req.get("run_id"),
        "stage_id": req.get("stage_id"),
        "status": "preflight_failed",
        "produced_outputs": [],
        "validation_records": [],
    }

    archived_workspace: Path | None = None

    try:
        if req.get("stage_id") != "integrate_mzmine_annotations":
            raise RuntimeError("Expected integrate_mzmine_annotations stage.")
        if req.get("execution_adapter") != "generic_multi_group":
            raise RuntimeError("Expected generic_multi_group execution adapter.")

        registered = req.get("registered_inputs", {})
        quantitative_artifact = registered.get(
            "replicate_summary_artifact", {}
        )
        annotation_artifact = registered.get("annotation_artifact", {})
        quantitative_path = Path(quantitative_artifact.get("path", ""))
        annotation_path = Path(annotation_artifact.get("path", ""))

        for label, item, path in [
            ("quantitative", quantitative_artifact, quantitative_path),
            ("annotation", annotation_artifact, annotation_path),
        ]:
            if not path.is_file():
                raise RuntimeError(f"{label} input is missing: {path}")
            if sha(path) != str(item.get("sha256", "")).upper():
                raise RuntimeError(f"{label} input hash mismatch.")
            if item.get("validation_status") != "PASS":
                raise RuntimeError(f"{label} input is not validated.")

        quantitative = pd.read_csv(quantitative_path, low_memory=False)
        annotation = pd.read_csv(
            annotation_path,
            low_memory=False,
            keep_default_na=False,
        )
        missing = [
            column for column in REQUIRED_ANNOTATION_COLUMNS
            if column not in annotation.columns
        ]
        if missing:
            raise RuntimeError(
                "Canonical MZmine annotation columns are missing: "
                + ", ".join(missing)
            )

        if "mzmine_row_id" not in quantitative.columns:
            raise RuntimeError(
                "Multi-group summary lacks mzmine_row_id."
            )
        quantitative_keys = quantitative["mzmine_row_id"].map(canonical_id)
        annotation_keys = annotation["id"].map(canonical_id)
        if (quantitative_keys == "").any():
            raise RuntimeError(
                "Quantitative summary contains blank feature identifiers."
            )
        if (annotation_keys == "").any() or annotation_keys.duplicated().any():
            raise RuntimeError(
                "MZmine annotation identifiers must be nonblank and unique."
            )

        annotation = annotation.copy()
        annotation["_merge_key"] = annotation_keys
        quantitative_with_key = quantitative.copy()
        quantitative_with_key["_merge_key"] = quantitative_keys

        standardized = pd.DataFrame({
            "_merge_key": annotation["_merge_key"],
            "annotation_name": annotation[
                "preferred_annotation:compound_name"
            ],
            "annotation_formula": annotation[
                "preferred_annotation:mol_formula"
            ],
            "annotation_adduct": annotation[
                "preferred_annotation:adduct"
            ],
            "annotation_score": annotation[
                "preferred_annotation:score"
            ],
            "annotation_compound_score": annotation[
                "preferred_annotation:compound_annotation_score"
            ],
            "annotation_database": annotation[
                "preferred_annotation:database_name"
            ],
            "annotation_method": annotation[
                "preferred_annotation:annotation_method"
            ],
            "annotation_comment": annotation[
                "preferred_annotation:comment"
            ],
            "annotation_mz": annotation["mz"],
            "annotation_mz_diff": annotation[
                "preferred_annotation:mz_diff"
            ],
            "annotation_mz_diff_ppm": annotation[
                "preferred_annotation:mz_diff_ppm"
            ],
            "annotation_inchi": annotation[
                "preferred_annotation:inchi"
            ],
            "annotation_inchi_key": annotation[
                "preferred_annotation:inchi_key"
            ],
            "annotation_smiles": annotation[
                "preferred_annotation:smiles"
            ],
            "annotation_isomeric_smiles": annotation[
                "preferred_annotation:isomeric_smiles"
            ],
            "annotation_classyfire_superclass": annotation[
                "preferred_annotation:classyfire_superclass"
            ],
            "annotation_classyfire_class": annotation[
                "preferred_annotation:classyfire_class"
            ],
            "annotation_classyfire_subclass": annotation[
                "preferred_annotation:classyfire_subclass"
            ],
            "annotation_classyfire_parent": annotation[
                "preferred_annotation:classyfire_parent"
            ],
            "annotation_pubchem_cid": annotation[
                "preferred_annotation:pubchem_cid"
            ],
        })

        merged = quantitative_with_key.merge(
            standardized,
            on="_merge_key",
            how="left",
            sort=False,
            validate="many_to_one",
        )
        merged.drop(columns=["_merge_key"], inplace=True)
        merged["annotation_name"] = (
            merged["annotation_name"].fillna("").astype(str)
        )
        merged["annotation_status"] = merged[
            "annotation_name"
        ].str.strip().map(
            lambda value: "Annotated" if value else "Unannotated"
        )

        preservation = compare_quantitative(
            quantitative.reset_index(drop=True),
            merged[quantitative.columns].reset_index(drop=True),
        )
        if preservation["status"] != "PASS":
            raise RuntimeError(
                "Quantitative columns were not preserved value-for-value."
            )

        workspace = Path(req["output_working_directory"]).resolve()
        archived_workspace = prepare_retry_workspace(workspace)

        output_name = (
            f"{req['dataset_id']}_MultiGroup_Annotated_Master.csv"
        )
        output_path = workspace / output_name
        merged.to_csv(output_path, index=False)

        status_path = workspace / "multi_group_annotation_status.csv"
        pd.DataFrame([
            {
                "annotation_status": "Annotated",
                "feature_count": int(
                    (merged["annotation_status"] == "Annotated").sum()
                ),
            },
            {
                "annotation_status": "Unannotated",
                "feature_count": int(
                    (merged["annotation_status"] == "Unannotated").sum()
                ),
            },
        ]).to_csv(status_path, index=False)

        adapter_hash = sha(Path(__file__))
        outputs = []
        for path in [output_path, status_path]:
            frame = pd.read_csv(path, low_memory=False)
            outputs.append({
                "artifact_role": path.name,
                "path": str(path),
                "sha256": sha(path),
                "bytes": path.stat().st_size,
                "rows": len(frame),
                "columns": len(frame.columns),
            })

        report.update(
            status="completed",
            execution_started=True,
            archived_previous_workspace=(
                str(archived_workspace)
                if archived_workspace is not None
                else None
            ),
            execution_copy_hash=adapter_hash,
            stdout_path=str(workspace / "stdout.log"),
            stderr_path=str(workspace / "stderr.log"),
            produced_outputs=outputs,
            quantitative_preservation=preservation,
            annotation_equivalence={
                "status": "NOT_EVALUABLE",
                "reason": (
                    "No approved dataset-specific multi-group annotation "
                    "oracle; canonical ID merge and quantitative preservation passed."
                ),
            },
            row_retention={
                "status": "PASS",
                "quantitative_rows": len(quantitative),
                "output_rows": len(merged),
            },
            schema={
                "status": "PASS",
                "rows": len(merged),
                "columns": len(merged.columns),
                "column_names": list(merged.columns),
            },
            validation_records=[{
                "check_id": "multi_group.annotation_merge",
                "status": "PASS",
                "category": "scientific",
                "blocking": False,
                "message": (
                    "Canonical MZmine annotations were merged by feature ID; "
                    "all quantitative values and all feature rows were preserved."
                ),
            }],
        )
        (workspace / "stdout.log").write_text(
            "Multi-group annotation merge completed.\n",
            encoding="utf-8",
        )
        (workspace / "stderr.log").write_text("", encoding="utf-8")
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(
            json.dumps(report, indent=2),
            encoding="utf-8",
        )
        return 0
    except Exception as exc:
        report.update(
            status="failed",
            error=str(exc),
            archived_previous_workspace=(
                str(archived_workspace)
                if archived_workspace is not None
                else None
            ),
            quantitative_preservation={"status": "FAIL"},
            annotation_equivalence={"status": "NOT_EVALUABLE"},
            recommended_correction=(
                "Inspect the multi-group annotation request and preserved diagnostics."
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
