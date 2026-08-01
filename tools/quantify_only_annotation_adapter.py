#!/usr/bin/env python3
"""Merge canonical MZmine annotations into quantify-only sample metrics."""
from __future__ import annotations
import argparse, hashlib, json
from pathlib import Path
import pandas as pd


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()
    req = json.loads(Path(args.request).read_text(encoding="utf-8"))
    report_path = Path(args.report)
    report = {"schema_version": "1.0.0", "run_id": req.get("run_id"),
              "stage_id": req.get("stage_id"), "status": "preflight_failed",
              "validation_records": [], "produced_outputs": []}
    try:
        if req.get("execution_adapter") != "generic_quantify_only":
            raise RuntimeError("Quantify-only annotation received the wrong execution adapter.")
        inputs = req.get("registered_inputs", {})
        quantitative = inputs.get("replicate_summary_artifact", {})
        annotation = inputs.get("annotation_artifact", {})
        qpath = Path(quantitative.get("path", "")); apath = Path(annotation.get("path", ""))
        for label, item, path in (("quantitative", quantitative, qpath), ("annotation", annotation, apath)):
            if not path.is_file() or sha(path) != str(item.get("sha256", "")).upper():
                raise RuntimeError(f"{label.title()} artifact identity failed.")
            if item.get("active_status") != "active" or item.get("validation_status") != "PASS":
                raise RuntimeError(f"{label.title()} artifact must be active and validated.")
        q = pd.read_csv(qpath, low_memory=False)
        a = pd.read_csv(apath, dtype=str, keep_default_na=False)
        if "mzmine_row_id" not in q.columns:
            raise RuntimeError("Quantitative table lacks mzmine_row_id.")
        mappings = req.get("annotation_column_mappings", {})
        feature_col = mappings.get("feature_id", "id")
        if feature_col not in a.columns:
            raise RuntimeError(f"Mapped annotation feature ID column is absent: {feature_col}")
        a[feature_col] = a[feature_col].astype(str).str.strip()
        if (a[feature_col] == "").any() or a[feature_col].duplicated().any():
            raise RuntimeError("Annotation feature IDs must be nonblank and unique.")
        qkey = q["mzmine_row_id"].astype(str).str.strip()
        if qkey.duplicated().any():
            raise RuntimeError("Quantitative mzmine_row_id values must be unique.")
        lookup = a.set_index(feature_col, drop=False)
        source_map = {
            "annotation_name": mappings.get("annotation_name", "preferred_annotation:compound_name"),
            "annotation_formula": mappings.get("formula", "preferred_annotation:mol_formula"),
            "annotation_adduct": mappings.get("adduct", "preferred_annotation:adduct"),
            "annotation_score": mappings.get("score", "preferred_annotation:score"),
            "annotation_compound_score": mappings.get("compound_score", "preferred_annotation:compound_annotation_score"),
            "annotation_database": mappings.get("database", "preferred_annotation:database_name"),
            "annotation_mz_diff": "preferred_annotation:mz_diff",
            "annotation_mz_diff_ppm": "preferred_annotation:mz_diff_ppm",
        }
        out = q.copy()
        for target, source in source_map.items():
            if source in lookup.columns:
                out[target] = qkey.map(lookup[source])
            else:
                out[target] = ""
        out["annotation_match_status"] = qkey.map(
            lambda x: "MATCHED" if x in lookup.index else "UNMATCHED")
        quantitative_columns = list(q.columns)
        if not out[quantitative_columns].equals(q[quantitative_columns]):
            raise RuntimeError("Quantitative columns changed during annotation integration.")
        workspace = Path(req["output_working_directory"])
        workspace.mkdir(parents=True, exist_ok=False)
        output = workspace / f'{req["dataset_id"]}_QuantifyOnly_Annotated_Master.csv'
        out.to_csv(output, index=False)
        reloaded = pd.read_csv(output, low_memory=False)
        if len(reloaded) != len(q) or not set(quantitative_columns).issubset(reloaded.columns):
            raise RuntimeError("Annotated-master row or quantitative-column retention failed.")
        stdout = workspace / "stdout.log"; stderr = workspace / "stderr.log"
        stdout.write_text("Quantify-only annotation integration completed.\n", encoding="utf-8")
        stderr.write_text("", encoding="utf-8")
        report.update(
            status="completed", execution_started=True,
            execution_copy_hash=sha(Path(__file__)),
            stdout_path=str(stdout), stderr_path=str(stderr),
            produced_outputs=[{
                "artifact_role": output.name, "path": str(output),
                "sha256": sha(output), "bytes": output.stat().st_size,
                "rows": len(out), "columns": len(out.columns),
            }],
            quantitative_preservation={"status": "PASS", "fields_tested": len(quantitative_columns),
                                       "changed_cells": [], "maximum_absolute_difference": 0.0},
            annotation_equivalence={"status": "NOT_EVALUABLE",
                                    "reason": "No dataset-specific historical annotation oracle was supplied."},
            row_retention={"status": "PASS", "quantitative_rows": len(q), "output_rows": len(out)},
            schema={"status": "PASS", "rows": len(out), "columns": len(out.columns),
                    "column_names": list(out.columns)},
            validation_records=[{
                "check_id": "quantify_only.annotation", "status": "PASS",
                "category": "scientific", "blocking": False,
                "message": "All per-sample quantitative columns were preserved and canonical MZmine annotations were merged by feature ID.",
            }],
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 0
    except Exception as exc:
        report.update(status="failed", error=str(exc),
                      recommended_correction="Inspect the preserved quantify-only annotation request and both parent artifacts.")
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
