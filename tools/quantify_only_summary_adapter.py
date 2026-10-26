#!/usr/bin/env python3
"""Create one target row with individual readout columns for every sample."""
from __future__ import annotations
import argparse, hashlib, json, sys
from pathlib import Path
import pandas as pd
sys.path.insert(0, str(Path(__file__).resolve().parent))
from normalization_routing import provenance, resolve_method, summary_columns


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()


def fail(report_path: Path, report: dict, message: str) -> int:
    report.update(status="failed", error=message,
                  recommended_correction="Inspect the preserved quantify-only summary request and extraction artifact.")
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
    return 1


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
            raise RuntimeError("Quantify-only summary received the wrong execution adapter.")
        artifact = req.get("registered_inputs", {}).get("pixel_intensity_artifact", {})
        source = Path(artifact.get("path", ""))
        if not source.is_file() or sha(source) != str(artifact.get("sha256", "")).upper():
            raise RuntimeError("Pixel-intensity artifact identity failed.")
        if artifact.get("active_status") != "active" or artifact.get("validation_status") != "PASS":
            raise RuntimeError("Pixel-intensity artifact must be active and validated.")
        samples = req.get("samples", [])
        sample_ids = [str(x.get("sample_id", "")).strip() for x in samples]
        if not sample_ids or any(not x for x in sample_ids) or len(sample_ids) != len(set(sample_ids)):
            raise RuntimeError("At least one unique nonblank sample ID is required.")
        frame = pd.read_csv(source, low_memory=False)
        method = resolve_method(req)
        overall_source, local_source = summary_columns(method)
        required = {
            "dataset_id", "target_id", "mzmine_row_id", "target_mz", "sample_id",
            overall_source,
            local_source,
            "conservative_detected_pixel_count",
            "conservative_detection_fraction",
            "primary_quantification_eligible", "primary_qc_status",
        }
        missing = sorted(required - set(frame.columns))
        if missing:
            raise RuntimeError("Extraction columns are missing: " + ", ".join(missing))
        if set(frame["sample_id"].astype(str)) != set(sample_ids):
            raise RuntimeError("Extraction sample coverage does not match the request.")
        key = ["dataset_id", "target_id", "mzmine_row_id", "target_mz"]
        if frame.duplicated(key + ["sample_id"]).any():
            raise RuntimeError("Extraction contains duplicated target/sample rows.")
        target_count = frame[key].drop_duplicates().shape[0]
        if len(frame) != target_count * len(sample_ids):
            raise RuntimeError("Extraction dimensions are incomplete for the registered sample set.")
        out = frame[key].drop_duplicates().sort_values(
            ["mzmine_row_id", "target_mz"], kind="stable").reset_index(drop=True)
        frame["primary_mean_all_valid_pixels"] = frame[overall_source]
        frame["primary_mean_conservative_detected_pixels"] = frame[local_source]
        metrics = [
            "primary_mean_all_valid_pixels",
            "primary_mean_conservative_detected_pixels",
            "conservative_detected_pixel_count",
            "conservative_detection_fraction",
            "primary_quantification_eligible", "primary_qc_status",
        ]
        suffix = {
            "primary_mean_all_valid_pixels": "overall_normalized_signal",
            "primary_mean_conservative_detected_pixels": "local_strong_pixel_intensity",
            "conservative_detected_pixel_count": "conservative_detected_pixel_count",
            "conservative_detection_fraction": "conservative_detection_fraction",
            "primary_quantification_eligible": "primary_quantification_eligible",
            "primary_qc_status": "primary_qc_status",
        }
        for sample_id in sample_ids:
            part = frame.loc[frame["sample_id"].astype(str) == sample_id, key + metrics].copy()
            part = part.rename(columns={metric: f"{sample_id}_{suffix[metric]}" for metric in metrics})
            out = out.merge(part, on=key, how="left", validate="one_to_one")
        if len(out) != target_count:
            raise RuntimeError("Per-sample summary row retention failed.")
        workspace = Path(req["output_working_directory"])
        workspace.mkdir(parents=True, exist_ok=False)
        out.insert(4, "primary_normalization_method", method)
        output = workspace / f'{req["dataset_id"]}_QuantifyOnly_Sample_Metrics.csv'
        out.to_csv(output, index=False)
        stdout = workspace / "stdout.log"; stderr = workspace / "stderr.log"
        stdout.write_text("Quantify-only per-sample pivot completed.\n", encoding="utf-8")
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
            equivalence={"status": "NOT_EVALUABLE",
                         "reason": "No comparison or historical oracle applies to per-sample pivoting."},
            output_validation={"status": "PASS", "targets": target_count,
                               "samples": len(sample_ids), "rows": len(out)},
            normalization=provenance(method, req.get("approved_scientific_configuration", {}).get("normalization_scale", 1000000)),
            validation_records=[{
                "check_id": "quantify_only.sample_metrics", "status": "PASS",
                "category": "scientific", "blocking": False,
                "message": "Every target/sample extraction row was retained in individual per-sample columns; no comparison statistic was calculated.",
            }],
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 0
    except Exception as exc:
        return fail(report_path, report, str(exc))


if __name__ == "__main__":
    raise SystemExit(main())
