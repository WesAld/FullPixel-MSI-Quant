#!/usr/bin/env python3
"""Strict frozen-source adapter for the approved Script 03 boundary."""
from __future__ import annotations
import argparse, csv, hashlib, json, math, os, re, shutil, subprocess, sys, time
from pathlib import Path
from subprocess_runtime import run_external_process

ROOT = Path(__file__).resolve().parents[1]
EXPECTED_SCRIPT_HASH = "3D3E2E7A6E81972FE8F6EA4D54F5BCD52F47D1B66C62E35F8E1CFF00ABB60A97"
EXPECTED_INPUT_HASH = "73CBAAB145F8F74A00E82E85EF9A7E726205E70EB822FDFD492C2841B831A617"
EXPECTED_SAMPLES = ["CTRL_R1", "CTRL_R2", "CTRL_R3", "TRT_R1", "TRT_R2", "TRT_R3"]
EXPECTED_FILES = [
    "analysis_configuration_used.csv",
    "analysis_validation_summary.csv",
    "PVL_Single_Negative_ThreeReadout_Summary.csv",
    "PVL_Single_Negative_ThreeReadout_Summary.xlsx",
    "replicate_variability_summary.csv",
    "sessionInfo.txt",
    "three_readout_analysis_report.txt",
    "three_readout_status_summary.csv",
    "03_three_readout_replicate_summary_used.R",
]
REFERENCE_PATH = Path(
    "C:/MALDI-MSI/PVL_MSI_Analysis/03_Replicate_Summaries/"
    "PVL_Single_Negative/ThreeReadout_ReplicateSummary_20260730_150047/"
    "PVL_Single_Negative_ThreeReadout_Summary.csv"
)
UNRESOLVED = object()

def resolve_executable(configured):
    value = str(configured or "").strip()
    if not value:
        return None
    candidate = Path(value)
    if candidate.is_file():
        return str(candidate.resolve())
    resolved = shutil.which(value)
    return str(Path(resolved).resolve()) if resolved else None

def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()

def issue(check_id, message, category="configuration"):
    return {"check_id": check_id, "status": "FAIL", "message": message,
            "category": category, "blocking": True}

def validate_request(req: dict, check_environment=True,
                     r_executable=UNRESOLVED):
    problems = []
    expected = {"stage_id": "summarize_replicate_readouts",
        "scientific_core_version": "1.0.0"}
    for field, value in expected.items():
        if req.get(field) != value:
            problems.append(issue(f"scope.{field}", f"{field} must equal {value}.", "scientific"))
    if req.get("exposure_label") not in ("Single", "Sequential"):
        problems.append(issue("scope.exposure_label",
            "exposure_label must equal Single or Sequential.", "scientific"))
    if req.get("ion_mode") not in ("NEG", "POS"):
        problems.append(issue("scope.ion_mode", "ion_mode must equal NEG or POS.", "scientific"))
    profile = req.get("scientific_profile", {}).get("profile_id")
    if profile not in ("pvl-fullpixel-reference-v1", "fullpixel-40um-1600px-v1",
                       "pvl-positive-technical-candidate-v1"):
        problems.append(issue("profile.unsupported", "A registered PVL technical profile is required.", "scientific"))
    samples = [x.get("sample_id") for x in req.get("samples", [])]
    if samples != EXPECTED_SAMPLES:
        problems.append(issue("profile.samples", "Exact ordered six-sample PVL mapping is required.", "scientific"))
    conditions = req.get("conditions", [])
    if len(conditions) != 2:
        problems.append(issue("profile.conditions", "Exactly two mapped conditions are required.", "scientific"))
    reps = req.get("replicates", [])
    if len(reps) != 6:
        problems.append(issue("profile.replicates", "Exactly six approved replicate mappings are required.", "scientific"))
    source = ROOT / req.get("frozen_script", {}).get("repository_relative_path", "")
    if not source.is_file() or sha(source) != EXPECTED_SCRIPT_HASH or \
       req.get("frozen_script", {}).get("sha256", "").upper() != EXPECTED_SCRIPT_HASH:
        problems.append(issue("integrity.frozen_script", "Frozen Script 03 identity failed.", "scientific"))
    input_info = req.get("registered_inputs", {}).get("pixel_intensity_artifact", {})
    input_path = Path(input_info.get("path", ""))
    if not input_path.is_file():
        problems.append(issue("input.pixel_intensity_missing", "Registered Script 01 artifact is missing.", "input"))
    elif sha(input_path) != input_info.get("sha256", "").upper():
        problems.append(issue("input.pixel_intensity_hash", "Script 01 artifact hash differs from its registered identity.", "scientific"))
    elif req.get("ion_mode") == "NEG" and req.get("exposure_label") == "Single" and sha(input_path) != EXPECTED_INPUT_HASH:
        problems.append(issue("input.pixel_intensity_hash", "Negative Script 01 artifact hash is not approved.", "scientific"))
    if input_info.get("active_status") != "active" or input_info.get("validation_status") != "PASS":
        problems.append(issue("input.pixel_intensity_inactive", "Script 01 artifact must be active and validated.", "scientific"))
    reference_value = req.get("regression_reference", {}).get("path") or (
        str(REFERENCE_PATH)
        if req.get("ion_mode") == "NEG" and req.get("exposure_label") == "Single"
        else ""
    )
    reference = Path(reference_value) if reference_value else None
    expected_ref_hash = req.get("regression_reference", {}).get("sha256", "")
    if req.get("ion_mode") == "NEG" and req.get("exposure_label") == "Single" and (reference is None or not reference.is_file()):
        problems.append(issue("reference.missing", "Script 03 regression reference is unavailable.", "input"))
    elif reference is not None and reference.is_file() and expected_ref_hash and sha(reference) != expected_ref_hash.upper():
        problems.append(issue("reference.hash", "Script 03 regression reference hash differs.", "scientific"))
    workspace = (ROOT / req.get("output_working_directory", "")).resolve()
    if workspace.exists() and any(workspace.iterdir()):
        problems.append(issue("workspace.not_empty", "Workspace must be absent or empty.", "infrastructure"))
    if check_environment:
        rex = (resolve_executable(req.get("environment_requirements", {}).get(
            "r_executable", "")) if r_executable is UNRESOLVED
            else r_executable)
        if rex is None:
            problems.append(issue("environment.r", "Configured Rscript is unavailable.", "environment"))
        else:
            expression = "quit(status=if(all(vapply(c('data.table','openxlsx'),requireNamespace,logical(1),quietly=TRUE)))0 else 9)"
            result = run_external_process([rex, "-e", expression],
                scientific_script="R package preflight", capture_output=True, text=True)
            if result.returncode:
                problems.append(issue("environment.packages", "data.table and openxlsx must be preinstalled.", "environment"))
    return problems

def replace_once(text: str, pattern: str, replacement: str, label: str):
    updated, count = re.subn(pattern, replacement, text, count=0, flags=re.MULTILINE)
    if count != 1:
        raise RuntimeError(f"Approved source anchor {label!r} occurred {count} times.")
    return updated, {"field": label, "occurrences": count}

def make_execution_copy(req: dict, workspace: Path):
    source = ROOT / req["frozen_script"]["repository_relative_path"]
    original = source.read_text(encoding="utf-8")
    generated = original
    changes = []
    dataset_id = f"PVL_{req.get('exposure_label', 'Single')}_{'Positive' if req.get('ion_mode') == 'POS' else 'Negative'}"
    substitutions = [
        (r'^dataset_id <- "PVL_Single_Negative"$',
         f'dataset_id <- "{dataset_id}"', "dataset_id"),
        (r'^input_csv <-\r?\n  "[^"]+"$',
         f'input_csv <-\n  "{Path(req["registered_inputs"]["pixel_intensity_artifact"]["path"]).as_posix()}"',
         "input_csv"),
        (r'^analysis_root <- "[^"]+"$',
         f'analysis_root <- "{workspace.as_posix()}"', "analysis_root"),
    ]
    for pattern, replacement, label in substitutions:
        generated, record = replace_once(generated, pattern, replacement, label)
        changes.append(record)
    boundary = "required_packages <- c(\"data.table\", \"openxlsx\")"
    if original.count(boundary) != 1 or generated.count(boundary) != 1:
        raise RuntimeError("Scientific-body boundary is not unique.")
    original_body = original[original.index(boundary):]
    generated_body = generated[generated.index(boundary):]
    if original_body != generated_body:
        raise RuntimeError("Scientific body changed outside the approved configuration panel.")
    workspace.mkdir(parents=True, exist_ok=False)
    copy_path = workspace / "execution_copy_script03.R"
    copy_path.write_text(generated, encoding="utf-8", newline="")
    transformations = {
        "schema_version": "1.0.0", "source_hash": sha(source),
        "execution_copy_hash": sha(copy_path), "substitutions": changes,
        "scientific_body_hash": hashlib.sha256(original_body.encode("utf-8")).hexdigest().upper(),
        "scientific_body_unchanged": True,
    }
    (workspace / "configuration_transformations.json").write_text(
        json.dumps(transformations, indent=2), encoding="utf-8")
    return copy_path, transformations

def csv_shape(path: Path):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.reader(handle)
        header = next(reader)
        rows = sum(1 for _ in reader)
    return rows, len(header), header

def numeric(value):
    if value == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None

def compare_csv(actual: Path, reference: Path, abs_tol=1e-12, rel_tol=1e-12):
    with actual.open(newline="", encoding="utf-8-sig") as a, \
         reference.open(newline="", encoding="utf-8-sig") as b:
        ar, br = csv.reader(a), csv.reader(b)
        ah, bh = next(ar), next(br)
        if ah != bh:
            return {"status": "FAIL", "reason": "column identity/order mismatch",
                    "actual_columns": ah, "reference_columns": bh}
        failed, max_abs, max_rel, row_count = [], 0.0, 0.0, 0
        for row_count, (av, bv) in enumerate(zip(ar, br), start=1):
            if len(av) != len(bv):
                failed.append({"row": row_count, "reason": "row width"})
                continue
            for col, (x, y) in enumerate(zip(av, bv)):
                if x == y:
                    continue
                nx, ny = numeric(x), numeric(y)
                if nx is None or ny is None:
                    failed.append({"row": row_count, "column": ah[col],
                                   "actual": x, "reference": y})
                    continue
                if math.isnan(nx) or math.isnan(ny):
                    equal = math.isnan(nx) and math.isnan(ny)
                    diff = rel = 0.0
                elif math.isinf(nx) or math.isinf(ny):
                    equal = nx == ny
                    diff = rel = 0.0
                else:
                    diff = abs(nx - ny)
                    rel = diff / max(abs(ny), 1e-300)
                    max_abs, max_rel = max(max_abs, diff), max(max_rel, rel)
                    equal = diff <= abs_tol + rel_tol * abs(ny)
                if not equal and len(failed) < 100:
                    failed.append({"row": row_count, "column": ah[col],
                                   "actual": x, "reference": y,
                                   "absolute_difference": diff,
                                   "relative_difference": rel})
        extra_a, extra_b = next(ar, None), next(br, None)
        if extra_a is not None or extra_b is not None:
            failed.append({"reason": "row count mismatch"})
    return {"status": "PASS" if not failed else "FAIL",
            "row_count": row_count, "column_count": len(ah),
            "row_identity": "target_id in stored row order",
            "maximum_absolute_difference": max_abs,
            "maximum_relative_difference": max_rel,
            "failed_cells": failed, "absolute_tolerance": abs_tol,
            "relative_tolerance": rel_tol}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()
    req = json.loads(Path(args.request).read_text(encoding="utf-8"))
    report_path = Path(args.report)
    report = {"schema_version": "1.0.0", "stage_id": req.get("stage_id"),
              "run_id": req.get("run_id"), "status": "preflight_failed",
              "validation_records": [], "execution_started": False,
              "produced_outputs": []}
    configured_rex = req.get("environment_requirements", {}).get(
        "r_executable", "")
    resolved_rex = resolve_executable(configured_rex)
    problems = validate_request(req, r_executable=resolved_rex)
    report["validation_records"] = problems
    if problems:
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 2
    workspace = (ROOT / req["output_working_directory"]).resolve()
    try:
        copy_path, transformations = make_execution_copy(req, workspace)
        report.update(frozen_script_hash=EXPECTED_SCRIPT_HASH,
                      execution_copy_hash=sha(copy_path),
                      configuration_transformations=transformations,
                      r_executable_configured=configured_rex,
                      r_executable_resolved=resolved_rex)
        stdout, stderr = workspace / "stdout.log", workspace / "stderr.log"
        report.update(execution_started=True, stdout_path=str(stdout),
                      stderr_path=str(stderr), status="running",
                      started_at=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()))
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        with stdout.open("w", encoding="utf-8") as out, stderr.open("w", encoding="utf-8") as err:
            process = run_external_process([resolved_rex, str(copy_path)],
                scientific_script=copy_path, project_path=workspace,
                stdout=out, stderr=err)
        report["process_exit_status"] = process.returncode
        if process.returncode:
            raise RuntimeError("Frozen Script 03 process exited unsuccessfully.")
        dataset_id = f"PVL_{req.get('exposure_label', 'Single')}_{'Positive' if req.get('ion_mode') == 'POS' else 'Negative'}"
        expected_files = [name.replace("PVL_Single_Negative", dataset_id)
                          for name in EXPECTED_FILES]
        parent = workspace / "03_Replicate_Summaries" / dataset_id
        directories = [x for x in parent.glob("ThreeReadout_ReplicateSummary_*") if x.is_dir()]
        if len(directories) != 1:
            raise RuntimeError(f"Expected exactly one Script 03 output directory; found {len(directories)}.")
        output_dir = directories[0]
        missing = [name for name in expected_files if not (output_dir / name).is_file()]
        if missing:
            raise RuntimeError("Missing expected Script 03 outputs: " + ", ".join(missing))
        summary = output_dir / f"{dataset_id}_ThreeReadout_Summary.csv"
        reference_value = req.get("regression_reference", {}).get("path") or (
            str(REFERENCE_PATH)
            if req.get("ion_mode") == "NEG" and req.get("exposure_label") == "Single"
            else ""
        )
        reference = Path(reference_value) if reference_value else None
        equivalence = compare_csv(summary, reference) if reference is not None and reference.is_file() else {
            "status": "NOT_EVALUABLE", "reason": "No approved Positive regression oracle.",
            "row_count": csv_shape(summary)[0], "column_count": csv_shape(summary)[1],
            "failed_cells": [], "maximum_absolute_difference": None,
            "maximum_relative_difference": None}
        report["equivalence"] = equivalence
        outputs = []
        for name in expected_files:
            path = output_dir / name
            rows = columns = None
            if path.suffix.lower() == ".csv":
                rows, columns, _ = csv_shape(path)
            outputs.append({"artifact_role": name, "path": str(path),
                            "sha256": sha(path), "bytes": path.stat().st_size,
                            "rows": rows, "columns": columns})
        report["produced_outputs"] = outputs
        if req.get("ion_mode") == "NEG" and req.get("exposure_label") == "Single" and equivalence["status"] != "PASS":
            raise RuntimeError("Script 03 numerical or schema equivalence failed.")
        report.update(status="completed",
                      completed_at=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                      recommended_correction=None)
        report["validation_records"] = [{
            "check_id": "script03.regression_equivalence",
            "status": "PASS" if equivalence["status"] == "PASS" else "NOT_EVALUABLE",
            "message": "Primary Script 03 summary satisfies the registered equivalence policy." if equivalence["status"] == "PASS" else "Positive Script 03 execution passed internal validation; no approved complete regression oracle is available.",
            "category": "scientific", "blocking": False}]
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 0
    except Exception as exc:
        report.update(status="failed", error=str(exc),
                      recommended_correction="Inspect preserved Script 03 diagnostics; do not activate outputs.")
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 1

if __name__ == "__main__":
    raise SystemExit(main())
