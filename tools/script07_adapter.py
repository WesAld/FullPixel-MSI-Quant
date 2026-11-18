#!/usr/bin/env python3
"""Request-driven configuration adapter for frozen Script 07."""
import argparse, hashlib, json, subprocess, time
from pathlib import Path
from subprocess_runtime import run_external_process

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "validated-source/07_export_three_graphs_to_graphpad_xlsx_VALIDATED_PUBLICATION_READY.R"
SCRIPT_HASH = "94055F3210D95D6AF0EA35D246D9F234A24FF429496E179DA91EE58A1FBBB0A0"

def sha(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()
    request = json.loads(Path(args.request).read_text(encoding="utf-8"))
    report_path = Path(args.report)
    positive = Path(request["positive_master"]["path"])
    negative = Path(request["negative_master"]["path"])
    workspace = Path(request["output_working_directory"]).resolve()
    exposure = request.get("exposure_label", "Single")
    if sha(SCRIPT) != SCRIPT_HASH:
        raise RuntimeError("Frozen Script 07 hash mismatch.")
    for mode, path in (("Positive", positive), ("Negative", negative)):
        expected = request[f"{mode.lower()}_master"]["sha256"].upper()
        if not path.is_file() or sha(path) != expected:
            raise RuntimeError(f"{mode} annotated-master identity failed.")
    workspace.mkdir(parents=True, exist_ok=False)
    original = SCRIPT.read_text(encoding="utf-8")
    start = original.index('analysis_root <- "C:/MALDI-MSI/PVL_MSI_Analysis"')
    marker = "###############################################################################\n# CONSTANTS AND SMALL HELPERS"
    body = original[original.index(marker):]
    panel = f'''analysis_root <- "{workspace.as_posix()}"
exposure_label <- "{exposure}"

dataset_inputs <- data.table::data.table(
  dataset_id = c("PVL_{exposure}_Positive", "PVL_{exposure}_Negative"),
  ion_mode = c("Positive", "Negative"),
  annotated_master_csv = c(
    "{positive.as_posix()}",
    "{negative.as_posix()}"
  )
)

output_parent <- file.path(
  analysis_root,
  paste0("05_Combined_", exposure_label, "_Overview")
)

control_sample_ids <- c("CTRL_R1", "CTRL_R2", "CTRL_R3")
treatment_sample_ids <- c("TRT_R1", "TRT_R2", "TRT_R3")
control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "PVL"

minimum_annotation_score_for_export <- 0.6
expected_all_coordinate_pixels <- 1600L
require_all_replicates_for_audit <- TRUE
minimum_replicates_if_not_all_required <- 2L
numeric_validation_tolerance <- 1e-10
write_excel_output <- TRUE
write_csv_outputs <- TRUE

'''
    generated = original[:start] + panel + body
    if generated[generated.index(marker):] != body:
        raise RuntimeError("Script 07 scientific body changed.")
    execution_copy = workspace / "execution_copy_script07.R"
    execution_copy.write_text(generated, encoding="utf-8", newline="")
    stdout = workspace / "stdout.log"
    stderr = workspace / "stderr.log"
    with stdout.open("w") as out, stderr.open("w") as err:
        process = run_external_process(
            [request["r_executable"], str(execution_copy)],
            scientific_script=execution_copy, project_path=workspace,
            stdout=out, stderr=err)
    outputs = list(workspace.rglob("*_Three_Readouts_GraphPad_Ready.xlsx"))
    status = "PASS" if process.returncode == 0 and len(outputs) == 1 else "FAIL"
    report = {
        "schema_version": "1.0.0", "status": status,
        "classification": "Script 07 technical-validation candidate",
        "frozen_script_hash": SCRIPT_HASH,
        "execution_copy_hash": sha(execution_copy),
        "scientific_body_unchanged": True,
        "positive_master": request["positive_master"],
        "negative_master": request["negative_master"],
        "candidate_workbook": {
            "path": str(outputs[0]) if len(outputs) == 1 else "",
            "sha256": sha(outputs[0]) if len(outputs) == 1 else ""},
        "process_exit_status": process.returncode,
        "stdout": str(stdout), "stderr": str(stderr),
        "completed_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(json.dumps(report, indent=2))
    return 0 if status == "PASS" else 2

if __name__ == "__main__":
    raise SystemExit(main())
