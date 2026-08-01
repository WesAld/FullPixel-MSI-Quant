#!/usr/bin/env python3
"""Request-driven generic sample-set adapter for validated Script 01 calculations."""
from __future__ import annotations
import argparse, csv, hashlib, json, math, subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPT_HASH = "D846E8BB354B3BED88A321B8D61FE0BE2F5FE42943C3365F12AAFD6C47F96CB0"
END_MARKER = "\n###############################################################################\n\n###############################################################################\n# PACKAGE CHECKING AND RUN STATE"

def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()

def issue(code, message, category="configuration"):
    return {"check_id": code, "status": "FAIL", "category": category,
            "message": message, "blocking": True}

def rquote(value) -> str:
    return '"' + str(value).replace("\\", "/").replace('"', '\\"') + '"'

def rvec(values):
    return "c(" + ", ".join(rquote(v) for v in values) + ")"

def target_shape(path: Path, id_column: str, mz_column: str):
    with path.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        header = reader.fieldnames or []
        rows = list(reader)
    if id_column not in header or mz_column not in header:
        return header, rows, ["Mapped feature-ID or m/z column is absent."]
    errors = []
    ids = []
    for index, row in enumerate(rows, 1):
        identifier = str(row.get(id_column, "")).strip()
        if not identifier:
            errors.append(f"Blank feature identifier at row {index}.")
        ids.append(identifier)
        try:
            value = float(row.get(mz_column, ""))
            if not math.isfinite(value):
                raise ValueError
        except (TypeError, ValueError):
            errors.append(f"Nonfinite target m/z at row {index}.")
    if len(ids) != len(set(ids)):
        errors.append("Feature identifiers are duplicated.")
    return header, rows, errors

def validate_request(req, check_environment=True):
    problems = []
    if req.get("stage_id") != "extract_pixel_intensities":
        problems.append(issue("stage.unsupported", "Expected extract_pixel_intensities.", "scientific"))
    if req.get("ion_mode") not in ("POS", "NEG"):
        problems.append(issue("scope.ion_mode", "Ion mode must be POS or NEG.", "scientific"))
    if not str(req.get("exposure_label", "")).strip():
        problems.append(issue("scope.exposure", "Exposure label must be nonblank.", "scientific"))
    adapter = req.get("execution_adapter", "generic_two_group")
    quantify_only = adapter == "generic_quantify_only" or req.get("analysis_type") == "quantify_only"
    multi_group = adapter == "generic_multi_group" or req.get("analysis_type") == "compare_multiple_groups"
    conditions = req.get("conditions", [])
    if quantify_only:
        if len(conditions) < 1:
            problems.append(issue("design.conditions", "At least one quantification condition record is required.", "scientific"))
        if req.get("comparison") not in (None, {}):
            problems.append(issue("design.comparison", "Quantify samples only must not define a group comparison.", "scientific"))
    elif multi_group:
        roles = [x.get("analytical_role") for x in conditions]
        comparisons = req.get("comparisons", [])
        if len(conditions) < 3 or roles.count("reference") != 1 or roles.count("comparison") < 2:
            problems.append(issue(
                "design.conditions",
                "Compare multiple groups requires one reference and at least two comparison conditions.",
                "scientific"))
        if len(comparisons) != max(0, len(conditions) - 1):
            problems.append(issue(
                "design.comparisons",
                "Every non-reference condition requires one reference-versus-treatment comparison.",
                "scientific"))
    elif len(conditions) != 2 or {x.get("analytical_role") for x in conditions} != {"reference", "comparison"}:
        problems.append(issue("design.conditions", "Exactly one reference and one comparison condition are required.", "scientific"))
    samples = req.get("samples", [])
    ids = [x.get("sample_id") for x in samples]
    if len(ids) != len(set(ids)):
        problems.append(issue("design.sample_ids", "Internal sample IDs must be unique.", "scientific"))
    grouped = {}
    for sample in samples:
        grouped.setdefault(sample.get("condition_id"), []).append(sample)
        for role in ("imzml_path", "ibd_path"):
            value = sample.get(role, "")
            if not value or not Path(value).is_file():
                problems.append(issue(f"input.{role}", f"Missing {role} for {sample.get('sample_id')}: {value}", "input"))
        if sample.get("imzml_path") and sample.get("ibd_path"):
            if Path(sample["imzml_path"]).stem.lower() != Path(sample["ibd_path"]).stem.lower():
                problems.append(issue("input.pair_stem", f"imzML/IBD stems differ for {sample.get('sample_id')}.", "input"))
    counts = sorted(len(v) for v in grouped.values())
    if quantify_only:
        if len(samples) < 1:
            problems.append(issue("design.samples", "Quantify samples only requires at least one sample.", "scientific"))
    elif multi_group:
        condition_ids = {x.get("condition_id") for x in conditions}
        if set(grouped) != condition_ids or len(grouped) < 3 or any(len(v) < 2 for v in grouped.values()):
            problems.append(issue(
                "design.replicates",
                "Compare multiple groups requires every configured condition and at least two samples per condition.",
                "scientific"))
        reference_ids = [x.get("condition_id") for x in conditions if x.get("analytical_role") == "reference"]
        if len(reference_ids) == 1:
            for comparison in req.get("comparisons", []):
                if comparison.get("reference_condition_id") != reference_ids[0]:
                    problems.append(issue(
                        "design.reference",
                        "Every multi-group comparison must use the same reference condition.",
                        "scientific"))
                if comparison.get("comparison_condition_id") not in condition_ids:
                    problems.append(issue(
                        "design.comparison_condition",
                        "A multi-group comparison references an unknown condition.",
                        "scientific"))
    elif set(grouped) != {"CTRL", "TRT"} or len(counts) != 2 or counts[0] < 2 or counts[0] != counts[1]:
        problems.append(issue("design.replicates", "Generic v1 requires equal replicate counts and at least two samples in each internal role.", "scientific"))
    acquisition = req.get("acquisition_metadata", {})
    try:
        pixel_size = float(acquisition.get("pixel_size_um"))
    except (TypeError, ValueError):
        pixel_size = float("nan")
    if not str(acquisition.get("matrix", "")).strip():
        problems.append(issue("acquisition.matrix", "Matrix metadata must be nonblank.", "scientific"))
    if not math.isfinite(pixel_size) or pixel_size <= 0:
        problems.append(issue("acquisition.pixel_size", "Pixel size must be positive.", "scientific"))
    if acquisition.get("full_pixel") is not True:
        problems.append(issue("acquisition.full_pixel", "Generic FullPixel v1 requires full-pixel data.", "scientific"))
    coordinates = req.get("pixel_metadata", {}).get("expected_coordinate_count")
    if not isinstance(coordinates, int) or coordinates <= 0:
        problems.append(issue("acquisition.coordinates", "Expected coordinate count must be a positive integer.", "scientific"))
    target = req.get("registered_inputs", {}).get("aligned_target_table", {})
    target_path = Path(target.get("path", ""))
    if not target_path.is_file():
        problems.append(issue("input.target", f"Aligned target table is unavailable: {target_path}", "input"))
    else:
        if sha(target_path) != str(target.get("sha256", "")).upper():
            problems.append(issue("input.target_hash", "Aligned target-table hash mismatch.", "scientific"))
        mappings = req.get("column_mappings", {})
        _, rows, errors = target_shape(target_path, mappings.get("feature_id", ""), mappings.get("target_mz", ""))
        for error in errors[:20]:
            problems.append(issue("input.target_structure", error, "input"))
        if not rows:
            problems.append(issue("input.target_empty", "Aligned target table contains no feature rows.", "input"))
    registry = req.get("registered_inputs", {}).get("raw_file_hash_registry", {})
    registry_path = Path(registry.get("path", ""))
    if not registry_path.is_file() or sha(registry_path) != str(registry.get("sha256", "")).upper():
        problems.append(issue("input.registry", "Raw-file hash registry is missing or modified.", "infrastructure"))
    else:
        records = json.loads(registry_path.read_text(encoding="utf-8")).get("files", [])
        expected_paths = {
            str(Path(sample[role]).resolve()).lower()
            for sample in samples for role in ("imzml_path", "ibd_path")
        }
        registered_paths = {
            str(Path(record.get("path", "")).resolve()).lower()
            for record in records if record.get("path")
        }
        if len(records) != 2 * len(samples) or registered_paths != expected_paths:
            problems.append(issue(
                "input.registry_coverage",
                "Raw-file hash registry must contain exactly the registered imzML and IBD files.",
                "scientific"))
        for record in records:
            path = Path(record.get("path", ""))
            if not path.is_file() or sha(path) != str(record.get("sha256", "")).upper():
                problems.append(issue("input.raw_hash", f"Raw input hash mismatch: {record.get('role')}", "scientific"))
    frozen = ROOT / req.get("frozen_script", {}).get("repository_relative_path", "")
    if not frozen.is_file() or sha(frozen) != SCRIPT_HASH:
        problems.append(issue("integrity.frozen_script", "Validated Script 01 identity failed.", "infrastructure"))
    workspace = Path(req.get("output_working_directory", ""))
    if workspace.exists() and any(workspace.iterdir()):
        problems.append(issue("workspace.not_empty", "Output workspace must be absent or empty.", "infrastructure"))
    if check_environment:
        rex = Path(req.get("environment_requirements", {}).get("r_executable", ""))
        if not rex.is_file():
            problems.append(issue("environment.r", "Configured Rscript is unavailable.", "environment"))
    return problems

def make_copy(req, workspace: Path):
    frozen = ROOT / req["frozen_script"]["repository_relative_path"]
    original = frozen.read_text(encoding="utf-8")
    start_token = 'dataset_id <- "PVL_Single_Negative"'
    if original.count(start_token) != 1 or original.count(END_MARKER) != 1:
        raise RuntimeError("Validated Script 01 configuration boundary is not unique.")
    start = original.index(start_token)
    end = original.index(END_MARKER)
    samples = req["samples"]
    acq = req["acquisition_metadata"]
    target_path = Path(req["registered_inputs"]["aligned_target_table"]["path"])
    mappings = req["column_mappings"]
    _, target_rows, _ = target_shape(target_path, mappings["feature_id"], mappings["target_mz"])
    analysis = (workspace / "analysis").resolve()
    mode = "Positive" if req["ion_mode"] == "POS" else "Negative"
    cfg = req["approved_scientific_configuration"]
    panel = f'''dataset_id <- {rquote(req["dataset_id"])}

raw_data_root <-
  {rquote(req["registered_inputs"]["raw_data_root"])}

mzmine_csv <-
  {rquote(target_path.as_posix())}

analysis_root <-
  {rquote(analysis.as_posix())}

expected_exposure <- {rquote(req["exposure_label"])}
expected_ion_mode <- {rquote(mode)}
expected_matrix <- {rquote(acq["matrix"])}
expected_pixel_size_um <- {float(acq["pixel_size_um"])}
expected_full_pixel <- TRUE
expected_coordinate_pixels <- {int(req["pixel_metadata"]["expected_coordinate_count"])}L
mzmine_expected_row_range <- c({len(target_rows)}L, {len(target_rows)}L)

expected_sample_ids <- {rvec([x["sample_id"] for x in samples])}

registered_sample_manifest <- data.frame(
  sample_id = {rvec([x["sample_id"] for x in samples])},
  condition = {rvec([x["condition_id"] for x in samples])},
  replicate = {rvec([x["replicate_id"] for x in samples])},
  exposure = rep({rquote(req["exposure_label"])}, {len(samples)}L),
  mode = rep({rquote(mode)}, {len(samples)}L),
  matrix = rep({rquote(acq["matrix"])}, {len(samples)}L),
  pixel_size_um = rep({float(acq["pixel_size_um"])}, {len(samples)}L),
  full_pixel = rep(TRUE, {len(samples)}L),
  imzml_path = {rvec([Path(x["imzml_path"]).as_posix() for x in samples])},
  ibd_path = {rvec([Path(x["ibd_path"]).as_posix() for x in samples])},
  stringsAsFactors = FALSE
)

absolute_tolerance <- {cfg["absolute_tolerance"]}
ppm_tolerance <- {cfg["ppm_tolerance"]}

inclusive_detection_threshold <- {cfg["inclusive_detection_threshold"]}
conservative_detection_threshold <- {cfg["conservative_detection_threshold"]}

normalization_scale <- {cfg["normalization_scale"]}

save_pixel_level_output <- {"TRUE" if req["retention_options"]["save_pixel_level_output"] else "FALSE"}

primary_detection_threshold <- {cfg["primary_detection_threshold"]}
primary_minimum_detected_pixels <- {int(cfg["primary_minimum_detected_pixels"])}L
'''
    generated = original[:start] + panel + original[end:]
    old_exposure = '''  if (!scalar_text(expected_exposure) ||
      !expected_exposure %in% c("Single", "Sequential")) {
    stop_critical("Configuration error: expected_exposure must be Single or Sequential.")
  }
'''
    new_exposure = '''  if (!scalar_text(expected_exposure)) {
    stop_critical("Configuration error: expected_exposure must be a nonblank label.")
  }
'''
    if old_exposure not in generated:
        raise RuntimeError("Exposure-validation anchor was not found.")
    generated = generated.replace(old_exposure, new_exposure, 1)
    old_samples = '''  if (!is.character(expected_sample_ids) || length(expected_sample_ids) != 6L ||
      anyNA(expected_sample_ids) || any(!nzchar(trimws(expected_sample_ids)))) {
    stop_critical("Configuration error: expected_sample_ids must contain exactly six nonblank entries.")
  }
'''
    new_samples = '''  if (!is.character(expected_sample_ids) || length(expected_sample_ids) < 1L ||
      anyNA(expected_sample_ids) || any(!nzchar(trimws(expected_sample_ids)))) {
    stop_critical("Configuration error: expected_sample_ids must contain at least one nonblank entry.")
  }
  if (!is.data.frame(registered_sample_manifest) ||
      nrow(registered_sample_manifest) != length(expected_sample_ids)) {
    stop_critical("Configuration error: registered_sample_manifest does not match expected_sample_ids.")
  }
'''
    if old_samples not in generated:
        raise RuntimeError("Sample-validation anchor was not found.")
    generated = generated.replace(old_samples, new_samples, 1)
    generated = generated.replace('paste0("SixSample_", timestamp)', 'paste0("SampleSet_", timestamp)', 1)
    discover_start = generated.index("discover_imzml_pairs <- function(output_dir) {")
    discover_end = generated.index("###############################################################################\n# MZMINE-TARGET VALIDATION", discover_start)
    generic_discovery = '''discover_imzml_pairs <- function(output_dir) {
  log_message("Using the explicit registered MSI sample manifest.")
  selected <- data.table::as.data.table(registered_sample_manifest)
  for (column in c("imzml_path", "ibd_path")) {
    selected[, (column) := vapply(get(column), function(path)
      normalizePath(path, winslash = "/", mustWork = TRUE), character(1))]
  }
  selected[, matching_basename :=
    tolower(tools::file_path_sans_ext(basename(imzml_path))) ==
    tolower(tools::file_path_sans_ext(basename(ibd_path)))]
  selected[, selected := TRUE]
  selected[, selection_evidence := "explicit registered project sample"]
  selected[, exclusion_reason := NA_character_]
  if (any(!selected$matching_basename)) {
    data.table::fwrite(selected, file.path(output_dir, "discovered_input_files.csv"))
    stop_critical("At least one registered imzML/IBD pair has different basenames.")
  }
  if (anyDuplicated(selected$sample_id)) stop_critical("Registered sample IDs are duplicated.")
  if (!identical(selected$sample_id, expected_sample_ids))
    stop_critical("Registered samples do not match expected_sample_ids in order.")
  data.table::fwrite(selected, file.path(output_dir, "discovered_input_files.csv"))
  selected
}

'''
    generated = generated[:discover_start] + generic_discovery + generated[discover_end:]
    generated = generated.replace("Conclusion: Complete configured six-sample extraction finished without changing the analytical calculations.",
                                  "Conclusion: Complete configured sample-set extraction finished without changing the analytical calculations.")
    generated = generated.replace("# SIX-SAMPLE COMBINATION AND QC OUTPUT", "# SAMPLE-SET COMBINATION AND QC OUTPUT")
    generated = generated.replace("run_six_sample_extraction", "run_sample_set_extraction")
    generated = generated.replace("six_sample_combined_feature_summary.csv", "sample_set_combined_feature_summary.csv")
    workspace.mkdir(parents=True, exist_ok=False)
    path = workspace / "execution_copy_generic_script01.R"
    path.write_text(generated, encoding="utf-8", newline="\n")
    transformations = {
        "source_hash": sha(frozen), "execution_copy_hash": sha(path),
        "configuration_boundary": "request-driven",
        "input_discovery": "explicit registered sample manifest",
        "sample_count": len(samples), "target_count": len(target_rows),
        "calculation_sections_changed": False,
        "generic_boundary_changes": ["dynamic exposure label", "dynamic sample count",
            "explicit registered imzML/IBD manifest", "generic output naming"]
    }
    (workspace / "configuration_transformations.json").write_text(json.dumps(transformations, indent=2), encoding="utf-8")
    return path, transformations, len(target_rows)

def inspect_output(path: Path, expected_samples, expected_targets):
    with path.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        header = reader.fieldnames or []
        rows = list(reader)
    required = {"dataset_id", "target_id", "mzmine_row_id", "target_mz", "sample_id",
        "condition", "replicate", "normalized_mean_all_valid_pixels",
        "normalized_mean_conservative_detected_pixels", "conservative_detected_pixel_count",
        "conservative_detection_fraction", "primary_quantification_eligible", "primary_qc_status"}
    missing = sorted(required - set(header))
    sample_ids = {row.get("sample_id") for row in rows}
    target_ids = {row.get("target_id") for row in rows}
    expected_rows = len(expected_samples) * expected_targets
    status = "PASS" if (not missing and len(rows) == expected_rows and
                        sample_ids == set(expected_samples) and len(target_ids) == expected_targets) else "FAIL"
    return {"status": status, "rows": len(rows), "columns": len(header),
            "missing_columns": missing, "sample_ids": sorted(sample_ids),
            "unique_targets": len(target_ids), "expected_rows": expected_rows}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()
    req = json.loads(Path(args.request).read_text(encoding="utf-8"))
    report_path = Path(args.report)
    report = {"schema_version": "1.0.0", "run_id": req.get("run_id"),
              "stage_id": req.get("stage_id"), "status": "preflight_failed",
              "validation_records": [], "produced_outputs": []}
    problems = validate_request(req)
    report["validation_records"] = problems
    if problems:
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 2
    workspace = Path(req["output_working_directory"]).resolve()
    try:
        copy, transformations, target_count = make_copy(req, workspace)
        stdout, stderr = workspace / "stdout.log", workspace / "stderr.log"
        report.update(status="running", execution_started=True, frozen_script_hash=SCRIPT_HASH,
                      execution_copy_hash=sha(copy), configuration_transformations=transformations,
                      stdout_path=str(stdout), stderr_path=str(stderr))
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        rex = req["environment_requirements"]["r_executable"]
        with stdout.open("w", encoding="utf-8") as out, stderr.open("w", encoding="utf-8") as err:
            process = subprocess.run([rex, str(copy)], cwd=workspace, stdout=out, stderr=err)
        report["process_exit_status"] = process.returncode
        if process.returncode:
            raise RuntimeError("Generic Script 01 execution exited unsuccessfully.")
        parent = workspace / "analysis" / "02_Pixel_Quantification" / req["dataset_id"]
        directories = [p for p in parent.glob("SampleSet_*") if p.is_dir()]
        if len(directories) != 1:
            raise RuntimeError(f"Expected one SampleSet output directory; found {len(directories)}.")
        output = directories[0] / "sample_set_combined_feature_summary.csv"
        if not output.is_file():
            raise RuntimeError("Required generic sample-set summary is missing.")
        validation = inspect_output(output, [x["sample_id"] for x in req["samples"]], target_count)
        if validation["status"] != "PASS":
            raise RuntimeError("Generic extraction output structure failed validation.")
        artifact = {"artifact_id": "art-" + req["run_id"] + "-combined",
                    "artifact_role": "sample_set_combined_feature_summary.csv",
                    "path": str(output), "sha256": sha(output), "bytes": output.stat().st_size,
                    "rows": validation["rows"], "columns": validation["columns"]}
        report.update(status="completed", produced_outputs=[artifact], output_validation=validation,
                      validation_records=[{"check_id": "generic.script01.output", "status": "PASS",
                        "category": "scientific", "blocking": False,
                        "message": "Dynamic sample and target dimensions, sample coverage, and required quantitative columns passed."}])
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 0
    except Exception as exc:
        report.update(status="failed", error=str(exc),
                      recommended_correction="Inspect preserved generic extraction diagnostics; no output was activated.")
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 1

if __name__ == "__main__":
    raise SystemExit(main())
