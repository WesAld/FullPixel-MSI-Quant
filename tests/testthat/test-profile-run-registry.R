test_that("historical profile resolves explicitly to canonical profile", {
  resolved <- fpq_resolve_profile("fullpixel-40um-1600px-v1")
  expect_identical(resolved$resolved_id, "pvl-fullpixel-reference-v1")
  expect_identical(resolved$relationship, "predecessor_to_canonical")
  expect_error(fpq_resolve_profile("missing-profile"), class = "fpq_configuration_error")
})

test_that("technically validated workflow scopes are executable with warning", {
  expect_true(fpq_workflow_stage_status("extract_pixel_intensities")$executable)
  expect_true(fpq_workflow_stage_status("extract_pixel_intensities", "POS")$executable)
  expect_identical(fpq_workflow_stage_status(
    "extract_pixel_intensities", "POS")$status, "candidate_workflow_ready")
  expect_true(fpq_workflow_stage_status(
    "extract_pixel_intensities", "NEG", "Sequential")$executable)
  expect_true(fpq_workflow_stage_status("summarize_replicate_readouts")$executable)
})

test_that("run transitions persist and invalid transitions fail", {
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-run-test", "Run")
  request <- list(run_id = "run-test-001", dataset_id = "dset-test",
    stage_id = "extract_pixel_intensities", ion_mode = "NEG", exposure_label = "Single",
    scientific_profile = list(profile_id = "pvl-fullpixel-reference-v1"),
    scientific_core_version = "1.0.0", frozen_script = list(script_id = "extract"),
    registered_inputs = list())
  fpq_create_run(project, request)
  expect_identical(fpq_load_run(project, request$run_id)$status, "configured")
  fpq_transition_run(project, request$run_id, "preflight_running")
  expect_identical(fpq_load_run(project, request$run_id)$status, "preflight_running")
  expect_error(fpq_transition_run(project, request$run_id, "completed"),
               class = "fpq_configuration_error")
})

test_that("artifact registry uses exact hash and fail-closed activation", {
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-art-test", "Artifacts")
  file <- tempfile(); writeLines("value", file)
  artifact <- list(artifact_id = "art-test", artifact_type = "pixel_intensity_table",
    path = normalizePath(file), project_id = project$manifest$project_id,
    dataset_id = "dset-test", producing_run_id = "run-test",
    sha256 = fpq_hash_file(file), file_size = unname(file.info(file)$size),
    validation_status = "PASS", scientific_profile_id = "pvl-fullpixel-reference-v1",
    scientific_core_version = "1.0.0", parent_artifact_ids = list())
  fpq_register_artifact(project, artifact, TRUE)
  expect_identical(fpq_inspect_artifact(project, "art-test")$active_status, "active")
  writeLines("changed", file)
  expect_identical(fpq_verify_artifact(project, "art-test")$active_status, "stale")
  artifact$artifact_id <- "art-failed"; artifact$validation_status <- "FAIL"
  expect_error(fpq_register_artifact(project, artifact, TRUE),
               class = "fpq_configuration_error")
})

test_that("logs, validation and provenance persist", {
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-record-test", "Records")
  fpq_log_event(project, "test.event")
  expect_true(file.info(fpq_project_paths(root)$logs)$size > 0)
  fpq_store_validation(project, list(fpq_validation_record("test", "PASS", "ok")))
  fpq_store_provenance(project, list(list(schema_version = "1.0.0",
    provenance_edge_id = "prov-test", project_id = project$manifest$project_id,
    subject_type = "source_file", subject_id = "source",
    relationship = "derived_from", object_type = "artifact", object_id = "artifact",
    recorded_at = fpq_now(), run_id = NULL, evidence_hash = NULL, notes = NULL)))
  expect_length(fpq_trace_provenance(project, "artifact"), 1)
})
