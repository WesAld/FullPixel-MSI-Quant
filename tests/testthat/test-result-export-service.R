make_result_export_fixture <- function(method = "raw", status = "completed",
                                       validated = TRUE, jackknife = FALSE) {
  root <- tempfile("fpq-export-project-")
  project <- fpq_create_project(root, "prj-export-test", "Project")
  workflow_id <- paste0("workflow-export-", method)
  run_dir <- file.path(fpq_project_paths(project$root)$runs, workflow_id)
  dir.create(run_dir, recursive = TRUE)
  source <- file.path(run_dir, "validated-final.xlsx")
  writeBin(charToRaw(paste("validated workbook", method)), source)
  sha256 <- fpq_hash_file(source)
  artifact_id <- paste0("artifact-final-", method)
  artifact <- fpq_register_artifact(project, list(
    artifact_id = artifact_id,
    artifact_type = "final_results_workbook",
    display_name = "Validated final workbook",
    project_id = project$manifest$project_id,
    producing_run_id = workflow_id,
    producing_stage_id = "validating_workbook",
    path = normalizePath(source, winslash = "/"),
    path_type = "project_relative",
    filename = basename(source),
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    file_size = file.info(source)$size,
    sha256 = sha256,
    validation_status = if (validated) "PASS" else "FAIL",
    scientific_profile_id = "generic-two-group-v1",
    scientific_status = "approved",
    scientific_core_version = "1.0.0"), activate = FALSE)
  workflow <- list(
    schema_version = "1.0.0", workflow_id = workflow_id,
    project_id = project$manifest$project_id, status = status,
    created_at = fpq_now(), updated_at = fpq_now(), finished_at = fpq_now(),
    output_folder = normalizePath(run_dir, winslash = "/"),
    display_metadata = list(
      normalization = list(application_version = "4.0.0-dev",
        primary_normalization_method = method),
      jackknife = list(enabled = jackknife, requested = jackknife)),
    final_workbook = list(path = artifact$path, sha256 = sha256,
                          artifact_id = artifact_id))
  fpq_atomic_write_json(workflow,
    fpq_workflow_record_path(project, workflow_id))
  list(project = project, workflow = workflow, source = source,
       sha256 = sha256, artifact = artifact)
}

test_that("method-aware export names cover all supported normalizations", {
  expected <- c(raw = "RAW", spectrum_sum = "TIC", rms = "RMS", median = "MEDIAN")
  for (method in names(expected)) {
    fixture <- make_result_export_fixture(method)
    expect_equal(fpq_result_export_filename(fixture$project, fixture$workflow),
      paste0("Project_", expected[[method]],
             "_MALDIPixelQuant_Results.xlsx"))
  }
})

test_that("successful export is byte-identical and records provenance", {
  fixture <- make_result_export_fixture("rms", jackknife = TRUE)
  destination <- tempfile("fpq-export-destination-")
  dir.create(destination)
  result <- fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, destination,
    repository_root = normalizePath(file.path(getwd(), "..", "..")))
  expect_true(result$exported)
  expect_equal(result$filename,
    "Project_RMS_MALDIPixelQuant_Results.xlsx")
  expect_identical(toupper(fpq_hash_file(result$path)), toupper(fixture$sha256))
  expect_identical(readBin(result$path, "raw", n = file.info(result$path)$size),
                   readBin(fixture$source, "raw", n = file.info(fixture$source)$size))
  provenance <- fpq_read_collection(
    fpq_project_paths(fixture$project$root)$provenance)
  edge <- provenance[[length(provenance)]]
  expect_equal(edge$relationship, "frozen_as_researcher_export")
  expect_equal(edge$run_id, fixture$workflow$workflow_id)
  expect_equal(edge$normalization_method, "rms")
  expect_true(edge$jackknife_enabled)
  expect_equal(edge$source_sha256, edge$destination_sha256)
  expect_true(nzchar(edge$software_commit))
})

test_that("historical workflow method controls export despite current configuration", {
  fixture <- make_result_export_fixture("raw")
  current_project_method <- "median"
  destination <- tempfile("fpq-export-history-")
  dir.create(destination)
  result <- fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, destination)
  expect_equal(result$method, "raw")
  expect_equal(current_project_method, "median")
  expect_match(result$filename, "_RAW_")
  expect_false(grepl("MEDIAN", result$filename))
})

test_that("cancel and filename conflict never alter files or provenance", {
  fixture <- make_result_export_fixture("median")
  destination <- tempfile("fpq-export-conflict-")
  dir.create(destination)
  cancelled <- fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, "")
  expect_false(cancelled$exported)
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)
  filename <- fpq_result_export_filename(fixture$project, fixture$workflow)
  existing <- file.path(destination, filename)
  writeBin(charToRaw("do not replace"), existing)
  before <- fpq_hash_file(existing)
  expect_error(fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, destination), class = "fpq_error")
  expect_identical(fpq_hash_file(existing), before)
  expect_length(fpq_read_collection(
    fpq_project_paths(fixture$project$root)$provenance), 0L)
})

test_that("invalid runs and artifacts are ineligible", {
  incomplete <- make_result_export_fixture("raw", status = "failed")
  expect_false(fpq_result_export_eligibility(incomplete$project,
    incomplete$workflow$workflow_id)$eligible)
  invalid <- make_result_export_fixture("raw", validated = FALSE)
  expect_false(fpq_result_export_eligibility(invalid$project,
    invalid$workflow$workflow_id)$eligible)
  unlink(invalid$source)
  expect_false(fpq_result_export_eligibility(invalid$project,
    invalid$workflow$workflow_id)$eligible)
})

test_that("invalid destination fails without provenance", {
  fixture <- make_result_export_fixture("raw")
  missing <- file.path(tempfile("fpq-missing-parent-"), "not-created")
  expect_error(fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, missing), class = "fpq_error")
  expect_false(dir.exists(missing))
  expect_length(fpq_read_collection(
    fpq_project_paths(fixture$project$root)$provenance), 0L)
  existing <- tempfile("fpq-unwritable-")
  dir.create(existing)
  expect_error(fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, existing,
    directory_writable = function(path) FALSE), class = "fpq_error")
  expect_length(list.files(existing, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(fpq_read_collection(
    fpq_project_paths(fixture$project$root)$provenance), 0L)
})

test_that("copy hash mismatch leaves no export and no provenance", {
  fixture <- make_result_export_fixture("spectrum_sum")
  destination <- tempfile("fpq-export-mismatch-")
  dir.create(destination)
  calls <- 0L
  mismatching_hash <- function(path) {
    calls <<- calls + 1L
    if (calls == 1L) paste(rep("0", 64L), collapse = "") else fpq_hash_file(path)
  }
  expect_error(fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, destination,
    hash_file = mismatching_hash), class = "fpq_error")
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(fpq_read_collection(
    fpq_project_paths(fixture$project$root)$provenance), 0L)
})

test_that("alternate filename uses the existing Windows-safe sanitizer", {
  fixture <- make_result_export_fixture("raw")
  destination <- tempfile("fpq-export-alternate-")
  dir.create(destination)
  result <- fpq_export_validated_result(fixture$project,
    fixture$workflow$workflow_id, destination, "CON: alternate?.xlsx")
  expect_equal(result$filename, "CON_alternate.xlsx")
  expect_true(file.exists(result$path))
})

test_that("V4 Workflow and Run History expose the generic export action", {
  workflow_source <- paste(readLines(file.path("..", "..", "R",
    "shiny_module_profile_validation_workflow.R"), warn = FALSE), collapse = "\n")
  history_source <- paste(readLines(file.path("..", "..", "R", "v4_pages.R"),
    warn = FALSE), collapse = "\n")
  expect_match(workflow_source, '"Freeze / Export Result"', fixed = TRUE)
  expect_match(history_source, '"Freeze / Export Result"', fixed = TRUE)
  expect_match(history_source, 'identical(request$action, "freeze")', fixed = TRUE)
})
