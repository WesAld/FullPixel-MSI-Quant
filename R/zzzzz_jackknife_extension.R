# Loaded after existing workflow extensions. Adds only the optional V3
# downstream biological-replicate jackknife workbook layer.

fpq_jackknife_analysis_type <- function(workflow = list(), config = list(),
                                        manifest = list()) {
  workflow$analysis_type %||% config$analysis_type %||%
    manifest$project_preferences$analysis_type %||% ""
}

fpq_add_jackknife_workbook <- function(project, workflow, masters, result,
                                       repository_root, attempt_id = NULL) {
  config <- fpq_load_project_configuration(project)
  analysis_type <- fpq_jackknife_analysis_type(workflow, config,
    project$manifest)
  if (!identical(analysis_type, "compare_two_groups")) return(result)
  design <- fpq_jackknife_design(config)
  requested <- fpq_jackknife_enabled(config)
  # A design that became ineligible after configuration must not block the
  # ordinary workflow. Record it as unavailable and omit jackknife output.
  enabled <- requested && isTRUE(design$eligible)

  token <- format(Sys.time(), "%Y%m%d%H%M%S")
  output_directory <- file.path(fpq_project_paths(project$root)$results,
    paste0("jackknife-", token))
  metadata <- fpq_all_features_build_display_metadata(project, workflow, masters)
  request <- list(
    schema_version = "1.0.0",
    workflow_id = workflow$workflow_id,
    project_id = project$manifest$project_id,
    analysis_type = "compare_two_groups",
    primary_normalization_method = fpq_all_features_normalization_method(workflow),
    display_metadata = metadata,
    jackknife = list(enabled = enabled, requested = requested,
      eligibility_message = design$reason,
      resampling_unit = "matched_biological_replicate_pairs",
      replicate_ids = as.list(design$replicate_ids),
      n_pairs = design$n_pairs),
    source_workbook = list(path = normalizePath(result$path,
      winslash = "/", mustWork = TRUE), sha256 = fpq_hash_file(result$path)),
    output_filename = basename(result$path),
    output_working_directory = output_directory
  )
  request_path <- file.path(workflow$output_folder, "requests",
    paste0("jackknife-export-", token, ".json"))
  report_path <- file.path(workflow$output_folder,
    paste0("jackknife-export-report-", token, ".json"))
  fpq_atomic_write_json(request, request_path)
  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python.exe")
  process <- processx::run(python, c(file.path(repository_root, "tools",
    "jackknife_workbook_adapter.py"), request_path, "--report", report_path),
    wd = repository_root, error_on_status = FALSE)
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  if (process$status != 0L || is.null(report) || !identical(report$status, "PASS"))
    fpq_stop("workflow.jackknife_export_failed",
      report$error %||% "Jackknife workbook export failed.", "scientific")
  final_path <- normalizePath(report$output_workbook$path,
    winslash = "/", mustWork = TRUE)
  final_hash <- fpq_hash_file(final_path)
  if (!identical(final_hash, report$output_workbook$sha256))
    fpq_stop("workflow.jackknife_hash",
      "Jackknife workbook hash verification failed.", "scientific")
  artifact <- fpq_register_artifact(project, list(
    artifact_id = paste0("art-", workflow$workflow_id,
      "-jackknife-final-workbook-", token),
    artifact_type = "final_results_workbook",
    display_name = basename(final_path), path = final_path,
    project_id = project$manifest$project_id, dataset_id = NULL,
    ion_mode = NULL, exposure = workflow$exposure,
    producing_run_id = workflow$workflow_id,
    producing_stage_id = "jackknife_robustness_analysis",
    sha256 = final_hash, file_size = unname(file.info(final_path)$size),
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    validation_status = "PASS",
    scientific_profile_id = project$manifest$scientific_profile_id,
    scientific_core_version = project$manifest$scientific_core_version,
    # The prior final workbook is the immutable source recorded by hash in the
    # adapter request/report. Do not retain it as an active parent because the
    # replacement activation supersedes it and would immediately mark this
    # artifact stale. Retain the annotated scientific inputs instead.
    parent_artifact_ids = as.list(Filter(function(x) nzchar(x),
      vapply(masters, function(x) x$artifact_id %||% "", character(1L)))),
    notes = paste("V3 optional matched biological-replicate-pair jackknife:",
      if (enabled) "enabled" else "disabled", "; method",
      fpq_all_features_normalization_method(workflow),
      "; pixels were not resampling units.")), activate = TRUE)
  result$path <- final_path
  result$sha256 <- artifact$sha256
  result$artifact_id <- artifact$artifact_id
  result$report$jackknife <- report
  result$validation$jackknife <- list(status = "PASS", enabled = enabled)
  result
}

if (!exists("fpq_run_script07_and_presentation_before_jackknife",
            inherits = FALSE))
  fpq_run_script07_and_presentation_before_jackknife <-
    fpq_run_script07_and_presentation

fpq_run_script07_and_presentation <- function(
    project, workflow, masters, repository_root, attempt_id = NULL) {
  result <- fpq_run_script07_and_presentation_before_jackknife(
    project, workflow, masters, repository_root, attempt_id)
  fpq_add_jackknife_workbook(project, workflow, masters, result,
    repository_root, attempt_id)
}
