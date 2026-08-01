# All-registered-features extension for two-group comparison workbooks.
# Loaded after the quantify-only extension so both workflow branches remain intact.

fpq_all_features_sheet_name <- function() "All_Features_TIC_Normalized"

fpq_all_features_is_quantify_only <- function(workflow) {
  identical(workflow$execution_adapter %||% "", "generic_quantify_only") ||
    identical(workflow$analysis_type %||% "", "quantify_only")
}

fpq_all_features_build_display_metadata <- function(project, workflow,
                                                    masters) {
  metadata <- workflow$display_metadata %||% list()
  if (length(metadata$sample_mappings %||% list())) return(metadata)

  config <- fpq_load_project_configuration(project)
  modes <- names(masters)
  mappings <- unlist(lapply(modes, function(mode) {
    samples <- fpq_internal_mode_samples(config, mode)
    lapply(samples, function(sample) list(
      user_facing_value = sample$original_sample_id,
      user_facing_label = sample$display_sample_id %||%
        sample$biological_sample %||% sample$original_sample_id,
      original_condition_id = sample$original_condition_id,
      original_replicate_id = sample$original_replicate_id,
      internal_value = sample$sample_id,
      ion_mode = mode
    ))
  }), recursive = FALSE)

  first_mode <- modes[[1L]]
  first_samples <- fpq_internal_mode_samples(config, first_mode)
  reference <- Filter(function(x) identical(x$condition_id, "CTRL"),
                      first_samples)
  comparison <- Filter(function(x) identical(x$condition_id, "TRT"),
                       first_samples)

  metadata$project_name <- project$manifest$project_name
  metadata$exposure_label <- workflow$exposure
  metadata$reference_condition <-
    reference[[1L]]$display_condition %||% "CTRL"
  metadata$comparison_condition <-
    comparison[[1L]]$display_condition %||% "TRT"
  metadata$sample_mappings <- mappings
  metadata
}

fpq_add_all_features_comparison_sheet <- function(
    project, workflow, masters, result, repository_root,
    attempt_id = NULL) {
  if (fpq_all_features_is_quantify_only(workflow)) return(result)
  if (is.null(result$path) || !file.exists(result$path))
    fpq_stop("workflow.all_features_source_missing",
      "The validated comparison workbook is unavailable.", "scientific")
  if (!length(masters))
    fpq_stop("workflow.all_features_masters_missing",
      "No annotated masters are available for the all-features sheet.",
      "scientific")

  existing_sheets <- tryCatch(openxlsx::getSheetNames(result$path),
                              error = function(e) character())
  if (fpq_all_features_sheet_name() %in% existing_sheets) return(result)

  token <- format(Sys.time(), "%Y%m%d%H%M%S")
  suffix <- if (is.null(attempt_id) || !nzchar(attempt_id)) "" else
    paste0("-", gsub("[^A-Za-z0-9_-]+", "-", attempt_id))
  # Store the published workbook in a short, stable project-level path.
  # Deep workflow paths plus the long publication filename can exceed the
  # traditional Windows MAX_PATH limit and become unavailable after restart.
  output_directory <- file.path(
    project$root,
    "results",
    paste0("all-features-", token)
  )
  metadata <- fpq_all_features_build_display_metadata(
    project, workflow, masters)

  request <- list(
    schema_version = "1.0.0",
    workflow_id = workflow$workflow_id,
    project_id = project$manifest$project_id,
    analysis_type = "compare_two_groups",
    exposure_label = workflow$exposure,
    display_metadata = metadata,
    source_workbook = list(
      path = normalizePath(result$path, winslash = "/", mustWork = TRUE),
      sha256 = fpq_hash_file(result$path),
      artifact_id = result$artifact_id %||% NULL
    ),
    masters = unname(lapply(names(masters), function(mode) list(
      ion_mode = mode,
      path = normalizePath(masters[[mode]]$path,
                           winslash = "/", mustWork = TRUE),
      sha256 = masters[[mode]]$sha256,
      artifact_id = masters[[mode]]$artifact_id
    ))),
    output_filename = basename(result$path),
    output_working_directory = output_directory
  )

  request_path <- file.path(
    workflow$output_folder, "requests",
    paste0("all-features-comparison-export", suffix, "-", token, ".json"))
  report_path <- file.path(
    workflow$output_folder,
    paste0("all-features-comparison-export-report", suffix, "-", token,
           ".json"))
  stdout <- file.path(
    workflow$output_folder,
    paste0("all-features-comparison-export", suffix, "-", token,
           ".stdout.log"))
  stderr <- file.path(
    workflow$output_folder,
    paste0("all-features-comparison-export", suffix, "-", token,
           ".stderr.log"))
  fpq_atomic_write_json(request, request_path)

  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python.exe")
  if (!nzchar(python))
    fpq_stop("workflow.all_features_python_missing",
      "Python is required to create the all-features comparison sheet.",
      "environment")

  process <- processx::run(
    python,
    c(file.path(repository_root, "tools",
                "all_features_comparison_adapter.py"),
      request_path, "--report", report_path),
    wd = repository_root,
    stdout = stdout,
    stderr = stderr,
    error_on_status = FALSE
  )
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  if (process$status != 0L || is.null(report) ||
      !identical(report$status, "PASS"))
    fpq_stop("workflow.all_features_export_failed",
      report$error %||% paste(
        "All-features comparison export failed; inspect preserved diagnostics."),
      "scientific")

  final_path <- normalizePath(report$final_workbook$path,
                              winslash = "/", mustWork = TRUE)
  final_hash <- fpq_hash_file(final_path)
  if (!identical(final_hash, report$final_workbook$sha256))
    fpq_stop("workflow.all_features_hash",
      "All-features comparison workbook hash verification failed.",
      "scientific")

  artifact <- fpq_register_artifact(project, list(
    artifact_id = paste0(
      "art-", workflow$workflow_id,
      "-all-features-final-workbook-", token),
    artifact_type = "final_results_workbook",
    display_name = basename(final_path),
    path = final_path,
    project_id = project$manifest$project_id,
    dataset_id = NULL,
    ion_mode = NULL,
    exposure = workflow$exposure,
    producing_run_id = workflow$workflow_id,
    producing_stage_id = "create_combined_publication_export",
    sha256 = final_hash,
    file_size = unname(file.info(final_path)$size),
    validation_status = "PASS",
    scientific_profile_id = project$manifest$scientific_profile_id,
    scientific_core_version = project$manifest$scientific_core_version,
    # The previous final workbook is the immutable source file used to build
    # this workbook, but it must not be an active dependency. Activating this
    # replacement supersedes the previous final workbook; retaining it as a
    # parent would therefore mark the new workbook stale immediately.
    # Annotated masters remain the active scientific dependencies.
    parent_artifact_ids = as.list(Filter(
      function(x) !is.null(x) && nzchar(x),
      unname(vapply(masters, `[[`, "", "artifact_id"))
    )),
    notes = paste(
      "Six-sheet two-group comparison workbook;",
      "All_Features_TIC_Normalized retains every registered extracted feature,",
      "MZmine annotation score, individual TIC-normalized intensities,",
      "condition means, linear fold change, and log2 fold change;",
      "existing workbook sheets preserved; source final workbook recorded in",
      "the all-features adapter report but intentionally excluded from active",
      "parent dependencies to prevent self-supersession; Technical Validation Candidate"
    )
  ), activate = TRUE)

  list(
    path = final_path,
    sha256 = artifact$sha256,
    artifact_id = artifact$artifact_id,
    audit_workbook = result$audit_workbook %||% result$path,
    validation = list(
      status = "PASS",
      errors = character(),
      workbook_type = "two_group_comparison_all_features",
      all_features = report$validation
    ),
    report = report
  )
}

if (!exists("fpq_run_script07_and_presentation_before_all_features",
            inherits = FALSE)) {
  fpq_run_script07_and_presentation_before_all_features <-
    fpq_run_script07_and_presentation
}

fpq_run_script07_and_presentation <- function(
    project, workflow, masters, repository_root,
    attempt_id = NULL) {
  result <- fpq_run_script07_and_presentation_before_all_features(
    project, workflow, masters, repository_root, attempt_id)
  fpq_add_all_features_comparison_sheet(
    project, workflow, masters, result, repository_root, attempt_id)
}

fpq_latest_active_final_workbook <- function(project) {
  artifacts <- Filter(function(x)
    identical(x$artifact_type, "final_results_workbook") &&
      identical(x$active_status, "active") &&
      identical(x$validation_status, "PASS") &&
      identical(x$project_id, project$manifest$project_id),
    fpq_list_artifacts(project))
  if (!length(artifacts))
    fpq_stop("all_features.no_final_workbook",
      "This project has no active validated final workbook.")
  timestamps <- vapply(artifacts, function(x)
    x$registered_at %||% x$created_at %||% "", character(1L))
  artifacts[[order(timestamps, decreasing = TRUE)[[1L]]]]
}

fpq_all_features_masters_for_workbook <- function(project, final_artifact,
                                                  workflow) {
  artifacts <- fpq_list_artifacts(project)
  parent_ids <- unlist(final_artifact$parent_artifact_ids %||% list(),
                       use.names = FALSE)
  masters <- Filter(function(x)
    identical(x$artifact_type, "annotated_three_readout_master") &&
      identical(x$validation_status, "PASS") &&
      identical(x$project_id, project$manifest$project_id) &&
      x$artifact_id %in% parent_ids,
    artifacts)

  if (!length(masters)) {
    masters <- Filter(function(x)
      identical(x$artifact_type, "annotated_three_readout_master") &&
        identical(x$active_status, "active") &&
        identical(x$validation_status, "PASS") &&
        identical(x$project_id, project$manifest$project_id) &&
        identical(x$exposure %||% "", workflow$exposure %||% ""),
      artifacts)
  }
  if (!length(masters))
    fpq_stop("all_features.no_masters",
      "No validated annotated masters were found for this comparison workbook.")

  # Keep the most recently registered master per ion mode.
  modes <- unique(vapply(masters, function(x)
    toupper(x$ion_mode %||% ""), character(1L)))
  result <- list()
  for (mode in modes[nzchar(modes)]) {
    candidates <- Filter(function(x)
      identical(toupper(x$ion_mode %||% ""), mode), masters)
    timestamps <- vapply(candidates, function(x)
      x$registered_at %||% x$created_at %||% "", character(1L))
    result[[mode]] <- candidates[[order(timestamps,
                                        decreasing = TRUE)[[1L]]]]
  }
  result
}

fpq_upgrade_active_comparison_workbook <- function(
    project_root, repository_root = fpq_repo_root()) {
  project <- fpq_open_project(project_root)
  config <- fpq_load_project_configuration(project)
  analysis_type <- config$analysis_type %||%
    config$analysis_design$analysis_type %||% "compare_two_groups"
  if (identical(analysis_type, "quantify_only"))
    fpq_stop("all_features.quantify_only",
      "Quantify Samples Only already exports all registered features and does not use the comparison sheet.")

  final_artifact <- fpq_latest_active_final_workbook(project)
  current_sheets <- openxlsx::getSheetNames(final_artifact$path)
  if (fpq_all_features_sheet_name() %in% current_sheets)
    return(list(status = "already_present", path = final_artifact$path,
                artifact_id = final_artifact$artifact_id))

  workflows <- fpq_list_workflow_records(project)
  matching <- Filter(function(x)
    identical(x$workflow_id, final_artifact$producing_run_id), workflows)
  workflow <- if (length(matching)) matching[[1L]] else {
    completed <- Filter(function(x) identical(x$status, "completed"), workflows)
    if (!length(completed))
      fpq_stop("all_features.workflow_missing",
        "No completed workflow record was found for the active workbook.")
    completed[[1L]]
  }
  masters <- fpq_all_features_masters_for_workbook(
    project, final_artifact, workflow)
  base_result <- list(
    path = final_artifact$path,
    sha256 = final_artifact$sha256,
    artifact_id = final_artifact$artifact_id,
    audit_workbook = final_artifact$path,
    validation = list(status = "PASS", errors = character())
  )
  upgraded <- fpq_add_all_features_comparison_sheet(
    project, workflow, masters, base_result, repository_root,
    attempt_id = "existing-workbook-upgrade")
  try(fpq_update_workflow_record(project, workflow$workflow_id, list(
    final_workbook = upgraded[c(
      "path", "sha256", "artifact_id", "audit_workbook", "validation")],
    updated_at = fpq_now()
  )), silent = TRUE)
  c(list(status = "upgraded"), upgraded)
}
