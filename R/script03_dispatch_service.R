fpq_validate_script03_input <- function(project, request) {
  input <- request$registered_inputs$pixel_intensity_artifact
  if (is.null(input))
    fpq_stop("script03.input_missing",
             "A registered Script 01 pixel-intensity artifact is required.",
             "input")
  if (!identical(input$project_id, project$manifest$project_id))
    fpq_stop("script03.input_project", "Upstream artifact belongs to another project.",
             "scientific")
  if (!identical(input$dataset_id, request$dataset_id))
    fpq_stop("script03.input_dataset", "Upstream artifact belongs to another dataset.",
             "scientific")
  registered <- fpq_inspect_artifact(project, input$artifact_id)
  if (!identical(registered$artifact_type, "pixel_intensity_table") ||
      !identical(registered$active_status, "active") ||
      !identical(registered$validation_status, "PASS"))
    fpq_stop("script03.input_inactive",
             "Upstream pixel-intensity artifact is not active and validated.",
             "scientific")
  if (!file.exists(registered$path) ||
      !identical(fpq_hash_file(registered$path), registered$sha256) ||
      !identical(registered$sha256, input$sha256))
    fpq_stop("script03.input_hash",
             "Upstream pixel-intensity artifact identity failed.",
             "scientific")
  registered
}

fpq_script03_artifact_type <- function(filename) {
  if (grepl("_QuantifyOnly_Sample_Metrics[.]csv$", filename))
    return("replicate_readout_summary")
  if (grepl("_ThreeReadout_Summary[.]csv$", filename))
    return("replicate_readout_summary")
  if (grepl("_ThreeReadout_Summary[.]xlsx$", filename))
    return("replicate_readout_summary_workbook")
  switch(filename,
    "PVL_Single_Negative_ThreeReadout_Summary.csv" =
      "replicate_readout_summary",
    "PVL_Single_Negative_ThreeReadout_Summary.xlsx" =
      "replicate_readout_summary_workbook",
    "analysis_validation_summary.csv" = "replicate_readout_validation",
    "three_readout_status_summary.csv" = "replicate_readout_status",
    "replicate_variability_summary.csv" = "replicate_variability_summary",
    "analysis_configuration_used.csv" = "configuration_snapshot",
    "sessionInfo.txt" = "scientific_execution_record",
    "three_readout_analysis_report.txt" = "validation_report",
    "03_three_readout_replicate_summary_used.R" =
      "scientific_execution_record",
    "scientific_execution_record")
}

fpq_script03_media_type <- function(filename) {
  if (grepl("[.]csv$", filename, ignore.case = TRUE)) "text/csv" else
    if (grepl("[.]xlsx$", filename, ignore.case = TRUE))
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" else
        "text/plain"
}

fpq_script03_adapter_failure_message <- function(report) {
  if (is.null(report) || !is.list(report))
    return("Script 03 adapter failed without a passing regression report.")
  error <- trimws(as.character(report$error %||% ""))
  if (nzchar(error)) return(error)
  records <- report$validation_records %||% list()
  blocking <- Filter(function(record)
    isTRUE(record$blocking) && identical(record$status %||% "", "FAIL") &&
      nzchar(trimws(as.character(record$message %||% ""))), records)
  if (length(blocking))
    return(trimws(as.character(blocking[[1]]$message)))
  correction <- trimws(as.character(report$recommended_correction %||% ""))
  if (nzchar(correction)) return(correction)
  "Script 03 adapter failed without a passing regression report."
}

fpq_dispatch_script03 <- function(project, request_path,
                                  repository_root = fpq_repo_root(),
                                  python = Sys.which("python")) {
  request <- fpq_read_json(request_path)
  if (!identical(request$project_id, project$manifest$project_id))
    fpq_stop("dispatch.project_mismatch", "Execution request belongs to a different project.")
  stage <- fpq_workflow_stage_status(request$stage_id, request$ion_mode,
                                     request$exposure_label)
  if (!stage$executable)
    fpq_stop("dispatch.scope_blocked", stage$reason, "scientific")
  config <- list(
    analysis_type = request$analysis_type %||% "compare_two_groups",
    ion_mode = request$ion_mode,
    enabled_ion_modes = list(request$ion_mode),
    exposure_label = request$exposure_label,
    conditions = request$conditions,
    samples = lapply(request$samples, function(x) modifyList(x,
      list(ion_mode = request$ion_mode, inclusion_status = "included"))),
    comparisons = if (identical(
        request$analysis_type %||% "",
        "compare_multiple_groups"))
      request$comparisons %||% list()
    else if (is.null(request$comparison)) list()
    else list(request$comparison))
  compatibility <- fpq_validate_profile_compatibility(
    config, request$scientific_profile$profile_id)
  fpq_store_validation(project, compatibility$records)
  if (!compatibility$classification %in%
      c("profile_compatible", "profile_candidate_compatible"))
    fpq_stop("dispatch.profile_incompatible",
             compatibility$records[[1]]$message, "scientific")
  upstream <- fpq_validate_script03_input(project, request)
  environment <- fpq_check_environment(request, repository_root)
  if (environment$status == "incompatible")
    fpq_stop("dispatch.environment_incompatible",
             "Script 03 execution environment is incompatible.", "environment")
  integrity <- fpq_verify_scientific_core(repository_root)
  if (integrity$status != "compatible")
    fpq_stop("dispatch.integrity", "Frozen scientific-core integrity failed.",
             "scientific")

  run <- fpq_create_run(project, request)
  fpq_prepare_workspace_record(project, run$run_id,
                               request$output_working_directory)
  fpq_transition_run(project, run$run_id, "preflight_running")
  fpq_transition_run(project, run$run_id, "ready")
  fpq_transition_run(project, run$run_id, "starting")
  running <- fpq_transition_run(project, run$run_id, "running")
  identity <- fpq_load_worker_identity(project, run$run_id)
  if (!is.null(identity)) {
    running$worker_id <- identity$worker_id
    running$process_id <- identity$process_id
    fpq_atomic_write_json(running, fpq_run_path(project, run$run_id))
    fpq_touch_worker_heartbeat(project, run$run_id, "dispatching_script03")
  }
  fpq_set_workspace_state(project, run$run_id, "active")
  report_path <- file.path(dirname(fpq_run_path(project, run$run_id)),
                           "adapter_report.json")
  adapter_stdout <- file.path(dirname(report_path), "adapter-process.stdout.log")
  adapter_stderr <- file.path(dirname(report_path), "adapter-process.stderr.log")
  adapter_file <- if (identical(request$execution_adapter %||% "",
      "generic_quantify_only")) "quantify_only_summary_adapter.py"
    else if (identical(request$execution_adapter %||% "",
      "generic_two_group")) "generic_script03_adapter.py"
    else if (identical(request$execution_adapter %||% "",
      "generic_multi_group")) "multi_group_summary_adapter.py"
    else "script03_adapter.py"
  process <- processx::process$new(python,
    c(file.path(repository_root, "tools", adapter_file),
      request_path, "--report", report_path), wd = repository_root,
    stdout = adapter_stdout, stderr = adapter_stderr,
    cleanup = FALSE, supervise = FALSE)
  cancelled <- FALSE
  while (process$is_alive()) {
    if (!is.null(identity))
      fpq_touch_worker_heartbeat(project, run$run_id,
                                 "script03_process_active")
    if (fpq_cancellation_requested(project, run$run_id)) {
      current <- fpq_load_run(project, run$run_id)
      if (current$status != "cancelling")
        fpq_transition_run(project, run$run_id, "cancelling")
      try(process$kill_tree(), silent = TRUE)
      process$wait(timeout = 10000)
      cancelled <- TRUE
      break
    }
    Sys.sleep(0.25)
  }
  status <- process$get_exit_status()
  if (is.null(status)) status <- 1L
  if (cancelled) {
    partial <- if (dir.exists(request$output_working_directory))
      list.files(request$output_working_directory, recursive = TRUE,
                 full.names = TRUE) else character()
    return(list(run = fpq_finalize_cancelled_run(project, run$run_id,
      "cooperative_marker_process_tree", status, partial),
      artifact = NULL, artifacts = list(), adapter_report = NULL))
  }
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  candidate_evidence <- identical(report$equivalence$status %||% "", "NOT_EVALUABLE") &&
    ((request$execution_adapter %||% "") %in%
      c("generic_two_group", "generic_quantify_only", "generic_multi_group") ||
     identical(request$exposure_label, "Sequential") ||
     identical(request$ion_mode, "POS"))
  if (is.null(report) || status != 0L || report$status != "completed" ||
      (!identical(report$equivalence$status, "PASS") && !candidate_evidence)) {
    failed <- fpq_transition_run(project, run$run_id, "failed",
      list(adapter_exit_status = status))
    failed$failure <- list(message =
      fpq_script03_adapter_failure_message(report),
      adapter_report = report_path, stdout = adapter_stdout,
      stderr = adapter_stderr)
    fpq_atomic_write_json(failed, fpq_run_path(project, run$run_id))
    fpq_set_workspace_state(project, run$run_id, "failed_preserved")
    fpq_stop("dispatch.script03_failed", failed$failure$message,
             "infrastructure")
  }

  fpq_transition_run(project, run$run_id, "validating")
  validation <- fpq_validation_record("script03.regression_equivalence",
    if (candidate_evidence) "WARNING" else "PASS",
    if (candidate_evidence) {
      if (identical(request$execution_adapter %||% "",
                    "generic_quantify_only"))
        paste(
          "Quantify-only per-sample readout validation passed;",
          "no comparison statistics were calculated."
        )
      else if (identical(request$execution_adapter %||% "", "generic_two_group"))
        paste(
          "Generic two-group Script 03 internal validation passed;",
          "no dataset-specific historical oracle was supplied."
        )
      else if (identical(request$execution_adapter %||% "", "generic_multi_group"))
        paste(
          "Multi-group pairwise frozen Script 03 validations passed;",
          "no dataset-specific historical oracle was supplied."
        )
      else if (identical(request$ion_mode, "POS") &&
               identical(request$exposure_label, "Single"))
        "Positive-mode Script 03 internal validation passed; no approved complete Positive regression oracle is available, and independent scientific review remains pending."
      else
        "Script 03 internal validation passed; independent Sequential review remains pending."
    } else "Script 03 output matches the provisional golden reference.",
    if (candidate_evidence) "WARNING" else "INFO", FALSE, observed = list(
      maximum_absolute_difference =
        report$equivalence$maximum_absolute_difference %||% NA_real_,
      maximum_relative_difference =
        report$equivalence$maximum_relative_difference %||% NA_real_),
    expected = list(absolute_tolerance =
      report$equivalence$absolute_tolerance,
      relative_tolerance = report$equivalence$relative_tolerance),
    consequence = "Replicate-summary output may be registered.",
    remediation = "None.")
  validation$run_id <- run$run_id
  validation$stage_id <- request$stage_id
  fpq_store_validation(project, list(validation))

  artifacts <- lapply(report$produced_outputs, function(produced) {
    primary <- grepl(
      "(_ThreeReadout_Summary|_QuantifyOnly_Sample_Metrics)[.]csv$",
      produced$artifact_role)
    artifact <- list(artifact_id = paste0("art-", run$run_id, "-",
      gsub("(^-+|-+$)", "", gsub("[^a-z0-9]+", "-",
        tolower(produced$artifact_role)))),
      artifact_type = fpq_script03_artifact_type(produced$artifact_role),
      display_name = produced$artifact_role, path = produced$path,
      project_id = project$manifest$project_id, dataset_id = request$dataset_id,
      producing_run_id = run$run_id, sha256 = produced$sha256,
      file_size = as.integer(produced$bytes), validation_status = "PASS",
      ion_mode = request$ion_mode, exposure = request$exposure_label,
      producing_stage_id = request$stage_id, path_type = "external_absolute",
      media_type = fpq_script03_media_type(produced$artifact_role),
      row_count = produced$rows, column_count = produced$columns,
      schema_id = if (primary) {
        if (identical(request$execution_adapter %||% "",
                      "generic_quantify_only"))
          "quantify_only.per_sample_metrics.v1"
        else if (identical(request$execution_adapter %||% "", "generic_two_group"))
          "script03.generic_three_readout_summary.v1"
        else if (identical(request$execution_adapter %||% "", "generic_multi_group"))
          "script03.generic_multi_group_three_readout_summary.v1"
        else
          "script03.three_readout_summary.94_columns.v1"
      } else NULL,
      scientific_profile_id = request$scientific_profile$profile_id,
      scientific_core_version = request$scientific_core_version,
      parent_artifact_ids = list(upstream$artifact_id),
      notes = paste(
        "Script 03 execution-copy hash", report$execution_copy_hash,
        "reference classification",
        if ((request$execution_adapter %||% "") %in%
            c("generic_two_group", "generic_quantify_only", "generic_multi_group"))
          "technical_validation_candidate"
        else "provisional_golden"
      ))
    fpq_register_artifact(project, artifact, activate = primary)
  })
  primary <- Filter(function(x)
    identical(x$artifact_type, "replicate_readout_summary"), artifacts)[[1]]
  edges <- c(list(list(schema_version = "1.0.0",
    provenance_edge_id = fpq_id("prov"),
    project_id = project$manifest$project_id, subject_type = "artifact",
    subject_id = upstream$artifact_id, relationship = "used_by",
    object_type = "run", object_id = run$run_id, recorded_at = fpq_now(),
    run_id = run$run_id, evidence_hash = upstream$sha256,
    notes = "Validated active Script 01 output")),
    lapply(artifacts, function(x) list(schema_version = "1.0.0",
      provenance_edge_id = fpq_id("prov"),
      project_id = project$manifest$project_id, subject_type = "artifact",
      subject_id = x$artifact_id, relationship = "produced_by",
      object_type = "run", object_id = run$run_id, recorded_at = fpq_now(),
      run_id = run$run_id, evidence_hash = x$sha256,
      notes = paste("Frozen Script 03", request$frozen_script$sha256,
                    "execution copy", report$execution_copy_hash))))
  fpq_store_provenance(project, edges)
  completed <- fpq_transition_run(project, run$run_id, "completed")
  completed$output_artifacts <- lapply(artifacts, `[[`, "artifact_id")
  completed$validation_records <- list(validation$validation_record_id)
  completed$logs <- list(stdout = report$stdout_path,
                         stderr = report$stderr_path,
                         adapter_stdout = adapter_stdout,
                         adapter_stderr = adapter_stderr)
  completed$exit_status <- status
  completed$parent_run_id <- upstream$producing_run_id
  completed$parent_artifact_id <- upstream$artifact_id
  completed$execution_copy_hash <- report$execution_copy_hash
  fpq_atomic_write_json(completed, fpq_run_path(project, run$run_id))
  fpq_set_workspace_state(project, run$run_id, "completed_preserved")
  list(run = completed, artifact = primary, artifacts = artifacts,
       adapter_report = report)
}
