fpq_dispatch_compatibility_configuration <- function(request) {
  analysis_type <- request$analysis_type %||% "compare_two_groups"

  conditions <- lapply(
    seq_along(request$conditions %||% list()),
    function(index) {
      item <- (request$conditions %||% list())[[index]]
      condition_id <- trimws(as.character(
        item$condition_id %||% ""
      ))
      if (!nzchar(condition_id))
        fpq_stop(
          "dispatch.condition_id_missing",
          paste("Execution condition", index, "has no condition_id."),
          "configuration"
        )

      list(
        condition_id = condition_id,
        analytical_role = item$analytical_role %||% "comparison"
      )
    }
  )

  samples <- lapply(
    seq_along(request$samples %||% list()),
    function(index) {
      item <- (request$samples %||% list())[[index]]
      sample_id <- trimws(as.character(item$sample_id %||% ""))
      condition_id <- trimws(as.character(
        item$condition_id %||% ""
      ))
      replicate_id <- trimws(as.character(
        item$replicate_id %||% ""
      ))

      if (!nzchar(sample_id))
        fpq_stop(
          "dispatch.sample_id_missing",
          paste("Execution sample", index, "has no sample_id."),
          "configuration"
        )
      if (!nzchar(condition_id))
        fpq_stop(
          "dispatch.sample_condition_missing",
          paste("Execution sample", sample_id,
                "has no condition_id."),
          "configuration"
        )
      if (!nzchar(replicate_id))
        fpq_stop(
          "dispatch.sample_replicate_missing",
          paste("Execution sample", sample_id,
                "has no replicate_id."),
          "configuration"
        )

      list(
        sample_id = sample_id,
        condition_id = condition_id,
        replicate_id = replicate_id,
        ion_mode = request$ion_mode,
        inclusion_status = "included"
      )
    }
  )

  source_comparisons <- if (identical(
      analysis_type, "compare_multiple_groups")) {
    request$comparisons %||% list()
  } else if (!is.null(request$comparison)) {
    list(request$comparison)
  } else {
    list()
  }

  comparisons <- lapply(
    seq_along(source_comparisons),
    function(index) {
      item <- source_comparisons[[index]]
      comparison_id <- trimws(as.character(
        item$comparison_id %||%
          paste0("comparison-", index)
      ))
      reference_id <- trimws(as.character(
        item$reference_condition_id %||% ""
      ))
      treatment_id <- trimws(as.character(
        item$comparison_condition_id %||% ""
      ))

      if (!nzchar(reference_id) || !nzchar(treatment_id))
        fpq_stop(
          "dispatch.comparison_condition_missing",
          paste("Execution comparison", comparison_id,
                "does not identify both conditions."),
          "configuration"
        )

      list(
        comparison_id = comparison_id,
        reference_condition_id = reference_id,
        comparison_condition_id = treatment_id,
        enabled = isTRUE(item$enabled %||% TRUE)
      )
    }
  )

  list(
    analysis_type = analysis_type,
    ion_mode = request$ion_mode,
    enabled_ion_modes = list(request$ion_mode),
    exposure_label = request$exposure_label,
    conditions = conditions,
    samples = samples,
    comparisons = comparisons,
    mass_tolerance = request$display_metadata$mass_tolerance %||%
      fpq_default_mass_tolerance()
  )
}

fpq_dispatch_adapter <- function(project, request_path,
                                 repository_root = fpq_repo_root(),
                                 python = Sys.which("python")) {
  request <- fpq_read_json(request_path)
  if (!identical(request$project_id, project$manifest$project_id))
    fpq_stop("dispatch.project_mismatch", "Execution request belongs to a different project.")
  if (identical(request$stage_id, "summarize_replicate_readouts"))
    return(fpq_dispatch_script03(project, request_path, repository_root, python))
  if (identical(request$stage_id, "integrate_mzmine_annotations"))
    return(fpq_dispatch_script04(project, request_path, repository_root, python))
  stage <- fpq_workflow_stage_status(request$stage_id, request$ion_mode,
                                     request$exposure_label)
  if (!stage$executable) fpq_stop("dispatch.scope_blocked", stage$reason, "scientific")
  compatibility_config <-
    fpq_dispatch_compatibility_configuration(request)
  compatibility <- fpq_validate_profile_compatibility(
    compatibility_config, request$scientific_profile$profile_id)
  fpq_store_validation(project, compatibility$records)
  if (!compatibility$classification %in%
      c("profile_compatible", "profile_candidate_compatible"))
    fpq_stop("dispatch.profile_incompatible", compatibility$records[[1]]$message, "scientific")
  environment <- fpq_check_environment(request, repository_root)
  if (environment$status == "incompatible")
    fpq_stop("dispatch.environment_incompatible", "Execution environment is incompatible.",
             "environment")
  integrity <- fpq_verify_scientific_core(repository_root)
  if (integrity$status != "compatible")
    fpq_stop("dispatch.integrity", "Frozen scientific-core integrity failed.", "scientific")

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
    fpq_touch_worker_heartbeat(project, run$run_id, "dispatching")
  }
  fpq_set_workspace_state(project, run$run_id, "active")
  report_path <- file.path(dirname(fpq_run_path(project, run$run_id)), "adapter_report.json")
  adapter_stdout <- file.path(dirname(fpq_run_path(project, run$run_id)),
                              "adapter-process.stdout.log")
  adapter_stderr <- file.path(dirname(fpq_run_path(project, run$run_id)),
                              "adapter-process.stderr.log")
  adapter_file <- if ((request$execution_adapter %||% "") %in%
      c("generic_two_group", "generic_quantify_only",
        "generic_multi_group"))
    "generic_extraction_adapter.py" else "pvl_adapter.py"
  process <- processx::process$new(python,
    c(file.path(repository_root, "tools", adapter_file), request_path,
      "--report", report_path), wd = repository_root,
    stdout = adapter_stdout, stderr = adapter_stderr,
    cleanup = FALSE, supervise = FALSE)
  cancelled <- FALSE
  while (process$is_alive()) {
    if (!is.null(identity))
      fpq_touch_worker_heartbeat(project, run$run_id, "scientific_process_active")
    if (fpq_cancellation_requested(project, run$run_id)) {
      current <- fpq_load_run(project, run$run_id)
      if (current$status != "cancelling")
        fpq_transition_run(project, run$run_id, "cancelling")
      try(process$kill_tree(), silent = TRUE)
      process$wait(timeout = 10000)
      cancelled <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  result_status <- process$get_exit_status()
  if (is.null(result_status)) result_status <- 1L
  result <- list(status = as.integer(result_status),
    stdout = if (file.exists(adapter_stdout))
      paste(readLines(adapter_stdout, warn = FALSE), collapse = "\n") else "",
    stderr = if (file.exists(adapter_stderr))
      paste(readLines(adapter_stderr, warn = FALSE), collapse = "\n") else "")
  if (cancelled) {
    partial <- if (dir.exists(request$output_working_directory))
      list.files(request$output_working_directory, recursive = TRUE,
                 full.names = TRUE) else character()
    return(list(run = fpq_finalize_cancelled_run(project, run$run_id,
      "cooperative_marker_process_tree", result$status, partial),
      artifact = NULL, adapter_report = NULL))
  }
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  if (is.null(report) || result$status != 0L || report$status != "completed") {
    record <- fpq_transition_run(project, run$run_id, "failed",
                                 list(adapter_exit_status = result$status))
    record$failure <- list(message = report$recommended_correction %||%
                             "Adapter failed without a structured report.",
                           stdout = result$stdout, stderr = result$stderr)
    fpq_atomic_write_json(record, fpq_run_path(project, run$run_id))
    fpq_set_workspace_state(project, run$run_id, "failed_preserved")
    fpq_stop("dispatch.adapter_failed", record$failure$message, "infrastructure")
  }
  if (!is.null(identity))
    fpq_touch_worker_heartbeat(project, run$run_id, "validating")
  fpq_transition_run(project, run$run_id, "validating")
  produced <- report$produced_outputs[[1]]
  output <- list(artifact_id = produced$artifact_id,
    artifact_type = "pixel_intensity_table", path = produced$path,
    project_id = project$manifest$project_id, dataset_id = request$dataset_id,
    producing_run_id = request$run_id, sha256 = produced$sha256,
    file_size = produced$bytes, validation_status = "PASS",
    ion_mode = request$ion_mode, exposure = request$exposure_label,
    producing_stage_id = request$stage_id, path_type = "external_absolute",
    schema_id = if (identical(request$execution_adapter %||% "",
      "generic_quantify_only")) "script01.generic_quantify_only_sample_set.v1"
      else if (identical(request$execution_adapter %||% "",
        "generic_two_group")) "script01.generic_sample_set_feature_summary.v1"
      else if (identical(request$execution_adapter %||% "",
        "generic_multi_group")) "script01.generic_multi_group_sample_set.v1"
      else "script01.six_sample_combined_feature_summary.v1",
    scientific_profile_id = request$scientific_profile$profile_id,
    scientific_core_version = request$scientific_core_version,
    parent_artifact_ids = c(request$registered_inputs$aligned_target_table$artifact_id),
    notes = paste("execution-copy hash", report$execution_copy_hash))
  fpq_register_artifact(project, output, activate = TRUE)
  edge <- list(schema_version = "1.0.0", provenance_edge_id = fpq_id("prov"),
    project_id = project$manifest$project_id, subject_type = "artifact",
    subject_id = output$artifact_id, relationship = "produced_by", object_type = "run",
    object_id = request$run_id, recorded_at = fpq_now(), run_id = request$run_id,
    evidence_hash = output$sha256,
    notes = paste("Frozen hash", report$frozen_script_hash,
                  "execution-copy hash", report$execution_copy_hash))
  provenance <- list(edge)
  registry_value <- request$registered_inputs$raw_file_hash_registry$path
  registry_path <- if (file.exists(registry_value)) registry_value else
    file.path(repository_root, registry_value)
  if (file.exists(registry_path)) {
    source_registry <- fpq_read_json(registry_path)
    provenance <- c(provenance, lapply(source_registry$files, function(source) list(
      schema_version = "1.0.0", provenance_edge_id = fpq_id("prov"),
      project_id = project$manifest$project_id, subject_type = "source_file",
      subject_id = source$role, relationship = "used_by", object_type = "run",
      object_id = request$run_id, recorded_at = fpq_now(), run_id = request$run_id,
      evidence_hash = source$sha256, notes = source$path)))
  }
  provenance <- c(provenance, list(list(schema_version = "1.0.0",
    provenance_edge_id = fpq_id("prov"), project_id = project$manifest$project_id,
    subject_type = "artifact",
    subject_id = request$registered_inputs$aligned_target_table$artifact_id,
    relationship = "used_by", object_type = "run", object_id = request$run_id,
    recorded_at = fpq_now(), run_id = request$run_id,
    evidence_hash = request$registered_inputs$aligned_target_table$sha256,
    notes = request$registered_inputs$aligned_target_table$path)))
  fpq_store_provenance(project, provenance)
  completed <- fpq_transition_run(project, run$run_id, "completed")
  completed$output_artifacts <- list(output$artifact_id)
  completed$validation_records <- report$validation_records
  completed$logs <- list(stdout = report$stdout_path, stderr = report$stderr_path)
  completed$exit_status <- result$status
  fpq_atomic_write_json(completed, fpq_run_path(project, run$run_id))
  fpq_set_workspace_state(project, run$run_id, "completed_preserved")
  list(run = completed, artifact = output, adapter_report = report)
}
