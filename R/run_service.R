fpq_run_transitions <- function() list(
  configured = c("preflight_running"),
  preflight_running = c("preflight_failed", "ready"),
  preflight_failed = character(),
  ready = c("starting"),
  starting = c("running", "cancelling", "failed"),
  running = c("cancelling", "failed", "validating"),
  cancelling = c("cancelled", "failed"),
  cancelled = character(),
  failed = character(),
  validating = c("validation_failed", "failed", "completed"),
  validation_failed = character(),
  completed = character()
)
fpq_run_path <- function(project, run_id)
  file.path(fpq_project_paths(project$root)$runs, run_id, "run.json")

fpq_create_run <- function(project, request) {
  run_id <- request$run_id
  path <- fpq_run_path(project, run_id)
  if (file.exists(path)) fpq_stop("run.duplicate", "Run ID already exists.")
  now <- fpq_now()
  stage_status <- fpq_workflow_stage_status(
    request$stage_id, request$ion_mode, request$exposure_label)
  acknowledgement_path <- file.path(fpq_project_paths(project$root)$reports,
    paste0(run_id, "-candidate-acknowledgement.json"))
  acknowledgement <- if (file.exists(acknowledgement_path))
    fpq_read_json(acknowledgement_path) else NULL
  record <- list(schema_version = "1.0.0", run_id = run_id,
    project_id = project$manifest$project_id, dataset_id = request$dataset_id,
    stage_id = request$stage_id, ion_mode = request$ion_mode,
    exposure = request$exposure_label, scientific_profile = request$scientific_profile,
    scientific_status = stage_status$scientific_status %||% "approved",
    independent_scientific_review =
      if (identical(stage_status$scientific_status,
                    "technical_validation_candidate")) "pending" else "not_applicable",
    candidate_acknowledgement = acknowledgement,
    scientific_core_version = request$scientific_core_version,
    frozen_script = request$frozen_script, input_artifacts = request$registered_inputs,
    configuration_snapshot = request, status = "configured",
    status_history = list(list(status = "configured", timestamp = now)),
    created_at = now, updated_at = now, logs = list(), validation_records = list(),
    output_artifacts = list(), failure = NULL, cancellation = NULL,
    working_directory = request$output_working_directory,
    worker_id = NULL, process_id = NULL, exit_status = NULL,
    start_time = NULL, finish_time = NULL)
  fpq_atomic_write_json(record, path)
  fpq_log_event(project, "run.created", dataset_id = request$dataset_id, run_id = run_id,
                stage_id = request$stage_id)
  record
}
fpq_load_run <- function(project, run_id) fpq_read_json(fpq_run_path(project, run_id))

fpq_transition_run <- function(project, run_id, target, details = list()) {
  record <- fpq_load_run(project, run_id)
  allowed <- fpq_run_transitions()[[record$status]]
  if (!target %in% allowed)
    fpq_stop("run.transition_invalid",
             paste("Invalid run transition:", record$status, "->", target), "configuration")
  record$status <- target; record$updated_at <- fpq_now()
  if (target == "running" && is.null(record$start_time))
    record$start_time <- record$updated_at
  if (target %in% c("cancelled", "failed", "validation_failed", "completed"))
    record$finish_time <- record$updated_at
  record$status_history <- c(record$status_history,
                             list(c(list(status = target, timestamp = record$updated_at), details)))
  fpq_atomic_write_json(record, fpq_run_path(project, run_id))
  fpq_log_event(project, "run.transition", dataset_id = record$dataset_id,
                run_id = run_id, stage_id = record$stage_id,
                details = list(status = target))
  record
}

fpq_recover_project <- function(project, action = c("inspect", "remove_stale_lock"),
                                stale_seconds = 3600) {
  action <- match.arg(action)
  paths <- fpq_project_paths(project$root)
  lock <- file.path(paths$internal_root, ".fpq-project.lock")
  temporary <- list.files(paths$temporary, full.names = TRUE, all.files = TRUE,
                          no.. = TRUE)
  report <- list(lock_exists = file.exists(lock), abandoned_temporary = temporary,
                 backups = list.files(project$root, pattern = "\\.bak$", recursive = TRUE,
                                      full.names = TRUE))
  if (action == "remove_stale_lock") {
    if (!file.exists(lock)) return(report)
    age <- as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "secs"))
    if (is.na(age) || age <= stale_seconds)
      fpq_stop("project.lock_active", "Lock is not stale; it was not removed.", "infrastructure")
    unlink(lock)
    fpq_log_event(project, "recovery.stale_lock_removed",
                  details = list(age_seconds = age), level = "WARNING")
    report$lock_removed <- TRUE
  }
  report
}
