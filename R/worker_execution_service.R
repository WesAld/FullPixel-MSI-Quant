fpq_worker_paths <- function(project, run_id) {
  root <- file.path(dirname(fpq_run_path(project, run_id)))
  list(
    root = root,
    identity = file.path(root, "worker-identity.json"),
    heartbeat = file.path(root, "worker-heartbeat.json"),
    cancellation = file.path(root, "cancellation-request.json"),
    completion = file.path(root, "worker-completion.json"),
    exit = file.path(root, "worker-exit.json"),
    workspace = file.path(root, "workspace.json")
  )
}

fpq_windows_process_info <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0L)
    return(list(alive = FALSE, pid = pid))
  if (.Platform$OS.type != "windows") {
    alive <- tryCatch({
      result <- processx::run("kill", c("-0", as.character(pid)),
        wd = fpq_safe_subprocess_cwd(), echo = FALSE,
        error_on_status = FALSE)
      identical(result$status, 0L)
    }, error = function(e) FALSE)
    return(list(alive = alive, pid = pid, parent_pid = NULL,
                command_line = NULL, creation_time = NULL))
  }
  powershell <- Sys.which("powershell")
  if (!nzchar(powershell)) powershell <- Sys.which("pwsh")
  if (!nzchar(powershell))
    fpq_stop("worker.inspector_unavailable",
             "PowerShell is required for safe persisted worker inspection.",
             "environment")
  script <- paste0(
    "$p=Get-CimInstance Win32_Process -Filter 'ProcessId=", pid, "';",
    "if($null -eq $p){exit 3};",
    "[pscustomobject]@{pid=[int]$p.ProcessId;parent_pid=[int]$p.ParentProcessId;",
    "command_line=[string]$p.CommandLine;creation_time=[string]$p.CreationDate}",
    "|ConvertTo-Json -Compress"
  )
  result <- processx::run(powershell,
    c("-NoProfile", "-NonInteractive", "-Command", script),
    wd = fpq_safe_subprocess_cwd(), echo = FALSE,
    error_on_status = FALSE, timeout = 10000)
  if (result$status == 3L || !nzchar(trimws(result$stdout)))
    return(list(alive = FALSE, pid = pid))
  if (result$status != 0L)
    fpq_stop("worker.inspection_failed", trimws(result$stderr), "infrastructure")
  parsed <- jsonlite::fromJSON(result$stdout, simplifyVector = TRUE)
  list(alive = TRUE, pid = as.integer(parsed$pid),
       parent_pid = as.integer(parsed$parent_pid),
       command_line = parsed$command_line %||% "",
       creation_time = parsed$creation_time %||% NULL)
}

fpq_parent_pid <- function(pid = Sys.getpid()) {
  info <- tryCatch(fpq_windows_process_info(pid), error = function(e) NULL)
  if (is.null(info)) NULL else info$parent_pid
}

fpq_register_worker <- function(project, request_path, repository_root,
                                worker_id, pid = Sys.getpid(),
                                parent_pid = fpq_parent_pid(pid)) {
  request <- fpq_read_json(request_path)
  if (!identical(request$project_id, project$manifest$project_id))
    fpq_stop("worker.project_mismatch", "Worker request belongs to another project.")
  paths <- fpq_worker_paths(project, request$run_id)
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(paths$identity))
    fpq_stop("worker.identity_exists", "Worker identity is already registered.",
             "infrastructure")
  identity <- list(schema_version = "1.0.0", worker_id = worker_id,
    project_id = project$manifest$project_id, run_id = request$run_id,
    process_id = as.integer(pid), parent_process_id = parent_pid,
    launch_timestamp = fpq_now(),
    worker_script_path = normalizePath(
      file.path(repository_root, "tools", "fpq_dispatch_worker.R"),
      winslash = "/", mustWork = TRUE),
    execution_request_path = normalizePath(request_path, winslash = "/",
                                           mustWork = TRUE),
    execution_request_hash = fpq_hash_file(request_path),
    repository_root = normalizePath(repository_root, winslash = "/",
                                    mustWork = TRUE),
    working_directory_path = normalizePath(request$output_working_directory,
      winslash = "/", mustWork = FALSE),
    current_stage = request$stage_id)
  fpq_atomic_write_json(identity, paths$identity)
  fpq_touch_worker_heartbeat(project, request$run_id, "registered", worker_id)
  fpq_log_event(project, "worker.registered", dataset_id = request$dataset_id,
    run_id = request$run_id, stage_id = request$stage_id,
    details = list(worker_id = worker_id, process_id = as.integer(pid)))
  identity
}

fpq_load_worker_identity <- function(project, run_id) {
  path <- fpq_worker_paths(project, run_id)$identity
  if (!file.exists(path)) return(NULL)
  fpq_read_json(path)
}

fpq_write_worker_terminal_markers <- function(project, run_id, worker_id,
                                               exit_status, terminal_status) {
  paths <- fpq_worker_paths(project, run_id)
  exit_record <- list(schema_version = "1.0.0",
    project_id = project$manifest$project_id, run_id = run_id,
    worker_id = worker_id, process_id = as.integer(Sys.getpid()),
    recorded_at = fpq_now(), exit_status = as.integer(exit_status),
    terminal_status = terminal_status)
  fpq_atomic_write_json(exit_record, paths$exit)
  if (terminal_status %in% c("completed", "cancelled", "failed",
                             "validation_failed"))
    fpq_atomic_write_json(list(schema_version = "1.0.0",
      project_id = project$manifest$project_id, run_id = run_id,
      worker_id = worker_id, completed_at = fpq_now(),
      status = terminal_status, exit_status = as.integer(exit_status)),
      paths$completion)
  invisible(exit_record)
}

fpq_touch_worker_heartbeat <- function(project, run_id, phase,
                                       worker_id = NULL) {
  paths <- fpq_worker_paths(project, run_id)
  identity <- if (file.exists(paths$identity)) fpq_read_json(paths$identity) else NULL
  resolved <- worker_id %||% identity$worker_id
  if (is.null(resolved))
    fpq_stop("worker.identity_missing", "Cannot write heartbeat without worker identity.",
             "infrastructure")
  record <- list(schema_version = "1.0.0", worker_id = resolved,
    project_id = project$manifest$project_id, run_id = run_id,
    heartbeat_timestamp = fpq_now(), phase = phase,
    process_id = identity$process_id %||% as.integer(Sys.getpid()))
  fpq_atomic_write_json(record, paths$heartbeat)
  invisible(record)
}

fpq_worker_identity_matches <- function(identity, process_info) {
  if (is.null(identity) || !isTRUE(process_info$alive)) return(FALSE)
  if (!identical(as.integer(identity$process_id), as.integer(process_info$pid)))
    return(FALSE)
  command <- process_info$command_line %||% ""
  nzchar(command) &&
    grepl(identity$worker_id, command, fixed = TRUE) &&
    grepl(identity$run_id, command, fixed = TRUE)
}

fpq_inspect_worker <- function(project, run_id,
                               inspector = fpq_windows_process_info,
                               stale_seconds = 30) {
  paths <- fpq_worker_paths(project, run_id)
  identity <- fpq_load_worker_identity(project, run_id)
  if (is.null(identity))
    return(list(outcome = "worker_missing", identity_match = FALSE,
                process_alive = FALSE, heartbeat_current = FALSE))
  process <- tryCatch(inspector(identity$process_id),
    error = function(e) list(alive = NA, pid = identity$process_id,
                             inspection_error = conditionMessage(e)))
  match <- isTRUE(process$alive) && fpq_worker_identity_matches(identity, process)
  heartbeat <- if (file.exists(paths$heartbeat)) fpq_read_json(paths$heartbeat) else NULL
  heartbeat_age <- if (is.null(heartbeat)) Inf else
    as.numeric(difftime(Sys.time(),
      as.POSIXct(heartbeat$heartbeat_timestamp, format = "%Y-%m-%dT%H:%M:%OSZ",
                 tz = "UTC"), units = "secs"))
  current <- is.finite(heartbeat_age) && heartbeat_age <= stale_seconds
  outcome <- if (match) "active_worker_reconnected" else if (isTRUE(process$alive))
    "manual_review_required" else "worker_missing"
  list(outcome = outcome, identity_match = match,
       process_alive = process$alive, process = process,
       heartbeat_current = current, heartbeat_age_seconds = heartbeat_age,
       identity = identity,
       completion_marker = file.exists(paths$completion),
       exit_marker = file.exists(paths$exit),
       cancellation_requested = file.exists(paths$cancellation))
}

fpq_cancellation_requested <- function(project, run_id) {
  file.exists(fpq_worker_paths(project, run_id)$cancellation)
}

fpq_request_cancellation <- function(project, run_id,
                                     inspector = fpq_windows_process_info) {
  run <- fpq_load_run(project, run_id)
  if (run$status %in% c("cancelled", "completed", "failed",
                        "preflight_failed", "validation_failed"))
    return(list(requested = FALSE, idempotent = TRUE, status = run$status,
                reason = "Run is terminal."))
  if (!run$status %in% c("starting", "running", "cancelling", "validating"))
    fpq_stop("cancellation.state_invalid",
             paste("Run is not cancellable from state", run$status),
             "configuration")
  paths <- fpq_worker_paths(project, run_id)
  identity <- fpq_load_worker_identity(project, run_id)
  if (is.null(identity))
    fpq_stop("cancellation.identity_missing",
             "Cancellation is blocked because persisted worker identity is missing.",
             "infrastructure")
  inspected <- fpq_inspect_worker(project, run_id, inspector)
  if (isTRUE(inspected$process_alive) && !isTRUE(inspected$identity_match))
    fpq_stop("cancellation.identity_mismatch",
             "PID is active but does not match the registered worker; no signal was sent.",
             "infrastructure")
  if (file.exists(paths$cancellation))
    return(list(requested = TRUE, idempotent = TRUE, status = run$status,
                request = fpq_read_json(paths$cancellation)))
  request <- list(schema_version = "1.0.0",
    cancellation_request_id = fpq_id("cancel"),
    project_id = project$manifest$project_id, run_id = run_id,
    worker_id = identity$worker_id, process_id = identity$process_id,
    requested_at = fpq_now(), method = "cooperative_marker",
    escalation_policy = "identity-verified process tree only",
    termination_confirmed = FALSE)
  fpq_atomic_write_json(request, paths$cancellation)
  if (run$status != "cancelling")
    run <- fpq_transition_run(project, run_id, "cancelling",
      list(cancellation_request_id = request$cancellation_request_id))
  run$cancellation <- request
  fpq_atomic_write_json(run, fpq_run_path(project, run_id))
  fpq_log_event(project, "run.cancellation_requested",
    dataset_id = run$dataset_id, run_id = run_id, stage_id = run$stage_id,
    details = list(cancellation_request_id = request$cancellation_request_id),
    level = "WARNING")
  list(requested = TRUE, idempotent = FALSE, status = "cancelling",
       request = request)
}

fpq_terminate_worker_tree <- function(project, run_id,
                                      inspector = fpq_windows_process_info,
                                      executor = processx::run) {
  identity <- fpq_load_worker_identity(project, run_id)
  if (is.null(identity))
    fpq_stop("cancellation.identity_missing", "Worker identity is missing.",
             "infrastructure")
  inspected <- fpq_inspect_worker(project, run_id, inspector)
  if (!isTRUE(inspected$process_alive))
    return(list(signal_sent = FALSE, already_exited = TRUE))
  if (!isTRUE(inspected$identity_match))
    fpq_stop("cancellation.identity_mismatch",
             "Process identity does not match; process tree was not terminated.",
             "infrastructure")
  if (.Platform$OS.type == "windows") {
    result <- executor("taskkill",
      c("/PID", as.character(identity$process_id), "/T", "/F"),
      wd = fpq_safe_subprocess_cwd(), echo = FALSE,
      error_on_status = FALSE)
  } else {
    result <- executor("kill", c("-TERM", as.character(identity$process_id)),
      wd = fpq_safe_subprocess_cwd(), echo = FALSE,
      error_on_status = FALSE)
  }
  fpq_log_event(project, "worker.termination_escalated",
    run_id = run_id, details = list(worker_id = identity$worker_id,
      process_id = identity$process_id, exit_status = result$status),
    level = "WARNING")
  list(signal_sent = identical(result$status, 0L), already_exited = FALSE,
       exit_status = result$status, stdout = result$stdout, stderr = result$stderr)
}

fpq_finalize_cancelled_run <- function(project, run_id, method,
                                       exit_status = NULL,
                                       partial_outputs = character()) {
  run <- fpq_load_run(project, run_id)
  if (run$status == "cancelled") return(run)
  if (run$status != "cancelling")
    fpq_stop("cancellation.finalize_state",
             "Only a cancelling run can be finalized as cancelled.",
             "configuration")
  completed <- fpq_transition_run(project, run_id, "cancelled",
    list(termination_confirmed = TRUE, cancellation_method = method,
         exit_status = exit_status))
  completed$cancellation <- modifyList(completed$cancellation %||% list(),
    list(confirmed_at = fpq_now(), termination_confirmed = TRUE,
         method = method, exit_status = exit_status,
         partial_outputs = as.list(partial_outputs)))
  completed$finish_time <- fpq_now()
  completed$output_artifacts <- list()
  fpq_atomic_write_json(completed, fpq_run_path(project, run_id))
  paths <- fpq_worker_paths(project, run_id)
  fpq_atomic_write_json(list(schema_version = "1.0.0",
    project_id = project$manifest$project_id, run_id = run_id,
    worker_id = fpq_load_worker_identity(project, run_id)$worker_id,
    completed_at = fpq_now(), status = "cancelled",
    exit_status = exit_status, partial_outputs = as.list(partial_outputs)),
    paths$completion)
  fpq_set_workspace_state(project, run_id, "cancelled_preserved")
  completed
}

fpq_prepare_workspace_record <- function(project, run_id, workspace_path) {
  paths <- fpq_worker_paths(project, run_id)
  workspace_records <- list.files(fpq_project_paths(project$root)$runs,
    pattern = "^workspace[.]json$", recursive = TRUE, full.names = TRUE)
  existing <- lapply(workspace_records, fpq_read_json)
  wanted <- normalizePath(workspace_path, mustWork = FALSE)
  collisions <- Filter(function(x)
    !identical(x$run_id, run_id) &&
      identical(normalizePath(x$path, mustWork = FALSE), wanted), existing)
  if (length(collisions))
    fpq_stop("workspace.shared",
             "Execution workspace is already associated with another run.",
             "infrastructure")
  if (file.exists(paths$workspace))
    fpq_stop("workspace.registered", "Workspace is already registered for this run.",
             "infrastructure")
  record <- list(schema_version = "1.0.0",
    project_id = project$manifest$project_id, run_id = run_id,
    path = normalizePath(workspace_path, winslash = "/", mustWork = FALSE),
    state = "prepared", prepared_at = fpq_now(), updated_at = fpq_now(),
    retention = "Preserve failed and cancelled workspaces for diagnostics.")
  fpq_atomic_write_json(record, paths$workspace)
  record
}

fpq_set_workspace_state <- function(project, run_id, state) {
  allowed <- c("prepared", "active", "completed_preserved", "failed_preserved",
               "cancelled_preserved", "eligible_for_cleanup", "cleaned")
  if (!state %in% allowed)
    fpq_stop("workspace.state_invalid", "Unsupported workspace lifecycle state.")
  path <- fpq_worker_paths(project, run_id)$workspace
  if (!file.exists(path)) return(NULL)
  record <- fpq_read_json(path)
  record$state <- state
  record$updated_at <- fpq_now()
  fpq_atomic_write_json(record, path)
  record
}

fpq_recover_interrupted_runs <- function(project,
                                         inspector = fpq_windows_process_info,
                                         stale_seconds = 30) {
  nonterminal <- c("starting", "running", "cancelling", "validating")
  runs <- Filter(function(x) x$status %in% nonterminal, fpq_list_runs(project))
  outcomes <- lapply(runs, function(run) {
    paths <- fpq_worker_paths(project, run$run_id)
    inspection <- fpq_inspect_worker(project, run$run_id, inspector,
                                     stale_seconds)
    outcome <- if (isTRUE(inspection$identity_match))
      "active_worker_reconnected" else if (file.exists(paths$completion)) {
        marker <- fpq_read_json(paths$completion)
        if (identical(marker$status, "completed") &&
            run$status == "validating") "worker_completed_pending_validation"
        else if (identical(marker$status, "cancelled")) "cancellation_unconfirmed"
        else "worker_failed"
      } else if (run$status == "validating")
        "validation_interrupted"
      else if (isTRUE(inspection$process_alive))
        "manual_review_required"
      else "worker_missing"
    fpq_log_event(project, paste0("recovery.", outcome),
      dataset_id = run$dataset_id, run_id = run$run_id,
      stage_id = run$stage_id, details = list(
        heartbeat_current = inspection$heartbeat_current,
        identity_match = inspection$identity_match),
      level = if (outcome == "active_worker_reconnected") "INFO" else "WARNING")
    list(run_id = run$run_id, prior_status = run$status,
         outcome = outcome, inspection = inspection)
  })
  outcomes
}
