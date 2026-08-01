make_m5_worker_fixture <- function(status = "running") {
  root <- tempfile("fpq-m5-worker-")
  project <- fpq_create_project(root, "prj-m5-worker", "M5 worker")
  request <- fpq_read_json(file.path(fpq_repo_root(), "config", "examples",
    "pvl_single_negative_extraction.execution_request.json"))
  request$project_id <- project$manifest$project_id
  request$run_id <- paste0("run-m5-", sample.int(999999L, 1))
  request$output_working_directory <- tempfile("fpq-m5-workspace-")
  request_path <- file.path(root, "config", "snapshots",
                            paste0(request$run_id, ".json"))
  fpq_atomic_write_json(request, request_path)
  fpq_create_run(project, request)
  if (status != "configured") {
    for (next_status in c("preflight_running", "ready", "starting", "running")) {
      fpq_transition_run(project, request$run_id, next_status)
      if (next_status == status) break
    }
  }
  identity <- fpq_register_worker(project, request_path, fpq_repo_root(),
    paste0("worker-", request$run_id), pid = 424242L, parent_pid = 111L)
  list(project = project, request = request, request_path = request_path,
       identity = identity)
}

matching_inspector <- function(identity) {
  force(identity)
  function(pid) list(alive = TRUE, pid = as.integer(pid),
    parent_pid = identity$parent_process_id,
    command_line = paste("Rscript fpq_dispatch_worker.R",
                         identity$worker_id, identity$run_id),
    creation_time = identity$launch_timestamp)
}

test_that("worker identity and heartbeat are persisted separately", {
  x <- make_m5_worker_fixture()
  paths <- fpq_worker_paths(x$project, x$request$run_id)
  expect_true(file.exists(paths$identity))
  expect_true(file.exists(paths$heartbeat))
  expect_identical(fpq_read_json(paths$identity)$execution_request_hash,
                   fpq_hash_file(x$request_path))
  heartbeat <- fpq_touch_worker_heartbeat(x$project, x$request$run_id,
                                          "scientific_process_active")
  expect_identical(heartbeat$worker_id, x$identity$worker_id)
  expect_identical(heartbeat$phase, "scientific_process_active")
})

test_that("cancellation is explicit, identity checked and idempotent", {
  x <- make_m5_worker_fixture()
  inspector <- matching_inspector(x$identity)
  first <- fpq_request_cancellation(x$project, x$request$run_id, inspector)
  second <- fpq_request_cancellation(x$project, x$request$run_id, inspector)
  expect_true(first$requested)
  expect_false(first$idempotent)
  expect_true(second$idempotent)
  expect_identical(fpq_load_run(x$project, x$request$run_id)$status,
                   "cancelling")
  completed <- fpq_finalize_cancelled_run(x$project, x$request$run_id,
    "test-confirmed", exit_status = 15L, partial_outputs = "partial.csv")
  expect_identical(completed$status, "cancelled")
  expect_length(fpq_list_artifacts(x$project), 0)
  expect_true(completed$cancellation$termination_confirmed)
})

test_that("PID reuse cannot terminate an unrelated process", {
  x <- make_m5_worker_fixture()
  unrelated <- function(pid) list(alive = TRUE, pid = as.integer(pid),
    parent_pid = 999L, command_line = "Rscript unrelated-analysis.R")
  called <- FALSE
  executor <- function(...) {
    called <<- TRUE
    list(status = 0L, stdout = "", stderr = "")
  }
  expect_error(fpq_request_cancellation(x$project, x$request$run_id,
                                        unrelated),
               class = "fpq_infrastructure_error")
  expect_error(fpq_terminate_worker_tree(x$project, x$request$run_id,
                                        unrelated, executor),
               class = "fpq_infrastructure_error")
  expect_false(called)
  expect_identical(fpq_load_run(x$project, x$request$run_id)$status,
                   "running")
})

test_that("identity-verified process tree termination uses the exact PID", {
  x <- make_m5_worker_fixture()
  observed <- NULL
  executor <- function(command, args, ...) {
    observed <<- list(command = command, args = args)
    list(status = 0L, stdout = "terminated", stderr = "")
  }
  result <- fpq_terminate_worker_tree(x$project, x$request$run_id,
    matching_inspector(x$identity), executor)
  expect_true(result$signal_sent)
  expect_true(as.character(x$identity$process_id) %in% observed$args)
  expect_true("/T" %in% observed$args)
})

test_that("recovery distinguishes active, missing and interrupted validation", {
  x <- make_m5_worker_fixture()
  active <- fpq_recover_interrupted_runs(x$project,
    matching_inspector(x$identity), stale_seconds = 60)
  expect_identical(active[[1]]$outcome, "active_worker_reconnected")

  missing <- fpq_recover_interrupted_runs(x$project,
    function(pid) list(alive = FALSE, pid = pid), stale_seconds = 60)
  expect_identical(missing[[1]]$outcome, "worker_missing")

  y <- make_m5_worker_fixture()
  fpq_transition_run(y$project, y$request$run_id, "validating")
  interrupted <- fpq_recover_interrupted_runs(y$project,
    function(pid) list(alive = FALSE, pid = pid))
  expect_identical(interrupted[[1]]$outcome, "validation_interrupted")
})

test_that("stale heartbeat alone does not classify a matching live worker failed", {
  x <- make_m5_worker_fixture()
  heartbeat_path <- fpq_worker_paths(x$project, x$request$run_id)$heartbeat
  heartbeat <- fpq_read_json(heartbeat_path)
  heartbeat$heartbeat_timestamp <- "2000-01-01T00:00:00.000Z"
  fpq_atomic_write_json(heartbeat, heartbeat_path)
  inspected <- fpq_inspect_worker(x$project, x$request$run_id,
    matching_inspector(x$identity), stale_seconds = 1)
  expect_false(inspected$heartbeat_current)
  expect_identical(inspected$outcome, "active_worker_reconnected")
})

test_that("workspace records are unique and cancelled workspaces are preserved", {
  x <- make_m5_worker_fixture()
  record <- fpq_prepare_workspace_record(x$project, x$request$run_id,
                                         x$request$output_working_directory)
  expect_identical(record$state, "prepared")
  expect_error(fpq_prepare_workspace_record(x$project, x$request$run_id,
                                            x$request$output_working_directory),
               class = "fpq_infrastructure_error")
  fpq_set_workspace_state(x$project, x$request$run_id,
                          "cancelled_preserved")
  persisted <- fpq_read_json(
    fpq_worker_paths(x$project, x$request$run_id)$workspace)
  expect_identical(persisted$state, "cancelled_preserved")
})

test_that("terminal cancellation requests do not corrupt run state", {
  x <- make_m5_worker_fixture()
  inspector <- matching_inspector(x$identity)
  fpq_request_cancellation(x$project, x$request$run_id, inspector)
  fpq_finalize_cancelled_run(x$project, x$request$run_id, "test")
  result <- fpq_request_cancellation(x$project, x$request$run_id, inspector)
  expect_false(result$requested)
  expect_true(result$idempotent)
  expect_identical(fpq_load_run(x$project, x$request$run_id)$status,
                   "cancelled")
})

test_that("terminal worker markers preserve exit and completion evidence", {
  x <- make_m5_worker_fixture()
  fpq_write_worker_terminal_markers(x$project, x$request$run_id,
    x$identity$worker_id, 0L, "completed")
  paths <- fpq_worker_paths(x$project, x$request$run_id)
  expect_true(file.exists(paths$exit))
  expect_true(file.exists(paths$completion))
  expect_identical(fpq_read_json(paths$exit)$exit_status, 0L)
  expect_identical(fpq_read_json(paths$completion)$status, "completed")
})
