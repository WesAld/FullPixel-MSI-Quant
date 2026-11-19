make_running_adapter_diagnostic_run <- function(run_id = "run-adapter-failure") {
  root <- tempfile("fpq-adapter-diagnostics-")
  project <- fpq_create_project(root, paste0("prj-", run_id), "Adapter diagnostics")
  request <- list(run_id = run_id, dataset_id = "dset-diagnostics",
    stage_id = "extract_pixel_intensities", ion_mode = "POS",
    exposure_label = "Single",
    scientific_profile = list(profile_id = "pvl-fullpixel-reference-v1"),
    scientific_core_version = "1.0.0",
    frozen_script = list(script_id = "extract"), registered_inputs = list(),
    output_working_directory = file.path(root, "scientific-output"))
  fpq_create_run(project, request)
  for (status in c("preflight_running", "ready", "starting", "running"))
    fpq_transition_run(project, run_id, status)
  fpq_prepare_workspace_record(project, run_id,
                               request$output_working_directory)
  fpq_set_workspace_state(project, run_id, "active")
  list(project = project, request = request)
}

test_that("structured adapter failures persist both diagnostic layers", {
  fixture <- make_running_adapter_diagnostic_run()
  run_dir <- dirname(fpq_run_path(fixture$project, fixture$request$run_id))
  scientific_stdout <- file.path(fixture$request$output_working_directory,
                                 "stdout.log")
  scientific_stderr <- file.path(fixture$request$output_working_directory,
                                 "stderr.log")
  dir.create(dirname(scientific_stderr), recursive = TRUE)
  writeLines("SYNTHETIC SCIENTIFIC EXTRACTION ERROR", scientific_stderr)
  report_path <- file.path(run_dir, "adapter_report.json")
  adapter_stdout <- file.path(run_dir, "adapter-process.stdout.log")
  adapter_stderr <- file.path(run_dir, "adapter-process.stderr.log")
  report <- list(status = "failed", error = "Synthetic adapter exception",
    recommended_correction = "Synthetic remediation",
    stdout_path = scientific_stdout, stderr_path = scientific_stderr,
    process_exit_status = 27L)

  fpq_record_adapter_failure(fixture$project, fixture$request$run_id,
    exit_status = 1L, report = report, report_path = report_path,
    adapter_stdout = adapter_stdout, adapter_stderr = adapter_stderr,
    adapter_stdout_text = "adapter output", adapter_stderr_text = "adapter error",
    command = list(executable = "python", args = list("adapter.py", "request.json")),
    adapter_script = "adapter.py", request_path = "request.json",
    working_directory = "repository")

  failed <- fpq_read_json(fpq_run_path(fixture$project, fixture$request$run_id))
  expect_identical(failed$status, "failed")
  expect_identical(failed$exit_status, 1L)
  expect_identical(failed$failure$message, "Synthetic adapter exception")
  expect_identical(failed$failure$exception, "Synthetic adapter exception")
  expect_identical(failed$failure$recommended_correction, "Synthetic remediation")
  expect_identical(failed$failure$adapter_report_path, report_path)
  expect_identical(failed$logs$stdout, scientific_stdout)
  expect_identical(failed$logs$stderr, scientific_stderr)
  expect_identical(failed$failure$scientific_stderr_path, scientific_stderr)
  expect_identical(failed$failure$scientific_process_exit_status, 27L)
  expect_identical(readLines(failed$logs$stderr),
                   "SYNTHETIC SCIENTIFIC EXTRACTION ERROR")
  expect_identical(failed$failure$command$executable, "python")
  expect_length(failed$output_artifacts, 0L)
})

test_that("missing adapter reports retain process diagnostics and fallback", {
  fixture <- make_running_adapter_diagnostic_run("run-adapter-no-report")
  run_dir <- dirname(fpq_run_path(fixture$project, fixture$request$run_id))
  report_path <- file.path(run_dir, "adapter_report.json")
  adapter_stdout <- file.path(run_dir, "adapter-process.stdout.log")
  adapter_stderr <- file.path(run_dir, "adapter-process.stderr.log")

  fpq_record_adapter_failure(fixture$project, fixture$request$run_id,
    exit_status = 9L, report_path = report_path,
    adapter_stdout = adapter_stdout, adapter_stderr = adapter_stderr,
    adapter_stdout_text = "partial output", adapter_stderr_text = "python failed",
    command = list(executable = "python", args = list("adapter.py")),
    adapter_script = "adapter.py", request_path = "request.json",
    working_directory = "repository")

  failed <- fpq_load_run(fixture$project, fixture$request$run_id)
  expect_identical(failed$status, "failed")
  expect_identical(failed$exit_status, 9L)
  expect_identical(failed$failure$message, "python failed")
  expect_identical(failed$failure$stdout, "partial output")
  expect_identical(failed$failure$stderr, "python failed")
  expect_identical(failed$failure$adapter_stdout_path, adapter_stdout)
  expect_identical(failed$failure$adapter_stderr_path, adapter_stderr)
  expect_length(failed$logs, 0L)
})

test_that("process exceptions remain the primary diagnostic", {
  fixture <- make_running_adapter_diagnostic_run("run-adapter-launch-error")
  run_dir <- dirname(fpq_run_path(fixture$project, fixture$request$run_id))

  fpq_record_adapter_failure(fixture$project, fixture$request$run_id,
    report_path = file.path(run_dir, "adapter_report.json"),
    adapter_stdout = file.path(run_dir, "adapter-process.stdout.log"),
    adapter_stderr = file.path(run_dir, "adapter-process.stderr.log"),
    command = list(executable = "missing-python", args = list("adapter.py")),
    adapter_script = "adapter.py", request_path = "request.json",
    working_directory = "repository",
    process_exception = "Synthetic process launch failure")

  failed <- fpq_load_run(fixture$project, fixture$request$run_id)
  expect_identical(failed$failure$message, "Synthetic process launch failure")
  expect_identical(failed$failure$exception, "Synthetic process launch failure")
  expect_null(failed$exit_status)
  expect_false(is.null(failed$finish_time))
})

test_that("completed run schema remains free of failure diagnostics", {
  fixture <- make_running_adapter_diagnostic_run("run-adapter-success")
  fpq_transition_run(fixture$project, fixture$request$run_id, "validating")
  completed <- fpq_transition_run(fixture$project, fixture$request$run_id,
                                  "completed")
  completed$logs <- list(stdout = "scientific-stdout.log",
                         stderr = "scientific-stderr.log")
  completed$exit_status <- 0L
  fpq_atomic_write_json(completed,
                        fpq_run_path(fixture$project, fixture$request$run_id))

  persisted <- fpq_load_run(fixture$project, fixture$request$run_id)
  expect_identical(persisted$status, "completed")
  expect_identical(persisted$exit_status, 0L)
  expect_identical(persisted$logs$stderr, "scientific-stderr.log")
  expect_null(persisted$failure)
})
