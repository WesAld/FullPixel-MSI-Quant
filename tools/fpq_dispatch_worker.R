#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L)
  stop(paste("Usage: fpq_dispatch_worker.R <project-root> <request-json>",
             "<repository-root> <worker-id> <run-id>"),
       call. = FALSE)
project_root <- normalizePath(args[[1]], mustWork = TRUE)
request_path <- normalizePath(args[[2]], mustWork = TRUE)
repository_root <- normalizePath(args[[3]], mustWork = TRUE)
worker_id <- args[[4]]
run_id <- args[[5]]
Sys.setenv(FULLPIXEL_MSI_QUANT_ROOT = repository_root)
devtools::load_all(repository_root, quiet = TRUE)
project <- fpq_open_project(project_root)
worker_main <- function() {
request <- fpq_read_json(request_path)
if (!identical(request$run_id, run_id))
  stop("Command-line run ID does not match the execution request.", call. = FALSE)
identity <- fpq_register_worker(project, request_path, repository_root, worker_id)
paths <- fpq_worker_paths(project, run_id)
exit_status <- 1L
terminal_status <- "failed"
on.exit({
  current <- tryCatch(fpq_load_run(project, run_id), error = function(e) NULL)
  if (!is.null(current)) terminal_status <- current$status
  fpq_write_worker_terminal_markers(project, run_id, worker_id,
                                    exit_status, terminal_status)
}, add = TRUE)
result <- tryCatch(
  fpq_dispatch_adapter(project, request_path, repository_root),
  error = function(e) {
    current <- tryCatch(fpq_load_run(project, run_id), error = function(x) NULL)
    if (!is.null(current) && !current$status %in%
        c("failed", "cancelled", "validation_failed", "completed")) {
      allowed <- fpq_run_transitions()[[current$status]]
      if ("failed" %in% allowed) {
        current <- fpq_transition_run(project, run_id, "failed",
          list(worker_error = conditionMessage(e)))
        current$failure <- list(message = conditionMessage(e),
                                class = class(e)[[1]])
        fpq_atomic_write_json(current, fpq_run_path(project, run_id))
        fpq_set_workspace_state(project, run_id, "failed_preserved")
      }
    }
    stop(e)
  })
exit_status <- if (identical(result$run$status, "completed") ||
                   identical(result$run$status, "cancelled")) 0L else 1L
cat(jsonlite::toJSON(list(status = result$run$status,
  run_id = result$run$run_id,
  artifact_id = result$artifact$artifact_id %||% NULL,
  worker_id = identity$worker_id), auto_unbox = TRUE, null = "null"), "\n")
invisible(result)
}
worker_main()
