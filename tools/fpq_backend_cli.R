#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: fpq_backend_cli.R <command> [arguments]", call. = FALSE)
repository_root <- normalizePath(file.path(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE))), ".."), mustWork = TRUE)
devtools::load_all(repository_root, quiet = TRUE)
command <- args[[1]]
if (command == "create-project") {
  if (length(args) != 4L) stop("create-project <root> <project-id> <name>", call. = FALSE)
  print(fpq_project_status(fpq_create_project(args[[2]], args[[3]], args[[4]])))
} else if (command == "status") {
  print(fpq_project_status(fpq_open_project(args[[2]])))
} else if (command == "artifacts") {
  print(fpq_list_artifacts(fpq_open_project(args[[2]])))
} else if (command == "provenance") {
  print(fpq_trace_provenance(fpq_open_project(args[[2]]), args[[3]]))
} else if (command == "environment") {
  print(fpq_check_environment(repository_root = repository_root))
} else if (command == "validate-profile") {
  project <- fpq_open_project(args[[2]])
  config <- fpq_read_json(args[[3]])
  print(fpq_validate_profile_compatibility(config, project$manifest$scientific_profile_id))
} else {
  stop("Unknown backend development command.", call. = FALSE)
}
