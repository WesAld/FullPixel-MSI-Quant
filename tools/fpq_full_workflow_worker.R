#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L)
  stop("Usage: fpq_full_workflow_worker.R <project-root> <workflow-id> <repository-root>",
       call. = FALSE)
project_root <- normalizePath(args[[1]], mustWork = TRUE)
workflow_id <- args[[2]]
repository_root <- normalizePath(args[[3]], mustWork = TRUE)
Sys.setenv(FULLPIXEL_MSI_QUANT_ROOT = repository_root)
pkgload::load_all(repository_root, quiet = TRUE)
project <- fpq_open_project(project_root)
fpq_execute_full_workflow(project, workflow_id, repository_root)
