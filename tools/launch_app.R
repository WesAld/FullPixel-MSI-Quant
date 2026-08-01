#!/usr/bin/env Rscript
fpq_launcher_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg))
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  configured <- Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT", unset = "")
  if (nzchar(configured))
    return(file.path(normalizePath(configured, mustWork = TRUE), "tools", "launch_app.R"))
  stop("Set FULLPIXEL_MSI_QUANT_ROOT before sourcing tools/launch_app.R.", call. = FALSE)
}

fpq_launch_app <- function(port = 3838L, launch_browser = interactive()) {
  script <- fpq_launcher_script_path()
  repository_root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
  Sys.setenv(FULLPIXEL_MSI_QUANT_ROOT = repository_root)
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(repository_root, quiet = TRUE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(repository_root, quiet = TRUE)
  } else {
    stop("Install pkgload or devtools to run the application from repository source.",
         call. = FALSE)
  }
  shiny::runApp(fpq_shiny_app(fpq_resolve_repository_root(repository_root)),
                host = "127.0.0.1", port = as.integer(port),
                launch.browser = launch_browser)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)

  port_values <- grep("^--port=", args, value = TRUE)
  port_arg <- if (length(port_values)) {
    sub("^--port=", "", port_values[[1]])
  } else {
    "3838"
  }

  browser_values <- grep("^--launch-browser=", args, value = TRUE)
  launch_browser <- if (length(browser_values)) {
    tolower(sub("^--launch-browser=", "", browser_values[[1]])) %in%
      c("1", "true", "yes")
  } else {
    interactive()
  }

  port <- suppressWarnings(as.integer(port_arg))

  if (is.na(port) || port < 1L || port > 65535L)
    stop("Port must be an integer from 1 to 65535.", call. = FALSE)

  fpq_launch_app(port, launch_browser = launch_browser)
}
