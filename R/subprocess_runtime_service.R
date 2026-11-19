fpq_subprocess_runtime_root <- function() {
  configured <- trimws(Sys.getenv("MALDIPIXELQUANT_RUNTIME_DIR", ""))
  if (nzchar(configured)) {
    root <- configured
  } else {
    local <- trimws(Sys.getenv("LOCALAPPDATA", ""))
    root <- if (nzchar(local))
      file.path(local, "MALDIPixelQuant", "runtime") else
      file.path(tempdir(), "MALDIPixelQuant-runtime")
  }
  if (!dir.exists(root) && !dir.create(root, recursive = TRUE,
                                        showWarnings = FALSE))
    fpq_stop("subprocess.runtime_unavailable",
             paste("Could not create the subprocess runtime directory:", root),
             "infrastructure")
  if (!dir.exists(root))
    fpq_stop("subprocess.runtime_invalid",
             paste("The subprocess runtime path is not a directory:", root),
             "infrastructure")
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

fpq_safe_subprocess_cwd <- function() fpq_subprocess_runtime_root()
