fpq_verify_scientific_core <- function(repository_root = fpq_repo_root()) {
  manifest <- yaml::read_yaml(file.path(repository_root, "config", "scientific_core_manifest.yml"))
  records <- lapply(manifest$scripts, function(entry) {
    path <- file.path(repository_root, entry$repository_relative_path)
    observed <- if (file.exists(path)) fpq_hash_file(path) else NULL
    ok <- !is.null(observed) && identical(observed, toupper(entry$sha256))
    fpq_validation_record(paste0("integrity.", entry$script_id %||% basename(path)),
      if (ok) "PASS" else "FAIL", if (ok) "Frozen scientific source is intact."
      else "Frozen scientific source hash mismatch.", if (ok) "INFO" else "CRITICAL",
      !ok, observed, toupper(entry$sha256), "Scientific execution is blocked on mismatch.",
      "Restore the registered immutable source.")
  })
  list(status = if (all(vapply(records, function(x) x$status == "PASS", logical(1))))
    "compatible" else "incompatible", records = records)
}

fpq_resolve_rscript_executable <- function(value = NULL) {
  requested <- value %||% ""
  if (!nzchar(requested) || identical(requested, "Rscript")) {
    executable <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
    installed <- file.path(R.home("bin"), executable)
    if (file.exists(installed))
      return(normalizePath(installed, winslash = "/", mustWork = TRUE))
    discovered <- unname(Sys.which("Rscript"))
    if (nzchar(discovered))
      return(normalizePath(discovered, winslash = "/", mustWork = TRUE))
    return(installed)
  }
  requested
}

fpq_environment_check_status <- function(check) {
  if (is.list(check) && !is.null(check$status)) check$status else as.character(check)
}

fpq_environment_check_is_actionable <- function(name, check) {
  if (is.list(check) && !is.null(check$required_for_current_workflow))
    return(isTRUE(check$required_for_current_workflow))
  if (is.list(check) && identical(check$impact_on_current_workflow %||% "", "none"))
    return(FALSE)
  !name %in% c("r_version", "storage")
}

fpq_environment_workflow_status <- function(environment) {
  checks <- environment$checks %||% list()
  if (!length(checks)) return(environment$status %||% "unresolved")
  actionable <- checks[vapply(names(checks), function(name)
    fpq_environment_check_is_actionable(name, checks[[name]]), logical(1L))]
  statuses <- vapply(actionable, fpq_environment_check_status, character(1L))
  if ("incompatible" %in% statuses) "incompatible" else
    if ("unresolved" %in% statuses) "unresolved" else
      if ("compatible_with_warning" %in% statuses) "compatible_with_warning" else
        "compatible"
}

fpq_environment_detail_records <- function(environment) {
  checks <- environment$checks %||% list()
  records <- list()
  for (name in names(checks)) {
    check <- checks[[name]]
    status <- fpq_environment_check_status(check)
    if (identical(status, "compatible")) next
    actionable <- fpq_environment_check_is_actionable(name, check)
    record_status <- if (actionable && identical(status, "incompatible")) "FAIL" else
      "WARNING"
    observed <- if (is.list(check)) check$observed %||% status else check
    records[[length(records) + 1L]] <- fpq_validation_record(
      paste0("environment.", name), record_status,
      if (actionable)
        paste("Environment check may affect the current workflow:", name)
      else
        paste("Optional environment check:", name,
              "Impact on current workflow: None."),
      if (actionable && identical(status, "incompatible")) "CRITICAL" else "WARNING",
      actionable && identical(status, "incompatible"),
      paste(unlist(observed), collapse = "; "),
      if (actionable) "compatible" else "optional",
      if (actionable) "Required workflow capability may be unavailable."
      else "None for the current selected workflow.",
      if (actionable) "Install or repair the required component."
      else "No action required for this workflow.")
  }
  records
}

fpq_check_environment <- function(request = NULL, repository_root = fpq_repo_root()) {
  rscript <- fpq_resolve_rscript_executable(
    if (!is.null(request))
      request$environment_requirements$r_executable
    else NULL
  )
  packages <- if (!is.null(request)) request$environment_requirements$required_packages else
    c("jsonlite", "yaml", "digest", "processx")
  checks <- list(
    r = list(status = if (file.exists(rscript)) "compatible" else "incompatible",
             observed = rscript),
    r_version = list(status = "compatible_with_warning", observed = R.version.string,
      required_for_current_workflow = FALSE, impact_on_current_workflow = "none"),
    platform = list(status = if (.Platform$OS.type == "windows") "compatible" else
      "compatible_with_warning", observed = R.version$platform),
    packages = list(status = if (all(vapply(packages, requireNamespace, logical(1),
                                            quietly = TRUE))) "compatible" else "incompatible",
                    observed = setNames(vapply(packages, requireNamespace, logical(1),
                                               quietly = TRUE), packages)),
    python = list(status = if (nzchar(Sys.which("python"))) "compatible" else "incompatible",
                  observed = unname(Sys.which("python"))),
    storage = list(status = "compatible_with_warning",
                   required_for_current_workflow = FALSE,
                   impact_on_current_workflow = "none",
                   observed = paste("Free-space threshold is not platform-normalized;",
                                    "this optional check is nonblocking.")),
    scientific_core = fpq_verify_scientific_core(repository_root)$status
  )
  statuses <- vapply(checks, function(x) if (is.list(x) && !is.null(x$status)) x$status else x,
                     character(1))
  overall <- if ("incompatible" %in% statuses) "incompatible" else
    if ("unresolved" %in% statuses) "unresolved" else
      if ("compatible_with_warning" %in% statuses) "compatible_with_warning" else "compatible"
  environment <- list(status = overall, checks = checks)
  environment$workflow_status <- fpq_environment_workflow_status(environment)
  environment
}
