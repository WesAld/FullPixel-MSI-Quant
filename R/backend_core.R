fpq_stop <- function(code, message, category = "configuration", details = list()) {
  condition <- structure(
    c(list(message = message, call = NULL, code = code, category = category), details),
    class = c(paste0("fpq_", category, "_error"), "fpq_error", "error", "condition")
  )
  stop(condition)
}

fpq_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
fpq_id <- function(prefix) paste0(prefix, "-", format(Sys.time(), "%Y%m%d%H%M%OS3", tz = "UTC"),
                                  "-", sprintf("%06d", sample.int(999999L, 1L)))
fpq_hash_file <- function(path) {
  if (!file.exists(path)) fpq_stop("file.missing", paste("File does not exist:", path), "input")
  toupper(digest::digest(file = path, algo = "sha256", serialize = FALSE))
}
fpq_hash_object <- function(x) toupper(digest::digest(x, algo = "sha256"))

fpq_resolve_repository_root <- function(explicit = NULL) {
  configured <- if (!is.null(explicit) && length(explicit) == 1L &&
    nzchar(explicit)) explicit else Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT", unset = "")
  if (!nzchar(configured))
    fpq_stop("repository.root_unresolved",
      paste("Set FULLPIXEL_MSI_QUANT_ROOT or supply the repository root explicitly."),
      "infrastructure")
  root <- tryCatch(normalizePath(configured, mustWork = TRUE),
    error = function(e) fpq_stop("repository.root_missing",
      paste("Repository root does not exist:", configured), "infrastructure"))
  required <- c("DESCRIPTION", file.path("config", "scientific_core_manifest.yml"))
  missing <- required[!file.exists(file.path(root, required))]
  if (length(missing))
    fpq_stop("repository.root_invalid",
      paste("Repository root is missing:", paste(missing, collapse = ", ")),
      "infrastructure")
  root
}

fpq_repo_root <- function() fpq_resolve_repository_root()

fpq_read_json <- function(path) {
  if (!file.exists(path)) fpq_stop("record.missing", paste("Record does not exist:", path), "input")
  last_error <- NULL
  for (attempt in seq_len(5L)) {
    value <- tryCatch(
      suppressWarnings(jsonlite::fromJSON(path, simplifyVector = FALSE)),
      error = function(e) {
        last_error <<- e
        NULL
      })
    if (!is.null(value)) return(value)
    if (attempt < 5L) Sys.sleep(0.05 * attempt)
  }
  fpq_stop("record.malformed", conditionMessage(last_error), "configuration")
}

fpq_atomic_write_json <- function(value, path, validate = NULL, simulate_interrupt = FALSE) {
  if (!is.null(validate)) validate(value)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid())
  backup <- paste0(path, ".bak")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, null = "null",
                              digits = NA), temporary, useBytes = TRUE)
  fpq_read_json(temporary)
  if (simulate_interrupt) fpq_stop("atomic_write.interrupted", "Simulated interruption before publication.",
                                   "infrastructure")
  if (file.exists(path) && !file.copy(path, backup, overwrite = TRUE))
    fpq_stop("atomic_write.backup_failed", paste("Cannot back up", path), "infrastructure")
  if (!file.rename(temporary, path))
    fpq_stop("atomic_write.publish_failed", paste("Cannot publish", path), "infrastructure")
  invisible(path)
}

fpq_project_paths <- function(project_root) {
  root <- normalizePath(project_root, mustWork = FALSE)
  list(root = root, manifest = file.path(root, "project.json"),
       datasets = file.path(root, "config", "datasets"),
       run_configurations = file.path(root, "config", "runs"),
       snapshots = file.path(root, "config", "snapshots"),
       runs = file.path(root, "runs"), artifacts = file.path(root, "registry", "artifacts.json"),
       logs = file.path(root, "logs", "events.jsonl"),
       validations = file.path(root, "validation", "records.json"),
       provenance = file.path(root, "provenance", "edges.json"),
       reports = file.path(root, "reports"), temporary = file.path(root, "temporary"))
}

fpq_with_project_lock <- function(project_root, expr, stale_seconds = 3600) {
  lock <- file.path(project_root, ".fpq-project.lock")
  if (file.exists(lock)) {
    age <- as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "secs"))
    if (is.na(age) || age <= stale_seconds)
      fpq_stop("project.locked", "Project has an active lock.", "infrastructure")
    fpq_stop("project.lock_stale", "Project lock is stale; explicit recovery is required.",
             "infrastructure")
  }
  writeLines(c(as.character(Sys.getpid()), fpq_now()), lock)
  on.exit(unlink(lock), add = TRUE)
  force(expr)
}

fpq_validation_record <- function(check_id, status, message, severity = "ERROR",
                                  blocking = identical(status, "FAIL"), observed = NULL,
                                  expected = NULL, consequence = "", remediation = "") {
  normalized_severity <- if (severity == "ERROR") "CRITICAL" else severity
  list(schema_version = "1.0.0", validation_record_id = fpq_id("val"),
       project_id = "unbound", run_id = NULL, artifact_id = NULL, stage_id = NULL,
       ion_mode = NULL, check_id = check_id, check_name = check_id,
       category = "schema", status = status, severity = normalized_severity,
       blocking_status = blocking, observed_value = observed, expected_value = expected,
       message = message, scientific_consequence = consequence, remediation = remediation,
       affected_sample = NULL, affected_feature = NULL, affected_file = NULL,
       timestamp = fpq_now(), validator_version = "1.0.0",
       profile_version = "1.0.0")
}
