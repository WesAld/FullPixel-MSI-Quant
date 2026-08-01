fpq_read_collection <- function(path) if (file.exists(path)) fpq_read_json(path) else list()
fpq_write_collection <- function(value, path) fpq_atomic_write_json(value, path)

fpq_normalize_artifact <- function(artifact, activate = FALSE) {
  now <- fpq_now()
  profile <- artifact$scientific_profile %||% list(
    profile_id = artifact$scientific_profile_id %||% "unresolved",
    profile_version = "1.0.0")
  list(schema_version = artifact$schema_version %||% "1.0.0",
    artifact_id = artifact$artifact_id, artifact_type = artifact$artifact_type,
    display_name = artifact$display_name %||% artifact$artifact_id,
    project_id = artifact$project_id, dataset_id = artifact$dataset_id %||% NULL,
    ion_mode = artifact$ion_mode %||% NULL, exposure = artifact$exposure %||% "Single",
    producing_run_id = artifact$producing_run_id %||% NULL,
    producing_stage_id = artifact$producing_stage_id %||% NULL,
    path = artifact$path, path_type = artifact$path_type %||% "external_absolute",
    filename = artifact$filename %||% basename(artifact$path),
    media_type = artifact$media_type %||% "text/csv",
    file_size = as.integer(artifact$file_size), sha256 = artifact$sha256,
    created_at = artifact$created_at %||% now, registered_at = now,
    row_count = artifact$row_count %||% NULL, column_count = artifact$column_count %||% NULL,
    schema_id = artifact$schema_id %||% NULL,
    validation_status = artifact$validation_status,
    active_status = if (activate) "active" else "registered",
    superseded_status = FALSE, superseded_by = NULL,
    external_modification_status = "unchanged",
    parent_artifact_ids = as.list(artifact$parent_artifact_ids %||% list()),
    downstream_consumer_ids = as.list(artifact$downstream_consumer_ids %||% list()),
    scientific_profile = profile,
    scientific_core_version = artifact$scientific_core_version,
    notes = artifact$notes %||% NULL)
}

fpq_register_artifact <- function(project, artifact, activate = FALSE) {
  required <- c("artifact_id", "artifact_type", "path", "project_id", "sha256", "file_size",
                "validation_status", "scientific_profile_id", "scientific_core_version")
  if (length(setdiff(required, names(artifact))))
    fpq_stop("artifact.fields_missing", "Artifact record is incomplete.")
  registry <- fpq_read_collection(fpq_project_paths(project$root)$artifacts)
  if (any(vapply(registry, function(x) identical(x$artifact_id, artifact$artifact_id), logical(1))))
    fpq_stop("artifact.duplicate", "Artifact ID already exists.")
  if (activate && !identical(artifact$validation_status, "PASS"))
    fpq_stop("artifact.activation_blocked", "Only validated artifacts can be active.")
  artifact <- fpq_normalize_artifact(artifact, activate)
  if (activate) {
    for (i in seq_along(registry)) {
      same_slot <- identical(registry[[i]]$artifact_type, artifact$artifact_type) &&
        identical(registry[[i]]$dataset_id, artifact$dataset_id) &&
        identical(registry[[i]]$ion_mode, artifact$ion_mode) &&
        identical(registry[[i]]$exposure, artifact$exposure) &&
        identical(registry[[i]]$active_status, "active")
      if (same_slot) {
        registry[[i]]$active_status <- "superseded"
        registry[[i]]$superseded_status <- TRUE
        registry[[i]]$superseded_by <- artifact$artifact_id
      }
    }
  }
  registry <- c(registry, list(artifact))
  fpq_write_collection(registry, fpq_project_paths(project$root)$artifacts)
  if (activate) fpq_refresh_downstream_staleness(project)
  fpq_log_event(project, "artifact.registered", run_id = artifact$producing_run_id %||% NULL,
                details = list(artifact_id = artifact$artifact_id, active = activate))
  artifact
}
fpq_list_artifacts <- function(project, filters = list()) {
  if (is.null(project)) return(list())
  records <- fpq_read_collection(fpq_project_paths(project$root)$artifacts)
  for (field in names(filters))
    records <- Filter(function(x) identical(x[[field]], filters[[field]]), records)
  records
}
fpq_inspect_artifact <- function(project, artifact_id) {
  found <- Filter(function(x) identical(x$artifact_id, artifact_id), fpq_list_artifacts(project))
  if (length(found) != 1L) fpq_stop("artifact.not_found", "Artifact not found.")
  found[[1]]
}
fpq_update_artifact_status <- function(project, artifact_id, status) {
  allowed <- c("active", "superseded", "stale", "external_unavailable", "registered")
  if (!status %in% allowed) fpq_stop("artifact.status", "Unsupported artifact status.")
  registry <- fpq_list_artifacts(project)
  index <- which(vapply(registry, function(x) identical(x$artifact_id, artifact_id), logical(1)))
  if (length(index) != 1L) fpq_stop("artifact.not_found", "Artifact not found.")
  registry[[index]]$active_status <- status
  fpq_write_collection(registry, fpq_project_paths(project$root)$artifacts)
  if (status %in% c("superseded", "stale", "external_unavailable"))
    fpq_refresh_downstream_staleness(project)
  registry[[index]]
}
fpq_verify_artifact <- function(project, artifact_id) {
  artifact <- fpq_inspect_artifact(project, artifact_id)
  if (!file.exists(artifact$path)) {
    registry <- fpq_list_artifacts(project)
    index <- which(vapply(registry, function(x) identical(x$artifact_id, artifact_id), logical(1)))
    registry[[index]]$external_modification_status <- "missing"
    registry[[index]]$active_status <- "invalid"
    fpq_write_collection(registry, fpq_project_paths(project$root)$artifacts)
    fpq_refresh_downstream_staleness(project)
    return(registry[[index]])
  }
  if (!identical(fpq_hash_file(artifact$path), artifact$sha256))
    return(fpq_update_artifact_status(project, artifact_id, "stale"))
  artifact
}

fpq_refresh_downstream_staleness <- function(project) {
  registry <- fpq_list_artifacts(project)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_along(registry)) {
      parents <- registry[[i]]$parent_artifact_ids %||% list()
      if (!length(parents) || registry[[i]]$active_status %in%
          c("stale", "invalid", "superseded")) next
      parent_records <- Filter(function(x) x$artifact_id %in% parents, registry)
      healthy <- length(parent_records) == length(parents) &&
        all(vapply(parent_records, function(x)
          x$active_status == "active" && file.exists(x$path) &&
            identical(fpq_hash_file(x$path), x$sha256), logical(1)))
      if (!healthy) {
        registry[[i]]$active_status <- "stale"
        registry[[i]]$notes <- paste(registry[[i]]$notes %||% "",
          "Marked stale because an upstream artifact is unavailable, changed, or no longer active.")
        changed <- TRUE
      }
    }
  }
  fpq_write_collection(registry, fpq_project_paths(project$root)$artifacts)
  invisible(registry)
}

fpq_log_event <- function(project, event_code, dataset_id = NULL, run_id = NULL,
                          stage_id = NULL, details = list(), level = "INFO") {
  event <- list(schema_version = "1.0.0", timestamp = fpq_now(), level = level,
    run_id = run_id, project_id = project$manifest$project_id, stage_id = stage_id,
    ion_mode = details$ion_mode %||% NULL, event_code = event_code,
    message = details$message %||% event_code, phase = details$phase %||% NULL,
    sample_id = details$sample_id %||% NULL, file = details$file %||% NULL,
    progress_current = details$progress_current %||% NULL,
    progress_total = details$progress_total %||% NULL,
    exception_class = details$exception_class %||% NULL,
    diagnostic_details = c(list(dataset_id = dataset_id),
                           details[setdiff(names(details), c("ion_mode", "message", "phase",
                             "sample_id", "file", "progress_current", "progress_total",
                             "exception_class"))]),
    stream = details$stream %||% if (grepl("validation|artifact|recovery|run",
                                           event_code)) "audit" else
      "application_diagnostic")
  path <- fpq_project_paths(project$root)$logs
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cat(jsonlite::toJSON(event, auto_unbox = TRUE, null = "null"), "\n",
      file = path, append = TRUE)
  invisible(event)
}

fpq_store_validation <- function(project, records, replace = FALSE) {
  records <- lapply(records, function(record) {
    record$project_id <- project$manifest$project_id
    record
  })
  path <- fpq_project_paths(project$root)$validations
  existing <- if (isTRUE(replace)) list() else fpq_read_collection(path)
  fpq_write_collection(c(existing, records), path)
  invisible(records)
}
fpq_store_provenance <- function(project, edges) {
  required <- c("schema_version", "provenance_edge_id", "project_id", "subject_type",
                "subject_id", "relationship", "object_type", "object_id",
                "recorded_at", "run_id", "evidence_hash", "notes")
  for (edge in edges)
    if (length(setdiff(required, names(edge))))
      fpq_stop("provenance.fields_missing", "Provenance edge does not satisfy the contract.")
  path <- fpq_project_paths(project$root)$provenance
  existing <- fpq_read_collection(path)
  fpq_write_collection(c(existing, edges), path)
  invisible(edges)
}
fpq_artifact_parents <- function(project, artifact_id)
  fpq_inspect_artifact(project, artifact_id)$parent_artifact_ids %||% list()
fpq_artifact_consumers <- function(project, artifact_id) {
  records <- fpq_list_artifacts(project)
  Filter(function(x) artifact_id %in% (x$parent_artifact_ids %||% list()), records)
}
fpq_trace_provenance <- function(project, artifact_id) {
  edges <- fpq_read_collection(fpq_project_paths(project$root)$provenance)
  visited <- character(); frontier <- artifact_id; result <- list()
  while (length(frontier)) {
    current <- frontier[[1]]; frontier <- frontier[-1]
    if (current %in% visited) next
    visited <- c(visited, current)
    linked <- Filter(function(e) identical(e$subject_id, current) ||
                       identical(e$object_id, current), edges)
    result <- c(result, linked)
    counterparts <- vapply(linked, function(e)
      if (identical(e$subject_id, current)) e$object_id else
        if (identical(e$object_id, current)) e$subject_id else "",
      character(1))
    frontier <- unique(c(frontier, counterparts[nzchar(counterparts)]))
  }
  if (!length(result)) return(result)
  keys <- vapply(result, function(x) jsonlite::toJSON(x, auto_unbox = TRUE,
                                                      null = "null"), character(1))
  result[!duplicated(keys)]
}
