fpq_validate_project_manifest <- function(x) {
  required <- c("schema_version", "application_id", "application_version", "project_id",
                "project_name", "project_created_at", "project_updated_at", "project_status",
                "scientific_profile_id", "scientific_profile_version",
                "scientific_core_version", "registered_datasets", "active_artifacts")
  missing <- setdiff(required, names(x))
  if (length(missing)) fpq_stop("project.manifest_missing_fields",
                                paste("Missing:", paste(missing, collapse = ", ")))
  if (!identical(x$application_id, "FullPixelMSIQuant"))
    fpq_stop("project.application_id", "Unexpected application identifier.")
  if (!grepl("^prj-[a-z0-9][a-z0-9-]{2,63}$", x$project_id))
    fpq_stop("project.id_invalid", "Project ID does not satisfy the project contract.")
  if (!x$project_status %in% c("active", "archived", "invalid"))
    fpq_stop("project.status_invalid", "Invalid project status.")
  invisible(TRUE)
}

fpq_sanitize_project_filename <- function(project_name) {
  value <- trimws(as.character(project_name %||% ""))
  value <- gsub('[<>:"/\\\\|?*[:cntrl:]]+', "_", value, perl = TRUE)
  value <- gsub("[[:space:]]+", "_", value, perl = TRUE)
  value <- gsub("_+", "_", value, perl = TRUE)
  value <- gsub("^[._ ]+|[._ ]+$", "", value, perl = TRUE)
  if (!nzchar(value)) value <- "MALDIPixelQuant_Project"
  if (toupper(value) %in% c("CON", "PRN", "AUX", "NUL",
      paste0("COM", 1:9), paste0("LPT", 1:9))) value <- paste0("Project_", value)
  substr(value, 1L, 160L)
}

fpq_project_final_workbook_path <- function(project) {
  file.path(fpq_project_paths(project$root)$root,
    paste0(fpq_sanitize_project_filename(project$manifest$project_name),
           "_MALDIPixelQuant_Results.xlsx"))
}

fpq_atomic_copy_file <- function(source, destination, expected_sha256 = NULL) {
  if (!file.exists(source))
    fpq_stop("project.export_source_missing", "Validated workbook is unavailable.",
             "infrastructure")
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  token <- gsub("[^A-Za-z0-9]", "", fpq_id("copy"))
  temporary <- file.path(dirname(destination), paste0(".", basename(destination),
    ".", token, ".tmp"))
  backup <- file.path(dirname(destination), paste0(".", basename(destination),
    ".", token, ".bak"))
  on.exit(unlink(c(temporary, backup), force = TRUE), add = TRUE)
  if (!file.copy(source, temporary, overwrite = TRUE, copy.date = TRUE))
    fpq_stop("project.export_copy_failed", "Could not stage the validated workbook.",
             "infrastructure")
  source_hash <- toupper(expected_sha256 %||% fpq_hash_file(source))
  if (!identical(toupper(fpq_hash_file(temporary)), source_hash))
    fpq_stop("project.export_hash_failed", "Staged workbook hash verification failed.",
             "infrastructure")
  had_destination <- file.exists(destination)
  if (had_destination && !file.rename(destination, backup))
    fpq_stop("project.export_backup_failed", "Could not preserve the current workbook.",
             "infrastructure")
  if (!file.rename(temporary, destination)) {
    if (had_destination && file.exists(backup)) file.rename(backup, destination)
    fpq_stop("project.export_publish_failed", "Could not publish the validated workbook.",
             "infrastructure")
  }
  if (!identical(toupper(fpq_hash_file(destination)), source_hash)) {
    unlink(destination, force = TRUE)
    if (had_destination && file.exists(backup)) file.rename(backup, destination)
    fpq_stop("project.export_identity_failed", "Published workbook identity check failed.",
             "infrastructure")
  }
  unlink(backup, force = TRUE)
  invisible(destination)
}

fpq_promote_validated_workbook <- function(project, artifact) {
  if (!identical(artifact$artifact_type, "final_results_workbook") ||
      !identical(artifact$validation_status, "PASS") ||
      !file.exists(artifact$path %||% "")) return(list(promoted = FALSE))
  validation <- tryCatch(fpq_validate_published_final_results_workbook(artifact$path),
    error = function(e) list(status = "FAIL"))
  if (!identical(validation$status, "PASS")) return(list(promoted = FALSE))
  destination <- fpq_project_final_workbook_path(project)
  fpq_atomic_copy_file(artifact$path, destination, artifact$sha256)
  edge <- list(schema_version = "1.0.0", provenance_edge_id = fpq_id("prov"),
    project_id = project$manifest$project_id, subject_type = "artifact",
    subject_id = artifact$artifact_id,
    relationship = "promoted_as_researcher_facing_copy",
    object_type = "file", object_id = normalizePath(destination, winslash = "/"),
    recorded_at = fpq_now(), run_id = artifact$producing_run_id %||% NULL,
    evidence_hash = artifact$sha256,
    notes = "Researcher-facing alias of the validated internal final workbook.")
  fpq_store_provenance(project, list(edge))
  list(promoted = TRUE, path = destination, sha256 = fpq_hash_file(destination),
       source_artifact_id = artifact$artifact_id)
}

fpq_create_project <- function(project_root, project_id, project_name,
                               profile_id = "generic-two-group-v1",
                               project_description = NULL,
                               project_preferences = list()) {
  if (dir.exists(project_root) || file.exists(project_root))
    fpq_stop("project.destination_exists", "Project destination already exists.", "configuration")
  parent <- dirname(normalizePath(project_root, mustWork = FALSE))
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
  staging <- paste0(normalizePath(project_root, mustWork = FALSE), ".creating-", Sys.getpid())
  on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
  paths <- fpq_project_paths(staging, layout = "v4")
  dirs <- c(paths$datasets, paths$run_configurations, paths$snapshots, paths$runs,
            paths$results,
            dirname(paths$artifacts), dirname(paths$logs), dirname(paths$validations),
            dirname(paths$provenance), paths$reports, paths$temporary)
  vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)
  now <- fpq_now()
  manifest <- list(schema_version = "1.0.0", application_id = "FullPixelMSIQuant",
    application_version = "2.0.0", project_id = project_id, project_name = project_name,
    project_description = project_description, project_created_at = now, project_updated_at = now,
    project_status = "active",
    repository_root_policy = "Project resources are relative; external scientific inputs are absolute.",
    project_data_root = list(path = fpq_project_internal_directory_name(),
      path_type = "project_relative", portability = "portable"),
    analysis_root = list(path = file.path(fpq_project_internal_directory_name(), "runs"),
      path_type = "project_relative", portability = "portable"),
    scientific_profile_id = profile_id, scientific_profile_version = "1.0.0",
    scientific_core_version = "1.0.0", registered_datasets = list(),
    active_artifacts = list(), project_preferences = project_preferences,
    provenance_references = list(),
    compatibility = list(minimum_application_version = "1.0.0"))
  fpq_atomic_write_json(manifest, paths$manifest, fpq_validate_project_manifest)
  fpq_atomic_write_json(list(), paths$artifacts)
  fpq_atomic_write_json(list(), paths$validations)
  fpq_atomic_write_json(list(), paths$provenance)
  staged <- fpq_open_project(staging)
  if (!file.exists(fpq_project_paths(staged$root)$manifest))
    fpq_stop("project.manifest_not_persisted",
             "The staged project manifest could not be verified.", "infrastructure")
  if (!file.rename(staging, project_root))
    fpq_stop("project.publish_failed", "Could not publish transactional project.", "infrastructure")
  published <- tryCatch(fpq_open_project(project_root), error = function(e) {
    if (dir.exists(project_root)) unlink(project_root, recursive = TRUE)
    stop(e)
  })
  if (!file.exists(fpq_project_paths(published$root)$manifest))
    fpq_stop("project.manifest_not_persisted",
             "The published project manifest could not be verified.", "infrastructure")
  published
}

fpq_open_project <- function(project_root) {
  paths <- fpq_project_paths(project_root)
  manifest <- fpq_read_json(paths$manifest)
  fpq_validate_project_manifest(manifest)
  structure(list(root = paths$root, manifest = manifest), class = "fpq_project")
}

fpq_save_project <- function(project, updates = list()) {
  allowed <- c("project_name", "project_description", "project_preferences")
  denied <- setdiff(names(updates), allowed)
  if (length(denied)) fpq_stop("project.metadata_locked",
                               paste("Fields are not editable:", paste(denied, collapse = ", ")))
  manifest <- modifyList(project$manifest, updates)
  manifest$project_updated_at <- fpq_now()
  fpq_atomic_write_json(manifest, fpq_project_paths(project$root)$manifest,
                        fpq_validate_project_manifest)
  fpq_open_project(project$root)
}

fpq_archive_project <- function(project) {
  manifest <- project$manifest
  manifest$project_status <- "archived"
  manifest$project_updated_at <- fpq_now()
  fpq_atomic_write_json(manifest, fpq_project_paths(project$root)$manifest,
                        fpq_validate_project_manifest)
  fpq_open_project(project$root)
}
fpq_close_project <- function(project) invisible(fpq_validate_project_manifest(project$manifest))
fpq_project_status <- function(project) list(project_id = project$manifest$project_id,
                                             status = project$manifest$project_status,
                                             updated_at = project$manifest$project_updated_at)
