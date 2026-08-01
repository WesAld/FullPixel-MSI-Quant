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
  paths <- fpq_project_paths(staging)
  dirs <- c(paths$datasets, paths$run_configurations, paths$snapshots, paths$runs,
            dirname(paths$artifacts), dirname(paths$logs), dirname(paths$validations),
            dirname(paths$provenance), paths$reports, paths$temporary)
  vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE)
  now <- fpq_now()
  manifest <- list(schema_version = "1.0.0", application_id = "FullPixelMSIQuant",
    application_version = "1.0.0", project_id = project_id, project_name = project_name,
    project_description = project_description, project_created_at = now, project_updated_at = now,
    project_status = "active",
    repository_root_policy = "Project resources are relative; external scientific inputs are absolute.",
    project_data_root = list(path = ".", path_type = "project_relative", portability = "portable"),
    analysis_root = list(path = "runs", path_type = "project_relative", portability = "portable"),
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
