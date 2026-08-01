fpq_register_file <- function(path, artifact_type, path_type = "external_absolute") {
  exact <- normalizePath(path, mustWork = TRUE)
  if (!file.access(exact, 4L) == 0L)
    fpq_stop("file.unreadable", paste("File is not readable:", exact), "input")
  list(path = exact, path_type = path_type, artifact_type = artifact_type,
       sha256 = fpq_hash_file(exact), file_size = unname(file.info(exact)$size),
       registered_at = fpq_now(), external_path_available = TRUE)
}

fpq_validate_pair <- function(imzml_path, ibd_path) {
  if (!file.exists(imzml_path)) fpq_stop("dataset.imzml_missing", "imzML file is missing.", "input")
  if (!file.exists(ibd_path)) fpq_stop("dataset.ibd_missing", "IBD file is missing.", "input")
  stem <- function(x) tolower(tools::file_path_sans_ext(basename(x)))
  if (!identical(stem(imzml_path), stem(ibd_path)))
    fpq_stop("dataset.pair_ambiguous", "imzML and IBD basenames do not match.", "input")
  invisible(TRUE)
}

fpq_register_dataset <- function(project, dataset) {
  fpq_validate_dynamic_configuration(dataset)
  if (is.null(dataset$dataset_id) || !grepl("^dset-", dataset$dataset_id))
    fpq_stop("dataset.id_invalid", "Dataset ID must begin with dset-.")
  existing <- list.files(fpq_project_paths(project$root)$datasets, pattern = "\\.json$")
  if (paste0(dataset$dataset_id, ".json") %in% existing)
    fpq_stop("dataset.duplicate", "Dataset ID is already registered.")
  pairs <- dataset$raw_data_pairs %||% list()
  pair_keys <- character()
  registered_pairs <- lapply(pairs, function(pair) {
    fpq_validate_pair(pair$imzml_path, pair$ibd_path)
    key <- paste(normalizePath(pair$imzml_path), normalizePath(pair$ibd_path), sep = "|")
    if (key %in% pair_keys) fpq_stop("dataset.pair_duplicate", "Duplicate imzML/IBD pair.")
    pair_keys <<- c(pair_keys, key)
    list(sample_id = pair$sample_id,
         imzml = fpq_register_file(pair$imzml_path, "centroided_imzML"),
         ibd = fpq_register_file(pair$ibd_path, "centroided_IBD"))
  })
  dataset$registered_raw_data_pairs <- registered_pairs
  dataset$registered_aligned_feature_list <- fpq_register_file(
    dataset$aligned_feature_list$path, "aligned_mzmine_target_list")
  if (!is.null(dataset$annotated_feature_list))
    dataset$registered_annotated_feature_list <- fpq_register_file(
      dataset$annotated_feature_list$path, "annotated_mzmine_feature_list")
  dataset$registration_time <- fpq_now()
  saved <- fpq_save_configuration(project, "datasets", dataset$dataset_id, dataset,
                                  fpq_validate_dynamic_configuration)
  manifest <- saved$project$manifest
  manifest$registered_datasets <- c(manifest$registered_datasets, dataset$dataset_id)
  manifest$project_updated_at <- fpq_now()
  fpq_atomic_write_json(manifest, fpq_project_paths(project$root)$manifest,
                        fpq_validate_project_manifest)
  fpq_log_event(project, "dataset.registered", dataset_id = dataset$dataset_id,
                details = list(configuration_hash = saved$sha256))
  profile_id <- project$manifest$scientific_profile_id
  core_version <- project$manifest$scientific_core_version
  for (pair in registered_pairs) {
    for (kind in c("imzml", "ibd")) {
      file_record <- pair[[kind]]
      artifact_id <- paste("art", dataset$dataset_id, pair$sample_id, kind, sep = "-")
      fpq_register_artifact(project, list(artifact_id = artifact_id,
        artifact_type = "registered_raw_data", display_name = paste(pair$sample_id,
          toupper(kind)), path = file_record$path, path_type = "external_absolute",
        project_id = project$manifest$project_id, dataset_id = dataset$dataset_id,
        ion_mode = dataset$ion_mode, exposure = dataset$exposure_label,
        producing_run_id = NULL, producing_stage_id = NULL,
        sha256 = file_record$sha256, file_size = file_record$file_size,
        media_type = if (kind == "imzml") "application/xml" else "application/octet-stream",
        validation_status = "PASS", scientific_profile_id = profile_id,
        scientific_core_version = core_version,
        notes = "External source file; never modified or copied by registration."), FALSE)
    }
  }
  target <- dataset$registered_aligned_feature_list
  fpq_register_artifact(project, list(
    artifact_id = paste0("art-", dataset$dataset_id, "-aligned-targets"),
    artifact_type = "aligned_feature_target_list",
    display_name = "Aligned MZmine feature target list",
    path = target$path, path_type = "external_absolute",
    project_id = project$manifest$project_id, dataset_id = dataset$dataset_id,
    ion_mode = dataset$ion_mode, exposure = dataset$exposure_label,
    producing_run_id = NULL, producing_stage_id = NULL,
    sha256 = target$sha256, file_size = target$file_size, media_type = "text/csv",
    validation_status = "PASS", scientific_profile_id = profile_id,
    scientific_core_version = core_version,
    notes = "Supplies feature identifiers and target m/z values, not quantitative intensity."), FALSE)
  fpq_open_project(project$root)
}

fpq_verify_registered_file <- function(record) {
  if (!file.exists(record$path))
    return(fpq_validation_record("artifact.external_available", "FAIL",
      "Registered external path is unavailable.", blocking = TRUE,
      observed = FALSE, expected = TRUE, remediation = "Restore or explicitly re-register the file."))
  observed <- fpq_hash_file(record$path)
  if (!identical(observed, record$sha256))
    return(fpq_validation_record("artifact.hash", "FAIL", "Registered source has changed.",
      blocking = TRUE, observed = observed, expected = record$sha256,
      remediation = "Review the external modification and explicitly register a new artifact."))
  fpq_validation_record("artifact.hash", "PASS", "Registered file is available and unchanged.",
                        severity = "INFO", blocking = FALSE, observed = observed,
                        expected = record$sha256)
}
