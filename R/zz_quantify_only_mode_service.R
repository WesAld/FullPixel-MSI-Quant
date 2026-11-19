# Quantify Samples Only extension for Generic FullPixel MSI Quant v1.
# This file is loaded after the existing application services. It preserves
# the validated two-group branch and adds a separate per-sample branch.

fpq_quantify_analysis_type <- function(config) {
  config$analysis_type %||% config$analysis_design$analysis_type %||%
    "quantify_only"
}

# Return the first nonblank user-facing sample name. Empty strings do not
# block fallback to sample_label or sample_id.
fpq_quantify_sample_name <- function(x, fallback = "") {
  candidates <- c(
    x$biological_sample %||% NA_character_,
    x$sample_label %||% NA_character_,
    x$display_sample_id %||% NA_character_,
    x$original_sample_id %||% NA_character_,
    x$sample_id %||% NA_character_,
    fallback
  )
  candidates <- trimws(as.character(candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates)) candidates[[1]] else ""
}

if (!exists("fpq_apply_analysis_design_two_group_base", inherits = FALSE))
  fpq_apply_analysis_design_two_group_base <- fpq_apply_analysis_design
if (!exists("fpq_configuration_from_detected_rows_two_group_base", inherits = FALSE))
  fpq_configuration_from_detected_rows_two_group_base <-
    fpq_configuration_from_detected_rows
if (!exists("fpq_validate_profile_compatibility_two_group_base", inherits = FALSE))
  fpq_validate_profile_compatibility_two_group_base <-
    fpq_validate_profile_compatibility
if (!exists("fpq_scientific_status_model_two_group_base", inherits = FALSE))
  fpq_scientific_status_model_two_group_base <- fpq_scientific_status_model
if (!exists("fpq_dataset_registration_status_two_group_base", inherits = FALSE))
  fpq_dataset_registration_status_two_group_base <-
    fpq_dataset_registration_status
if (!exists("fpq_internal_mode_samples_two_group_base", inherits = FALSE))
  fpq_internal_mode_samples_two_group_base <- fpq_internal_mode_samples
if (!exists("fpq_prepare_mode_extraction_request_two_group_base", inherits = FALSE))
  fpq_prepare_mode_extraction_request_two_group_base <-
    fpq_prepare_mode_extraction_request
if (!exists("fpq_prepare_full_workflow_two_group_base", inherits = FALSE))
  fpq_prepare_full_workflow_two_group_base <- fpq_prepare_full_workflow
if (!exists("fpq_run_script07_and_presentation_two_group_base", inherits = FALSE))
  fpq_run_script07_and_presentation_two_group_base <-
    fpq_run_script07_and_presentation

fpq_apply_analysis_design <- function(config, analysis_type, ion_mode) {
  supported <- c("quantify_only", "compare_two_groups",
                 "compare_multiple_groups")
  if (!analysis_type %in% supported)
    fpq_stop("analysis_design.type", "Select a supported analysis design.")
  config <- fpq_apply_analysis_design_two_group_base(
    config, analysis_type, ion_mode)
  if (identical(analysis_type, "quantify_only"))
    config$comparisons <- list()
  config
}

fpq_configuration_from_detected_rows <- function(config, rows) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(fpq_configuration_from_detected_rows_two_group_base(config, rows))

  included <- Filter(function(x) isTRUE(x$include), rows %||% list())
  config$samples <- lapply(seq_along(included), function(index) {
    x <- included[[index]]
    biological <- fpq_quantify_sample_name(x, paste0("Sample_", index))
    dataset_id <- fpq_mode_dataset_id(biological, x$ion_mode)
    replicate <- trimws(x$replicate %||% "")
    if (!nzchar(replicate)) replicate <- paste0("S", index)
    list(sample_id = dataset_id, sample_label = biological,
      biological_sample = biological, condition_id = "samples",
      replicate_id = replicate, inclusion_status = "included",
      ion_mode = toupper(x$ion_mode), imzml_path = x$imzml_path,
      ibd_path = x$ibd_path, notes = "Quantify samples only")
  })
  config$conditions <- if (length(config$samples)) list(list(
    condition_id = "samples", condition_label = "Independent samples",
    analytical_role = "quantification", is_reference = FALSE,
    display_order = 1L, treatment_label = NULL, dose = NULL,
    notes = "No group comparison")) else list()
  config$comparisons <- list()
  config
}

fpq_generic_quantify_only_design <- function(config) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(list(valid = FALSE,
      message = "The saved analysis type is not Quantify samples only."))
  comparisons <- Filter(function(x) isTRUE(x$enabled %||% TRUE),
                        config$comparisons %||% list())
  if (length(comparisons))
    return(list(valid = FALSE,
      message = "Quantify samples only must not contain a group comparison."))
  modes <- fpq_active_project_modes(config)
  if (!length(modes) || any(!modes %in% c("POS", "NEG")))
    return(list(valid = FALSE,
      message = "Select Positive, Negative, or both ion modes."))
  samples <- Filter(function(x)
    identical(x$inclusion_status %||% "included", "included"),
    config$samples %||% list())
  branches <- list()
  for (mode in modes) {
    branch <- Filter(function(x)
      identical(toupper(x$ion_mode %||% ""), mode), samples)
    if (!length(branch))
      return(list(valid = FALSE,
        message = paste(mode, "requires at least one included sample.")))
    ids <- vapply(branch, function(x) trimws(x$sample_id %||% ""),
                  character(1))
    labels <- vapply(branch, fpq_quantify_sample_name, character(1))
    if (any(!nzchar(ids)) || anyDuplicated(ids))
      return(list(valid = FALSE,
        message = paste(mode, "sample identifiers must be nonblank and unique.")))
    if (any(!nzchar(labels)))
      return(list(valid = FALSE,
        message = paste(mode, "contains a blank biological sample name.")))
    branches[[mode]] <- list(sample_count = length(branch))
  }
  list(valid = TRUE, modes = modes, branches = branches,
       sample_count = length(samples))
}

fpq_validate_profile_compatibility <- function(config, profile_id) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(fpq_validate_profile_compatibility_two_group_base(config, profile_id))
  structural <- fpq_validate_structural(config)
  if (!identical(structural$classification, "structurally_valid"))
    return(structural)
  supported <- c("generic-two-group-v1", "pvl-fullpixel-reference-v1",
                 "fullpixel-40um-1600px-v1")
  if (!profile_id %in% supported)
    return(list(classification = "profile_unsupported", records = list(
      fpq_validation_record("profile.unsupported", "FAIL",
        "The selected profile has no installed Quantify samples only adapter.",
        "CRITICAL", TRUE))))
  design <- fpq_generic_quantify_only_design(config)
  if (!isTRUE(design$valid))
    return(list(classification = "profile_incompatible", records = list(
      fpq_validation_record("profile.quantify_only_design", "FAIL",
        design$message, "CRITICAL", TRUE,
        consequence = "Per-sample execution is blocked.",
        remediation = "Confirm at least one sample per selected ion mode and remove comparisons."))))
  branches <- setNames(as.list(rep("technical_validation_candidate",
                                   length(design$modes))), design$modes)
  list(classification = "profile_candidate_compatible",
    scientific_status = list(
      overall = "technical_validation_candidate",
      branches = branches, execution_allowed = TRUE,
      acknowledgement_required = TRUE,
      independent_positive_review = if ("POS" %in% design$modes) "pending"
        else "not_applicable",
      message = paste(
        "Quantify samples only is technically validated for",
        design$sample_count, "registered sample records; no comparison statistics will be produced.")),
    records = list(fpq_validation_record(
      "profile.quantify_only_compatibility", "WARNING",
      paste("Per-sample quantification accepted for",
            paste(design$modes, collapse = "+"), "with",
            design$sample_count, "registered sample records and no comparison."),
      "WARNING", FALSE,
      consequence = paste(
        "Extraction, spectrum-sum normalization, annotation integration,",
        "and individual-sample workbook export are allowed."),
      remediation = paste(
        "Retain the Technical Validation Candidate label until an",
        "independent dataset-specific validation is approved."))))
}

fpq_scientific_status_model <- function(config,
    repository_root = fpq_repo_root(), evidence = NULL) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(fpq_scientific_status_model_two_group_base(
      config, repository_root, evidence))
  design <- fpq_generic_quantify_only_design(config)
  if (!isTRUE(design$valid))
    return(list(overall = "unsupported", branches = list(),
      execution_allowed = FALSE, acknowledgement_required = FALSE,
      independent_positive_review = "not_applicable",
      message = design$message))
  list(overall = "technical_validation_candidate",
    branches = setNames(as.list(rep("technical_validation_candidate",
                                    length(design$modes))), design$modes),
    execution_allowed = TRUE, acknowledgement_required = TRUE,
    independent_positive_review = if ("POS" %in% design$modes) "pending"
      else "not_applicable",
    message = paste(
      "Per-sample FullPixel quantification is technically allowed;",
      "no group statistics will be calculated."))
}

fpq_dataset_registration_status <- function(config) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(fpq_dataset_registration_status_two_group_base(config))
  samples <- Filter(function(x)
    identical(x$inclusion_status %||% "included", "included"),
    config$samples %||% list())
  mzmine <- config$mzmine_registrations %||% list()
  modes <- fpq_active_project_modes(config)
  missing <- character()
  if (!length(samples)) missing <- c(missing, "Confirm at least one MSI sample.")
  if (length(samples) && any(!vapply(samples, function(x)
    file.exists(x$imzml_path %||% "") && file.exists(x$ibd_path %||% ""),
    logical(1)))) missing <- c(missing, "Restore missing imzML/IBD files.")
  for (mode in modes) {
    branch <- Filter(function(x)
      identical(toupper(x$ion_mode %||% ""), mode), samples)
    if (!length(branch)) missing <- c(missing,
      paste("Register at least one", mode, "sample."))
    if (is.null(mzmine[[mode]]) || !identical(mzmine[[mode]]$status, "PASS") ||
        !file.exists(mzmine[[mode]]$path %||% ""))
      missing <- c(missing, paste("Confirm the", if (mode == "POS")
        "Positive" else "Negative", "MZmine column mapping."))
  }
  biological <- unique(vapply(
    samples, fpq_quantify_sample_name, character(1)))
  list(status = if (length(missing)) "NOT_REGISTERED" else "REGISTERED",
    blocking = length(missing) > 0L, missing = unique(missing),
    summary = list(msi_datasets = length(samples),
      mzmine_feature_lists = length(mzmine),
      biological_samples = length(biological), comparisons = 0L))
}

fpq_internal_mode_samples <- function(config, mode) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(fpq_internal_mode_samples_two_group_base(config, mode))
  rows <- Filter(function(x)
    identical(toupper(x$ion_mode %||% ""), mode) &&
      identical(x$inclusion_status %||% "included", "included"),
    config$samples %||% list())
  if (!length(rows))
    fpq_stop("workflow.quantify_samples_missing",
      paste(mode, "requires at least one included sample."), "scientific")
  labels <- tolower(vapply(rows, fpq_quantify_sample_name, character(1)))
  ids <- vapply(rows, function(x) tolower(x$sample_id), character(1))
  rows <- rows[order(labels, ids)]
  result <- lapply(seq_along(rows), function(index) {
    row <- rows[[index]]
    internal <- paste0("S", formatC(index, width = 3L, flag = "0"))
    display <- fpq_quantify_sample_name(row, internal)
    original_replicate <- trimws(row$replicate_id %||% "")
    if (!nzchar(original_replicate)) original_replicate <- internal
    list(sample_id = internal, original_sample_id = row$sample_id,
      display_sample_id = display,
      original_condition_id = row$condition_id %||% "samples",
      display_condition = "Independent samples",
      condition_id = "SAMPLES", replicate_id = internal,
      original_replicate_id = original_replicate,
      imzml_path = normalizePath(row$imzml_path, mustWork = TRUE,
                                 winslash = "/"),
      ibd_path = normalizePath(row$ibd_path, mustWork = TRUE,
                               winslash = "/"))
  })
  unname(result)
}

fpq_prepare_mode_extraction_request <- function(
    project, config, mode, workflow_id,
    repository_root = fpq_repo_root(), hash_raw_inputs = TRUE) {
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(fpq_prepare_mode_extraction_request_two_group_base(
      project, config, mode, workflow_id, repository_root, hash_raw_inputs))
  mode <- toupper(mode)
  mzmine <- config$mzmine_registrations[[mode]]
  if (is.null(mzmine) || !identical(mzmine$status, "PASS"))
    fpq_stop("workflow.mzmine_missing",
      paste("Confirm the", mode, "MZmine mapping."), "input")
  samples <- fpq_internal_mode_samples(config, mode)
  token <- paste(workflow_id, tolower(mode), sep = "-")
  request <- fpq_read_json(file.path(repository_root, "config", "examples",
    "pvl_single_negative_extraction.execution_request.json"))
  exposure <- fpq_project_exposure_label(project, config)
  acquisition <- fpq_mode_acquisition_metadata(project, config, mode)
  request$request_id <- paste0("req-", token, "-extract")
  request$run_id <- paste0("run-", token, "-extract")
  request$project_id <- project$manifest$project_id
  request$dataset_id <- fpq_workflow_mode_dataset_id(
    project, exposure, mode, FALSE)
  request$stage_id <- "extract_pixel_intensities"
  request$analysis_type <- "quantify_only"
  request$ion_mode <- mode
  request$exposure_label <- exposure
  request$execution_adapter <- "generic_quantify_only"
  request$scientific_profile <- list(
    profile_id = project$manifest$scientific_profile_id,
    profile_version = project$manifest$scientific_profile_version,
    reference_status = "technical_validation_candidate")
  request$registered_inputs$raw_data_root <- fpq_common_parent(
    unlist(lapply(samples, function(x) c(x$imzml_path, x$ibd_path))))
  request$registered_inputs$raw_file_hash_registry <- if (hash_raw_inputs)
    fpq_write_raw_hash_registry(project, workflow_id, mode, samples) else
      list(path = "pending", sha256 = paste(rep("0", 64), collapse = ""))
  request$registered_inputs$aligned_target_table <- list(
    artifact_id = paste0("art-", token, "-mzmine"),
    path = normalizePath(mzmine$path, winslash = "/"),
    sha256 = fpq_hash_file(mzmine$path))
  request$output_working_directory <- file.path(fpq_project_paths(project$root)$runs,
    workflow_id, "scientific-output", tolower(mode), "extract")
  request$conditions <- list(list(condition_id = "SAMPLES",
    analytical_role = "quantification",
    display_condition_id = "samples",
    display_label = "Independent samples"))
  request$samples <- unname(lapply(samples, function(x) x[
    c("sample_id", "condition_id", "replicate_id", "imzml_path", "ibd_path")]))
  request$replicates <- unname(lapply(samples, function(x)
    x[c("condition_id", "replicate_id")]))
  request$internal_role_mappings <- unname(lapply(samples, function(x) list(
    user_facing_value = x$original_sample_id,
    user_facing_label = x$display_sample_id,
    original_condition_id = x$original_condition_id,
    original_replicate_id = x$original_replicate_id,
    internal_value = x$sample_id, ion_mode = mode,
    reason = "Quantify-only canonical sample mapping",
    compatibility_rule = "one or more independent samples; no comparison",
    provenance_requirement = "required")))
  request$comparison <- NULL
  request$column_mappings <- list(target_mz = mzmine$mappings$mz,
                                  feature_id = mzmine$mappings$feature_id)
  request$acquisition_metadata <- acquisition[c("matrix", "pixel_size_um",
                                                 "full_pixel")]
  request$pixel_metadata$expected_coordinate_count <-
    acquisition$expected_coordinate_count
  request$pixel_metadata$pixel_geometry_policy <-
    acquisition$pixel_geometry_policy
  request$display_metadata <- list(
    project_name = project$manifest$project_name,
    dataset_prefix = fpq_generic_token(project$manifest$project_name, "Project"),
    exposure_label = exposure, analysis_type = "quantify_only",
    sample_mappings = request$internal_role_mappings)
  request$primary_normalization_method <- fpq_normalization_method(config)
  request$approved_scientific_configuration$primary_normalization_method <-
    request$primary_normalization_method
  request$display_metadata$normalization <- list(
    application_version = "2.0.0",
    primary_normalization_method = request$primary_normalization_method,
    normalization_scale = request$approved_scientific_configuration$normalization_scale,
    normalization_definition_version = fpq_normalization_definition_version())
  fpq_apply_mass_tolerance_to_request(request, config)
}

fpq_prepare_full_workflow <- function(project, config, validations,
                                      repository_root = fpq_repo_root(),
                                      hash_raw_inputs = TRUE) {
  prepared <- fpq_prepare_full_workflow_two_group_base(
    project, config, validations, repository_root, hash_raw_inputs)
  if (!identical(fpq_quantify_analysis_type(config), "quantify_only"))
    return(prepared)
  mappings <- unlist(lapply(prepared$requests, function(x)
    x$display_metadata$sample_mappings %||% list()), recursive = FALSE)
  metadata <- prepared$requests[[1]]$display_metadata
  metadata$sample_mappings <- mappings
  prepared$record <- fpq_update_workflow_record(project,
    prepared$record$workflow_id, list(
      execution_adapter = "generic_quantify_only",
      analysis_type = "quantify_only",
      display_metadata = metadata))
  prepared
}

fpq_run_quantify_only_export <- function(project, workflow, masters,
                                         repository_root,
                                         attempt_id = NULL) {
  suffix <- if (is.null(attempt_id) || !nzchar(attempt_id)) "" else
    paste0("-", fpq_generic_token(attempt_id, "resume"))
  request <- list(schema_version = "1.0.0",
    workflow_id = workflow$workflow_id,
    project_id = project$manifest$project_id,
    analysis_type = "quantify_only",
    exposure_label = workflow$exposure,
    scientific_status = "technical_validation_candidate",
    display_metadata = workflow$display_metadata,
    masters = unname(lapply(names(masters), function(mode) list(
      ion_mode = mode, path = masters[[mode]]$path,
      sha256 = masters[[mode]]$sha256,
      artifact_id = masters[[mode]]$artifact_id))),
    output_working_directory = file.path(workflow$output_folder,
      "scientific-output", paste0("quantify-only-final-export", suffix)))
  request_path <- file.path(workflow$output_folder, "requests",
    paste0("quantify-only-publication-export", suffix, ".json"))
  report_path <- file.path(workflow$output_folder,
    paste0("quantify-only-publication-export-report", suffix, ".json"))
  stdout <- file.path(workflow$output_folder,
    paste0("quantify-only-publication-export", suffix, ".stdout.log"))
  stderr <- file.path(workflow$output_folder,
    paste0("quantify-only-publication-export", suffix, ".stderr.log"))
  fpq_atomic_write_json(request, request_path)
  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python.exe")
  process <- processx::run(python,
    c(file.path(repository_root, "tools", "quantify_only_export_adapter.py"),
      request_path, "--report", report_path),
    wd = fpq_safe_subprocess_cwd(), stdout = stdout, stderr = stderr,
    error_on_status = FALSE)
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  if (process$status != 0L || is.null(report) ||
      !identical(report$status, "PASS"))
    fpq_stop("workflow.quantify_only_export_failed",
      report$error %||% paste(
        "Quantify-only workbook export failed; inspect preserved diagnostics."),
      "scientific")
  final_path <- normalizePath(report$final_workbook$path,
                              winslash = "/", mustWork = TRUE)
  final_hash <- fpq_hash_file(final_path)
  if (!identical(final_hash, report$final_workbook$sha256))
    fpq_stop("workflow.quantify_only_export_hash",
      "Quantify-only final workbook hash verification failed.", "scientific")
  artifact <- fpq_register_artifact(project, list(
    artifact_id = paste0("art-", workflow$workflow_id,
                         "-quantify-only-final-workbook", suffix),
    artifact_type = "final_results_workbook",
    display_name = basename(final_path), path = final_path,
    project_id = project$manifest$project_id, dataset_id = NULL,
    ion_mode = NULL, exposure = workflow$exposure,
    producing_run_id = workflow$workflow_id,
    producing_stage_id = "create_combined_publication_export",
    sha256 = final_hash,
    file_size = unname(file.info(final_path)$size),
    validation_status = "PASS",
    scientific_profile_id = project$manifest$scientific_profile_id,
    scientific_core_version = project$manifest$scientific_core_version,
    parent_artifact_ids = lapply(masters, `[[`, "artifact_id"),
    notes = paste(
      "Quantify samples only five-sheet workbook;",
      "individual normalized-intensity, strong-pixel intensity,",
      "detection fraction and count columns; no group statistics;",
      "Technical Validation Candidate")), activate = TRUE)
  list(path = final_path, sha256 = artifact$sha256,
       artifact_id = artifact$artifact_id, audit_workbook = final_path,
       validation = list(status = "PASS", errors = character()),
       report = report)
}

fpq_run_script07_and_presentation <- function(project, workflow, masters,
                                               repository_root,
                                               attempt_id = NULL) {
  if (identical(workflow$execution_adapter %||% "",
                "generic_quantify_only"))
    return(fpq_run_quantify_only_export(project, workflow, masters,
                                        repository_root, attempt_id))
  fpq_run_script07_and_presentation_two_group_base(
    project, workflow, masters, repository_root, attempt_id)
}
