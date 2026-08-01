fpq_workflow_record_path <- function(project, workflow_id)
  file.path(fpq_project_paths(project$root)$runs, workflow_id, "workflow.json")

fpq_load_workflow_record <- function(project, workflow_id)
  fpq_read_json(fpq_workflow_record_path(project, workflow_id))

fpq_list_workflow_records <- function(project) {
  if (is.null(project)) return(list())
  files <- list.files(fpq_project_paths(project$root)$runs,
    pattern = "^workflow[.]json$", recursive = TRUE, full.names = TRUE)
  records <- lapply(files, fpq_read_json)
  records[order(vapply(records, `[[`, "", "created_at"), decreasing = TRUE)]
}

fpq_update_workflow_record <- function(project, workflow_id, updates) {
  record <- fpq_load_workflow_record(project, workflow_id)
  record <- modifyList(record, updates)
  record$updated_at <- fpq_now()
  fpq_atomic_write_json(record, fpq_workflow_record_path(project, workflow_id))
  record
}

fpq_workflow_stage_names <- function() c(
  preparing = "Preparing run",
  reading_msi = "Reading MSI data",
  extracting_pos = "Extracting Positive-mode intensities",
  extracting_neg = "Extracting Negative-mode intensities",
  normalizing = "Spectrum-sum normalization",
  summarizing = "Summarizing replicate readouts",
  annotating = "Integrating MZmine annotations",
  combined_export = "Creating combined publication export",
  final_workbook = "Creating final five-sheet workbook",
  validating_workbook = "Validating final workbook")

fpq_new_workflow_stages <- function(modes) {
  lapply(names(fpq_workflow_stage_names()), function(id) list(
    stage_id = id, display_name = unname(fpq_workflow_stage_names()[[id]]),
    status = if ((id == "extracting_pos" && !"POS" %in% modes) ||
                 (id == "extracting_neg" && !"NEG" %in% modes)) "skipped" else
      "waiting", started_at = NULL, finished_at = NULL, message = NULL))
}

fpq_set_workflow_stage <- function(project, workflow_id, stage_id, status,
                                   message = NULL) {
  record <- fpq_load_workflow_record(project, workflow_id)
  index <- which(vapply(record$stages,
    function(x) identical(x$stage_id, stage_id), logical(1)))
  if (length(index) != 1L)
    fpq_stop("workflow.stage_missing", paste("Unknown workflow stage:", stage_id))
  now <- fpq_now()
  record$stages[[index]]$status <- status
  record$stages[[index]]$message <- message
  if (status == "running" && is.null(record$stages[[index]]$started_at))
    record$stages[[index]]$started_at <- now
  if (status %in% c("completed", "failed", "cancelled", "skipped"))
    record$stages[[index]]$finished_at <- now
  record$current_stage <- stage_id
  record$updated_at <- now
  fpq_atomic_write_json(record, fpq_workflow_record_path(project, workflow_id))
  record
}


fpq_generic_token <- function(value, fallback = "Project", maximum = 48L) {
  token <- trimws(as.character(value %||% ""))
  token <- gsub("[^A-Za-z0-9]+", "_", token)
  token <- gsub("(^_+|_+$)", "", token)
  if (!nzchar(token)) token <- fallback
  substr(token, 1L, maximum)
}

fpq_project_exposure_label <- function(project, config) {
  value <- config$exposure_label %||% config$exposure %||%
    project$manifest$project_preferences$default_exposure %||% "Unspecified"
  value <- trimws(as.character(value))
  if (!nzchar(value)) "Unspecified" else value
}

fpq_internal_mode_samples <- function(config, mode) {
  samples <- Filter(function(x)
    identical(toupper(x$ion_mode %||% ""), mode) &&
      identical(x$inclusion_status %||% "included", "included"),
    config$samples %||% list())
  comparisons <- Filter(function(x) isTRUE(x$enabled %||% TRUE),
                        config$comparisons %||% list())
  if (length(comparisons) != 1L)
    fpq_stop("workflow.comparison_count",
      "Generic v1 requires exactly one enabled comparison.", "scientific")
  comparison <- comparisons[[1]]
  groups <- c(comparison$reference_condition_id,
              comparison$comparison_condition_id)
  internal_groups <- c("CTRL", "TRT")
  condition_lookup <- setNames(config$conditions, vapply(
    config$conditions %||% list(), function(x) x$condition_id, character(1)))

  grouped <- lapply(groups, function(group) {
    rows <- Filter(function(x) identical(x$condition_id, group), samples)
    rows[order(vapply(rows, function(x)
      trimws(as.character(x$replicate_id %||% "")), character(1)))]
  })
  counts <- vapply(grouped, length, integer(1))
  if (any(counts < 2L))
    fpq_stop("workflow.sample_mapping",
      paste(mode, "requires at least two included replicates in each condition."),
      "scientific")
  if (length(unique(counts)) != 1L)
    fpq_stop("workflow.sample_mapping",
      paste(mode, "requires equal replicate counts in the two conditions for Generic v1."),
      "scientific")

  result <- unlist(Map(function(rows, group, internal_group) {
    lapply(seq_along(rows), function(index) {
      row <- rows[[index]]
      original_replicate <- trimws(as.character(row$replicate_id %||% ""))
      if (!nzchar(original_replicate))
        fpq_stop("workflow.replicate_missing",
          paste("Replicate identifier is missing for", row$sample_id), "scientific")
      list(
        sample_id = paste0(internal_group, "_R", index),
        original_sample_id = row$sample_id,
        display_sample_id = row$biological_sample %||%
          row$sample_label %||% row$sample_id,
        original_condition_id = group,
        display_condition = condition_lookup[[group]]$condition_label %||% group,
        condition_id = internal_group,
        replicate_id = paste0("R", index),
        original_replicate_id = original_replicate,
        imzml_path = normalizePath(row$imzml_path, mustWork = TRUE,
                                   winslash = "/"),
        ibd_path = normalizePath(row$ibd_path, mustWork = TRUE,
                                 winslash = "/")
      )
    })
  }, grouped, groups, internal_groups), recursive = FALSE)

  internal_ids <- vapply(result, `[[`, "", "sample_id")
  if (anyDuplicated(internal_ids))
    fpq_stop("workflow.sample_mapping",
      "Internal sample identifiers are not unique.", "scientific")
  unname(result)
}

fpq_write_raw_hash_registry <- function(project, workflow_id, mode, samples) {
  files <- unlist(lapply(samples, function(sample) list(
    list(role = paste0(sample$sample_id, "_imzML"),
      path = sample$imzml_path, sha256 = fpq_hash_file(sample$imzml_path)),
    list(role = paste0(sample$sample_id, "_IBD"),
      path = sample$ibd_path, sha256 = fpq_hash_file(sample$ibd_path)))),
    recursive = FALSE)
  path <- file.path(project$root, "runs", workflow_id, "requests",
                    paste0(tolower(mode), "-raw-input-hashes.json"))
  fpq_atomic_write_json(list(schema_version = "1.0.0", files = files), path)
  list(path = normalizePath(path, winslash = "/"),
       sha256 = fpq_hash_file(path))
}

fpq_workflow_mode_dataset_id <- function(project, exposure, mode,
                                         exact_pvl_reference = FALSE) {
  if (isTRUE(exact_pvl_reference))
    return(paste("PVL", exposure,
      if (mode == "POS") "Positive" else "Negative", sep = "_"))
  paste(fpq_generic_token(project$manifest$project_name, "Project"),
        fpq_generic_token(exposure, "Exposure"),
        if (mode == "POS") "Positive" else "Negative", sep = "_")
}

fpq_mode_acquisition_metadata <- function(project, config, mode) {
  preferences <- project$manifest$project_preferences %||% list()
  matrix <- if (mode == "POS")
    config$positive_matrix %||% preferences$positive_matrix %||% "DHB"
  else
    config$negative_matrix %||% preferences$negative_matrix %||% "9AA"
  pixel_size <- suppressWarnings(as.numeric(
    config$pixel_size_um %||% preferences$pixel_size_um %||% 40))
  coordinates <- suppressWarnings(as.integer(
    config$expected_coordinate_count %||%
      preferences$expected_coordinate_count %||% 1600))
  if (!nzchar(trimws(matrix %||% "")))
    fpq_stop("workflow.matrix_missing",
      paste(mode, "matrix metadata is blank."), "configuration")
  if (!is.finite(pixel_size) || pixel_size <= 0)
    fpq_stop("workflow.pixel_size",
      "Pixel size must be a positive number.", "configuration")
  if (is.na(coordinates) || coordinates <= 0)
    fpq_stop("workflow.coordinate_count",
      "Expected pixels per sample must be a positive integer.", "configuration")
  list(matrix = trimws(matrix), pixel_size_um = pixel_size,
       full_pixel = isTRUE(preferences$full_pixel %||% TRUE),
       expected_coordinate_count = coordinates)
}

fpq_prepare_mode_extraction_request <- function(
    project, config, mode, workflow_id,
    repository_root = fpq_repo_root(), hash_raw_inputs = TRUE) {
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
  exact_pvl <- project$manifest$scientific_profile_id %in%
      c("pvl-fullpixel-reference-v1", "fullpixel-40um-1600px-v1") &&
    identical(exposure, "Single") && identical(mode, "NEG") &&
    length(samples) == 6L
  acquisition <- fpq_mode_acquisition_metadata(project, config, mode)

  request$request_id <- paste0("req-", token, "-extract")
  request$run_id <- paste0("run-", token, "-extract")
  request$project_id <- project$manifest$project_id
  request$dataset_id <- fpq_workflow_mode_dataset_id(project, exposure, mode, exact_pvl)
  request$ion_mode <- mode
  request$exposure_label <- exposure
  request$execution_adapter <- if (exact_pvl)
    "pvl_reference" else "generic_two_group"
  request$scientific_profile <- list(
    profile_id = project$manifest$scientific_profile_id,
    profile_version = project$manifest$scientific_profile_version,
    reference_status = if (exact_pvl)
      "validated_reference" else "technical_validation_candidate")
  request$registered_inputs$raw_data_root <- fpq_common_parent(
    unlist(lapply(samples, function(x) c(x$imzml_path, x$ibd_path))))
  request$registered_inputs$raw_file_hash_registry <- if (hash_raw_inputs)
    fpq_write_raw_hash_registry(project, workflow_id, mode, samples) else
      list(path = "pending", sha256 = paste(rep("0", 64), collapse = ""))
  request$registered_inputs$aligned_target_table <- list(
    artifact_id = paste0("art-", token, "-mzmine"),
    path = normalizePath(mzmine$path, winslash = "/"),
    sha256 = fpq_hash_file(mzmine$path))
  request$output_working_directory <- file.path(project$root, "runs",
    workflow_id, "scientific-output", tolower(mode), "extract")
  request$conditions <- list(
    list(condition_id = "CTRL", analytical_role = "reference",
         display_condition_id = samples[[1]]$original_condition_id,
         display_label = samples[[1]]$display_condition),
    list(condition_id = "TRT", analytical_role = "comparison",
         display_condition_id = samples[[length(samples)]]$original_condition_id,
         display_label = samples[[length(samples)]]$display_condition)
  )
  request$samples <- unname(lapply(samples, function(x) x[
    c("sample_id", "condition_id", "replicate_id", "imzml_path", "ibd_path")]))
  request$replicates <- unname(lapply(samples, function(x)
    x[c("condition_id", "replicate_id")]))
  request$internal_role_mappings <- unname(lapply(samples, function(x) list(
    user_facing_value = x$original_sample_id,
    user_facing_label = x$display_sample_id,
    original_condition_id = x$original_condition_id,
    original_replicate_id = x$original_replicate_id,
    internal_value = x$sample_id,
    reason = "Generic two-group canonical role mapping",
    compatibility_rule = "equal replicate counts; two conditions",
    provenance_requirement = "required")))
  request$comparison <- list(comparison_id = "CTRL_vs_TRT",
    reference_condition_id = "CTRL", comparison_condition_id = "TRT",
    reference_display_label = request$conditions[[1]]$display_label,
    comparison_display_label = request$conditions[[2]]$display_label)
  request$column_mappings <- list(target_mz = mzmine$mappings$mz,
                                  feature_id = mzmine$mappings$feature_id)
  request$acquisition_metadata <- acquisition[c("matrix", "pixel_size_um",
                                                 "full_pixel")]
  request$pixel_metadata$expected_coordinate_count <-
    acquisition$expected_coordinate_count
  request$display_metadata <- list(
    project_name = project$manifest$project_name,
    dataset_prefix = fpq_generic_token(project$manifest$project_name, "Project"),
    exposure_label = exposure,
    reference_condition = request$conditions[[1]]$display_label,
    comparison_condition = request$conditions[[2]]$display_label,
    sample_mappings = request$internal_role_mappings)
  request
}

fpq_request_path <- function(project, workflow_id, request) {
  path <- file.path(project$root, "runs", workflow_id, "requests",
                    paste0(request$run_id, ".json"))
  fpq_atomic_write_json(request, path)
  normalizePath(path, winslash = "/")
}

fpq_prepare_full_workflow <- function(project, config, validations,
                                      repository_root = fpq_repo_root(),
                                      hash_raw_inputs = TRUE) {
  if (is.null(project)) fpq_stop("workflow.project_required", "Open a project first.")
  if (!length(validations) ||
      !validations$readiness %in% c("ready", "ready_with_warning",
                                    "candidate_workflow_ready"))
    fpq_stop("workflow.validation_required",
             "Validate the project successfully before starting.", "validation")
  registration <- fpq_dataset_registration_status(config)
  if (registration$blocking)
    fpq_stop("workflow.registration_incomplete",
             paste(registration$missing, collapse = " "), "input")
  modes <- fpq_active_project_modes(config, project$manifest)
  workflow_id <- fpq_id("workflow")
  record <- list(schema_version = "1.0.0", workflow_id = workflow_id,
    project_id = project$manifest$project_id,
    status = "prepared", scientific_status =
      validations$profile$scientific_status$overall %||% "approved",
    candidate_acknowledgement = NULL, modes = as.list(modes),
    exposure = fpq_project_exposure_label(project, config),
    created_at = fpq_now(), updated_at = fpq_now(), started_at = NULL,
    finished_at = NULL, current_stage = "preparing",
    output_folder = file.path(project$root, "runs", workflow_id),
    validation_snapshot_hash = if (file.exists(
      fpq_project_paths(project$root)$validations))
      fpq_hash_file(fpq_project_paths(project$root)$validations) else NULL,
    configuration_hash = fpq_hash_object(config),
    request_paths = list(), child_run_ids = list(),
    stages = fpq_new_workflow_stages(modes), final_workbook = NULL,
    failure = NULL, cancellation_requested = FALSE, retry_of = NULL)
  path <- fpq_workflow_record_path(project, workflow_id)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fpq_atomic_write_json(record, path)
  requests <- lapply(modes, function(mode)
    fpq_prepare_mode_extraction_request(project, config, mode, workflow_id,
      repository_root, hash_raw_inputs))
  paths <- lapply(requests, function(x)
    fpq_request_path(project, workflow_id, x))
  names(paths) <- modes
  record <- fpq_update_workflow_record(project, workflow_id,
    list(request_paths = paths,
      child_run_ids = lapply(requests, `[[`, "run_id"),
      execution_adapter = if (all(vapply(requests, function(x)
        identical(x$execution_adapter, "pvl_reference"), logical(1))))
          "pvl_reference" else "generic_two_group",
      display_metadata = requests[[1]]$display_metadata))
  list(record = record, requests = requests, paths = paths)
}

fpq_finalize_workflow_requests <- function(project, workflow_id) {
  workflow <- fpq_load_workflow_record(project, workflow_id)
  requests <- lapply(workflow$request_paths, fpq_read_json)
  changed <- FALSE
  for (mode in names(requests)) {
    request <- requests[[mode]]
    if (identical(request$registered_inputs$raw_file_hash_registry$path,
                  "pending")) {
      registry <- fpq_write_raw_hash_registry(project, workflow_id, mode,
                                              request$samples)
      request$registered_inputs$raw_file_hash_registry <- registry
      fpq_atomic_write_json(request, workflow$request_paths[[mode]])
      requests[[mode]] <- request
      changed <- TRUE
    }
  }
  if (changed)
    fpq_update_workflow_record(project, workflow_id,
      list(request_hashes = lapply(workflow$request_paths, fpq_hash_file)))
  requests
}

fpq_build_downstream_request <- function(project, workflow, extraction_request,
                                         upstream, stage_id, annotation = NULL) {
  upstream <- fpq_inspect_artifact(project, upstream$artifact_id)
  request <- extraction_request
  token <- sub("-extract$", "", extraction_request$run_id)
  request$stage_id <- stage_id
  request$run_id <- paste0(token, if (stage_id == "summarize_replicate_readouts")
    "-summary" else "-annotation")
  request$request_id <- sub("^run-", "req-", request$run_id)
  request$output_working_directory <- file.path(workflow$output_folder,
    "scientific-output", tolower(request$ion_mode),
    if (stage_id == "summarize_replicate_readouts") "summary" else "annotation")
  if (stage_id == "summarize_replicate_readouts") {
    request$frozen_script <- list(script_id = stage_id,
      repository_relative_path =
        "validated-source/03_three_readout_replicate_summary_PUBLICATION_READY.R",
      sha256 = "3D3E2E7A6E81972FE8F6EA4D54F5BCD52F47D1B66C62E35F8E1CFF00ABB60A97")
    request$registered_inputs <- list(pixel_intensity_artifact = upstream)
    request$regression_reference <- list(path = "", sha256 = "",
      classification = "technical_validation_candidate")
    request$environment_requirements$required_packages <- c("data.table", "openxlsx")
  } else {
    request$frozen_script <- list(script_id = stage_id,
      repository_relative_path =
        "validated-source/04_merge_annotations_three_readouts_PUBLICATION_READY.R",
      sha256 = "1150E5F10565B7C25784A9DD875380ACC748C95A8D5BE7B0431C1C5767C587D1")
    request$registered_inputs <- list(replicate_summary_artifact = upstream,
                                      annotation_artifact = annotation)
    request$annotation_column_mappings <- list(feature_id = "id", mz = "mz",
      annotation_name = "preferred_annotation:compound_name",
      formula = "preferred_annotation:mol_formula",
      adduct = "preferred_annotation:adduct",
      score = "preferred_annotation:score",
      compound_score = "preferred_annotation:compound_annotation_score",
      database = "preferred_annotation:database_name")
    request$regression_reference <- list(path = "", sha256 = "",
      classification = "technical_validation_candidate")
  }
  request
}

fpq_register_mzmine_artifact <- function(project, workflow, request, config) {
  mode <- request$ion_mode
  mzmine <- config$mzmine_registrations[[mode]]
  id <- paste0("art-", workflow$workflow_id, "-", tolower(mode), "-annotation")
  existing <- Filter(function(x) identical(x$artifact_id, id),
                     fpq_list_artifacts(project))
  if (length(existing)) return(existing[[1]])
  fpq_register_artifact(project, list(
    artifact_id = id, artifact_type = "annotated_feature_list",
    display_name = paste(mode, "aligned annotated MZmine feature list"),
    path = normalizePath(mzmine$path, winslash = "/"),
    project_id = project$manifest$project_id, dataset_id = request$dataset_id,
    ion_mode = mode, exposure = request$exposure_label,
    producing_run_id = NULL, producing_stage_id = "input_registration",
    sha256 = fpq_hash_file(mzmine$path),
    file_size = unname(file.info(mzmine$path)$size),
    validation_status = "PASS",
    scientific_profile_id = project$manifest$scientific_profile_id,
    scientific_core_version = project$manifest$scientific_core_version,
    notes = "Persisted confirmed MZmine mapping"), activate = TRUE)
}

fpq_ensure_extraction_target_artifact <- function(project, request) {
  input <- request$registered_inputs$aligned_target_table
  existing <- Filter(function(x) identical(x$artifact_id, input$artifact_id),
                     fpq_list_artifacts(project))
  if (length(existing)) return(existing[[1]])
  fpq_register_artifact(project, list(
    artifact_id = input$artifact_id, artifact_type = "aligned_target_table",
    display_name = paste(request$ion_mode, "MZmine target m/z list"),
    path = input$path, project_id = project$manifest$project_id,
    dataset_id = request$dataset_id, ion_mode = request$ion_mode,
    exposure = request$exposure_label, producing_run_id = NULL,
    producing_stage_id = "input_registration", sha256 = input$sha256,
    file_size = unname(file.info(input$path)$size),
    validation_status = "PASS",
    scientific_profile_id = project$manifest$scientific_profile_id,
    scientific_core_version = project$manifest$scientific_core_version,
    notes = "Confirmed aligned MZmine target list"), activate = TRUE)
}

fpq_workflow_cancel_requested <- function(project, workflow_id)
  isTRUE(fpq_load_workflow_record(project, workflow_id)$cancellation_requested)

fpq_request_workflow_cancellation <- function(project, workflow_id) {
  record <- fpq_update_workflow_record(project, workflow_id,
    list(cancellation_requested = TRUE, status = "cancelling"))
  request_runs <- vapply(record$request_paths %||% list(), function(path)
    fpq_read_json(path)$run_id, character(1))
  persisted_runs <- vapply(Filter(function(run)
    grepl(workflow_id, run$run_id, fixed = TRUE), fpq_list_runs(project)),
    `[[`, "", "run_id")
  active <- rev(unique(c(unlist(record$child_run_ids %||% list()),
                         request_runs, persisted_runs)))
  for (run_id in active) {
    run <- tryCatch(fpq_load_run(project, run_id), error = function(e) NULL)
    if (!is.null(run) && run$status %in% c("starting", "running", "cancelling"))
      try(fpq_request_cancellation(project, run_id), silent = TRUE)
  }
  record
}

fpq_workflow_stage_summary <- function(project) {
  workflows <- fpq_list_workflow_records(project)
  if (!length(workflows)) return(NULL)
  workflows[[1]]
}

fpq_copy_workflow_acknowledgement <- function(project, workflow, run_id) {
  acknowledgement <- workflow$candidate_acknowledgement
  if (is.null(acknowledgement) || !isTRUE(acknowledgement$acknowledged))
    return(invisible(NULL))
  acknowledgement$run_id <- run_id
  acknowledgement$workflow_id <- workflow$workflow_id
  path <- file.path(project$root, "reports",
                    paste0(run_id, "-candidate-acknowledgement.json"))
  fpq_atomic_write_json(acknowledgement, path)
  invisible(path)
}

fpq_run_pvl_script07_and_presentation <- function(project, workflow, masters,
                                               repository_root,
                                               attempt_id = NULL) {
  suffix <- if (is.null(attempt_id) || !nzchar(attempt_id)) "" else
    paste0("-", gsub("[^A-Za-z0-9_-]+", "-", attempt_id))
  combined_output_name <- paste0("combined-export", suffix)
  request <- list(schema_version = "1.0.0",
    workflow_id = workflow$workflow_id, project_id = project$manifest$project_id,
    exposure_label = workflow$exposure,
    scientific_status = workflow$scientific_status,
    positive_master = list(path = masters$POS$path, sha256 = masters$POS$sha256),
    negative_master = list(path = masters$NEG$path, sha256 = masters$NEG$sha256),
    output_working_directory = file.path(workflow$output_folder,
                                         "scientific-output",
                                         combined_output_name),
    r_executable = file.path(R.home("bin"), "Rscript.exe"))
  request_path <- file.path(workflow$output_folder, "requests",
                            paste0("combined-publication-export",
                                   suffix, ".json"))
  fpq_atomic_write_json(request, request_path)
  report_path <- file.path(workflow$output_folder,
                           paste0("combined-export-report", suffix, ".json"))
  stdout <- file.path(workflow$output_folder,
                      paste0("combined-export-worker", suffix, ".stdout.log"))
  stderr <- file.path(workflow$output_folder,
                      paste0("combined-export-worker", suffix, ".stderr.log"))
  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python.exe")
  process <- processx::run(python,
    c(file.path(repository_root, "tools", "script07_adapter.py"),
      request_path, "--report", report_path),
    wd = repository_root, stdout = stdout, stderr = stderr, error_on_status = FALSE)
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  if (process$status != 0L || is.null(report) || !identical(report$status, "PASS"))
    fpq_stop("workflow.combined_export_failed",
      "Create Combined Publication Export failed; inspect the preserved run diagnostics.",
      "scientific")
  audit <- report$candidate_workbook$path
  final_dir <- file.path(workflow$output_folder, "results")
  final_path <- file.path(final_dir, paste0("PVL_", workflow$exposure,
    "_MALDI_MSI_Final_Results_TECHNICAL_VALIDATION_CANDIDATE.xlsx"))
  fpq_create_final_results_workbook(audit, final_path,
    current_source_files = c(masters$POS$path, masters$NEG$path, audit),
    exposure = workflow$exposure)
  validation <- fpq_validate_final_results_workbook(final_path)
  if (!identical(validation$status, "PASS"))
    fpq_stop("workflow.final_workbook_failed",
             paste(validation$errors, collapse = "; "), "validation")
  artifact <- fpq_register_artifact(project, list(
    artifact_id = paste0("art-", workflow$workflow_id, "-final-workbook"),
    artifact_type = "final_results_workbook",
    display_name = basename(final_path), path = normalizePath(final_path),
    project_id = project$manifest$project_id, dataset_id = NULL,
    ion_mode = NULL, exposure = workflow$exposure,
    producing_run_id = workflow$workflow_id,
    producing_stage_id = "create_combined_publication_export",
    sha256 = fpq_hash_file(final_path),
    file_size = unname(file.info(final_path)$size),
    validation_status = "PASS",
    scientific_profile_id = project$manifest$scientific_profile_id,
    scientific_core_version = project$manifest$scientific_core_version,
    parent_artifact_ids = lapply(masters, `[[`, "artifact_id"),
    notes = "Five-sheet presentation contract PASS; Technical Validation Candidate"),
    activate = TRUE)
  list(path = final_path, sha256 = artifact$sha256,
       artifact_id = artifact$artifact_id, audit_workbook = audit,
       validation = validation, report = report)
}


fpq_run_generic_export <- function(project, workflow, masters,
                                   repository_root, attempt_id = NULL) {
  suffix <- if (is.null(attempt_id) || !nzchar(attempt_id)) "" else
    paste0("-", fpq_generic_token(attempt_id, "resume"))
  request <- list(
    schema_version = "1.0.0",
    workflow_id = workflow$workflow_id,
    project_id = project$manifest$project_id,
    exposure_label = workflow$exposure,
    scientific_status = "technical_validation_candidate",
    display_metadata = workflow$display_metadata,
    masters = unname(lapply(names(masters), function(mode) list(
      ion_mode = mode,
      path = masters[[mode]]$path,
      sha256 = masters[[mode]]$sha256,
      artifact_id = masters[[mode]]$artifact_id
    ))),
    output_working_directory = file.path(
      workflow$output_folder, "scientific-output",
      paste0("generic-final-export", suffix))
  )
  request_path <- file.path(workflow$output_folder, "requests",
                            paste0("generic-publication-export", suffix, ".json"))
  report_path <- file.path(workflow$output_folder,
                           paste0("generic-publication-export-report", suffix, ".json"))
  stdout <- file.path(workflow$output_folder,
                      paste0("generic-publication-export", suffix, ".stdout.log"))
  stderr <- file.path(workflow$output_folder,
                      paste0("generic-publication-export", suffix, ".stderr.log"))
  fpq_atomic_write_json(request, request_path)
  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python.exe")
  process <- processx::run(python,
    c(file.path(repository_root, "tools", "generic_export_adapter.py"),
      request_path, "--report", report_path),
    wd = repository_root, stdout = stdout, stderr = stderr,
    error_on_status = FALSE)
  report <- if (file.exists(report_path)) fpq_read_json(report_path) else NULL
  if (process$status != 0L || is.null(report) ||
      !identical(report$status, "PASS"))
    fpq_stop("workflow.generic_export_failed",
      report$error %||%
        "Generic publication export failed; inspect preserved diagnostics.",
      "scientific")
  final_path <- normalizePath(report$final_workbook$path,
                              winslash = "/", mustWork = TRUE)
  final_hash <- fpq_hash_file(final_path)
  if (!identical(final_hash, report$final_workbook$sha256))
    fpq_stop("workflow.generic_export_hash",
      "Generic final workbook hash verification failed.", "scientific")
  artifact <- fpq_register_artifact(project, list(
    artifact_id = paste0("art-", workflow$workflow_id,
                         "-generic-final-workbook", suffix),
    artifact_type = "final_results_workbook",
    display_name = basename(final_path),
    path = final_path,
    project_id = project$manifest$project_id,
    dataset_id = NULL,
    ion_mode = NULL,
    exposure = workflow$exposure,
    producing_run_id = workflow$workflow_id,
    producing_stage_id = "create_combined_publication_export",
    sha256 = final_hash,
    file_size = unname(file.info(final_path)$size),
    validation_status = "PASS",
    scientific_profile_id =
      project$manifest$scientific_profile_id,
    scientific_core_version =
      project$manifest$scientific_core_version,
    parent_artifact_ids = lapply(masters, `[[`, "artifact_id"),
    notes = paste(
      "Generic two-group five-sheet workbook;",
      "dynamic sample, condition, mode, project, and exposure labels;",
      "Technical Validation Candidate"
    )), activate = TRUE)
  list(path = final_path, sha256 = artifact$sha256,
       artifact_id = artifact$artifact_id,
       audit_workbook = final_path,
       validation = list(status = "PASS", errors = character()),
       report = report)
}

fpq_run_script07_and_presentation <- function(project, workflow, masters,
                                               repository_root,
                                               attempt_id = NULL) {
  if (identical(workflow$execution_adapter %||% "",
                "generic_two_group"))
    return(fpq_run_generic_export(project, workflow, masters,
                                  repository_root, attempt_id))
  fpq_run_pvl_script07_and_presentation(project, workflow, masters,
                                        repository_root, attempt_id)
}

fpq_execute_full_workflow <- function(project, workflow_id,
                                      repository_root = fpq_repo_root()) {
  workflow <- fpq_load_workflow_record(project, workflow_id)
  resumable_stages <- c("summarizing", "annotating", "combined_export",
                        "final_workbook", "validating_workbook")
  if (identical(workflow$status, "failed") &&
      workflow$failure$stage %in% resumable_stages)
    return(fpq_resume_full_workflow_after_extraction(
      project, workflow_id, repository_root))
  if (!identical(workflow$status, "prepared"))
    fpq_stop("workflow.not_prepared",
             "Workflow is neither prepared nor safely resumable.")
  workflow <- fpq_update_workflow_record(project, workflow_id,
    list(status = "running", started_at = fpq_now()))
  fail <- function(error, stage) {
    try(fpq_set_workflow_stage(project, workflow_id, stage, "failed",
                               conditionMessage(error)), silent = TRUE)
    fpq_update_workflow_record(project, workflow_id, list(
      status = "failed", finished_at = fpq_now(),
      failure = list(stage = stage, message = conditionMessage(error),
                     class = class(error)[[1]])))
    fpq_log_event(project, "workflow.failed", run_id = workflow_id,
      details = list(stage = stage, message = conditionMessage(error)))
    stop(error)
  }
  current <- "preparing"
  tryCatch({
    fpq_set_workflow_stage(project, workflow_id, "preparing", "running")
    config <- fpq_load_project_configuration(project)
    modes <- unlist(workflow$modes)
    requests <- fpq_finalize_workflow_requests(project, workflow_id)
    fpq_set_workflow_stage(project, workflow_id, "preparing", "completed")
    if (fpq_workflow_cancel_requested(project, workflow_id))
      stop(structure(list(message = "Workflow cancelled before execution."),
                     class = c("fpq_workflow_cancelled", "error", "condition")))

    current <- "reading_msi"
    fpq_set_workflow_stage(project, workflow_id, current, "running")
    for (request in requests)
      for (sample in request$samples)
        if (!file.exists(sample$imzml_path) || !file.exists(sample$ibd_path))
          fpq_stop("workflow.raw_input_missing",
            paste("MSI input is missing for", sample$sample_id), "input")
    fpq_set_workflow_stage(project, workflow_id, current, "completed")

    extractions <- list()
    for (mode in modes) {
      current <- if (mode == "POS") "extracting_pos" else "extracting_neg"
      fpq_set_workflow_stage(project, workflow_id, current, "running")
      request <- requests[[mode]]
      fpq_ensure_extraction_target_artifact(project, request)
      fpq_copy_workflow_acknowledgement(project, workflow, request$run_id)
      result <- fpq_dispatch_adapter(project,
        workflow$request_paths[[mode]], repository_root)
      extractions[[mode]] <- result$artifact
      fpq_set_workflow_stage(project, workflow_id, current, "completed")
    }
    current <- "normalizing"
    fpq_set_workflow_stage(project, workflow_id, current, "running",
      "Spectrum-sum normalization completed within each extraction stage.")
    fpq_set_workflow_stage(project, workflow_id, current, "completed")

    current <- "summarizing"
    fpq_set_workflow_stage(project, workflow_id, current, "running")
    summaries <- list()
    for (mode in modes) {
      request <- fpq_build_downstream_request(project, workflow,
        requests[[mode]], extractions[[mode]], "summarize_replicate_readouts")
      path <- fpq_request_path(project, workflow_id, request)
      fpq_copy_workflow_acknowledgement(project, workflow, request$run_id)
      result <- fpq_dispatch_script03(project, path, repository_root)
      summaries[[mode]] <- result$artifact
    }
    fpq_set_workflow_stage(project, workflow_id, current, "completed")

    current <- "annotating"
    fpq_set_workflow_stage(project, workflow_id, current, "running")
    masters <- list()
    for (mode in modes) {
      annotation <- fpq_register_mzmine_artifact(project, workflow,
                                                  requests[[mode]], config)
      request <- fpq_build_downstream_request(project, workflow,
        requests[[mode]], summaries[[mode]], "integrate_mzmine_annotations",
        annotation)
      path <- fpq_request_path(project, workflow_id, request)
      fpq_copy_workflow_acknowledgement(project, workflow, request$run_id)
      result <- fpq_dispatch_script04(project, path, repository_root)
      masters[[mode]] <- result$artifact
    }
    fpq_set_workflow_stage(project, workflow_id, current, "completed")

    if (!length(masters))
      fpq_stop("workflow.master_required",
               "At least one validated annotated master is required.",
               "input")
    if (!workflow$execution_adapter %in% c(
          "generic_two_group", "generic_quantify_only",
          "generic_multi_group") &&
        !all(c("POS", "NEG") %in% names(masters)))
      fpq_stop("workflow.combined_modes_required",
               "The PVL publication export requires validated Positive and Negative masters.",
               "input")
    current <- "combined_export"
    fpq_set_workflow_stage(project, workflow_id, current, "running")
    current <- "final_workbook"
    fpq_set_workflow_stage(project, workflow_id, "final_workbook", "running")
    result <- fpq_run_script07_and_presentation(project, workflow, masters,
                                                repository_root)
    fpq_set_workflow_stage(project, workflow_id, "combined_export", "completed")
    fpq_set_workflow_stage(project, workflow_id, "final_workbook", "completed")
    current <- "validating_workbook"
    fpq_set_workflow_stage(project, workflow_id, current, "running")
    if (!identical(result$validation$status, "PASS"))
      fpq_stop("workflow.final_validation_failed",
               "Final workbook validation failed.", "validation")
    fpq_set_workflow_stage(project, workflow_id, current, "completed")
    completed <- fpq_update_workflow_record(project, workflow_id, list(
      status = "completed", finished_at = fpq_now(),
      current_stage = "validating_workbook",
      final_workbook = result[c("path", "sha256", "artifact_id",
                                "audit_workbook")]))
    fpq_log_event(project, "workflow.completed", run_id = workflow_id,
                  details = list(final_workbook = result$path))
    completed
  }, fpq_workflow_cancelled = function(e) {
    for (stage in fpq_load_workflow_record(project, workflow_id)$stages)
      if (identical(stage$status, "running"))
        fpq_set_workflow_stage(project, workflow_id, stage$stage_id,
                               "cancelled", conditionMessage(e))
    fpq_update_workflow_record(project, workflow_id,
      list(status = "cancelled", finished_at = fpq_now()))
  }, error = function(e) fail(e, current))
}

fpq_retry_workflow <- function(project, workflow_id, config, validations,
                               repository_root = fpq_repo_root(),
                               hash_raw_inputs = TRUE) {
  prior <- fpq_load_workflow_record(project, workflow_id)
  if (!prior$status %in% c("failed", "cancelled"))
    fpq_stop("workflow.retry_status",
             "Only failed or cancelled workflows can be retried.")
  prepared <- fpq_prepare_full_workflow(project, config, validations,
    repository_root, hash_raw_inputs)
  prepared$record <- fpq_update_workflow_record(project,
    prepared$record$workflow_id, list(retry_of = workflow_id))
  prepared
}

fpq_resume_full_workflow_after_extraction <- function(
    project, workflow_id, repository_root = fpq_repo_root()) {
  workflow <- fpq_load_workflow_record(project, workflow_id)
  resumable_stages <- c("summarizing", "annotating", "combined_export",
                        "final_workbook", "validating_workbook")
  if (!identical(workflow$status, "failed") ||
      !workflow$failure$stage %in% resumable_stages)
    fpq_stop("workflow.resume_scope",
      "Safe checkpoint reuse is available only for a failed downstream stage.")

  config <- fpq_load_project_configuration(project)
  if (!identical(fpq_hash_object(config), workflow$configuration_hash))
    fpq_stop("workflow.resume_configuration_changed",
      paste("The project configuration changed after the failed workflow.",
            "Start a new workflow instead of reusing old checkpoints."),
      "validation")

  failed_stage <- workflow$failure$stage
  requests <- lapply(workflow$request_paths, fpq_read_json)
  modes <- unlist(workflow$modes)
  attempt_id <- fpq_id("resume")

  verified_checkpoint <- function(type, mode, required = TRUE) {
    request <- requests[[mode]]
    all_artifacts <- fpq_list_artifacts(project)
    matching <- Filter(function(x)
      identical(x$artifact_type, type) &&
        identical(x$dataset_id, request$dataset_id) &&
        identical(x$ion_mode, mode) &&
        identical(x$validation_status, "PASS") &&
        grepl(workflow_id, x$producing_run_id %||% "", fixed = TRUE) &&
        file.exists(x$path) &&
        identical(fpq_hash_file(x$path), x$sha256),
      all_artifacts)

    active <- Filter(function(x) identical(x$active_status, "active"), matching)
    if (length(active) > 1L)
      fpq_stop("workflow.resume_ambiguous",
        paste("Multiple active", mode, type, "artifacts were found."),
        "scientific")
    if (length(active) == 1L)
      return(fpq_verify_artifact(project, active[[1]]$artifact_id))

    if (!length(matching)) {
      if (required)
        fpq_stop("workflow.resume_artifact",
          paste("No hash-verified", mode, type, "checkpoint is available."),
          "scientific")
      return(NULL)
    }

    other_active <- Filter(function(x)
      identical(x$artifact_type, type) &&
        identical(x$dataset_id, request$dataset_id) &&
        identical(x$ion_mode, mode) &&
        identical(x$active_status, "active"),
      all_artifacts)
    if (length(other_active))
      fpq_stop("workflow.resume_superseded",
        paste("A newer active", mode, type,
              "artifact exists; the failed workflow will not supersede it."),
        "scientific")
    if (length(matching) != 1L)
      fpq_stop("workflow.resume_ambiguous",
        paste("Multiple reusable", mode, type, "artifacts were found."),
        "scientific")

    fpq_update_artifact_status(project, matching[[1]]$artifact_id, "active")
    fpq_verify_artifact(project, matching[[1]]$artifact_id)
  }

  resume_request <- function(extraction_request, upstream, stage_id,
                             annotation = NULL) {
    request <- fpq_build_downstream_request(project, workflow,
      extraction_request, upstream, stage_id, annotation)
    request$run_id <- paste0(request$run_id, "-", attempt_id)
    request$request_id <- sub("^run-", "req-", request$run_id)
    request$output_working_directory <- paste0(
      request$output_working_directory, "-", attempt_id)
    request
  }

  for (request in requests)
    fpq_ensure_extraction_target_artifact(project, request)

  extractions <- lapply(modes, function(mode)
    verified_checkpoint("pixel_intensity_table", mode, required = TRUE))
  names(extractions) <- modes

  workflow <- fpq_update_workflow_record(project, workflow_id,
    list(status = "running", failure = NULL, finished_at = NULL,
         cancellation_requested = FALSE,
         resumed_at = fpq_now(), resume_attempt_id = attempt_id,
         reuse_policy = paste(
           "hash-verified same-workflow checkpoints;",
           "missing downstream checkpoints rerun in isolated workspaces")))

  fail_resume <- function(error, stage) {
    try(fpq_set_workflow_stage(project, workflow_id, stage, "failed",
                               conditionMessage(error)), silent = TRUE)
    fpq_update_workflow_record(project, workflow_id, list(
      status = "failed", finished_at = fpq_now(),
      failure = list(stage = stage, message = conditionMessage(error),
                     class = class(error)[[1]])))
    fpq_log_event(project, "workflow.resume_failed", run_id = workflow_id,
      details = list(stage = stage, message = conditionMessage(error),
                     resume_attempt_id = attempt_id))
    stop(error)
  }

  current <- failed_stage %||% "summarizing"
  tryCatch({
    current <- "summarizing"
    summaries <- list()
    summary_reused <- character()
    summary_rerun <- character()
    for (mode in modes) {
      existing <- verified_checkpoint("replicate_readout_summary", mode,
                                      required = FALSE)
      if (!is.null(existing)) {
        summaries[[mode]] <- existing
        summary_reused <- c(summary_reused, mode)
      } else {
        current <- "summarizing"
        fpq_set_workflow_stage(project, workflow_id, current, "running",
          paste("Resuming with hash-verified extraction checkpoints;",
                "rerunning only missing summaries."))
        request <- resume_request(requests[[mode]], extractions[[mode]],
                                  "summarize_replicate_readouts")
        path <- fpq_request_path(project, workflow_id, request)
        fpq_copy_workflow_acknowledgement(project, workflow, request$run_id)
        summaries[[mode]] <- fpq_dispatch_script03(
          project, path, repository_root)$artifact
        summary_rerun <- c(summary_rerun, mode)
      }
    }
    fpq_set_workflow_stage(project, workflow_id, "summarizing", "completed",
      paste("Validated summaries ready.",
            if (length(summary_reused))
              paste("Reused:", paste(summary_reused, collapse = ", ")) else "",
            if (length(summary_rerun))
              paste("Reran:", paste(summary_rerun, collapse = ", ")) else ""))

    current <- "annotating"
    masters <- list()
    master_reused <- character()
    master_rerun <- character()
    for (mode in modes) {
      existing <- verified_checkpoint("annotated_three_readout_master", mode,
                                      required = FALSE)
      if (!is.null(existing)) {
        masters[[mode]] <- existing
        master_reused <- c(master_reused, mode)
      } else {
        current <- "annotating"
        fpq_set_workflow_stage(project, workflow_id, current, "running",
          paste("Using validated summary checkpoints;",
                "rerunning only missing annotation integrations."))
        annotation <- fpq_register_mzmine_artifact(
          project, workflow, requests[[mode]], config)
        request <- resume_request(requests[[mode]], summaries[[mode]],
          "integrate_mzmine_annotations", annotation)
        path <- fpq_request_path(project, workflow_id, request)
        fpq_copy_workflow_acknowledgement(project, workflow, request$run_id)
        masters[[mode]] <- fpq_dispatch_script04(
          project, path, repository_root)$artifact
        master_rerun <- c(master_rerun, mode)
      }
    }
    fpq_set_workflow_stage(project, workflow_id, "annotating", "completed",
      paste("Validated annotated masters ready.",
            if (length(master_reused))
              paste("Reused:", paste(master_reused, collapse = ", ")) else "",
            if (length(master_rerun))
              paste("Reran:", paste(master_rerun, collapse = ", ")) else ""))

    if (!length(masters))
      fpq_stop("workflow.master_required",
        "At least one validated annotated master is required.", "input")
    if (!workflow$execution_adapter %in% c(
          "generic_two_group", "generic_quantify_only",
          "generic_multi_group") &&
        !all(c("POS", "NEG") %in% names(masters)))
      fpq_stop("workflow.combined_modes_required",
        "The PVL publication export requires validated Positive and Negative masters.",
        "input")

    current <- "combined_export"
    fpq_set_workflow_stage(project, workflow_id, current, "running",
      "Creating export from hash-verified annotated-master checkpoints.")
    fpq_set_workflow_stage(project, workflow_id, "final_workbook", "running")
    result <- fpq_run_script07_and_presentation(
      project, workflow, masters, repository_root, attempt_id = attempt_id)
    fpq_set_workflow_stage(project, workflow_id, "combined_export", "completed")
    fpq_set_workflow_stage(project, workflow_id, "final_workbook", "completed")

    current <- "validating_workbook"
    fpq_set_workflow_stage(project, workflow_id, current, "running")
    if (!identical(result$validation$status, "PASS"))
      fpq_stop("workflow.final_validation_failed",
               "Final workbook validation failed.", "validation")
    fpq_set_workflow_stage(project, workflow_id, current, "completed")

    completed <- fpq_update_workflow_record(project, workflow_id, list(
      status = "completed", finished_at = fpq_now(),
      current_stage = "validating_workbook",
      final_workbook = result[c("path", "sha256", "artifact_id",
                                "audit_workbook")]))
    fpq_log_event(project, "workflow.resumed_completed", run_id = workflow_id,
      details = list(final_workbook = result$path,
                     resume_attempt_id = attempt_id))
    completed
  }, error = function(e) fail_resume(e, current))
}

