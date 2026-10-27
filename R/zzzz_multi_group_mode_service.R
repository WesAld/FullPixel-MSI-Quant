# Compare Multiple Groups extension for FullPixel MSI Quant.
# One configured reference group is compared descriptively with every selected
# non-reference group. Existing Quantify Samples Only and Compare Two Groups
# branches remain unchanged.

fpq_multi_group_analysis_type <- function(config) {
  config$analysis_type %||%
    config$analysis_design$analysis_type %||%
    "quantify_only"
}

fpq_multi_group_token <- function(value, fallback = "Group") {
  token <- gsub("[^A-Za-z0-9]+", "_", trimws(as.character(value %||% "")))
  token <- gsub("(^_+|_+$)", "", token)
  if (!nzchar(token)) fallback else substr(token, 1L, 64L)
}

if (!exists("fpq_configuration_from_detected_rows_before_multi",
            inherits = FALSE))
  fpq_configuration_from_detected_rows_before_multi <-
    fpq_configuration_from_detected_rows
if (!exists("fpq_validate_profile_compatibility_before_multi",
            inherits = FALSE))
  fpq_validate_profile_compatibility_before_multi <-
    fpq_validate_profile_compatibility
if (!exists("fpq_scientific_status_model_before_multi",
            inherits = FALSE))
  fpq_scientific_status_model_before_multi <-
    fpq_scientific_status_model
if (!exists("fpq_dataset_registration_status_before_multi",
            inherits = FALSE))
  fpq_dataset_registration_status_before_multi <-
    fpq_dataset_registration_status
if (!exists("fpq_internal_mode_samples_before_multi",
            inherits = FALSE))
  fpq_internal_mode_samples_before_multi <-
    fpq_internal_mode_samples
if (!exists("fpq_prepare_mode_extraction_request_before_multi",
            inherits = FALSE))
  fpq_prepare_mode_extraction_request_before_multi <-
    fpq_prepare_mode_extraction_request
if (!exists("fpq_prepare_full_workflow_before_multi",
            inherits = FALSE))
  fpq_prepare_full_workflow_before_multi <-
    fpq_prepare_full_workflow
if (!exists("fpq_run_script07_and_presentation_before_multi",
            inherits = FALSE))
  fpq_run_script07_and_presentation_before_multi <-
    fpq_run_script07_and_presentation
if (!exists("fpq_validate_published_final_results_workbook_before_multi",
            inherits = FALSE))
  fpq_validate_published_final_results_workbook_before_multi <-
    fpq_validate_published_final_results_workbook

fpq_configuration_from_detected_rows <- function(config, rows) {
  result <- fpq_configuration_from_detected_rows_before_multi(config, rows)
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(result)

  valid_condition_ids <- vapply(
    result$conditions %||% list(),
    function(x) x$condition_id %||% "",
    character(1L)
  )
  result$comparisons <- Filter(function(x) {
    (x$reference_condition_id %||% "") %in% valid_condition_ids &&
      (x$comparison_condition_id %||% "") %in% valid_condition_ids &&
      !identical(x$reference_condition_id,
                 x$comparison_condition_id)
  }, result$comparisons %||% list())
  result
}

fpq_generic_multi_group_design <- function(config) {
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(list(
      valid = FALSE,
      message = "The saved analysis type is not Compare multiple groups."
    ))

  conditions <- config$conditions %||% list()
  if (length(conditions) < 3L)
    return(list(
      valid = FALSE,
      message = "Compare multiple groups requires at least three conditions."
    ))

  condition_ids <- vapply(
    conditions,
    function(x) trimws(as.character(x$condition_id %||% "")),
    character(1L)
  )
  if (any(!nzchar(condition_ids)) || anyDuplicated(condition_ids))
    return(list(
      valid = FALSE,
      message = "Condition identifiers must be nonblank and unique."
    ))

  comparisons <- Filter(
    function(x) isTRUE(x$enabled %||% TRUE),
    config$comparisons %||% list()
  )
  if (length(comparisons) < 2L)
    return(list(
      valid = FALSE,
      message = paste(
        "Compare multiple groups requires at least two enabled",
        "reference-versus-treatment comparisons."
      )
    ))

  reference_ids <- unique(vapply(
    comparisons,
    function(x) trimws(as.character(
      x$reference_condition_id %||% "")),
    character(1L)
  ))
  if (length(reference_ids) != 1L ||
      !reference_ids[[1L]] %in% condition_ids)
    return(list(
      valid = FALSE,
      message = paste(
        "Every multi-group comparison must use the same configured",
        "reference condition."
      )
    ))
  reference_id <- reference_ids[[1L]]

  treatment_ids <- vapply(
    comparisons,
    function(x) trimws(as.character(
      x$comparison_condition_id %||% "")),
    character(1L)
  )
  if (any(!treatment_ids %in% condition_ids) ||
      any(treatment_ids == reference_id) ||
      anyDuplicated(treatment_ids))
    return(list(
      valid = FALSE,
      message = paste(
        "Each non-reference condition must appear once as a distinct",
        "comparison condition."
      )
    ))

  expected_treatments <- setdiff(condition_ids, reference_id)
  if (!setequal(treatment_ids, expected_treatments))
    return(list(
      valid = FALSE,
      message = paste(
        "Confirm one comparison for every non-reference condition.",
        "No configured group may be omitted."
      )
    ))

  modes <- fpq_active_project_modes(config)
  if (!length(modes) || any(!modes %in% c("POS", "NEG")))
    return(list(
      valid = FALSE,
      message = "Select Positive, Negative, or both ion modes."
    ))

  samples <- Filter(
    function(x) identical(
      x$inclusion_status %||% "included",
      "included"
    ),
    config$samples %||% list()
  )
  if (!length(samples))
    return(list(
      valid = FALSE,
      message = "Confirm the MSI sample records."
    ))

  sample_ids <- vapply(
    samples,
    function(x) trimws(as.character(x$sample_id %||% "")),
    character(1L)
  )
  if (any(!nzchar(sample_ids)) || anyDuplicated(sample_ids))
    return(list(
      valid = FALSE,
      message = "Sample identifiers must be nonblank and unique."
    ))

  branch_details <- list()
  for (mode in modes) {
    mode_samples <- Filter(
      function(x) identical(
        toupper(x$ion_mode %||% ""),
        mode
      ),
      samples
    )
    counts <- integer(length(condition_ids))
    names(counts) <- condition_ids

    for (condition_id in condition_ids) {
      group_samples <- Filter(
        function(x) identical(x$condition_id, condition_id),
        mode_samples
      )
      if (length(group_samples) < 2L)
        return(list(
          valid = FALSE,
          message = paste(
            mode, "requires at least two biological replicates in",
            condition_id, "."
          )
        ))

      replicates <- vapply(
        group_samples,
        function(x) trimws(as.character(
          x$replicate_id %||% "")),
        character(1L)
      )
      if (any(!nzchar(replicates)))
        return(list(
          valid = FALSE,
          message = paste(
            mode, condition_id,
            "contains a blank replicate identifier."
          )
        ))
      if (anyDuplicated(replicates))
        return(list(
          valid = FALSE,
          message = paste(
            mode, condition_id,
            "contains duplicate replicate identifiers."
          )
        ))
      counts[[condition_id]] <- length(group_samples)
    }

    unexpected <- setdiff(
      unique(vapply(
        mode_samples,
        function(x) x$condition_id %||% "",
        character(1L)
      )),
      condition_ids
    )
    if (length(unexpected))
      return(list(
        valid = FALSE,
        message = paste(
          mode, "contains samples assigned to unknown conditions:",
          paste(unexpected, collapse = ", ")
        )
      ))

    branch_details[[mode]] <- list(
      replicate_counts = as.list(counts),
      total_samples = sum(counts)
    )
  }

  condition_lookup <- setNames(conditions, condition_ids)
  ordered_treatments <- treatment_ids[
    order(vapply(treatment_ids, function(id)
      condition_lookup[[id]]$display_order %||%
        match(id, condition_ids), numeric(1L)))
  ]
  ordered_comparisons <- lapply(ordered_treatments, function(id) {
    comparisons[[which(treatment_ids == id)[[1L]]]]
  })

  list(
    valid = TRUE,
    modes = modes,
    conditions = conditions,
    condition_ids = condition_ids,
    reference_condition_id = reference_id,
    treatment_condition_ids = ordered_treatments,
    comparisons = ordered_comparisons,
    branches = branch_details,
    sample_count = length(samples)
  )
}

fpq_validate_profile_compatibility <- function(config, profile_id) {
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(fpq_validate_profile_compatibility_before_multi(
      config, profile_id))

  structural <- fpq_validate_structural(config)
  if (!identical(structural$classification,
                 "structurally_valid"))
    return(structural)

  supported <- c(
    "generic-two-group-v1",
    "pvl-fullpixel-reference-v1",
    "fullpixel-40um-1600px-v1"
  )
  if (!profile_id %in% supported)
    return(list(
      classification = "profile_unsupported",
      records = list(fpq_validation_record(
        "profile.multi_group_unsupported",
        "FAIL",
        paste(
          "The selected profile has no installed multi-group",
          "execution adapter."
        ),
        "CRITICAL",
        TRUE
      ))
    ))

  design <- fpq_generic_multi_group_design(config)
  if (!isTRUE(design$valid))
    return(list(
      classification = "profile_incompatible",
      records = list(fpq_validation_record(
        "profile.multi_group_design",
        "FAIL",
        design$message,
        "CRITICAL",
        TRUE,
        consequence = "Multi-group execution is blocked.",
        remediation = paste(
          "Confirm one reference group, every non-reference group,",
          "and at least two replicates per group."
        )
      ))
    ))

  list(
    classification = "profile_candidate_compatible",
    scientific_status = list(
      overall = "technical_validation_candidate",
      branches = setNames(
        as.list(rep(
          "technical_validation_candidate",
          length(design$modes)
        )),
        design$modes
      ),
      execution_allowed = TRUE,
      acknowledgement_required = TRUE,
      independent_positive_review =
        if ("POS" %in% design$modes) "pending"
        else "not_applicable",
      message = paste(
        "Multi-group reference-versus-treatment execution is",
        "technically validated; independent dataset-specific",
        "scientific review remains pending."
      )
    ),
    records = list(fpq_validation_record(
      "profile.multi_group_compatibility",
      "WARNING",
      paste(
        "Multi-group design accepted:",
        length(design$condition_ids), "conditions,",
        length(design$comparisons), "reference comparisons,",
        paste(design$modes, collapse = "+"), "ion mode(s)."
      ),
      "WARNING",
      FALSE,
      consequence = paste(
        "Extraction, frozen Script 03 pairwise summaries,",
        "annotation integration, and multi-group workbook export",
        "are allowed."
      ),
      remediation = paste(
        "Retain the Technical Validation Candidate label until an",
        "independent dataset-specific validation is approved."
      )
    ))
  )
}

fpq_scientific_status_model <- function(
    config,
    repository_root = fpq_repo_root(),
    evidence = NULL) {
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(fpq_scientific_status_model_before_multi(
      config, repository_root, evidence))

  design <- fpq_generic_multi_group_design(config)
  if (!isTRUE(design$valid))
    return(list(
      overall = "unsupported",
      branches = list(),
      execution_allowed = FALSE,
      acknowledgement_required = FALSE,
      independent_positive_review = "not_applicable",
      message = design$message
    ))

  list(
    overall = "technical_validation_candidate",
    branches = setNames(
      as.list(rep(
        "technical_validation_candidate",
        length(design$modes)
      )),
      design$modes
    ),
    execution_allowed = TRUE,
    acknowledgement_required = TRUE,
    independent_positive_review =
      if ("POS" %in% design$modes) "pending"
      else "not_applicable",
    message = paste(
      "Multi-group reference-versus-treatment FullPixel",
      "quantification is technically allowed."
    )
  )
}

fpq_dataset_registration_status <- function(config) {
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(fpq_dataset_registration_status_before_multi(config))

  samples <- Filter(
    function(x) identical(
      x$inclusion_status %||% "included",
      "included"
    ),
    config$samples %||% list()
  )
  mzmine <- config$mzmine_registrations %||% list()
  modes <- fpq_active_project_modes(config)
  missing <- character()

  if (!length(samples))
    missing <- c(missing, "Confirm MSI sample records.")
  if (length(samples) && any(!vapply(
      samples,
      function(x) file.exists(x$imzml_path %||% "") &&
        file.exists(x$ibd_path %||% ""),
      logical(1L))))
    missing <- c(missing, "Restore missing imzML/IBD files.")

  for (mode in modes) {
    if (is.null(mzmine[[mode]]) ||
        !identical(mzmine[[mode]]$status, "PASS") ||
        !file.exists(mzmine[[mode]]$path %||% ""))
      missing <- c(
        missing,
        paste(
          "Confirm the",
          if (mode == "POS") "Positive" else "Negative",
          "MZmine column mapping."
        )
      )
  }

  design <- fpq_generic_multi_group_design(config)
  if (!isTRUE(design$valid))
    missing <- c(missing, design$message)

  biological <- unique(vapply(
    samples,
    function(x) x$biological_sample %||%
      x$sample_label %||%
      x$sample_id,
    character(1L)
  ))

  list(
    status = if (length(missing))
      "NOT_REGISTERED" else "REGISTERED",
    blocking = length(missing) > 0L,
    missing = unique(missing),
    summary = list(
      msi_datasets = length(samples),
      mzmine_feature_lists = length(mzmine),
      biological_samples = length(biological),
      conditions = length(config$conditions %||% list()),
      comparisons = length(config$comparisons %||% list())
    )
  )
}

fpq_internal_mode_samples <- function(config, mode) {
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(fpq_internal_mode_samples_before_multi(config, mode))

  design <- fpq_generic_multi_group_design(config)
  if (!isTRUE(design$valid))
    fpq_stop(
      "workflow.multi_group_design",
      design$message,
      "scientific"
    )

  mode <- toupper(mode)
  samples <- Filter(
    function(x) identical(
      toupper(x$ion_mode %||% ""),
      mode
    ) && identical(
      x$inclusion_status %||% "included",
      "included"
    ),
    config$samples %||% list()
  )

  condition_ids <- c(
    design$reference_condition_id,
    design$treatment_condition_ids
  )
  condition_lookup <- setNames(
    config$conditions,
    vapply(config$conditions, function(x)
      x$condition_id, character(1L))
  )
  internal_conditions <- setNames(
    paste0(
      "G",
      formatC(
        seq_along(condition_ids),
        width = 3L,
        flag = "0"
      )
    ),
    condition_ids
  )

  result <- list()
  for (condition_id in condition_ids) {
    group_rows <- Filter(
      function(x) identical(x$condition_id, condition_id),
      samples
    )
    group_rows <- group_rows[order(vapply(
      group_rows,
      function(x) trimws(as.character(
        x$replicate_id %||% "")),
      character(1L)
    ))]
    internal_condition <- internal_conditions[[condition_id]]

    for (index in seq_along(group_rows)) {
      row <- group_rows[[index]]
      original_replicate <- trimws(as.character(
        row$replicate_id %||% ""))
      internal_replicate <- paste0("R", index)
      result[[length(result) + 1L]] <- list(
        sample_id = paste0(
          internal_condition,
          "_",
          internal_replicate
        ),
        original_sample_id = row$sample_id,
        display_sample_id = row$biological_sample %||%
          row$sample_label %||%
          row$sample_id,
        original_condition_id = condition_id,
        display_condition =
          condition_lookup[[condition_id]]$condition_label %||%
            condition_id,
        condition_id = internal_condition,
        replicate_id = internal_replicate,
        original_replicate_id = original_replicate,
        analytical_role =
          if (identical(condition_id,
                       design$reference_condition_id))
            "reference" else "comparison",
        imzml_path = normalizePath(
          row$imzml_path,
          mustWork = TRUE,
          winslash = "/"
        ),
        ibd_path = normalizePath(
          row$ibd_path,
          mustWork = TRUE,
          winslash = "/"
        )
      )
    }
  }

  internal_ids <- vapply(
    result,
    function(x) x$sample_id,
    character(1L)
  )
  if (anyDuplicated(internal_ids))
    fpq_stop(
      "workflow.multi_group_sample_mapping",
      "Internal multi-group sample identifiers are not unique.",
      "scientific"
    )

  unname(result)
}

fpq_prepare_mode_extraction_request <- function(
    project,
    config,
    mode,
    workflow_id,
    repository_root = fpq_repo_root(),
    hash_raw_inputs = TRUE) {
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(fpq_prepare_mode_extraction_request_before_multi(
      project,
      config,
      mode,
      workflow_id,
      repository_root,
      hash_raw_inputs
    ))

  design <- fpq_generic_multi_group_design(config)
  if (!isTRUE(design$valid))
    fpq_stop(
      "workflow.multi_group_design",
      design$message,
      "scientific"
    )

  mode <- toupper(mode)
  mzmine <- config$mzmine_registrations[[mode]]
  if (is.null(mzmine) ||
      !identical(mzmine$status, "PASS"))
    fpq_stop(
      "workflow.mzmine_missing",
      paste("Confirm the", mode, "MZmine mapping."),
      "input"
    )

  samples <- fpq_internal_mode_samples(config, mode)
  token <- paste(workflow_id, tolower(mode), sep = "-")
  request <- fpq_read_json(file.path(
    repository_root,
    "config",
    "examples",
    "pvl_single_negative_extraction.execution_request.json"
  ))
  exposure <- fpq_project_exposure_label(project, config)
  acquisition <- fpq_mode_acquisition_metadata(
    project, config, mode)

  condition_order <- unique(vapply(
    samples,
    function(x) x$original_condition_id,
    character(1L)
  ))
  condition_internal <- setNames(vapply(
    condition_order,
    function(id) {
      rows <- Filter(
        function(x) identical(
          x$original_condition_id, id),
        samples
      )
      rows[[1L]]$condition_id
    },
    character(1L)
  ), condition_order)
  condition_display <- setNames(vapply(
    condition_order,
    function(id) {
      rows <- Filter(
        function(x) identical(
          x$original_condition_id, id),
        samples
      )
      rows[[1L]]$display_condition
    },
    character(1L)
  ), condition_order)

  request$request_id <- paste0(
    "req-", token, "-extract")
  request$run_id <- paste0(
    "run-", token, "-extract")
  request$project_id <- project$manifest$project_id
  request$dataset_id <- fpq_workflow_mode_dataset_id(
    project, exposure, mode, FALSE)
  request$stage_id <- "extract_pixel_intensities"
  request$analysis_type <- "compare_multiple_groups"
  request$ion_mode <- mode
  request$exposure_label <- exposure
  request$execution_adapter <- "generic_multi_group"
  request$scientific_profile <- list(
    profile_id = project$manifest$scientific_profile_id,
    profile_version =
      project$manifest$scientific_profile_version,
    reference_status = "technical_validation_candidate"
  )
  request$registered_inputs$raw_data_root <-
    fpq_common_parent(unlist(lapply(
      samples,
      function(x) c(x$imzml_path, x$ibd_path)
    )))
  request$registered_inputs$raw_file_hash_registry <-
    if (hash_raw_inputs)
      fpq_write_raw_hash_registry(
        project, workflow_id, mode, samples)
    else
      list(
        path = "pending",
        sha256 = paste(rep("0", 64), collapse = "")
      )
  request$registered_inputs$aligned_target_table <- list(
    artifact_id = paste0(
      "art-", token, "-mzmine"),
    path = normalizePath(
      mzmine$path, winslash = "/"),
    sha256 = fpq_hash_file(mzmine$path)
  )
  request$output_working_directory <- file.path(
    project$root,
    "runs",
    workflow_id,
    "scientific-output",
    tolower(mode),
    "extract"
  )

  request$conditions <- unname(lapply(
    condition_order,
    function(id) list(
      condition_id = condition_internal[[id]],
      analytical_role =
        if (identical(id,
                     design$reference_condition_id))
          "reference" else "comparison",
      display_condition_id = id,
      display_label = condition_display[[id]],
      display_token = fpq_multi_group_token(
        condition_display[[id]], id)
    )
  ))
  request$samples <- unname(lapply(
    samples,
    function(x) x[c(
      "sample_id",
      "condition_id",
      "replicate_id",
      "imzml_path",
      "ibd_path"
    )]
  ))
  request$replicates <- unname(lapply(
    samples,
    function(x) x[c(
      "condition_id",
      "replicate_id"
    )]
  ))
  request$internal_role_mappings <- unname(lapply(
    samples,
    function(x) list(
      user_facing_value = x$original_sample_id,
      user_facing_label = x$display_sample_id,
      original_condition_id = x$original_condition_id,
      original_replicate_id = x$original_replicate_id,
      internal_value = x$sample_id,
      internal_condition_id = x$condition_id,
      ion_mode = mode,
      reason = "Generic multi-group canonical sample mapping",
      compatibility_rule =
        "one reference; at least two treatment groups; >=2 replicates per group",
      provenance_requirement = "required"
    )
  ))

  request$comparison <- NULL
  request$comparisons <- unname(lapply(
    design$comparisons,
    function(comparison) {
      reference_id <- comparison$reference_condition_id
      treatment_id <- comparison$comparison_condition_id
      list(
        comparison_id = comparison$comparison_id,
        reference_condition_id =
          condition_internal[[reference_id]],
        comparison_condition_id =
          condition_internal[[treatment_id]],
        reference_display_id = reference_id,
        comparison_display_id = treatment_id,
        reference_display_label =
          condition_display[[reference_id]],
        comparison_display_label =
          condition_display[[treatment_id]],
        reference_token = fpq_multi_group_token(
          condition_display[[reference_id]],
          reference_id
        ),
        comparison_token = fpq_multi_group_token(
          condition_display[[treatment_id]],
          treatment_id
        )
      )
    }
  ))
  request$column_mappings <- list(
    target_mz = mzmine$mappings$mz,
    feature_id = mzmine$mappings$feature_id
  )
  request$acquisition_metadata <-
    acquisition[c(
      "matrix",
      "pixel_size_um",
      "full_pixel"
    )]
  request$pixel_metadata$expected_coordinate_count <-
    acquisition$expected_coordinate_count
  request$pixel_metadata$pixel_geometry_policy <-
    acquisition$pixel_geometry_policy
  request$display_metadata <- list(
    project_name = project$manifest$project_name,
    dataset_prefix = fpq_generic_token(
      project$manifest$project_name,
      "Project"
    ),
    exposure_label = exposure,
    analysis_type = "compare_multiple_groups",
    reference_condition =
      condition_display[[design$reference_condition_id]],
    condition_mappings = request$conditions,
    comparisons = request$comparisons,
    sample_mappings = request$internal_role_mappings
  )
  request$primary_normalization_method <- fpq_normalization_method(config)
  request$approved_scientific_configuration$primary_normalization_method <-
    request$primary_normalization_method
  request$display_metadata$normalization <- list(
    application_version = "3.0.0-dev",
    primary_normalization_method = request$primary_normalization_method,
    normalization_scale = request$approved_scientific_configuration$normalization_scale,
    normalization_definition_version = fpq_normalization_definition_version())
  fpq_apply_mass_tolerance_to_request(request, config)
}

fpq_prepare_full_workflow <- function(
    project,
    config,
    validations,
    repository_root = fpq_repo_root(),
    hash_raw_inputs = TRUE) {
  prepared <- fpq_prepare_full_workflow_before_multi(
    project,
    config,
    validations,
    repository_root,
    hash_raw_inputs
  )
  if (!identical(fpq_multi_group_analysis_type(config),
                 "compare_multiple_groups"))
    return(prepared)

  mappings <- unlist(lapply(
    prepared$requests,
    function(x)
      x$display_metadata$sample_mappings %||% list()
  ), recursive = FALSE)
  metadata <- prepared$requests[[1L]]$display_metadata
  metadata$sample_mappings <- mappings

  prepared$record <- fpq_update_workflow_record(
    project,
    prepared$record$workflow_id,
    list(
      execution_adapter = "generic_multi_group",
      analysis_type = "compare_multiple_groups",
      display_metadata = metadata
    )
  )
  prepared
}

fpq_run_multi_group_export <- function(
    project,
    workflow,
    masters,
    repository_root,
    attempt_id = NULL) {
  token <- format(Sys.time(), "%Y%m%d%H%M%S")
  suffix <- if (is.null(attempt_id) ||
                !nzchar(attempt_id))
    "" else paste0(
      "-", fpq_multi_group_token(
        attempt_id, "resume"))

  output_directory <- file.path(
    project$root,
    "results",
    paste0("multi-group-", token, suffix)
  )
  request <- list(
    schema_version = "1.0.0",
    workflow_id = workflow$workflow_id,
    project_id = project$manifest$project_id,
    analysis_type = "compare_multiple_groups",
    exposure_label = workflow$exposure,
    scientific_status =
      "technical_validation_candidate",
    display_metadata = workflow$display_metadata,
    masters = unname(lapply(
      names(masters),
      function(mode) list(
        ion_mode = mode,
        path = masters[[mode]]$path,
        sha256 = masters[[mode]]$sha256,
        artifact_id = masters[[mode]]$artifact_id
      )
    )),
    output_working_directory = output_directory
  )

  request_path <- file.path(
    workflow$output_folder,
    "requests",
    paste0(
      "multi-group-publication-export",
      suffix,
      ".json"
    )
  )
  report_path <- file.path(
    workflow$output_folder,
    paste0(
      "multi-group-publication-export-report",
      suffix,
      ".json"
    )
  )
  stdout <- file.path(
    workflow$output_folder,
    paste0(
      "multi-group-publication-export",
      suffix,
      ".stdout.log"
    )
  )
  stderr <- file.path(
    workflow$output_folder,
    paste0(
      "multi-group-publication-export",
      suffix,
      ".stderr.log"
    )
  )
  fpq_atomic_write_json(request, request_path)

  python <- Sys.which("python")
  if (!nzchar(python))
    python <- Sys.which("python.exe")
  if (!nzchar(python))
    fpq_stop(
      "workflow.multi_group_python_missing",
      "Python is required for multi-group workbook export.",
      "environment"
    )

  process <- processx::run(
    python,
    c(
      file.path(
        repository_root,
        "tools",
        "multi_group_export_adapter.py"
      ),
      request_path,
      "--report",
      report_path
    ),
    wd = repository_root,
    stdout = stdout,
    stderr = stderr,
    error_on_status = FALSE
  )
  report <- if (file.exists(report_path))
    fpq_read_json(report_path) else NULL
  if (process$status != 0L ||
      is.null(report) ||
      !identical(report$status, "PASS"))
    fpq_stop(
      "workflow.multi_group_export_failed",
      report$error %||% paste(
        "Multi-group workbook export failed;",
        "inspect preserved diagnostics."
      ),
      "scientific"
    )

  final_path <- normalizePath(
    report$final_workbook$path,
    winslash = "/",
    mustWork = TRUE
  )
  final_hash <- fpq_hash_file(final_path)
  if (!identical(
      final_hash,
      report$final_workbook$sha256))
    fpq_stop(
      "workflow.multi_group_export_hash",
      "Multi-group final workbook hash verification failed.",
      "scientific"
    )

  validation <- fpq_validate_multi_group_final_results_workbook(
    final_path)
  if (!identical(validation$status, "PASS"))
    fpq_stop(
      "workflow.multi_group_workbook_validation",
      paste(validation$errors, collapse = "; "),
      "validation"
    )

  artifact <- fpq_register_artifact(
    project,
    list(
      artifact_id = paste0(
        "art-",
        workflow$workflow_id,
        "-multi-group-final-workbook",
        suffix
      ),
      artifact_type = "final_results_workbook",
      display_name = basename(final_path),
      path = final_path,
      project_id = project$manifest$project_id,
      dataset_id = NULL,
      ion_mode = NULL,
      exposure = workflow$exposure,
      producing_run_id = workflow$workflow_id,
      producing_stage_id =
        "create_combined_publication_export",
      sha256 = final_hash,
      file_size = unname(
        file.info(final_path)$size),
      validation_status = "PASS",
      scientific_profile_id =
        project$manifest$scientific_profile_id,
      scientific_core_version =
        project$manifest$scientific_core_version,
      parent_artifact_ids =
        lapply(masters, `[[`, "artifact_id"),
      notes = paste(
        "Generic multi-group six-sheet workbook;",
        "one reference versus every selected treatment;",
        "all registered features retained in",
        "All_Features_TIC_Normalized;",
        "descriptive fold changes only;",
        "Technical Validation Candidate"
      )
    ),
    activate = TRUE
  )

  list(
    path = final_path,
    sha256 = artifact$sha256,
    artifact_id = artifact$artifact_id,
    audit_workbook = final_path,
    validation = validation,
    report = report
  )
}

fpq_run_script07_and_presentation <- function(
    project,
    workflow,
    masters,
    repository_root,
    attempt_id = NULL) {
  if (identical(
      workflow$execution_adapter %||% "",
      "generic_multi_group"))
    return(fpq_run_multi_group_export(
      project,
      workflow,
      masters,
      repository_root,
      attempt_id
    ))

  fpq_run_script07_and_presentation_before_multi(
    project,
    workflow,
    masters,
    repository_root,
    attempt_id
  )
}

fpq_multi_group_final_workbook_sheets <- function() {
  c(
    "Mean_Log2FC",
    "All_Pixels",
    "TIC_normalized_intensity",
    "All_Features_TIC_Normalized",
    "QC",
    "README"
  )
}

fpq_multi_group_readme_value <- function(path, field) {
  readme <- tryCatch(
    suppressWarnings(openxlsx::read.xlsx(
      path,
      sheet = "README",
      check.names = FALSE
    )),
    error = function(e) data.frame()
  )
  if (!is.data.frame(readme) ||
      ncol(readme) < 2L ||
      !nrow(readme))
    return("")
  keys <- tolower(trimws(as.character(readme[[1L]])))
  index <- which(keys == tolower(field))
  if (!length(index)) "" else
    trimws(as.character(readme[[2L]][[index[[1L]]]]))
}

fpq_validate_multi_group_final_results_workbook <- function(path) {
  errors <- character()
  if (!file.exists(path))
    return(list(
      status = "FAIL",
      errors = "Workbook is missing.",
      workbook_type = "multi_group_comparison"
    ))

  sheets <- tryCatch(
    openxlsx::getSheetNames(path),
    error = function(e) {
      errors <<- c(
        errors,
        paste("openxlsx:", conditionMessage(e))
      )
      character()
    }
  )
  intensity_sheet <- fpq_primary_intensity_sheet_from_names(sheets)
  all_features_sheet <- fpq_all_features_sheet_from_names(sheets)
  expected <- c(
    "Mean_Log2FC", "All_Pixels", intensity_sheet,
    all_features_sheet, "QC", "README"
  )
  if (!identical(sheets, expected))
    errors <- c(
      errors,
      paste(
        "Worksheet names or order do not match",
        "the multi-group contract."
      )
    )

  read_headers <- function(sheet) {
    frame <- tryCatch(
      suppressWarnings(openxlsx::read.xlsx(
        path,
        sheet = sheet,
        rows = 1L,
        colNames = TRUE,
        check.names = FALSE
      )),
      error = function(e) NULL
    )
    if (is.null(frame)) character() else names(frame)
  }

  common_required <- c(
    "dataset_id",
    "ion_mode",
    "target_mz",
    "annotation_name",
    "GraphPad_label"
  )
  headers <- list(
    Mean_Log2FC = read_headers("Mean_Log2FC"),
    All_Pixels = read_headers("All_Pixels"),
    primary_intensity = if (is.na(intensity_sheet)) character() else
      read_headers(intensity_sheet)
  )
  for (sheet in names(headers)) {
    missing <- setdiff(
      common_required,
      headers[[sheet]]
    )
    if (length(missing))
      errors <- c(
        errors,
        paste(
          sheet,
          "is missing required metadata columns:",
          paste(missing, collapse = ", ")
        )
      )
  }

  overall_samples <- headers$Mean_Log2FC[
    endsWith(
      headers$Mean_Log2FC,
      "_overall_normalized_signal"
    )
  ]
  local_samples <- headers$primary_intensity[
    endsWith(
      headers$primary_intensity,
      "_local_strong_pixel_intensity"
    )
  ]
  detection_samples <- headers$All_Pixels[
    endsWith(
      headers$All_Pixels,
      "_conservative_detection_fraction"
    )
  ]
  count_samples <- headers$All_Pixels[
    endsWith(
      headers$All_Pixels,
      "_conservative_detected_pixel_count"
    )
  ]
  sample_counts <- c(
    overall = length(overall_samples),
    local = length(local_samples),
    detection = length(detection_samples),
    counts = length(count_samples)
  )
  if (any(sample_counts < 6L))
    errors <- c(
      errors,
      paste(
        "A multi-group workbook requires at least six",
        "individual sample columns."
      )
    )
  if (length(unique(sample_counts)) != 1L)
    errors <- c(
      errors,
      paste(
        "Individual sample-column counts disagree:",
        paste(
          names(sample_counts),
          sample_counts,
          sep = "=",
          collapse = ", "
        )
      )
    )

  overall_comparisons <- headers$Mean_Log2FC[
    startsWith(headers$Mean_Log2FC, "log2FC_")
  ]
  local_comparisons <-
    headers$primary_intensity[
      startsWith(
        headers$primary_intensity,
        "local_intensity_log2FC_"
      )
    ]
  detection_comparisons <- headers$All_Pixels[
    startsWith(
      headers$All_Pixels,
      "ion_positive_pixel_change_"
    ) &
      endsWith(headers$All_Pixels, "_pp")
  ]
  comparison_counts <- c(
    overall = length(overall_comparisons),
    local = length(local_comparisons),
    detection = length(detection_comparisons)
  )
  if (any(comparison_counts < 2L) ||
      length(unique(comparison_counts)) != 1L)
    errors <- c(
      errors,
      paste(
        "The workbook must contain at least two matching",
        "reference-versus-treatment comparison columns."
      )
    )

  all_features <- tryCatch(
    suppressWarnings(openxlsx::read.xlsx(
      path,
      sheet = all_features_sheet,
      check.names = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(all_features) ||
      !is.data.frame(all_features) ||
      !nrow(all_features)) {
    errors <- c(
      errors,
      "The all-features primary-intensity sheet is missing or empty."
    )
  } else {
    required <- c(
      "dataset_id",
      "ion_mode",
      "mzmine_row_id",
      "target_mz",
      "annotation_status",
      "annotation_name",
      "annotation_score"
    )
    missing <- setdiff(required, names(all_features))
    if (length(missing))
      errors <- c(
        errors,
        paste(
          "The all-features primary-intensity sheet is missing:",
          paste(missing, collapse = ", ")
        )
      )

    intensity_suffix <- if (identical(all_features_sheet,
                                      "All_Features_TIC_Normalized")) {
      "_TIC_normalized_intensity"
    } else if (identical(all_features_sheet, "All_Features_RAW")) {
      "_RAW_intensity"
    } else {
      paste0("_", sub("All_Features_", "", all_features_sheet),
             "_normalized_intensity")
    }
    individual <- names(all_features)[
      endsWith(
        names(all_features),
        intensity_suffix
      ) &
        !startsWith(
          names(all_features),
          "mean_"
        )
    ]
    means <- names(all_features)[
      startsWith(
        names(all_features),
        "mean_"
      ) &
        endsWith(
          names(all_features),
        intensity_suffix
        )
    ]
    fold <- names(all_features)[
      startsWith(names(all_features), "fold_change_")
    ]
    log2 <- names(all_features)[
      startsWith(names(all_features), "log2FC_")
    ]

    if (length(individual) != sample_counts[["overall"]])
      errors <- c(
        errors,
        paste(
          "All-features individual sample columns do not",
          "match the annotation-focused sheets."
        )
      )
    if (length(means) < 3L)
      errors <- c(
        errors,
        "All-features sheet requires at least three condition means."
      )
    if (length(fold) !=
        comparison_counts[["overall"]] ||
        length(log2) !=
        comparison_counts[["overall"]])
      errors <- c(
        errors,
        paste(
          "All-features fold-change columns do not match",
          "the configured comparison count."
        )
      )

    statuses <- unique(trimws(as.character(
      all_features$annotation_status
    )))
    if (any(!statuses %in% c(
        "Annotated", "Unannotated")))
      errors <- c(
        errors,
        "annotation_status contains unsupported values."
      )
  }

  qc <- tryCatch(
    suppressWarnings(openxlsx::read.xlsx(
      path,
      sheet = "QC",
      check.names = FALSE
    )),
    error = function(e) data.frame()
  )
  if (!is.data.frame(qc) ||
      !nrow(qc) ||
      !all(c("check", "status") %in% names(qc)))
    errors <- c(
      errors,
      "Multi-group QC sheet is missing or malformed."
    )
  else if (any(
      toupper(trimws(as.character(qc$status))) == "FAIL",
      na.rm = TRUE))
    errors <- c(
      errors,
      "One or more multi-group QC checks failed."
    )

  analysis_type <- fpq_multi_group_readme_value(
    path, "Analysis type")
  if (!identical(
      tolower(analysis_type),
      "compare multiple groups"))
    errors <- c(
      errors,
      paste(
        "README does not identify Compare multiple groups."
      )
    )

  reference <- fpq_multi_group_readme_value(
    path, "Reference condition")
  treatments <- fpq_multi_group_readme_value(
    path, "Comparison conditions")
  if (!nzchar(reference) || !nzchar(treatments))
    errors <- c(
      errors,
      paste(
        "README must identify the reference and comparison",
        "conditions."
      )
    )

  inferential_tokens <- c(
    "p_value",
    "pvalue",
    "q_value",
    "qvalue",
    "fdr",
    "adjusted_p",
    "paired_test",
    "imputed"
  )
  all_headers <- tolower(unlist(headers, use.names = FALSE))
  for (token in inferential_tokens)
    if (any(grepl(token, all_headers, fixed = TRUE)))
      errors <- c(
        errors,
        paste(
          "Unsupported inferential-statistics column:",
          token
        )
      )

  list(
    status = if (length(errors)) "FAIL" else "PASS",
    errors = unique(errors),
    sheets = sheets,
    workbook_type = "multi_group_comparison",
    individual_sample_columns = sample_counts,
    comparison_columns = comparison_counts,
    all_features_rows =
      if (is.data.frame(all_features))
        nrow(all_features) else 0L
  )
}

fpq_validate_published_final_results_workbook <- function(path) {
  sheets <- tryCatch(
    openxlsx::getSheetNames(path),
    error = function(e) character()
  )
  multi_group_contract <- length(sheets) == 6L &&
    identical(sheets[c(1L, 2L, 5L, 6L)],
              c("Mean_Log2FC", "All_Pixels", "QC", "README")) &&
    !is.na(fpq_primary_intensity_sheet_from_names(sheets)) &&
    !is.na(fpq_all_features_sheet_from_names(sheets))
  if (multi_group_contract) {
    analysis_type <- fpq_multi_group_readme_value(
      path, "Analysis type")
    if (identical(
        tolower(analysis_type),
        "compare multiple groups"))
      return(fpq_validate_multi_group_final_results_workbook(
        path))
  }

  fpq_validate_published_final_results_workbook_before_multi(
    path)
}
