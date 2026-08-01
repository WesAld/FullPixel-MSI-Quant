fpq_list_profiles <- function(repository_root = fpq_repo_root()) {
  files <- list.files(file.path(repository_root, "config", "scientific_profiles"),
                      pattern = "\\.ya?ml$", full.names = TRUE)
  lapply(files, yaml::read_yaml)
}
fpq_profile_aliases <- function(repository_root = fpq_repo_root())
  yaml::read_yaml(file.path(repository_root, "config", "profile_aliases.yml"))

fpq_resolve_profile <- function(profile_id, repository_root = fpq_repo_root()) {
  profiles <- fpq_list_profiles(repository_root)
  aliases <- fpq_profile_aliases(repository_root)
  entries <- aliases$aliases %||% aliases
  match <- Filter(function(x) identical(x$alias, profile_id) ||
                    identical(x$predecessor, profile_id) ||
                    identical(x$predecessor_profile_id, profile_id), entries)
  if (length(match) == 1L) {
    target <- match[[1]]$canonical %||% match[[1]]$successor %||%
      match[[1]]$canonical_profile_id
    resolved <- Filter(function(x) identical(x$profile_id, target), profiles)
    if (length(resolved) != 1L) fpq_stop("profile.alias_target_missing", "Alias target is unavailable.")
    return(list(requested_id = profile_id, resolved_id = target, profile = resolved[[1]],
                relationship = "predecessor_to_canonical",
                migration_record = match[[1]]))
  }
  direct <- Filter(function(x) identical(x$profile_id, profile_id), profiles)
  if (length(direct) == 1L)
    return(list(requested_id = profile_id, resolved_id = profile_id,
                profile = direct[[1]], relationship = "direct"))
  fpq_stop("profile.alias_unresolved", paste("Profile cannot be resolved:", profile_id))
}

fpq_compare_profiles <- function(left, right)
  list(identical = identical(left, right),
       changed_fields = union(setdiff(names(left), names(right)),
                              names(left)[vapply(names(left), function(n)
                                !identical(left[[n]], right[[n]]), logical(1))]))

fpq_validate_structural <- function(config) {
  tryCatch({ fpq_validate_dynamic_configuration(config)
    list(classification = "structurally_valid", records = list(
      fpq_validation_record("configuration.structural", "PASS",
                            "Generic configuration is structurally valid.", "INFO", FALSE))) },
    fpq_error = function(e) list(classification = "structurally_invalid",
      records = list(fpq_validation_record(e$code, "FAIL", e$message, blocking = TRUE,
                                           remediation = "Correct the configuration record."))))
}


fpq_generic_two_group_design <- function(config) {
  conditions <- config$conditions %||% list()
  comparisons <- Filter(function(x) isTRUE(x$enabled %||% TRUE),
                        config$comparisons %||% list())
  if (length(conditions) != 2L)
    return(list(valid = FALSE, message = "Generic execution requires exactly two conditions."))
  if (length(comparisons) != 1L)
    return(list(valid = FALSE, message = "Generic execution requires exactly one enabled comparison."))
  comparison <- comparisons[[1]]
  ids <- vapply(conditions, function(x) x$condition_id, character(1))
  if (!all(c(comparison$reference_condition_id,
             comparison$comparison_condition_id) %in% ids) ||
      identical(comparison$reference_condition_id,
                comparison$comparison_condition_id))
    return(list(valid = FALSE, message = "The comparison must reference two distinct configured conditions."))

  modes <- fpq_active_project_modes(config)
  if (!length(modes) || any(!modes %in% c("POS", "NEG")))
    return(list(valid = FALSE, message = "Select Positive, Negative, or both ion modes."))

  samples <- Filter(function(x)
    identical(x$inclusion_status %||% "included", "included"),
    config$samples %||% list())
  branch_details <- list()
  for (mode in modes) {
    branch <- Filter(function(x)
      identical(toupper(x$ion_mode %||% ""), mode), samples)
    ref <- Filter(function(x)
      identical(x$condition_id, comparison$reference_condition_id), branch)
    cmp <- Filter(function(x)
      identical(x$condition_id, comparison$comparison_condition_id), branch)
    if (length(ref) < 2L || length(cmp) < 2L)
      return(list(valid = FALSE,
        message = paste(mode, "requires at least two biological replicates in each condition.")))
    if (length(ref) != length(cmp))
      return(list(valid = FALSE,
        message = paste(mode, "requires equal replicate counts in the two conditions for Generic v1.")))
    replicate_key <- function(rows)
      vapply(rows, function(x) trimws(x$replicate_id %||% ""), character(1))
    if (any(!nzchar(replicate_key(ref))) || any(!nzchar(replicate_key(cmp))))
      return(list(valid = FALSE,
        message = paste(mode, "contains a blank replicate identifier.")))
    if (anyDuplicated(replicate_key(ref)) || anyDuplicated(replicate_key(cmp)))
      return(list(valid = FALSE,
        message = paste(mode, "contains duplicated replicate identifiers within a condition.")))
    branch_details[[mode]] <- list(reference_replicates = length(ref),
                                   comparison_replicates = length(cmp))
  }
  list(valid = TRUE, modes = modes, comparison = comparison,
       branches = branch_details)
}

fpq_validate_profile_compatibility <- function(config, profile_id) {
  structural <- fpq_validate_structural(config)
  if (structural$classification != "structurally_valid") return(structural)
  fail <- function(id, message, classification = "profile_incompatible")
    list(classification = classification, records = list(
      fpq_validation_record(id, "FAIL", message, "CRITICAL", TRUE,
        consequence = "Scientific execution is blocked.",
        remediation = "Correct the two-group design, sample assignments, or selected profile.")))

  supported <- c("generic-two-group-v1",
                 "pvl-fullpixel-reference-v1",
                 "fullpixel-40um-1600px-v1")
  if (!profile_id %in% supported)
    return(fail("profile.unsupported",
      "The selected profile has no installed execution adapter.",
      "profile_unsupported"))

  design <- fpq_generic_two_group_design(config)
  if (!isTRUE(design$valid))
    return(fail("profile.generic_design", design$message))

  tolerance <- fpq_mass_tolerance_settings(config)
  exact_pvl <- profile_id %in% c("pvl-fullpixel-reference-v1",
                                 "fullpixel-40um-1600px-v1") &&
    identical(config$exposure_label %||% config$exposure %||% "Single", "Single") &&
    identical(design$modes, "NEG") &&
    identical(design$branches$NEG$reference_replicates, 3L) &&
    identical(design$branches$NEG$comparison_replicates, 3L) &&
    isTRUE(tolerance$validated_default)

  list(
    classification = if (exact_pvl) "profile_compatible" else
      "profile_candidate_compatible",
    scientific_status = list(
      overall = if (exact_pvl) "approved" else "technical_validation_candidate",
      execution_allowed = TRUE,
      acknowledgement_required = !exact_pvl,
      message = if (exact_pvl)
        "Configuration matches the approved PVL Single-Negative reference boundary."
      else if (!isTRUE(tolerance$validated_default))
        paste0(
          "A custom strict mass tolerance is configured: ",
          format(tolerance$ppm, trim = TRUE), " ppm and ",
          format(tolerance$absolute_da, trim = TRUE, scientific = FALSE),
          " Da, using the smaller window; execution remains a Technical Validation Candidate."
        )
      else
        "Generic two-group execution is technically validated and remains a Technical Validation Candidate."
    ),
    records = list(fpq_validation_record(
      "profile.compatibility",
      if (exact_pvl) "PASS" else "WARNING",
      if (exact_pvl)
        "Configuration matches the approved PVL execution design."
      else if (!isTRUE(tolerance$validated_default))
        paste0(
          "Custom strict mass tolerance accepted for candidate execution: ",
          format(tolerance$ppm, trim = TRUE), " ppm and ",
          format(tolerance$absolute_da, trim = TRUE, scientific = FALSE),
          " Da; the smaller half-window is used."
        )
      else
        paste("Generic two-group design accepted:",
              paste(design$modes, collapse = "+"),
              "with equal replicate counts and request-driven metadata."),
      if (exact_pvl) "INFO" else "WARNING", FALSE,
      consequence = if (exact_pvl)
        "Approved reference execution is available."
      else
        "Execution is allowed, but outputs retain the Technical Validation Candidate label.",
      remediation = if (exact_pvl) "None."
      else "Retain the candidate label until an independent dataset-specific validation is approved."
    ))
  )
}

fpq_scientific_status_model <- function(config,
    repository_root = fpq_repo_root(), evidence = NULL) {
  design <- tryCatch(fpq_generic_two_group_design(config),
                     error = function(e) list(valid = FALSE,
                                              message = conditionMessage(e)))
  if (!isTRUE(design$valid))
    return(list(overall = "unsupported", branches = list(),
      execution_allowed = FALSE, acknowledgement_required = FALSE,
      independent_positive_review = "not_applicable",
      message = design$message %||% "The two-group design is invalid."))
  list(overall = "technical_validation_candidate",
    branches = setNames(as.list(rep("technical_validation_candidate",
                                    length(design$modes))), design$modes),
    execution_allowed = TRUE, acknowledgement_required = TRUE,
    independent_positive_review = if ("POS" %in% design$modes) "pending"
      else "not_applicable",
    message = "Generic two-group execution is technically allowed; independent dataset-specific validation remains pending.")
}

fpq_dataset_registration_status <- function(config) {
  samples <- config$samples %||% list()
  mzmine <- config$mzmine_registrations %||% list()
  modes <- fpq_active_project_modes(config)
  missing <- character()
  if (!length(samples)) missing <- c(missing, "Confirm MSI sample records.")
  if (length(samples) && any(!vapply(samples, function(x)
    file.exists(x$imzml_path %||% "") && file.exists(x$ibd_path %||% ""),
    logical(1)))) missing <- c(missing, "Restore missing imzML/IBD files.")
  for (mode in modes)
    if (is.null(mzmine[[mode]]) || !identical(mzmine[[mode]]$status, "PASS") ||
        !file.exists(mzmine[[mode]]$path %||% ""))
      missing <- c(missing, paste("Confirm the", if (mode == "POS") "Positive" else
        "Negative", "MZmine column mapping."))
  if (length(config$comparisons %||% list()) != 1L)
    missing <- c(missing, "Confirm exactly one two-group comparison.")
  biological <- unique(vapply(samples, function(x)
    x$biological_sample %||% x$sample_label %||% x$sample_id, character(1)))
  list(status = if (length(missing)) "NOT_REGISTERED" else "REGISTERED",
    blocking = length(missing) > 0L, missing = unique(missing),
    summary = list(msi_datasets = length(samples),
      mzmine_feature_lists = length(mzmine),
      biological_samples = length(biological),
      comparisons = length(config$comparisons %||% list())))
}

fpq_workflow_stage_status <- function(stage_id, ion_mode = "NEG",
                                      exposure = "Unspecified") {
  known <- c("extract_pixel_intensities", "summarize_replicate_readouts",
             "integrate_mzmine_annotations",
             "create_combined_publication_export")
  if (!stage_id %in% known)
    return(list(executable = FALSE, status = "unsupported",
                reason = "Unknown workflow stage.",
                scientific_status = "unsupported"))
  list(executable = TRUE, status = "candidate_workflow_ready",
    reason = "Generic two-group execution is a Technical Validation Candidate.",
    scientific_status = "technical_validation_candidate",
    acknowledgement_required = TRUE)
}
