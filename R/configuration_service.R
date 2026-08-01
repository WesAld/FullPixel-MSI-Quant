fpq_validate_dynamic_configuration <- function(config) {
  fpq_validate_mass_tolerance_configuration(config)
  conditions <- config$conditions %||% list()
  samples <- config$samples %||% list()
  comparisons <- config$comparisons %||% list()
  ids <- function(xs, field) vapply(xs, function(x) as.character(x[[field]]), character(1))
  duplicate_check <- function(values, code) if (anyDuplicated(values))
    fpq_stop(code, "Identifiers must be unique.")
  condition_ids <- ids(conditions, "condition_id")
  sample_ids <- ids(samples, "sample_id")
  comparison_ids <- ids(comparisons, "comparison_id")
  duplicate_check(condition_ids, "condition.duplicate")
  duplicate_check(sample_ids, "sample.duplicate")
  duplicate_check(comparison_ids, "comparison.duplicate")
  for (sample in samples) {
    if (!sample$condition_id %in% condition_ids)
      fpq_stop("sample.condition_missing", paste("Unknown condition:", sample$condition_id))
    if (is.null(sample$replicate_id) || !nzchar(sample$replicate_id))
      fpq_stop("sample.replicate_missing", paste("Missing replicate for", sample$sample_id))
  }
  pair_keys <- vapply(samples, function(sample) fpq_msi_pair_key(list(
    ion_mode = sample$ion_mode %||% "UNASSIGNED",
    imzml_path = sample$imzml_path %||% "",
    ibd_path = sample$ibd_path %||% "")), character(1))
  complete_pairs <- vapply(samples, function(sample)
    nzchar(sample$imzml_path %||% "") && nzchar(sample$ibd_path %||% ""), logical(1))
  if (anyDuplicated(pair_keys[complete_pairs]))
    fpq_stop("sample.path_duplicate",
             "The same normalized imzML/IBD pair is assigned twice within an ion mode.")
  for (comparison in comparisons)
    if (!comparison$reference_condition_id %in% condition_ids ||
        !comparison$comparison_condition_id %in% condition_ids)
      fpq_stop("comparison.condition_missing", paste("Invalid comparison:", comparison$comparison_id))
  invisible(TRUE)
}
`%||%` <- function(x, y) if (is.null(x)) y else x

fpq_default_mass_tolerance <- function() {
  list(
    mode = "validated_default",
    ppm = 10,
    absolute_da = 0.002,
    rule = "maximum_of_absolute_da_and_target_mz_times_ppm"
  )
}

fpq_mass_tolerance_settings <- function(config) {
  defaults <- fpq_default_mass_tolerance()
  configured <- config$mass_tolerance %||% list()
  if (!is.list(configured)) configured <- list()
  mode <- as.character(configured$mode %||% defaults$mode)
  ppm <- suppressWarnings(as.numeric(configured$ppm %||% defaults$ppm))
  absolute_da <- suppressWarnings(as.numeric(
    configured$absolute_da %||% defaults$absolute_da
  ))
  rule <- as.character(configured$rule %||% if (identical(
    mode, "custom_strict_combined"))
    "minimum_of_absolute_da_and_target_mz_times_ppm" else defaults$rule)
  if (identical(mode, "validated_default")) {
    ppm <- defaults$ppm
    absolute_da <- defaults$absolute_da
    rule <- defaults$rule
  }
  list(
    mode = mode,
    ppm = ppm,
    absolute_da = absolute_da,
    rule = rule,
    validated_default = identical(mode, "validated_default") &&
      isTRUE(all.equal(ppm, defaults$ppm)) &&
      isTRUE(all.equal(absolute_da, defaults$absolute_da)) &&
      identical(rule, defaults$rule)
  )
}

fpq_validate_mass_tolerance_configuration <- function(config) {
  configured <- config$mass_tolerance
  if (is.null(configured)) return(invisible(TRUE))
  if (!is.list(configured))
    fpq_stop("mass_tolerance.invalid", "Mass tolerance must be a configuration object.")
  mode <- as.character(configured$mode %||% "")
  allowed_modes <- c("validated_default", "custom_strict_combined")
  if (length(mode) != 1L || is.na(mode) || !mode %in% allowed_modes)
    fpq_stop(
      "mass_tolerance.mode",
      "Mass-tolerance mode must be validated_default or custom_strict_combined."
    )
  ppm <- suppressWarnings(as.numeric(configured$ppm %||% NA_real_))
  absolute_da <- suppressWarnings(as.numeric(
    configured$absolute_da %||% NA_real_
  ))
  rule <- as.character(configured$rule %||% "")
  if (length(ppm) != 1L || !is.finite(ppm) || ppm < 0.1 || ppm > 100)
    fpq_stop(
      "mass_tolerance.ppm",
      "PPM tolerance must be between 0.1 and 100."
    )
  if (length(absolute_da) != 1L || !is.finite(absolute_da) ||
      absolute_da < 0.000001 || absolute_da > 1)
    fpq_stop(
      "mass_tolerance.absolute_da",
      "Absolute m/z tolerance must be between 0.000001 and 1 Da."
    )
  if (identical(mode, "validated_default")) {
    if (!isTRUE(all.equal(ppm, 10)) ||
        !isTRUE(all.equal(absolute_da, 0.002)) ||
        !identical(rule, "maximum_of_absolute_da_and_target_mz_times_ppm"))
      fpq_stop(
        "mass_tolerance.default_mismatch",
        "Validated-default mode must use max(10 ppm, 0.002 Da)."
      )
  }
  if (identical(mode, "custom_strict_combined") &&
      !identical(rule, "minimum_of_absolute_da_and_target_mz_times_ppm"))
    fpq_stop(
      "mass_tolerance.rule",
      "Custom strict mode must use the smaller of the ppm-derived and absolute Da tolerances."
    )
  invisible(TRUE)
}

fpq_set_mass_tolerance <- function(config, mode, ppm = 10,
                                   absolute_da = 0.002) {
  mode <- as.character(mode %||% "")
  ppm <- suppressWarnings(as.numeric(ppm))
  absolute_da <- suppressWarnings(as.numeric(absolute_da))
  if (identical(mode, "validated_default")) {
    ppm <- 10
    absolute_da <- 0.002
    rule <- "maximum_of_absolute_da_and_target_mz_times_ppm"
  } else {
    rule <- "minimum_of_absolute_da_and_target_mz_times_ppm"
  }
  config$mass_tolerance <- list(
    mode = mode,
    ppm = ppm,
    absolute_da = absolute_da,
    rule = rule
  )
  fpq_validate_mass_tolerance_configuration(config)
  config
}

fpq_save_configuration <- function(project, kind, id, value, validator = NULL,
                                   simulate_interrupt = FALSE) {
  if (!kind %in% c("datasets", "runs", "snapshots"))
    fpq_stop("configuration.kind", "Unsupported configuration kind.")
  if (!is.null(value$schema_version) && !identical(value$schema_version, "1.0.0"))
    fpq_stop("configuration.schema_version", "Unsupported schema version.")
  if (!is.null(validator)) validator(value)
  path <- file.path(project$root, "config", kind, paste0(id, ".json"))
  fpq_atomic_write_json(value, path, validator, simulate_interrupt)
  updated <- fpq_save_project(project, list())
  list(path = path, sha256 = fpq_hash_file(path), project = updated)
}
fpq_load_configuration <- function(project, kind, id)
  fpq_read_json(file.path(project$root, "config", kind, paste0(id, ".json")))

fpq_add_condition <- function(config, condition) {
  config$conditions <- c(config$conditions %||% list(), list(condition))
  fpq_validate_dynamic_configuration(config); config
}
fpq_update_condition <- function(config, condition_id, updates) {
  index <- which(vapply(config$conditions, function(x) identical(x$condition_id, condition_id), logical(1)))
  if (length(index) != 1L) fpq_stop("condition.not_found", "Condition not found.")
  config$conditions[[index]] <- modifyList(config$conditions[[index]], updates)
  fpq_validate_dynamic_configuration(config); config
}
fpq_remove_condition <- function(config, condition_id, cascade = FALSE) {
  used_samples <- vapply(config$samples %||% list(), function(x) identical(x$condition_id, condition_id), logical(1))
  used_comparisons <- vapply(config$comparisons %||% list(), function(x)
    condition_id %in% c(x$reference_condition_id, x$comparison_condition_id), logical(1))
  if ((any(used_samples) || any(used_comparisons)) && !cascade)
    fpq_stop("condition.in_use", "Condition has samples or comparisons; explicit cascade is required.")
  config$samples <- (config$samples %||% list())[!used_samples]
  config$comparisons <- (config$comparisons %||% list())[!used_comparisons]
  config$conditions <- Filter(function(x) !identical(x$condition_id, condition_id), config$conditions)
  fpq_validate_dynamic_configuration(config); config
}
fpq_add_sample <- function(config, sample) {
  config$samples <- c(config$samples %||% list(), list(sample))
  fpq_validate_dynamic_configuration(config); config
}
fpq_update_sample <- function(config, sample_id, updates) {
  index <- which(vapply(config$samples, function(x) identical(x$sample_id, sample_id),
                       logical(1)))
  if (length(index) != 1L) fpq_stop("sample.not_found", "Sample not found.")
  config$samples[[index]] <- modifyList(config$samples[[index]], updates)
  fpq_validate_dynamic_configuration(config); config
}
fpq_remove_sample <- function(config, sample_id) {
  config$samples <- Filter(function(x) !identical(x$sample_id, sample_id), config$samples)
  fpq_validate_dynamic_configuration(config); config
}
fpq_add_comparison <- function(config, comparison) {
  config$comparisons <- c(config$comparisons %||% list(), list(comparison))
  fpq_validate_dynamic_configuration(config); config
}
fpq_remove_comparison <- function(config, comparison_id) {
  config$comparisons <- Filter(function(x) !identical(x$comparison_id, comparison_id), config$comparisons)
  fpq_validate_dynamic_configuration(config); config
}
fpq_reorder_conditions <- function(config, ordered_ids) {
  if (!setequal(ordered_ids, vapply(config$conditions, `[[`, "", "condition_id")))
    fpq_stop("condition.reorder_mismatch", "Reorder list must contain every condition exactly once.")
  config$conditions <- lapply(ordered_ids, function(id)
    config$conditions[[which(vapply(config$conditions, function(x) x$condition_id == id, logical(1)))]])
  for (i in seq_along(config$conditions)) config$conditions[[i]]$display_order <- i
  config
}
fpq_reorder_comparisons <- function(config, ordered_ids) {
  if (!setequal(ordered_ids, vapply(config$comparisons, `[[`, "", "comparison_id")))
    fpq_stop("comparison.reorder_mismatch", "Reorder list must contain every comparison exactly once.")
  config$comparisons <- lapply(ordered_ids, function(id)
    config$comparisons[[which(vapply(config$comparisons, function(x) x$comparison_id == id, logical(1)))]])
  for (i in seq_along(config$comparisons)) config$comparisons[[i]]$display_order <- i
  config
}
