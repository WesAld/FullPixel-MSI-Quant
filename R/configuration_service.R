fpq_validate_dynamic_configuration <- function(config) {
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
