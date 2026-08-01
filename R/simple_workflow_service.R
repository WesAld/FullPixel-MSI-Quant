fpq_common_parent <- function(paths) {
  normalized <- normalizePath(paths, mustWork = TRUE, winslash = "/")
  parts <- strsplit(normalized, "/", fixed = TRUE)
  limit <- min(lengths(parts))
  keep <- 0L
  for (i in seq_len(limit)) {
    values <- vapply(parts, `[[`, "", i)
    if (length(unique(tolower(values))) != 1L) break
    keep <- i
  }
  if (!keep) fpq_stop("workflow.common_root", "Selected MSI files have no common root.")
  paste(parts[[1]][seq_len(keep)], collapse = "/")
}

fpq_prepare_validated_negative_request <- function(project, config, mzmine,
                                                   repository_root = fpq_repo_root()) {
  if (is.null(project)) fpq_stop("workflow.project_required", "Open a project first.")
  if (!identical(toupper(config$ion_mode %||% ""), "NEG"))
    fpq_stop("workflow.negative_only",
             "This validated execution adapter currently supports Negative mode only.",
             "scientific")
  if (!identical(config$analysis_type %||% "", "compare_two_groups"))
    fpq_stop("workflow.comparison_required",
             "The validated reference requires the two-group comparison workflow.",
             "scientific")
  comparisons <- config$comparisons %||% list()
  if (length(comparisons) != 1L)
    fpq_stop("workflow.one_comparison",
             "Confirm exactly one two-group comparison.", "configuration")
  comparison <- comparisons[[1]]
  reference <- comparison$reference_condition_id
  target <- comparison$comparison_condition_id
  samples <- Filter(function(x) identical(x$inclusion_status %||% "included", "included"),
                    config$samples %||% list())
  by_group <- split(samples, vapply(samples, `[[`, "", "condition_id"))
  if (!setequal(names(by_group), c(reference, target)) ||
      any(lengths(by_group[c(reference, target)]) != 3L))
    fpq_stop("workflow.exact_six_samples",
      "Validated execution requires exactly three included samples in each of two groups.",
      "scientific")
  required_replicates <- c("R1", "R2", "R3")
  for (group in c(reference, target))
    if (!setequal(vapply(by_group[[group]], `[[`, "", "replicate_id"),
                  required_replicates))
      fpq_stop("workflow.exact_replicates",
        "Each group must contain exactly R1, R2, and R3.", "scientific")
  if (is.null(mzmine) || !identical(mzmine$status, "PASS"))
    fpq_stop("workflow.mzmine_unconfirmed",
             "Confirm the Negative MZmine column mapping first.", "input")

  template <- fpq_read_json(file.path(repository_root, "config", "examples",
    "pvl_single_negative_extraction.execution_request.json"))
  token <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
  internal_samples <- unlist(Map(function(group, internal_group) {
    rows <- by_group[[group]]
    rows <- rows[order(vapply(rows, `[[`, "", "replicate_id"))]
    lapply(rows, function(row) list(
      sample_id = paste0(internal_group, "_", row$replicate_id),
      condition_id = internal_group, replicate_id = row$replicate_id,
      imzml_path = normalizePath(row$imzml_path, mustWork = TRUE, winslash = "/"),
      ibd_path = normalizePath(row$ibd_path, mustWork = TRUE, winslash = "/")))
  }, c(reference, target), c("CTRL", "TRT")), recursive = FALSE)
  internal_samples <- unname(internal_samples)
  template$request_id <- paste0("req-simple-neg-", token)
  template$run_id <- paste0("run-simple-neg-", token)
  template$project_id <- project$manifest$project_id
  template$dataset_id <- config$dataset_id %||% "dset-simple-negative"
  template$registered_inputs$raw_data_root <- fpq_common_parent(
    unlist(lapply(internal_samples, function(x) c(x$imzml_path, x$ibd_path))))
  template$registered_inputs$aligned_target_table <- list(
    artifact_id = paste0("art-mzmine-neg-", token), path = mzmine$path,
    sha256 = fpq_hash_file(mzmine$path))
  template$output_working_directory <- file.path(project$root, "runs",
    template$run_id, "scientific-output")
  template$conditions <- list(
    list(condition_id = "CTRL", analytical_role = "reference"),
    list(condition_id = "TRT", analytical_role = "comparison"))
  template$samples <- internal_samples
  template$replicates <- lapply(internal_samples, function(x)
    list(condition_id = x$condition_id, replicate_id = x$replicate_id))
  template$comparison <- list(comparison_id = "CTRL_vs_TRT",
    reference_condition_id = "CTRL", comparison_condition_id = "TRT")
  template$column_mappings <- list(target_mz = mzmine$mappings$mz,
                                   feature_id = mzmine$mappings$feature_id)
  template$internal_role_mappings <- Map(function(row, original)
    list(user_facing_value = original$sample_id,
      internal_value = row$sample_id, reason = "Validated reference-role adapter",
      compatibility_rule = "exact six-role mapping",
      provenance_requirement = "required"), internal_samples,
    c(by_group[[reference]][order(vapply(by_group[[reference]], `[[`, "", "replicate_id"))],
      by_group[[target]][order(vapply(by_group[[target]], `[[`, "", "replicate_id"))]))
  template
}
