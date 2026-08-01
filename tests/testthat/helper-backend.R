make_condition <- function(id, role = "comparison", reference = FALSE, order = 1L)
  list(condition_id = id, condition_label = id, analytical_role = role,
       is_reference = reference, display_order = order)
make_sample <- function(id, condition, replicate)
  list(sample_id = id, sample_label = id, display_label = id,
       condition_id = condition, replicate_id = replicate,
       inclusion_status = "included")
make_comparison <- function(id, reference, comparison)
  list(comparison_id = id, comparison_label = id,
       reference_condition_id = reference, comparison_condition_id = comparison,
       enabled = TRUE, display_order = 1L)
make_design <- function(labels, counts) {
  conditions <- Map(function(label, i) make_condition(label,
    if (i == 1L) "reference" else "comparison", i == 1L, i),
    labels, seq_along(labels))
  samples <- unlist(Map(function(label, n) lapply(seq_len(n), function(i)
    make_sample(paste(label, i, sep = "-"), label, paste0("R", i))), labels, counts),
    recursive = FALSE)
  comparisons <- if (length(labels) > 1L) lapply(seq_along(labels)[-1], function(i)
    make_comparison(paste0("cmp-", i), labels[[1]], labels[[i]])) else list()
  list(conditions = conditions, samples = samples, comparisons = comparisons,
       ion_mode = "NEG", exposure_label = "Single")
}
expect_fpq_error <- function(object, code)
  expect_error(object, class = "fpq_error", regexp = code %||% "")
