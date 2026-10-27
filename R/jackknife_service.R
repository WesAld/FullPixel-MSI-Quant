# Optional V3 biological-replicate jackknife configuration and eligibility.

fpq_jackknife_enabled <- function(config = list()) {
  isTRUE(config$statistical_analyses$jackknife$enabled %||% FALSE)
}

fpq_set_jackknife_enabled <- function(config, enabled) {
  config$statistical_analyses <- config$statistical_analyses %||% list()
  config$statistical_analyses$jackknife <- list(
    enabled = isTRUE(enabled),
    resampling_unit = "matched_biological_replicate_pairs",
    definition_version = "fpq-jackknife-v1"
  )
  config
}

fpq_jackknife_design <- function(config = list()) {
  unavailable <- function(reason) list(
    eligible = FALSE, reason = reason, matched_pairs = list(),
    replicate_ids = character(), n_pairs = 0L)
  if (!identical(config$analysis_type %||% "", "compare_two_groups"))
    return(unavailable("Jackknife is available only for two-group comparisons."))
  comparisons <- Filter(function(x) isTRUE(x$enabled %||% TRUE),
                        config$comparisons %||% list())
  if (length(comparisons) != 1L)
    return(unavailable("Exactly one enabled two-group comparison is required."))
  comparison <- comparisons[[1L]]
  groups <- c(comparison$reference_condition_id,
              comparison$comparison_condition_id)
  if (any(!nzchar(groups %||% "")))
    return(unavailable("Reference and comparison conditions are incomplete."))
  samples <- Filter(function(x)
    identical(x$inclusion_status %||% "included", "included") &&
      (x$condition_id %||% "") %in% groups,
    config$samples %||% list())
  grouped_rows <- lapply(groups, function(group)
    Filter(function(x) identical(x$condition_id, group), samples))
  duplicate_within_mode <- vapply(grouped_rows, function(rows) {
    keys <- vapply(rows, function(x) paste(
      trimws(as.character(x$replicate_id %||% "")),
      toupper(trimws(as.character(x$ion_mode %||% ""))), sep = "|"),
      character(1L))
    anyDuplicated(keys) > 0L
  }, logical(1L))
  if (any(duplicate_within_mode))
    return(unavailable(paste(
      "Biological replicate identifiers must be unique within each condition",
      "and ion mode.")))
  by_group <- lapply(grouped_rows, function(rows) {
    ids <- trimws(vapply(rows,
                         function(x) as.character(x$replicate_id %||% ""),
                         character(1L)))
    ids <- ids[nzchar(ids)]
    # BOTH-mode projects repeat the same biological sample by ion mode.
    unique(ids)
  })
  if (!setequal(by_group[[1L]], by_group[[2L]]))
    return(unavailable(paste(
      "Reference and comparison samples do not have identical matched",
      "biological replicate IDs.")))
  ids <- sort(by_group[[1L]])
  pairs <- lapply(ids, function(id) list(
    replicate_id = id,
    reference_condition_id = groups[[1L]],
    comparison_condition_id = groups[[2L]]
  ))
  if (length(ids) < 3L)
    return(list(eligible = FALSE,
      reason = "At least 3 matched biological replicate pairs are required.",
      matched_pairs = pairs, replicate_ids = ids, n_pairs = length(ids)))
  list(eligible = TRUE,
    reason = paste(length(ids), "matched biological replicate pairs detected."),
    matched_pairs = pairs, replicate_ids = ids, n_pairs = length(ids))
}

fpq_jackknife_ui <- function(ns, config, enabled = fpq_jackknife_enabled(config),
                             prefix = "") {
  design <- fpq_jackknife_design(config)
  pair_items <- lapply(design$matched_pairs, function(pair)
    htmltools::tags$li(paste0(pair$replicate_id, ": ",
      pair$reference_condition_id, " ", pair$replicate_id, " ↔ ",
      pair$comparison_condition_id, " ", pair$replicate_id)))
  htmltools::tags$section(class = "fpq-review fpq-jackknife",
    htmltools::tags$h3("Statistical analyses"),
    htmltools::tags$h4("Jackknife robustness analysis",
      if (isTRUE(enabled)) fpq_badge("PASS", "ENABLED")),
    htmltools::tags$p(
      "Evaluate how strongly the calculated log2 fold changes depend on individual biological replicates."),
    fpq_badge(if (design$eligible) "PASS" else "WARNING",
      if (design$eligible) "Jackknife eligible" else "Jackknife unavailable"),
    htmltools::tags$p(design$reason),
    if (!isTRUE(enabled) && design$eligible)
      shiny::actionButton(ns(paste0(prefix, "include_jackknife")),
        "+ Include Jackknife Statistics", class = "btn-primary"),
    if (isTRUE(enabled)) htmltools::tagList(
      htmltools::tags$p(htmltools::tags$strong("Resampling unit: "),
        "Biological replicate pairs"),
      htmltools::tags$p(htmltools::tags$strong("Matched replicates:")),
      htmltools::tags$ul(pair_items),
      htmltools::tags$p(paste(design$n_pairs,
        "leave-one-pair-out estimates will be calculated.")),
      htmltools::tags$p(class = "fpq-warning-panel",
        "Jackknife resampling is performed across biological replicates, never individual MSI pixels."),
      shiny::actionButton(ns(paste0(prefix, "remove_jackknife")),
        "Remove Jackknife Statistics"))
  )
}
