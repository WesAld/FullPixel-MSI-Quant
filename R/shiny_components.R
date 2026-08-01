fpq_badge <- function(status, text = status) {
  class <- switch(tolower(status %||% ""),
    pass = "fpq-pass", completed = "fpq-pass", ready = "fpq-pass",
    warning = "fpq-warning", awaiting_validation = "fpq-warning",
    fail = "fpq-fail", blocked = "fpq-fail", incompatible = "fpq-fail",
    "fpq-neutral")
  htmltools::tags$span(class = paste("fpq-badge", class),
                       `aria-label` = paste("Status:", text), text)
}
fpq_empty_state <- function(title, message)
  htmltools::tags$div(class = "fpq-empty", htmltools::tags$h3(title),
                      htmltools::tags$p(message))
fpq_error_ui <- function(error)
  htmltools::tags$div(class = "fpq-error", role = "alert",
    `aria-live` = "assertive", tabindex = "-1",
    htmltools::tags$strong(error$message %||% "Operation failed."),
    htmltools::tags$p(paste("Error identifier:", error$code %||% "ui.unclassified")),
    htmltools::tags$p(htmltools::tags$strong("Remediation: "),
      "Correct the identified input or configuration and retry. Scientific execution remains blocked."),
    htmltools::tags$details(htmltools::tags$summary("Technical details"),
                           htmltools::tags$pre(conditionMessage(error))))

fpq_read_jsonl <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lapply(lines[nzchar(lines)], function(x)
    jsonlite::fromJSON(x, simplifyVector = FALSE))
}

fpq_preview_csv <- function(path, n = 25L) {
  if (!file.exists(path)) fpq_stop("preview.missing", "Artifact is unavailable.", "input")
  data <- utils::read.csv(path, nrows = n, check.names = FALSE,
                          stringsAsFactors = FALSE)
  list(data = data, columns = names(data), preview_rows = nrow(data))
}

fpq_make_demo_configuration <- function() {
  labels <- c("Vehicle", "Lino-Phe", "Ole-Dop", "A-Lino-Etha", "TUG-891")
  counts <- c(4L, 6L, 3L, 5L, 4L)
  conditions <- lapply(seq_along(labels), function(i)
    list(condition_id = tolower(gsub("[^A-Za-z0-9]+", "-", labels[[i]])),
      condition_label = labels[[i]], analytical_role = if (i == 1L) "reference" else "comparison",
      is_reference = i == 1L, display_order = i, treatment_label = if (i == 1L) NULL else labels[[i]],
      dose = NULL, notes = "Synthetic demonstration only"))
  samples <- unlist(Map(function(condition, n) lapply(seq_len(n), function(i)
    list(sample_id = paste0(condition$condition_id, "-", i),
      sample_label = paste(condition$condition_label, i), condition_id = condition$condition_id,
      replicate_id = paste0("bio-", i), inclusion_status = "included",
      imzml_path = paste0("PLACEHOLDER/", condition$condition_id, "-", i, ".imzML"),
      ibd_path = paste0("PLACEHOLDER/", condition$condition_id, "-", i, ".ibd"))),
    conditions, counts), recursive = FALSE)
  comparisons <- lapply(seq_along(conditions)[-1], function(i)
    list(comparison_id = paste0("cmp-", i), comparison_label = paste(
      conditions[[i]]$condition_label, "versus Vehicle"),
      reference_condition_id = conditions[[1]]$condition_id,
      comparison_condition_id = conditions[[i]]$condition_id,
      enabled = TRUE, display_order = i - 1L))
  list(schema_version = "1.0.0", dataset_id = "dset-synthetic-demo",
    dataset_name = "Synthetic demonstration only", ion_mode = "NEG",
    exposure_label = "Demonstration", conditions = conditions, samples = samples,
    comparisons = comparisons, demonstration = TRUE,
    warning = "Synthetic demonstration only - Not scientifically validated data")
}
