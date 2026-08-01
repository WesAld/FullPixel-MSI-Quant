###############################################################################
# Three-readout MALDI-MSI biological-replicate summary
###############################################################################

options(stringsAsFactors = FALSE)

###############################################################################
# USER CONFIGURATION PANEL
###############################################################################

dataset_id <- "PVL_Single_Negative"

input_csv <-
  "C:/MALDI-MSI/PVL_MSI_Analysis/02_Pixel_Quantification/PVL_Single_Negative/SixSample_20260730_145346/six_sample_combined_feature_summary.csv"

analysis_root <- "C:/MALDI-MSI/PVL_MSI_Analysis"

output_parent <- file.path(
  analysis_root,
  "03_Replicate_Summaries",
  dataset_id
)

control_sample_ids <- c(
  "CTRL_R1",
  "CTRL_R2",
  "CTRL_R3"
)

treatment_sample_ids <- c(
  "TRT_R1",
  "TRT_R2",
  "TRT_R3"
)

control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "PVL"

high_cv_threshold_percent <- 30
strong_signal_threshold <- 1000
minimum_strong_pixels <- 10L
minimum_supported_replicates <- 2L
absolute_log2fc_candidate_threshold <- 1

require_all_replicates_for_condition_statistics <- TRUE
write_excel_output <- TRUE

required_sample_ids <- c(
  control_sample_ids,
  treatment_sample_ids
)

###############################################################################

required_packages <- c("data.table", "openxlsx")

state <- new.env(parent = emptyenv())
state$warnings <- character()

log_message <- function(...) {
  message(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " ",
    paste(...)
  )
}

add_warning <- function(...) {
  warning_text <- paste(...)
  state$warnings <- c(state$warnings, warning_text)
  warning(warning_text, call. = FALSE)
}

stop_critical <- function(...) {
  stop(paste(...), call. = FALSE)
}

###############################################################################
# CONFIGURATION AND PACKAGE VALIDATION
###############################################################################

validate_user_configuration <- function() {
  scalar_text <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  scalar_logical <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }
  scalar_number <- function(x, minimum = 0, integer_required = FALSE) {
    valid <- is.numeric(x) && length(x) == 1L &&
      !is.na(x) && is.finite(x) && x >= minimum
    if (valid && integer_required) valid <- x == as.integer(x)
    valid
  }
  valid_sample_vector <- function(x) {
    is.character(x) && length(x) > 0L && !anyNA(x) &&
      all(nzchar(trimws(x)))
  }

  if (!scalar_text(dataset_id)) {
    stop_critical("Configuration error: dataset_id must be a nonblank string.")
  }
  if (!scalar_text(input_csv) || !file.exists(input_csv)) {
    stop_critical("Configuration error: input_csv does not exist:", input_csv)
  }
  if (!scalar_text(analysis_root)) {
    stop_critical("Configuration error: analysis_root must be a nonblank path.")
  }
  if (!scalar_text(output_parent)) {
    stop_critical("Configuration error: output_parent must be a nonblank path.")
  }
  if (!valid_sample_vector(control_sample_ids) ||
      !valid_sample_vector(treatment_sample_ids)) {
    stop_critical(
      "Configuration error: control and treatment sample vectors must contain nonblank sample IDs."
    )
  }
  if (anyDuplicated(control_sample_ids) || anyDuplicated(treatment_sample_ids)) {
    stop_critical("Configuration error: sample IDs are duplicated within a condition.")
  }
  if (length(intersect(control_sample_ids, treatment_sample_ids))) {
    stop_critical("Configuration error: a sample ID occurs in both conditions.")
  }
  if (length(control_sample_ids) != length(treatment_sample_ids)) {
    stop_critical(
      "Configuration error: control and treatment groups must contain equal numbers of samples."
    )
  }
  if (length(control_sample_ids) < 2L) {
    stop_critical(
      "Configuration error: at least two samples must be configured per condition."
    )
  }
  if (anyDuplicated(required_sample_ids)) {
    stop_critical("Configuration error: required_sample_ids contains duplicates.")
  }
  if (!scalar_text(control_condition_label) ||
      !scalar_text(treatment_condition_label)) {
    stop_critical("Configuration error: condition labels must be nonblank.")
  }
  if (identical(control_condition_label, treatment_condition_label)) {
    stop_critical("Configuration error: condition labels must differ.")
  }
  if (!scalar_text(output_treatment_label)) {
    stop_critical("Configuration error: output_treatment_label must be nonblank.")
  }
  if (!scalar_number(high_cv_threshold_percent) ||
      !scalar_number(strong_signal_threshold) ||
      !scalar_number(minimum_strong_pixels, minimum = 1,
                     integer_required = TRUE) ||
      !scalar_number(minimum_supported_replicates, minimum = 1,
                     integer_required = TRUE) ||
      !scalar_number(absolute_log2fc_candidate_threshold)) {
    stop_critical(
      "Configuration error: thresholds must be finite, nonnegative, and integer-valued where required."
    )
  }
  if (minimum_supported_replicates > length(control_sample_ids)) {
    stop_critical(
      "Configuration error: minimum_supported_replicates exceeds the configured replicates per condition."
    )
  }
  if (!scalar_logical(require_all_replicates_for_condition_statistics) ||
      !scalar_logical(write_excel_output)) {
    stop_critical(
      "Configuration error: logical settings must each be a single TRUE or FALSE."
    )
  }
  invisible(TRUE)
}

check_and_load_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, repos = "https://cloud.r-project.org")
    }
    if (!requireNamespace(package, quietly = TRUE)) {
      stop_critical("Required package is unavailable:", package)
    }
  }
  suppressPackageStartupMessages({
    library(data.table)
    library(openxlsx)
  })
  data.table::data.table(
    package = packages,
    version = vapply(
      packages,
      function(package) as.character(utils::packageVersion(package)),
      character(1)
    )
  )
}

create_output_directory <- function() {
  dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_parent)) {
    stop_critical("Could not create output_parent:", output_parent)
  }
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_dir <- file.path(
    output_parent,
    paste0("ThreeReadout_ReplicateSummary_", timestamp)
  )
  if (dir.exists(output_dir)) {
    stop_critical("Timestamped output directory already exists:", output_dir)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) {
    stop_critical("Could not create output directory:", output_dir)
  }
  output_dir
}

condition_statistics_rule <- function() {
  if (isTRUE(require_all_replicates_for_condition_statistics)) {
    "All configured biological replicates were required for condition-level statistics."
  } else {
    paste(
      "Finite biological-replicate values were used when at least",
      minimum_supported_replicates,
      "replicates were available per condition."
    )
  }
}

###############################################################################
# INPUT VALIDATION
###############################################################################

read_and_validate_input <- function() {
  log_message("Reading combined feature summary:", input_csv)
  data <- data.table::fread(
    input_csv,
    na.strings = c("", "NA", "NaN"),
    showProgress = FALSE
  )
  required_columns <- c(
    "dataset_id", "sample_id", "condition", "replicate", "target_id",
    "mzmine_row_id", "target_mz", "normalized_mean_all_valid_pixels",
    "normalized_mean_conservative_detected_pixels",
    "conservative_detected_pixel_count",
    "conservative_detection_fraction",
    "primary_quantification_eligible", "primary_qc_status",
    "primary_detection_threshold", "primary_minimum_detected_pixels"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns)) {
    stop_critical(
      "Input is missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  }

  character_columns <- c(
    "dataset_id", "sample_id", "condition", "replicate",
    "target_id", "mzmine_row_id"
  )
  for (column in character_columns) {
    data[, (column) := as.character(get(column))]
  }

  observed_dataset_ids <- unique(
    data$dataset_id[!is.na(data$dataset_id) & nzchar(trimws(data$dataset_id))]
  )
  if (length(observed_dataset_ids) != 1L || anyNA(data$dataset_id) ||
      any(!nzchar(trimws(data$dataset_id)))) {
    stop_critical(
      "Input must contain exactly one nonmissing, nonblank dataset ID."
    )
  }
  if (!identical(observed_dataset_ids, dataset_id)) {
    stop_critical(
      "Configured dataset_id does not match the input. Configured:",
      dataset_id,
      "Observed:",
      paste(observed_dataset_ids, collapse = ", ")
    )
  }
  if (anyNA(data$sample_id) || any(!nzchar(trimws(data$sample_id)))) {
    stop_critical("Input contains missing or blank sample IDs.")
  }
  if (anyNA(data$target_id) || any(!nzchar(trimws(data$target_id)))) {
    stop_critical("Input contains missing or blank target IDs.")
  }
  if (anyNA(data$mzmine_row_id) ||
      any(!nzchar(trimws(data$mzmine_row_id)))) {
    stop_critical("Input contains missing or blank MZmine row IDs.")
  }
  if (any(!is.finite(data$target_mz))) {
    stop_critical("Input contains missing or nonfinite target m/z values.")
  }

  observed_detection_thresholds <- suppressWarnings(
    as.numeric(data$primary_detection_threshold)
  )
  if (any(!is.finite(observed_detection_thresholds)) ||
      length(unique(observed_detection_thresholds)) != 1L ||
      unique(observed_detection_thresholds) != strong_signal_threshold) {
    stop_critical(
      paste(
        "Configured strong_signal_threshold does not match the threshold",
        "used during upstream pixel extraction."
      )
    )
  }
  observed_minimum_pixels <- suppressWarnings(
    as.numeric(data$primary_minimum_detected_pixels)
  )
  if (any(!is.finite(observed_minimum_pixels)) ||
      length(unique(observed_minimum_pixels)) != 1L ||
      unique(observed_minimum_pixels) != minimum_strong_pixels) {
    stop_critical(
      paste(
        "Configured minimum_strong_pixels does not match the upstream",
        "pixel-extraction setting."
      )
    )
  }

  observed_samples <- unique(data$sample_id)
  missing_samples <- setdiff(required_sample_ids, observed_samples)
  unexpected_samples <- setdiff(observed_samples, required_sample_ids)
  if (length(missing_samples) || length(unexpected_samples)) {
    stop_critical(
      "Input sample IDs do not exactly match the configured samples. Missing:",
      paste(missing_samples, collapse = ", "),
      "Unexpected:",
      paste(unexpected_samples, collapse = ", ")
    )
  }

  invalid_control_condition <- data[
    sample_id %chin% control_sample_ids &
      (is.na(condition) | condition != control_condition_label)
  ]
  invalid_treatment_condition <- data[
    sample_id %chin% treatment_sample_ids &
      (is.na(condition) | condition != treatment_condition_label)
  ]
  if (nrow(invalid_control_condition) || nrow(invalid_treatment_condition)) {
    stop_critical(
      "Input condition labels do not agree with configured sample-group membership."
    )
  }

  duplicate_pairs <- data[, .N, by = .(target_id, sample_id)][N != 1L]
  if (nrow(duplicate_pairs)) {
    stop_critical("Duplicate target-sample combinations are present.")
  }

  expected_rows_per_target <- length(required_sample_ids)
  rows_per_target <- data[, .N, by = target_id]
  if (any(rows_per_target$N != expected_rows_per_target)) {
    stop_critical(
      "At least one target does not have the configured number of sample rows."
    )
  }
  sample_presence <- data[
    , .(samples = paste(sample_id[order(match(sample_id, required_sample_ids))],
                        collapse = "|")),
    by = target_id
  ]
  expected_sample_string <- paste(required_sample_ids, collapse = "|")
  if (any(sample_presence$samples != expected_sample_string)) {
    stop_critical(
      "At least one target does not contain every configured sample exactly once."
    )
  }

  target_consistency <- data[, .(
    mzmine_row_ids = data.table::uniqueN(mzmine_row_id),
    target_mz_values = data.table::uniqueN(signif(target_mz, 12)),
    dataset_ids = data.table::uniqueN(dataset_id)
  ), by = target_id]
  if (any(target_consistency$mzmine_row_ids != 1L)) {
    stop_critical(
      "MZmine row IDs are inconsistent across samples for at least one target."
    )
  }
  if (any(target_consistency$target_mz_values != 1L)) {
    stop_critical(
      "Target m/z values are inconsistent across samples for at least one target."
    )
  }
  if (any(target_consistency$dataset_ids != 1L)) {
    stop_critical(
      "Dataset IDs are inconsistent across samples for at least one target."
    )
  }
  target_key_table <- unique(
    data[, .(target_id, mzmine_row_id, target_mz)]
  )
  if (anyDuplicated(target_key_table$mzmine_row_id)) {
    stop_critical(
      "A MZmine row ID is assigned to more than one target ID."
    )
  }

  data[, sample_order := match(sample_id, required_sample_ids)]
  data.table::setorder(data, target_id, sample_order)
  targets <- unique(data$target_id)
  expected_rows <- length(targets) * length(required_sample_ids)
  if (nrow(data) != expected_rows) {
    stop_critical(
      "Input row count mismatch. Expected",
      expected_rows,
      "but observed",
      nrow(data)
    )
  }
  log_message("Validated input rows:", nrow(data), "targets:", length(targets))
  list(
    data = data,
    targets = targets,
    expected_rows = expected_rows
  )
}

###############################################################################
# CONDITION STATISTICS AND DESCRIPTIVE CLASSIFICATION
###############################################################################

usable_condition_values <- function(values, configured_replicate_count) {
  finite_values <- values[is.finite(values)]
  if (isTRUE(require_all_replicates_for_condition_statistics)) {
    if (length(values) == configured_replicate_count &&
        length(finite_values) == configured_replicate_count) {
      return(values)
    }
    return(numeric())
  }
  if (length(finite_values) >= minimum_supported_replicates) {
    return(finite_values)
  }
  numeric()
}

condition_stats <- function(values, configured_replicate_count) {
  usable <- usable_condition_values(values, configured_replicate_count)
  if (!length(usable)) {
    return(list(
      mean = NA_real_, median = NA_real_, sd = NA_real_, cv = NA_real_,
      min = NA_real_, max = NA_real_
    ))
  }
  mean_value <- mean(usable)
  sd_value <- stats::sd(usable)
  cv_value <- if (is.finite(mean_value) && mean_value != 0) {
    100 * sd_value / mean_value
  } else {
    NA_real_
  }
  list(
    mean = mean_value,
    median = stats::median(usable),
    sd = sd_value,
    cv = cv_value,
    min = min(usable),
    max = max(usable)
  )
}

fc_stats <- function(control_mean, treatment_mean) {
  valid <- is.finite(control_mean) && is.finite(treatment_mean) &&
    control_mean > 0 && treatment_mean > 0
  if (!valid) return(list(fc = NA_real_, log2fc = NA_real_))
  fold_change <- treatment_mean / control_mean
  list(fc = fold_change, log2fc = log2(fold_change))
}

missing_replicates <- function(values, sample_ids) {
  missing <- !is.finite(values)
  if (!any(missing)) return("")
  paste(sample_ids[missing], collapse = ";")
}

condition_values_available <- function(values, configured_replicate_count) {
  length(
    usable_condition_values(values, configured_replicate_count)
  ) > 0L
}

fold_change_status <- function(
    control_values, treatment_values, control_mean, treatment_mean) {
  control_available <- condition_values_available(
    control_values, length(control_sample_ids)
  )
  treatment_available <- condition_values_available(
    treatment_values, length(treatment_sample_ids)
  )
  if (!control_available || !treatment_available) {
    missing <- c(
      missing_replicates(control_values, control_sample_ids),
      missing_replicates(treatment_values, treatment_sample_ids)
    )
    missing <- missing[nzchar(missing)]
    return(paste0(
      "missing_or_nonfinite_replicate",
      if (length(missing)) paste0(":", paste(missing, collapse = ";")) else ""
    ))
  }
  if (!is.finite(control_mean) || !is.finite(treatment_mean) ||
      control_mean <= 0 || treatment_mean <= 0) {
    return("nonpositive_or_nonfinite_condition_mean")
  }
  "available"
}

local_status <- function(
    control_values, treatment_values, control_counts, treatment_counts,
    control_mean, treatment_mean) {
  control_available <- condition_values_available(
    control_values, length(control_sample_ids)
  )
  treatment_available <- condition_values_available(
    treatment_values, length(treatment_sample_ids)
  )
  if (!control_available || !treatment_available) {
    missing <- c(
      missing_replicates(control_values, control_sample_ids),
      missing_replicates(treatment_values, treatment_sample_ids)
    )
    missing <- missing[nzchar(missing)]
    no_strong <- c(
      control_sample_ids[control_counts == 0],
      treatment_sample_ids[treatment_counts == 0]
    )
    parts <- character()
    if (length(missing)) {
      parts <- c(
        parts,
        paste0("missing_or_undefined_replicates=", paste(missing, collapse = ";"))
      )
    }
    if (length(no_strong)) {
      parts <- c(
        parts,
        paste0("zero_strong_pixel_replicates=", paste(no_strong, collapse = ";"))
      )
    }
    return(paste(c("unavailable", parts), collapse = "|"))
  }
  if (!is.finite(control_mean) || !is.finite(treatment_mean) ||
      control_mean <= 0 || treatment_mean <= 0) {
    return("unavailable|nonpositive_or_nonfinite_condition_mean")
  }
  "available"
}

replicate_variability_flag <- function(control_cv, treatment_cv) {
  if (!is.finite(control_cv) || !is.finite(treatment_cv)) {
    return("unavailable")
  }
  control_high <- control_cv > high_cv_threshold_percent
  treatment_high <- treatment_cv > high_cv_threshold_percent
  if (control_high && treatment_high) return("high_CV_in_both_groups")
  if (control_high) return("high_control_group_CV")
  if (treatment_high) return("high_treatment_group_CV")
  "reasonable"
}

legacy_variability_flag <- function(replicate_flag) {
  switch(
    replicate_flag,
    high_CV_in_both_groups = "high_CV_both",
    high_control_group_CV = "high_CTRL_CV",
    high_treatment_group_CV = "high_TRT_CV",
    unavailable = "unavailable",
    reasonable = "reasonable",
    replicate_flag
  )
}

overall_direction <- function(log2_fold_change) {
  if (!is.finite(log2_fold_change)) return("unavailable")
  if (log2_fold_change >= absolute_log2fc_candidate_threshold) {
    return("increased")
  }
  if (log2_fold_change <= -absolute_log2fc_candidate_threshold) {
    return("decreased")
  }
  "unchanged_or_small"
}

support_category <- function(control_supported, treatment_supported) {
  control_pass <- control_supported >= minimum_supported_replicates
  treatment_pass <- treatment_supported >= minimum_supported_replicates
  if (control_pass && treatment_pass) {
    return("supported_in_both_conditions")
  }
  if (control_pass) {
    return(paste0("supported_in_", control_condition_label, "_only"))
  }
  if (treatment_pass) {
    return(paste0("supported_in_", treatment_condition_label, "_only"))
  }
  "insufficient_strong_signal_support"
}

set_scalar_column <- function(data, column, value) {
  data.table::set(data, j = column, value = value)
  invisible(data)
}

add_replicate_columns <- function(row, values, suffix, source_sample_ids) {
  if (length(values) != length(source_sample_ids)) {
    stop_critical("Replicate-column generation received mismatched value and sample counts.")
  }
  columns <- paste0(source_sample_ids, "_", suffix)
  if (anyDuplicated(columns)) {
    stop_critical("Generated replicate-level output column names are duplicated.")
  }
  for (index in seq_along(columns)) {
    set_scalar_column(row, columns[index], values[index])
  }
  invisible(row)
}

add_condition_statistics <- function(row, statistics, condition_label, suffix) {
  set_scalar_column(row, paste0("mean_", condition_label, "_", suffix),
                    statistics$mean)
  set_scalar_column(row, paste0("median_", condition_label, "_", suffix),
                    statistics$median)
  set_scalar_column(row, paste0("sd_", condition_label, "_", suffix),
                    statistics$sd)
  set_scalar_column(row, paste0("cv_", condition_label, "_", suffix, "_percent"),
                    statistics$cv)
  set_scalar_column(row, paste0("min_", condition_label, "_", suffix),
                    statistics$min)
  set_scalar_column(row, paste0("max_", condition_label, "_", suffix),
                    statistics$max)
  invisible(row)
}

###############################################################################
# THREE-READOUT SUMMARIZATION
###############################################################################

calculate_three_readout_summary <- function(input_info) {
  data <- input_info$data
  targets <- input_info$targets
  control_indices <- seq_along(control_sample_ids)
  treatment_indices <- length(control_sample_ids) + seq_along(treatment_sample_ids)
  result_rows <- vector("list", length(targets))

  for (index in seq_along(targets)) {
    target <- targets[index]
    target_data <- data[target_id == target]
    target_data <- target_data[match(required_sample_ids, sample_id)]
    if (!identical(target_data$sample_id, required_sample_ids)) {
      stop_critical(
        "Replicate ordering validation failed for target:",
        target
      )
    }

    overall <- as.numeric(target_data$normalized_mean_all_valid_pixels)
    local <- as.numeric(
      target_data$normalized_mean_conservative_detected_pixels
    )
    counts <- as.integer(target_data$conservative_detected_pixel_count)
    fractions <- as.numeric(target_data$conservative_detection_fraction)
    eligible <- as.character(target_data$primary_quantification_eligible)
    qc_status <- as.character(target_data$primary_qc_status)

    control_overall <- overall[control_indices]
    treatment_overall <- overall[treatment_indices]
    control_local <- local[control_indices]
    treatment_local <- local[treatment_indices]
    control_counts <- counts[control_indices]
    treatment_counts <- counts[treatment_indices]
    control_fractions <- fractions[control_indices]
    treatment_fractions <- fractions[treatment_indices]

    control_overall_stats <- condition_stats(
      control_overall, length(control_sample_ids)
    )
    treatment_overall_stats <- condition_stats(
      treatment_overall, length(treatment_sample_ids)
    )
    overall_fc <- fc_stats(
      control_overall_stats$mean,
      treatment_overall_stats$mean
    )

    control_local_stats <- condition_stats(
      control_local, length(control_sample_ids)
    )
    treatment_local_stats <- condition_stats(
      treatment_local, length(treatment_sample_ids)
    )
    local_fc <- fc_stats(control_local_stats$mean, treatment_local_stats$mean)

    control_fraction_stats <- condition_stats(
      control_fractions, length(control_sample_ids)
    )
    treatment_fraction_stats <- condition_stats(
      treatment_fractions, length(treatment_sample_ids)
    )
    fraction_ratio <- if (
      is.finite(control_fraction_stats$mean) &&
      control_fraction_stats$mean > 0
    ) {
      treatment_fraction_stats$mean / control_fraction_stats$mean
    } else {
      NA_real_
    }

    control_supported <- sum(
      control_counts >= minimum_strong_pixels,
      na.rm = TRUE
    )
    treatment_supported <- sum(
      treatment_counts >= minimum_strong_pixels,
      na.rm = TRUE
    )
    support <- support_category(control_supported, treatment_supported)
    support_pass <- control_supported >= minimum_supported_replicates ||
      treatment_supported >= minimum_supported_replicates
    candidate_threshold_pass <- is.finite(overall_fc$log2fc) &&
      abs(overall_fc$log2fc) >= absolute_log2fc_candidate_threshold
    legacy_abs_log2fc_ge_1 <- is.finite(overall_fc$log2fc) &&
      abs(overall_fc$log2fc) >= 1
    variability <- replicate_variability_flag(
      control_overall_stats$cv,
      treatment_overall_stats$cv
    )

    row <- data.table::data.table(
      dataset_id = target_data$dataset_id[1],
      target_id = target,
      mzmine_row_id = target_data$mzmine_row_id[1],
      target_mz = target_data$target_mz[1]
    )

    add_replicate_columns(
      row, overall, "overall_normalized_signal", required_sample_ids
    )
    add_replicate_columns(
      row, local, "local_strong_pixel_intensity", required_sample_ids
    )
    add_replicate_columns(
      row, counts, "conservative_detected_pixel_count", required_sample_ids
    )
    add_replicate_columns(
      row, fractions, "conservative_detection_fraction", required_sample_ids
    )
    add_replicate_columns(
      row, eligible, "primary_quantification_eligible", required_sample_ids
    )
    add_replicate_columns(
      row, qc_status, "primary_qc_status", required_sample_ids
    )

    add_condition_statistics(
      row, control_overall_stats, control_condition_label, "overall_signal"
    )
    add_condition_statistics(
      row, treatment_overall_stats, treatment_condition_label, "overall_signal"
    )
    overall_fc_column <- paste0(
      "fold_change_overall_", treatment_condition_label,
      "_vs_", control_condition_label
    )
    overall_log2fc_column <- paste0(
      "log2FC_overall_", treatment_condition_label,
      "_vs_", control_condition_label
    )
    set_scalar_column(row, overall_fc_column, overall_fc$fc)
    set_scalar_column(row, overall_log2fc_column, overall_fc$log2fc)
    set_scalar_column(
      row,
      "overall_fold_change_status",
      fold_change_status(
        control_overall, treatment_overall,
        control_overall_stats$mean, treatment_overall_stats$mean
      )
    )

    add_condition_statistics(
      row, control_local_stats, control_condition_label,
      "local_strong_pixel_intensity"
    )
    add_condition_statistics(
      row, treatment_local_stats, treatment_condition_label,
      "local_strong_pixel_intensity"
    )
    local_fc_column <- paste0(
      "local_intensity_fold_change_", treatment_condition_label,
      "_vs_", control_condition_label
    )
    local_log2fc_column <- paste0(
      "local_intensity_log2FC_", treatment_condition_label,
      "_vs_", control_condition_label
    )
    set_scalar_column(row, local_fc_column, local_fc$fc)
    set_scalar_column(row, local_log2fc_column, local_fc$log2fc)
    set_scalar_column(
      row,
      "local_intensity_status",
      local_status(
        control_local, treatment_local, control_counts, treatment_counts,
        control_local_stats$mean, treatment_local_stats$mean
      )
    )

    add_condition_statistics(
      row, control_fraction_stats, control_condition_label,
      "detection_fraction"
    )
    add_condition_statistics(
      row, treatment_fraction_stats, treatment_condition_label,
      "detection_fraction"
    )
    fraction_difference_column <- paste0(
      "detection_fraction_difference_", treatment_condition_label,
      "_minus_", control_condition_label
    )
    fraction_ratio_column <- paste0(
      "detection_fraction_ratio_", treatment_condition_label,
      "_vs_", control_condition_label
    )
    set_scalar_column(
      row,
      fraction_difference_column,
      treatment_fraction_stats$mean - control_fraction_stats$mean
    )
    set_scalar_column(row, fraction_ratio_column, fraction_ratio)

    set_scalar_column(
      row,
      paste0(control_condition_label, "_supported_replicates"),
      control_supported
    )
    set_scalar_column(
      row,
      paste0(treatment_condition_label, "_supported_replicates"),
      treatment_supported
    )
    set_scalar_column(row, "strong_signal_support_pass", support_pass)
    set_scalar_column(row, "strong_signal_support_category", support)
    set_scalar_column(
      row,
      "overall_abs_log2FC_meets_candidate_threshold",
      candidate_threshold_pass
    )
    # Compatibility alias consumed by existing downstream scripts.
    set_scalar_column(
      row,
      "overall_abs_log2FC_ge_1",
      legacy_abs_log2fc_ge_1
    )
    set_scalar_column(
      row,
      "overall_direction",
      overall_direction(overall_fc$log2fc)
    )
    set_scalar_column(row, "replicate_variability_flag", variability)
    # Compatibility alias: values describe biological-replicate variability.
    set_scalar_column(
      row,
      "technical_variability_flag",
      legacy_variability_flag(variability)
    )
    set_scalar_column(
      row,
      "preliminary_quantitative_candidate",
      candidate_threshold_pass && support_pass
    )
    result_rows[[index]] <- row
  }

  summary <- data.table::rbindlist(result_rows, fill = TRUE)
  summary[, mzmine_row_id_sort := suppressWarnings(as.numeric(mzmine_row_id))]
  data.table::setorder(summary, mzmine_row_id_sort, target_mz)
  summary[, mzmine_row_id_sort := NULL]
  summary
}

###############################################################################
# OUTPUT VALIDATION
###############################################################################

numeric_values_equal <- function(observed, expected, tolerance = 1e-12) {
  if (length(observed) != length(expected)) return(FALSE)
  both_missing <- is.na(observed) & is.na(expected)
  both_finite <- is.finite(observed) & is.finite(expected)
  close <- both_finite &
    abs(observed - expected) <=
      tolerance * pmax(1, abs(observed), abs(expected))
  all(both_missing | close)
}

character_values_equal <- function(observed, expected) {
  if (length(observed) != length(expected)) return(FALSE)
  all((is.na(observed) & is.na(expected)) |
        (!is.na(observed) & !is.na(expected) & observed == expected))
}

validate_replicate_column_mapping <- function(input_data, summary) {
  mappings <- list(
    overall_normalized_signal = "normalized_mean_all_valid_pixels",
    local_strong_pixel_intensity =
      "normalized_mean_conservative_detected_pixels",
    conservative_detected_pixel_count =
      "conservative_detected_pixel_count",
    conservative_detection_fraction =
      "conservative_detection_fraction",
    primary_quantification_eligible =
      "primary_quantification_eligible",
    primary_qc_status = "primary_qc_status"
  )
  for (sample in required_sample_ids) {
    source <- input_data[sample_id == sample]
    source <- source[match(summary$target_id, target_id)]
    if (!identical(source$target_id, summary$target_id)) return(FALSE)
    for (suffix in names(mappings)) {
      output_column <- paste0(sample, "_", suffix)
      input_column <- mappings[[suffix]]
      if (!output_column %in% names(summary)) return(FALSE)
      observed <- summary[[output_column]]
      expected <- source[[input_column]]
      equal <- if (is.numeric(expected) || is.integer(expected)) {
        numeric_values_equal(as.numeric(observed), as.numeric(expected))
      } else {
        character_values_equal(as.character(observed), as.character(expected))
      }
      if (!equal) return(FALSE)
    }
  }
  TRUE
}

create_analysis_validation <- function(input_info, summary) {
  control_mean_column <- paste0(
    "mean_", control_condition_label, "_overall_signal"
  )
  treatment_mean_column <- paste0(
    "mean_", treatment_condition_label, "_overall_signal"
  )
  fold_change_column <- paste0(
    "fold_change_overall_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  log2fc_column <- paste0(
    "log2FC_overall_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  control_fraction_column <- paste0(
    "mean_", control_condition_label, "_detection_fraction"
  )
  treatment_fraction_column <- paste0(
    "mean_", treatment_condition_label, "_detection_fraction"
  )
  difference_column <- paste0(
    "detection_fraction_difference_", treatment_condition_label,
    "_minus_", control_condition_label
  )

  expected_fc <- ifelse(
    is.finite(summary[[control_mean_column]]) &
      is.finite(summary[[treatment_mean_column]]) &
      summary[[control_mean_column]] > 0 &
      summary[[treatment_mean_column]] > 0,
    summary[[treatment_mean_column]] / summary[[control_mean_column]],
    NA_real_
  )
  expected_log2fc <- ifelse(
    is.finite(expected_fc) & expected_fc > 0,
    log2(expected_fc),
    NA_real_
  )
  expected_difference <- summary[[treatment_fraction_column]] -
    summary[[control_fraction_column]]

  checks <- data.table::data.table(
    check = c(
      "summary_row_count_matches_unique_targets",
      "summary_mzmine_row_ids_present",
      "summary_target_mz_present",
      "summary_target_ids_not_duplicated",
      "replicate_columns_match_source_samples_and_targets",
      "overall_fold_changes_match_treatment_mean_divided_by_control_mean",
      "overall_log2_fold_changes_match_fold_changes",
      "detection_fraction_differences_match_treatment_minus_control",
      "no_target_rows_removed",
      "no_annotation_filtering_applied"
    ),
    passed = c(
      nrow(summary) == length(input_info$targets),
      all(!is.na(summary$mzmine_row_id) &
            nzchar(trimws(summary$mzmine_row_id))),
      all(is.finite(summary$target_mz)),
      !anyDuplicated(summary$target_id),
      validate_replicate_column_mapping(input_info$data, summary),
      numeric_values_equal(summary[[fold_change_column]], expected_fc),
      numeric_values_equal(summary[[log2fc_column]], expected_log2fc),
      numeric_values_equal(summary[[difference_column]], expected_difference),
      setequal(summary$target_id, input_info$targets),
      nrow(summary) == data.table::uniqueN(input_info$data$target_id)
    )
  )
  checks[, status := ifelse(passed, "PASS", "FAIL")]
  checks[, details := c(
    paste("Expected and observed targets:", length(input_info$targets)),
    "Each summary row contains a nonmissing MZmine row ID.",
    "Each summary row contains a finite target m/z.",
    "Target IDs are unique in the summary.",
    "Generated replicate columns reproduce their configured source samples.",
    "No pseudocount is used in fold-change validation.",
    "Log2 values are calculated directly from valid fold changes.",
    "Difference direction is treatment minus control.",
    "Every input target is retained in the output.",
    "No annotation column or annotation filter is used."
  )]
  checks[]
}

###############################################################################
# TABLES, REPORTING, AND PROVENANCE
###############################################################################

create_status_summary <- function(summary) {
  fold_change_column <- paste0(
    "fold_change_overall_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  log2fc_column <- paste0(
    "log2FC_overall_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  local_log2fc_column <- paste0(
    "local_intensity_log2FC_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  control_only_category <- paste0(
    "supported_in_", control_condition_label, "_only"
  )
  treatment_only_category <- paste0(
    "supported_in_", treatment_condition_label, "_only"
  )
  data.table::rbindlist(list(
    data.table::data.table(
      metric = "targets_processed",
      value = nrow(summary)
    ),
    data.table::data.table(
      metric = "valid_overall_fold_changes",
      value = sum(is.finite(summary[[fold_change_column]]))
    ),
    data.table::data.table(
      metric = "positive_overall_log2FC",
      value = sum(is.finite(summary[[log2fc_column]]) &
                    summary[[log2fc_column]] > 0)
    ),
    data.table::data.table(
      metric = "negative_overall_log2FC",
      value = sum(is.finite(summary[[log2fc_column]]) &
                    summary[[log2fc_column]] < 0)
    ),
    data.table::data.table(
      metric = "absolute_overall_log2FC_meets_candidate_threshold",
      value = sum(
        summary$overall_abs_log2FC_meets_candidate_threshold,
        na.rm = TRUE
      )
    ),
    data.table::data.table(
      metric = "strong_signal_support_pass",
      value = sum(summary$strong_signal_support_pass, na.rm = TRUE)
    ),
    data.table::data.table(
      metric = paste0("supported_only_in_", control_condition_label),
      value = sum(summary$strong_signal_support_category ==
                    control_only_category)
    ),
    data.table::data.table(
      metric = paste0("supported_only_in_", treatment_condition_label),
      value = sum(summary$strong_signal_support_category ==
                    treatment_only_category)
    ),
    data.table::data.table(
      metric = "supported_in_both_conditions",
      value = sum(summary$strong_signal_support_category ==
                    "supported_in_both_conditions")
    ),
    data.table::data.table(
      metric = "insufficient_strong_signal_support",
      value = sum(summary$strong_signal_support_category ==
                    "insufficient_strong_signal_support")
    ),
    data.table::data.table(
      metric = "local_intensity_fold_changes_available",
      value = sum(is.finite(summary[[local_log2fc_column]]))
    ),
    data.table::data.table(
      metric = "high_replicate_variability_in_either_condition",
      value = sum(
        summary$replicate_variability_flag %in% c(
          "high_control_group_CV",
          "high_treatment_group_CV",
          "high_CV_in_both_groups"
        )
      )
    ),
    data.table::data.table(
      metric = "preliminary_quantitative_candidates",
      value = sum(summary$preliminary_quantitative_candidate, na.rm = TRUE)
    )
  ))
}

create_methods_table <- function() {
  data.table::data.table(
    setting = c(
      "input_csv",
      "replicate_sample_ids",
      "control_sample_ids",
      "treatment_sample_ids",
      "primary_overall_signal_column",
      "local_positive_pixel_column",
      "spatial_coverage_column",
      "strong_pixel_raw_threshold",
      "minimum_strong_pixels_for_support",
      "minimum_supported_replicates",
      "absolute_log2fc_candidate_threshold",
      "preliminary_candidate_rule",
      "require_all_replicates_for_condition_statistics",
      "condition_statistics_rule",
      "pseudocount",
      "p_values",
      "high_cv_threshold_percent",
      "annotation_filtering",
      "configured_treatment_samples_retained",
      "biological_replicate_variability_interpretation"
    ),
    value = c(
      input_csv,
      paste(required_sample_ids, collapse = ", "),
      paste(control_sample_ids, collapse = ", "),
      paste(treatment_sample_ids, collapse = ", "),
      "normalized_mean_all_valid_pixels",
      "normalized_mean_conservative_detected_pixels",
      "conservative_detection_fraction",
      as.character(strong_signal_threshold),
      as.character(minimum_strong_pixels),
      as.character(minimum_supported_replicates),
      as.character(absolute_log2fc_candidate_threshold),
      paste(
        "Absolute overall log2 fold change is at least",
        absolute_log2fc_candidate_threshold,
        "and strong-signal support passes in either condition."
      ),
      as.character(require_all_replicates_for_condition_statistics),
      condition_statistics_rule(),
      "No pseudocount used.",
      "No p-values calculated.",
      as.character(high_cv_threshold_percent),
      "No annotation filtering applied.",
      paste("All retained:", paste(treatment_sample_ids, collapse = ", ")),
      "SD and CV describe variability among the biological replicate samples included in each condition."
    )
  )
}

get_running_script_path <- function() {
  command_arguments <- commandArgs(trailingOnly = FALSE)
  file_arguments <- sub(
    "^--file=",
    "",
    grep("^--file=", command_arguments, value = TRUE)
  )
  frame_files <- unlist(
    lapply(
      sys.frames(),
      function(frame) {
        if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
      }
    ),
    use.names = FALSE
  )
  candidates <- unique(c(file_arguments, frame_files))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) return(NA_character_)
  normalizePath(tail(candidates, 1L), winslash = "/", mustWork = TRUE)
}

write_configuration <- function(output_dir, package_versions, start_time) {
  configuration <- c(
    dataset_id = dataset_id,
    input_csv = input_csv,
    analysis_root = analysis_root,
    output_parent = output_parent,
    control_sample_ids = paste(control_sample_ids, collapse = ","),
    treatment_sample_ids = paste(treatment_sample_ids, collapse = ","),
    control_condition_label = control_condition_label,
    treatment_condition_label = treatment_condition_label,
    output_treatment_label = output_treatment_label,
    high_cv_threshold_percent = as.character(high_cv_threshold_percent),
    strong_signal_threshold = as.character(strong_signal_threshold),
    minimum_strong_pixels = as.character(minimum_strong_pixels),
    minimum_supported_replicates =
      as.character(minimum_supported_replicates),
    absolute_log2fc_candidate_threshold =
      as.character(absolute_log2fc_candidate_threshold),
    require_all_replicates_for_condition_statistics =
      as.character(require_all_replicates_for_condition_statistics),
    write_excel_output = as.character(write_excel_output),
    condition_statistics_rule = condition_statistics_rule(),
    r_version = R.version.string,
    run_date_time = format(start_time, "%Y-%m-%d %H:%M:%S %Z")
  )
  configuration_table <- data.table::data.table(
    setting = names(configuration),
    value = unname(configuration)
  )
  package_table <- data.table::data.table(
    setting = paste0("package_version_", package_versions$package),
    value = package_versions$version
  )
  data.table::fwrite(
    data.table::rbindlist(list(configuration_table, package_table)),
    file.path(output_dir, "analysis_configuration_used.csv")
  )
}

add_workbook_sheet <- function(workbook, sheet, data) {
  openxlsx::addWorksheet(workbook, sheet)
  openxlsx::writeDataTable(
    workbook,
    sheet,
    data,
    withFilter = TRUE,
    tableStyle = "TableStyleLight9"
  )
  openxlsx::freezePane(workbook, sheet, firstRow = TRUE)
  openxlsx::setColWidths(
    workbook,
    sheet,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}

write_excel_workbook <- function(
    summary, status_summary, methods, output_path) {
  individual_columns <- unique(c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    grep(
      paste0(
        "_(overall_normalized_signal|local_strong_pixel_intensity|",
        "conservative_detected_pixel_count|conservative_detection_fraction|",
        "primary_quantification_eligible|primary_qc_status)$"
      ),
      names(summary),
      value = TRUE
    )
  ))
  overall_columns <- unique(c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    grep("overall", names(summary), value = TRUE),
    "replicate_variability_flag",
    "technical_variability_flag",
    "preliminary_quantitative_candidate"
  ))
  local_columns <- unique(c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    grep(
      "local_strong_pixel_intensity|local_intensity",
      names(summary),
      value = TRUE
    ),
    grep("conservative_detected_pixel_count", names(summary), value = TRUE),
    paste0(control_condition_label, "_supported_replicates"),
    paste0(treatment_condition_label, "_supported_replicates"),
    "strong_signal_support_pass",
    "strong_signal_support_category"
  ))
  detection_columns <- unique(c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    grep("detection_fraction", names(summary), value = TRUE)
  ))
  qc_columns <- unique(c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    grep(
      "primary_quantification_eligible|primary_qc_status",
      names(summary),
      value = TRUE
    ),
    paste0(control_condition_label, "_supported_replicates"),
    paste0(treatment_condition_label, "_supported_replicates"),
    "strong_signal_support_pass",
    "strong_signal_support_category",
    "replicate_variability_flag",
    "technical_variability_flag",
    "preliminary_quantitative_candidate"
  ))

  workbook <- openxlsx::createWorkbook()
  add_workbook_sheet(workbook, "Three_Readout_Summary", summary)
  add_workbook_sheet(
    workbook, "Individual_Samples", summary[, ..individual_columns]
  )
  add_workbook_sheet(
    workbook, "Overall_Signal", summary[, ..overall_columns]
  )
  add_workbook_sheet(
    workbook, "Local_Positive_Pixels", summary[, ..local_columns]
  )
  add_workbook_sheet(
    workbook, "Detection_Fraction", summary[, ..detection_columns]
  )
  add_workbook_sheet(
    workbook, "Replicate_QC", summary[, ..qc_columns]
  )
  add_workbook_sheet(workbook, "Status_Summary", status_summary)
  add_workbook_sheet(workbook, "Methods_and_Settings", methods)
  openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)
}

write_analysis_report <- function(
    output_dir, input_info, summary, variability_summary, start_time) {
  fold_change_column <- paste0(
    "fold_change_overall_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  log2fc_column <- paste0(
    "log2FC_overall_", treatment_condition_label,
    "_vs_", control_condition_label
  )
  support_counts <- summary[
    , .N,
    by = strong_signal_support_category
  ][order(strong_signal_support_category)]
  report <- c(
    "Three-readout biological-replicate summary",
    paste("Dataset ID:", dataset_id),
    paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste(
      "Runtime seconds:",
      round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
    ),
    paste("Input:", input_csv),
    paste("Output directory:", output_dir),
    paste("Configured control samples:", paste(control_sample_ids, collapse = ", ")),
    paste(
      "Configured treatment samples:",
      paste(treatment_sample_ids, collapse = ", ")
    ),
    paste("Output treatment label:", output_treatment_label),
    "",
    "Validation:",
    paste("- Number of targets:", length(input_info$targets)),
    paste("- Expected input rows:", input_info$expected_rows),
    paste("- Observed input rows:", nrow(input_info$data)),
    "- Every target has one row for every configured sample.",
    "- Target m/z values and MZmine row IDs are consistent across samples.",
    "- Generated replicate-level columns were validated against source samples.",
    "- All configured biological replicates were retained.",
    "",
    "Condition statistics:",
    paste("-", condition_statistics_rule()),
    paste(
      "- Strong-signal support requires at least",
      minimum_strong_pixels,
      "conservatively detected pixels in a replicate."
    ),
    paste(
      "- The configured upstream strong-signal threshold is",
      strong_signal_threshold,
      "."
    ),
    "",
    "Descriptive counts:",
    paste(
      "- Valid overall fold changes:",
      sum(is.finite(summary[[fold_change_column]]))
    ),
    paste(
      "- Positive overall log2 fold changes:",
      sum(is.finite(summary[[log2fc_column]]) &
            summary[[log2fc_column]] > 0)
    ),
    paste(
      "- Negative overall log2 fold changes:",
      sum(is.finite(summary[[log2fc_column]]) &
            summary[[log2fc_column]] < 0)
    ),
    paste(
      "- Meeting the absolute log2 fold-change threshold:",
      sum(summary$overall_abs_log2FC_meets_candidate_threshold, na.rm = TRUE)
    ),
    paste(
      "- Passing strong-signal support:",
      sum(summary$strong_signal_support_pass, na.rm = TRUE)
    ),
    paste(
      "- Preliminary descriptive candidates:",
      sum(summary$preliminary_quantitative_candidate, na.rm = TRUE)
    ),
    "",
    "Support-category counts:",
    utils::capture.output(print(support_counts)),
    "",
    "Replicate-variability category counts:",
    utils::capture.output(print(variability_summary)),
    "",
    "Methods safeguards:",
    "- No p-values or statistical significance tests were calculated.",
    "- No pseudocount was used.",
    "- Missing values were not replaced with zero.",
    "- Annotation filtering was not applied.",
    "- No paired analysis was performed.",
    "- SD and CV describe variability among biological replicate samples.",
    "- High replicate variability was not used to remove features.",
    "- preliminary_quantitative_candidate is a descriptive review flag, not a statistical significance call.",
    paste(
      "- All configured treatment samples were retained:",
      paste(treatment_sample_ids, collapse = ", ")
    ),
    "",
    paste(
      "Warnings:",
      if (length(state$warnings)) {
        paste(state$warnings, collapse = " || ")
      } else {
        "none"
      }
    )
  )
  writeLines(
    report,
    file.path(output_dir, "three_readout_analysis_report.txt")
  )
}

###############################################################################
# MAIN WORKFLOW
###############################################################################

main <- function() {
  start_time <- Sys.time()
  validate_user_configuration()
  package_versions <- check_and_load_packages(required_packages)
  output_dir <- create_output_directory()
  write_configuration(output_dir, package_versions, start_time)

  running_script <- get_running_script_path()
  if (is.na(running_script)) {
    add_warning(
      "Could not determine the running script path; the script-used copy was not written."
    )
  } else {
    copied <- file.copy(
      running_script,
      file.path(
        output_dir,
        "03_three_readout_replicate_summary_used.R"
      ),
      overwrite = TRUE
    )
    if (!isTRUE(copied)) {
      add_warning("Could not copy the running script:", running_script)
    }
  }

  input_info <- read_and_validate_input()
  summary <- calculate_three_readout_summary(input_info)
  validation <- create_analysis_validation(input_info, summary)
  data.table::fwrite(
    validation,
    file.path(output_dir, "analysis_validation_summary.csv")
  )
  if (any(validation$status != "PASS")) {
    failed <- validation[status != "PASS", check]
    stop_critical(
      "Critical output validation failed:",
      paste(failed, collapse = ", ")
    )
  }

  status_summary <- create_status_summary(summary)
  variability_summary <- summary[
    , .(target_count = .N),
    by = replicate_variability_flag
  ][order(replicate_variability_flag)]
  methods <- create_methods_table()

  summary_csv <- file.path(
    output_dir,
    paste0(dataset_id, "_ThreeReadout_Summary.csv")
  )
  summary_xlsx <- file.path(
    output_dir,
    paste0(dataset_id, "_ThreeReadout_Summary.xlsx")
  )
  data.table::fwrite(summary, summary_csv, na = "")
  data.table::fwrite(
    status_summary,
    file.path(output_dir, "three_readout_status_summary.csv"),
    na = ""
  )
  data.table::fwrite(
    variability_summary,
    file.path(output_dir, "replicate_variability_summary.csv"),
    na = ""
  )

  if (isTRUE(write_excel_output)) {
    write_excel_workbook(
      summary,
      status_summary,
      methods,
      summary_xlsx
    )
  }
  write_analysis_report(
    output_dir,
    input_info,
    summary,
    variability_summary,
    start_time
  )
  utils::capture.output(
    utils::sessionInfo(),
    file = file.path(output_dir, "sessionInfo.txt")
  )

  log_message("Wrote:", summary_csv)
  if (isTRUE(write_excel_output)) log_message("Wrote:", summary_xlsx)
  log_message("Complete. Output directory:", output_dir)
  invisible(list(
    output_dir = output_dir,
    summary = summary,
    status_summary = status_summary,
    variability_summary = variability_summary,
    validation = validation
  ))
}

main()
