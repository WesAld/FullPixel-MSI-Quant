# Non-frozen V2 normalization extension installed into generated Script 01
# execution copies. The validated spectrum-sum implementation remains untouched.

fpq_v2_normalization_methods <- c("raw", "spectrum_sum", "rms", "median")

fpq_v2_stable_rms <- function(values) {
  positive <- as.numeric(values[is.finite(values) & values > 0])
  if (!length(positive)) return(0)
  scale <- max(positive)
  if (!is.finite(scale) || scale <= 0) return(0)
  scale * sqrt(mean((positive / scale)^2))
}

fpq_v2_median_positive <- function(values) {
  positive <- as.numeric(values[is.finite(values) & values > 0])
  if (!length(positive)) return(0)
  stats::median(positive)
}

fpq_v2_install_normalization_extension <- function(method = "spectrum_sum") {
  method <- trimws(tolower(as.character(method)[[1L]]))
  if (!method %in% fpq_v2_normalization_methods) {
    stop("Unsupported primary_normalization_method: ", method, call. = FALSE)
  }

  legacy_inspect <- inspect_imzml_object
  legacy_extract <- extract_all_pixels
  legacy_summarize <- summarize_target_pixels

  inspect_imzml_object <<- function(obj) {
    result <- legacy_inspect(obj)
    n <- length(result$int_list)
    rms <- numeric(n)
    med <- numeric(n)
    for (i in seq_len(n)) {
      if (isTRUE(result$invalid[i])) {
        rms[i] <- NA_real_
        med[i] <- NA_real_
      } else {
        values <- as.numeric(result$int_list[[i]])
        rms[i] <- fpq_v2_stable_rms(values)
        med[i] <- fpq_v2_median_positive(values)
      }
    }
    result$rms_denominator <- rms
    result$median_denominator <- med
    result
  }

  extract_all_pixels <<- function(inspect, targets, selected, output_dir,
                                   write_pixel_file = save_pixel_level_output,
                                   pixel_filename = "target_pixel_values.csv") {
    values <- legacy_extract(inspect, targets, selected, output_dir,
                             write_pixel_file = FALSE,
                             pixel_filename = pixel_filename)
    rms <- inspect$rms_denominator[values$pixel_index]
    med <- inspect$median_denominator[values$pixel_index]
    valid_rms <- is.finite(rms) & rms > 0 & values$spectrum_valid == TRUE
    valid_med <- is.finite(med) & med > 0 & values$spectrum_valid == TRUE
    values[, rms_denominator := rms]
    values[, valid_rms_denominator := valid_rms]
    values[, rms_normalized_per_million := fifelse(
      valid_rms, (raw_target_intensity / rms) * normalization_scale, NA_real_)]
    values[, median_denominator := med]
    values[, valid_median_denominator := valid_med]
    values[, median_normalized_per_million := fifelse(
      valid_med, (raw_target_intensity / med) * normalization_scale, NA_real_)]
    values[, primary_normalization_method := method]
    values[, primary_intensity := switch(method,
      raw = raw_target_intensity,
      spectrum_sum = spectrum_sum_normalized_per_million,
      rms = rms_normalized_per_million,
      median = median_normalized_per_million)]
    if (isTRUE(write_pixel_file))
      data.table::fwrite(values, file.path(output_dir, pixel_filename))
    values
  }

  summarize_target_pixels <<- function(pixel_values, output_dir,
                                        summary_filename = "target_summary.csv") {
    result <- legacy_summarize(pixel_values, output_dir, summary_filename)
    extra <- pixel_values[, {
      valid <- spectrum_valid == TRUE
      inc_rms <- inclusive_detected == TRUE & valid_rms_denominator == TRUE
      cons_rms <- conservative_detected == TRUE & valid_rms_denominator == TRUE
      inc_med <- inclusive_detected == TRUE & valid_median_denominator == TRUE
      cons_med <- conservative_detected == TRUE & valid_median_denominator == TRUE
      rms_all <- rms_normalized_per_million[valid_rms_denominator == TRUE]
      med_all <- median_normalized_per_million[valid_median_denominator == TRUE]
      .(
        rms_normalized_mean_all_valid_pixels = mean_or_na(rms_all),
        rms_normalized_median_all_valid_pixels = median_or_na(rms_all),
        rms_normalized_p95_all_valid_pixels = q95(rms_all),
        rms_normalized_maximum_all_valid_pixels = max_or_na(rms_all),
        rms_normalized_mean_inclusive_detected_pixels =
          mean_or_na(rms_normalized_per_million[inc_rms]),
        rms_normalized_median_inclusive_detected_pixels =
          median_or_na(rms_normalized_per_million[inc_rms]),
        rms_normalized_p95_inclusive_detected_pixels =
          q95(rms_normalized_per_million[inc_rms]),
        rms_normalized_maximum_inclusive_detected_pixels =
          max_or_na(rms_normalized_per_million[inc_rms]),
        rms_normalized_mean_conservative_detected_pixels =
          mean_or_na(rms_normalized_per_million[cons_rms]),
        rms_normalized_median_conservative_detected_pixels =
          median_or_na(rms_normalized_per_million[cons_rms]),
        rms_normalized_p95_conservative_detected_pixels =
          q95(rms_normalized_per_million[cons_rms]),
        rms_normalized_maximum_conservative_detected_pixels =
          max_or_na(rms_normalized_per_million[cons_rms]),
        median_normalized_mean_all_valid_pixels = mean_or_na(med_all),
        median_normalized_median_all_valid_pixels = median_or_na(med_all),
        median_normalized_p95_all_valid_pixels = q95(med_all),
        median_normalized_maximum_all_valid_pixels = max_or_na(med_all),
        median_normalized_mean_inclusive_detected_pixels =
          mean_or_na(median_normalized_per_million[inc_med]),
        median_normalized_median_inclusive_detected_pixels =
          median_or_na(median_normalized_per_million[inc_med]),
        median_normalized_p95_inclusive_detected_pixels =
          q95(median_normalized_per_million[inc_med]),
        median_normalized_maximum_inclusive_detected_pixels =
          max_or_na(median_normalized_per_million[inc_med]),
        median_normalized_mean_conservative_detected_pixels =
          mean_or_na(median_normalized_per_million[cons_med]),
        median_normalized_median_conservative_detected_pixels =
          median_or_na(median_normalized_per_million[cons_med]),
        median_normalized_p95_conservative_detected_pixels =
          q95(median_normalized_per_million[cons_med]),
        median_normalized_maximum_conservative_detected_pixels =
          max_or_na(median_normalized_per_million[cons_med])
      )
    }, by = .(target_id, mzmine_row_id)]
    result <- merge(result, extra, by = c("target_id", "mzmine_row_id"),
                    all.x = TRUE, sort = FALSE)
    result[, rms_primary_normalized_mean_detected_pixels := fifelse(
      primary_quantification_eligible,
      rms_normalized_mean_conservative_detected_pixels, NA_real_)]
    result[, median_primary_normalized_mean_detected_pixels := fifelse(
      primary_quantification_eligible,
      median_normalized_mean_conservative_detected_pixels, NA_real_)]
    result[, primary_normalization_method := method]
    result[, primary_mean_all_valid_pixels := switch(method,
      raw = raw_mean_all_valid_pixels,
      spectrum_sum = normalized_mean_all_valid_pixels,
      rms = rms_normalized_mean_all_valid_pixels,
      median = median_normalized_mean_all_valid_pixels)]
    result[, primary_mean_conservative_detected_pixels := switch(method,
      raw = raw_mean_conservative_detected_pixels,
      spectrum_sum = normalized_mean_conservative_detected_pixels,
      rms = rms_normalized_mean_conservative_detected_pixels,
      median = median_normalized_mean_conservative_detected_pixels)]
    data.table::fwrite(result, file.path(output_dir, summary_filename))
    result
  }

  invisible(method)
}
