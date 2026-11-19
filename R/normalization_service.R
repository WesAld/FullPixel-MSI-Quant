fpq_normalization_methods <- function() {
  c("raw", "spectrum_sum", "rms", "median")
}

fpq_normalization_method <- function(config = list()) {
  method <- config$primary_normalization_method %||% "spectrum_sum"
  method <- trimws(tolower(as.character(method)[[1L]]))
  if (!method %in% fpq_normalization_methods())
    fpq_stop("normalization.unsupported",
      paste("Unsupported primary normalization method:", method),
      "scientific")
  method
}

fpq_normalization_definition_version <- function() "2.0.0-dev-1"

fpq_normalization_labels <- function() {
  c(raw = "Raw / no normalization",
    spectrum_sum = "TIC / spectrum-sum",
    rms = "RMS",
    median = "Median")
}

fpq_normalization_ui_choices <- function() {
  labels <- fpq_normalization_labels()
  stats::setNames(names(labels), unname(labels))
}

fpq_normalization_descriptions <- function() {
  c(
    raw = paste("Measured target intensity without per-pixel spectral",
                "normalization."),
    spectrum_sum = paste("Target intensity divided by the sum of stored",
      "centroid intensities in the pixel and scaled by 1,000,000.",
      "Existing validated FullPixel normalization."),
    rms = paste("Target intensity divided by the root mean square of positive",
      "stored centroid-peak intensities in the pixel and scaled by 1,000,000."),
    median = paste("Target intensity divided by the median of positive stored",
      "centroid-peak intensities in the pixel and scaled by 1,000,000.")
  )
}

fpq_set_normalization_method <- function(config, method) {
  config$primary_normalization_method <- fpq_normalization_method(
    list(primary_normalization_method = method))
  config
}

fpq_primary_summary_columns <- function(method) {
  method <- fpq_normalization_method(
    list(primary_normalization_method = method))
  switch(method,
    raw = c(overall = "raw_mean_all_valid_pixels",
            local = "raw_mean_conservative_detected_pixels"),
    spectrum_sum = c(overall = "normalized_mean_all_valid_pixels",
                     local = "normalized_mean_conservative_detected_pixels"),
    rms = c(overall = "rms_normalized_mean_all_valid_pixels",
            local = "rms_normalized_mean_conservative_detected_pixels"),
    median = c(overall = "median_normalized_mean_all_valid_pixels",
               local = "median_normalized_mean_conservative_detected_pixels"))
}
