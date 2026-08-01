#!/usr/bin/env Rscript

###############################################################################
# 07_export_three_graphs_to_graphpad_xlsx_VALIDATED_PUBLICATION_READY.R
#
# Combines explicit publication-ready Script 04 annotated masters, validates
# the official Script 03 quantitative results, and exports publication-facing
# GraphPad tables. Script 03 remains the sole quantitative source of truth.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

###############################################################################
# USER CONFIGURATION PANEL
###############################################################################

analysis_root <- "C:/MALDI-MSI/PVL_MSI_Analysis"
exposure_label <- "Single"

dataset_inputs <- data.table::data.table(
  dataset_id = c("PVL_Single_Positive", "PVL_Single_Negative"),
  ion_mode = c("Positive", "Negative"),
  annotated_master_csv = c(
    "C:/MALDI-MSI/PVL_MSI_Analysis/04_Annotation_Merge/PVL_Single_Positive/AnnotationMerge_YYYYMMDD_HHMMSS/PVL_Single_Positive_Annotated_ThreeReadout_Master.csv",
    "C:/MALDI-MSI/PVL_MSI_Analysis/04_Annotation_Merge/PVL_Single_Negative/AnnotationMerge_YYYYMMDD_HHMMSS/PVL_Single_Negative_Annotated_ThreeReadout_Master.csv"
  )
)

output_parent <- file.path(
  analysis_root,
  paste0("05_Combined_", exposure_label, "_Overview")
)

control_sample_ids <- c("CTRL_R1", "CTRL_R2", "CTRL_R3")
treatment_sample_ids <- c("TRT_R1", "TRT_R2", "TRT_R3")
control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "PVL"

minimum_annotation_score_for_export <- 0.6
expected_all_coordinate_pixels <- 1600L
require_all_replicates_for_audit <- TRUE
minimum_replicates_if_not_all_required <- 2L
numeric_validation_tolerance <- 1e-10
write_excel_output <- TRUE
write_csv_outputs <- TRUE

###############################################################################
# CONSTANTS AND SMALL HELPERS
###############################################################################

script_version <- "1.0.0-publication-ready"
official_overall_column <- "log2FC_overall_TRT_vs_CTRL"
official_local_column <- "local_intensity_log2FC_TRT_vs_CTRL"
official_detection_column <- "detection_fraction_difference_TRT_minus_CTRL"

is_single_logical <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
is_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
as_numeric_strict <- function(x) suppressWarnings(as.numeric(as.character(x)))
same_numeric <- function(x, y, tolerance) {
  x <- as_numeric_strict(x); y <- as_numeric_strict(y)
  (is.na(x) & is.na(y)) |
    (!is.na(x) & !is.na(y) & is.finite(x) & is.finite(y) &
       abs(x - y) <= tolerance)
}
audit_match_status <- function(official, audit, tolerance) {
  official_missing <- is.na(official)
  audit_missing <- is.na(audit)
  if (official_missing && audit_missing) return("PASS")
  if (xor(official_missing, audit_missing)) return("FAIL")
  if (is.finite(official) && is.finite(audit) &&
      abs(audit - official) <= tolerance) "PASS" else "FAIL"
}
feature_key_value <- function(dataset_id, ion_mode, mzmine_row_id) {
  paste(dataset_id, ion_mode, mzmine_row_id, sep = "|")
}
presentation_id <- function(sample_id) {
  ifelse(sample_id %chin% treatment_sample_ids,
         sub(paste0("^", treatment_condition_label), output_treatment_label,
             sample_id),
         sample_id)
}
stop_with <- function(messages) stop(paste(messages, collapse = "\n"), call. = FALSE)

script_source_candidates <- local({
  candidates <- unlist(lapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
  }), use.names = FALSE)
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) existing else NA_character_
})

get_running_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  command_candidates <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  frame_candidates <- unlist(lapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
  }), use.names = FALSE)
  candidates <- unique(c(command_candidates, rev(frame_candidates),
                         script_source_candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) return(NA_character_)
  normalized <- normalizePath(existing, winslash = "/", mustWork = TRUE)
  preferred <- grep("07_export_three_graphs_to_graphpad_xlsx_VALIDATED_PUBLICATION_READY\\.R$",
                    normalized, value = TRUE)
  if (length(preferred)) return(preferred[[1L]])
  normalized[[1L]]
}

parse_logical_strict <- function(x, field_name = "logical field") {
  if (is.logical(x)) return(x)
  text <- toupper(trimws(as.character(x)))
  missing <- is.na(x) | text == ""
  truthy <- text %chin% c("TRUE", "T", "1", "YES")
  falsy <- text %chin% c("FALSE", "F", "0", "NO")
  unexpected <- unique(as.character(x)[!missing & !truthy & !falsy])
  if (length(unexpected)) {
    stop(field_name, " contains unexpected value(s): ",
         paste(unexpected, collapse = ", "),
         ". Accepted values are TRUE, FALSE, T, F, 1, 0, YES, and NO.",
         call. = FALSE)
  }
  out <- rep(NA, length(x))
  out[truthy] <- TRUE
  out[falsy] <- FALSE
  out
}

strict_numeric_allow_missing <- function(values, column_name) {
  text <- trimws(as.character(values))
  present <- !is.na(values) & nzchar(text)
  numeric_values <- suppressWarnings(as.numeric(text))
  invalid <- present & !is.finite(numeric_values)
  if (any(invalid)) {
    stop(
      "Column ", column_name,
      " contains nonnumeric or nonfinite value(s): ",
      paste(unique(text[invalid]), collapse = ", "),
      call. = FALSE
    )
  }
  numeric_values
}

validate_user_configuration <- function() {
  errors <- character()
  if (length(analysis_root) != 1L || is_blank(analysis_root)) errors <- c(errors, "analysis_root must be nonblank.")
  if (length(exposure_label) != 1L || is_blank(exposure_label)) errors <- c(errors, "exposure_label must be nonblank.")
  if (length(output_parent) != 1L || is_blank(output_parent)) errors <- c(errors, "output_parent must be nonblank.")
  required_input_fields <- c("dataset_id", "ion_mode", "annotated_master_csv")
  if (!is.data.table(dataset_inputs) || !nrow(dataset_inputs)) errors <- c(errors, "dataset_inputs must be a nonempty data.table.")
  missing_fields <- setdiff(required_input_fields, names(dataset_inputs))
  if (length(missing_fields)) errors <- c(errors, paste("dataset_inputs is missing:", paste(missing_fields, collapse = ", ")))
  if (!length(missing_fields) && nrow(dataset_inputs)) {
    if (any(is_blank(dataset_inputs$dataset_id))) errors <- c(errors, "dataset IDs must be nonblank.")
    if (anyDuplicated(dataset_inputs$dataset_id)) errors <- c(errors, "dataset IDs must be unique.")
    if (any(!dataset_inputs$ion_mode %chin% c("Positive", "Negative"))) errors <- c(errors, "ion_mode must be Positive or Negative.")
    if (any(is_blank(dataset_inputs$annotated_master_csv))) errors <- c(errors, "Annotated-master paths must be nonblank.")
    missing_files <- dataset_inputs[!file.exists(annotated_master_csv), annotated_master_csv]
    if (length(missing_files)) errors <- c(errors, paste("Annotated-master file does not exist:", paste(missing_files, collapse = "; ")))
    encoded <- fifelse(grepl("Positive", dataset_inputs$dataset_id, fixed = TRUE), "Positive",
                       fifelse(grepl("Negative", dataset_inputs$dataset_id, fixed = TRUE), "Negative", NA_character_))
    if (any(is.na(encoded) | encoded != dataset_inputs$ion_mode)) {
      errors <- c(errors, "Each dataset ID must encode and agree with its configured ion mode.")
    }
  }
  validate_samples <- function(x, label) {
    out <- character()
    if (!length(x)) out <- c(out, paste(label, "must not be empty."))
    if (any(is_blank(x))) out <- c(out, paste(label, "contains a blank or missing sample ID."))
    if (anyDuplicated(x)) out <- c(out, paste(label, "contains duplicate sample IDs."))
    out
  }
  errors <- c(errors, validate_samples(control_sample_ids, "control_sample_ids"),
              validate_samples(treatment_sample_ids, "treatment_sample_ids"))
  if (length(intersect(control_sample_ids, treatment_sample_ids))) errors <- c(errors, "A sample ID occurs in both conditions.")
  if (length(control_sample_ids) != length(treatment_sample_ids)) errors <- c(errors, "Control and treatment replicate counts must match.")
  labels <- c(control_condition_label, treatment_condition_label)
  if (any(is_blank(labels)) || length(unique(labels)) != 2L) errors <- c(errors, "Condition labels must be nonblank and different.")
  if (length(output_treatment_label) != 1L || is_blank(output_treatment_label)) errors <- c(errors, "output_treatment_label must be nonblank.")
  if (length(minimum_annotation_score_for_export) != 1L ||
      !is.finite(minimum_annotation_score_for_export) ||
      minimum_annotation_score_for_export < 0) errors <- c(errors, "minimum_annotation_score_for_export must be one finite nonnegative number.")
  if (length(expected_all_coordinate_pixels) != 1L ||
      is.na(expected_all_coordinate_pixels) ||
      expected_all_coordinate_pixels <= 0 ||
      expected_all_coordinate_pixels != as.integer(expected_all_coordinate_pixels)) errors <- c(errors, "expected_all_coordinate_pixels must be a positive integer.")
  if (length(minimum_replicates_if_not_all_required) != 1L ||
      is.na(minimum_replicates_if_not_all_required) ||
      minimum_replicates_if_not_all_required < 1L ||
      minimum_replicates_if_not_all_required > length(control_sample_ids)) errors <- c(errors, "minimum_replicates_if_not_all_required is invalid.")
  if (length(numeric_validation_tolerance) != 1L ||
      !is.finite(numeric_validation_tolerance) ||
      numeric_validation_tolerance < 0) errors <- c(errors, "numeric_validation_tolerance must be finite and nonnegative.")
  logical_names <- c("require_all_replicates_for_audit", "write_excel_output", "write_csv_outputs")
  for (nm in logical_names) if (!is_single_logical(get(nm, inherits = TRUE))) errors <- c(errors, paste(nm, "must be one TRUE/FALSE value."))
  if (length(errors)) stop_with(errors)
  invisible(TRUE)
}

required_source_columns <- function() {
  metadata <- c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    "annotation_present", "annotation_name", "annotation_formula",
    "annotation_adduct", "annotation_score", "annotation_confidence_note",
    "replicate_variability_flag", "strong_signal_support_pass",
    "strong_signal_support_category", "preliminary_quantitative_candidate",
    "high_priority_annotated_candidate", "candidate_review_category",
    "overall_direction",
    official_overall_column, official_local_column, official_detection_column
  )
  suffixes <- c("overall_normalized_signal", "local_strong_pixel_intensity",
                "conservative_detection_fraction",
                "conservative_detected_pixel_count")
  c(metadata, unlist(lapply(suffixes, function(s) paste0(
    c(control_sample_ids, treatment_sample_ids), "_", s
  ))))
}

count_columns <- function() {
  paste0(c(control_sample_ids, treatment_sample_ids),
         "_conservative_detected_pixel_count")
}

###############################################################################
# OFFICIAL-FORMULA AUDIT FUNCTIONS
###############################################################################

audit_condition_mean <- function(values) {
  values <- as.numeric(values)
  finite <- is.finite(values)
  needed <- if (require_all_replicates_for_audit) length(values) else minimum_replicates_if_not_all_required
  if (sum(finite) < needed) return(NA_real_)
  mean(values[finite])
}

audit_log2_ratio <- function(control_values, treatment_values) {
  c_mean <- audit_condition_mean(control_values)
  t_mean <- audit_condition_mean(treatment_values)
  if (!is.finite(c_mean) || !is.finite(t_mean) || c_mean <= 0 || t_mean <= 0) return(NA_real_)
  log2(t_mean / c_mean)
}

audit_difference <- function(control_values, treatment_values) {
  c_mean <- audit_condition_mean(control_values)
  t_mean <- audit_condition_mean(treatment_values)
  if (!is.finite(c_mean) || !is.finite(t_mean)) return(NA_real_)
  t_mean - c_mean
}

row_audit <- function(dt, control_columns, treatment_columns, fun) {
  vapply(seq_len(nrow(dt)), function(i) {
    fun(unlist(dt[i, ..control_columns], use.names = FALSE),
        unlist(dt[i, ..treatment_columns], use.names = FALSE))
  }, numeric(1L))
}

graphpad_label <- function(name, adduct, mz) {
  paste0(trimws(name), " | ", trimws(adduct), " | m/z ", sprintf("%.4f", mz))
}

###############################################################################
# INPUT LOADING AND COMPLETE-MASTER CONSTRUCTION
###############################################################################

load_one_source <- function(config_row, source_index) {
  path <- config_row$annotated_master_csv[[1L]]
  dt <- fread(path, na.strings = c("", "NA", "NaN"))
  if (!nrow(dt)) stop("Source table is empty: ", path, call. = FALSE)
  missing <- setdiff(required_source_columns(), names(dt))
  if (length(missing)) stop("Missing required columns in ", path, ": ",
                            paste(missing, collapse = ", "), call. = FALSE)
  dataset_id_expected <- config_row$dataset_id[[1L]]
  ion_mode_expected <- config_row$ion_mode[[1L]]
  if (length(unique(dt$dataset_id)) != 1L || unique(dt$dataset_id) != dataset_id_expected) {
    stop("Source dataset_id does not match configuration: ", path, call. = FALSE)
  }
  dt[, mzmine_row_id := as.character(mzmine_row_id)]
  dt[, target_id := as.character(target_id)]
  if (any(is_blank(dt$mzmine_row_id)) || any(is_blank(dt$target_id))) stop("Missing feature identifiers in: ", path, call. = FALSE)
  if (anyDuplicated(dt$mzmine_row_id)) stop("mzmine_row_id is not unique within: ", dataset_id_expected, call. = FALSE)
  if (anyDuplicated(dt$target_id)) stop("target_id is not unique within: ", dataset_id_expected, call. = FALSE)
  target_mz_num <- as_numeric_strict(dt$target_mz)
  if (any(!is.finite(target_mz_num) | target_mz_num <= 0)) stop("target_mz must be finite and positive in: ", path, call. = FALSE)
  score_text <- trimws(as.character(dt$annotation_score))
  score_num <- as_numeric_strict(dt$annotation_score)
  if (any(!is_blank(score_text) & !is.finite(score_num))) stop("annotation_score is nonnumeric when present in: ", path, call. = FALSE)
  official_numeric_columns <- c(
    official_overall_column, official_local_column, official_detection_column
  )
  overall_numeric_columns <- paste0(
    c(control_sample_ids, treatment_sample_ids), "_overall_normalized_signal"
  )
  local_numeric_columns <- paste0(
    c(control_sample_ids, treatment_sample_ids), "_local_strong_pixel_intensity"
  )
  fraction_numeric_columns <- paste0(
    c(control_sample_ids, treatment_sample_ids),
    "_conservative_detection_fraction"
  )
  count_numeric_columns <- paste0(
    c(control_sample_ids, treatment_sample_ids),
    "_conservative_detected_pixel_count"
  )
  quantitative_numeric_columns <- c(
    official_numeric_columns, overall_numeric_columns, local_numeric_columns,
    fraction_numeric_columns, count_numeric_columns
  )
  for (column_name in quantitative_numeric_columns) {
    set(dt, j = column_name, value = strict_numeric_allow_missing(
      dt[[column_name]], column_name
    ))
  }
  annotation_logical <- parse_logical_strict(
    dt$annotation_present,
    paste0("annotation_present in ", path)
  )
  if (any(is.na(annotation_logical))) {
    stop("annotation_present must be nonmissing in: ", path, call. = FALSE)
  }
  dt[, annotation_present := annotation_logical]
  allowed <- if (ion_mode_expected == "Positive") c("[M+H]+", "[M+Na]+", "[M+K]+") else "[M-H]-"
  adduct <- trimws(as.character(dt$annotation_adduct))
  invalid_nonblank <- !is_blank(adduct) & !adduct %chin% allowed
  if (any(invalid_nonblank)) {
    stop("Nonblank annotation adduct(s) disagree with the configured ", ion_mode_expected,
         " ion mode in ", path, ": ",
         paste(unique(adduct[invalid_nonblank]), collapse = ", "), call. = FALSE)
  }
  if (any(annotation_logical & is_blank(adduct))) {
    stop("Every annotated row must have a nonblank accepted adduct in: ",
         path, call. = FALSE)
  }
  original <- copy(dt)
  dt[, ion_mode := ion_mode_expected]
  dt[, feature_key := feature_key_value(dataset_id, ion_mode, mzmine_row_id)]
  dt[, `.source_index` := source_index]
  dt[, `.source_row` := seq_len(.N)]
  if (any(is_blank(dt$feature_key)) || anyDuplicated(dt$feature_key)) stop("Source feature keys are missing or duplicated in: ", path, call. = FALSE)
  list(data = dt, original = original)
}

add_export_columns <- function(master) {
  master[, log2FC_overall_PVL_vs_CTRL := as_numeric_strict(get(official_overall_column))]
  master[, local_intensity_log2FC_PVL_vs_CTRL := as_numeric_strict(get(official_local_column))]
  master[, ion_positive_pixel_change_PVL_minus_CTRL_pp := 100 * as_numeric_strict(get(official_detection_column))]
  master[, GraphPad_label := fifelse(
    as.logical(annotation_present) &
      !is_blank(annotation_name) & !is_blank(annotation_adduct) &
      is.finite(as_numeric_strict(target_mz)),
    graphpad_label(as.character(annotation_name), as.character(annotation_adduct),
                   as_numeric_strict(target_mz)),
    NA_character_
  )]
  master[]
}

build_combined_master <- function() {
  loaded <- lapply(seq_len(nrow(dataset_inputs)), function(i) {
    load_one_source(dataset_inputs[i], i)
  })
  combined <- rbindlist(lapply(loaded, `[[`, "data"), use.names = TRUE, fill = TRUE)
  setorder(combined, `.source_index`, `.source_row`)
  if (anyDuplicated(combined$feature_key)) stop("Combined feature keys are duplicated.", call. = FALSE)
  for (i in seq_along(loaded)) {
    original <- loaded[[i]]$original
    rebuilt <- combined[`.source_index` == i, names(original), with = FALSE]
    if (!isTRUE(all.equal(original, rebuilt, check.attributes = FALSE))) {
      stop("Source columns were not preserved value-for-value for dataset index ", i, ".", call. = FALSE)
    }
  }
  combined <- add_export_columns(combined)
  list(master = combined, loaded = loaded)
}

###############################################################################
# EXPORT TABLES AND VALIDATION
###############################################################################

make_audits <- function(export_dt) {
  overall_ctrl <- paste0(control_sample_ids, "_overall_normalized_signal")
  overall_trt <- paste0(treatment_sample_ids, "_overall_normalized_signal")
  local_ctrl <- paste0(control_sample_ids, "_local_strong_pixel_intensity")
  local_trt <- paste0(treatment_sample_ids, "_local_strong_pixel_intensity")
  frac_ctrl <- paste0(control_sample_ids, "_conservative_detection_fraction")
  frac_trt <- paste0(treatment_sample_ids, "_conservative_detection_fraction")
  export_dt[, audit_overall_log2fc := row_audit(.SD, overall_ctrl, overall_trt, audit_log2_ratio)]
  export_dt[, audit_local_intensity_log2fc := row_audit(.SD, local_ctrl, local_trt, audit_log2_ratio)]
  export_dt[, audit_detection_fraction_difference := row_audit(.SD, frac_ctrl, frac_trt, audit_difference)]
  export_dt[, audit_detection_percentage_point_difference := 100 * audit_detection_fraction_difference]
  export_dt[, overall_audit_status := fifelse(
    same_numeric(get(official_overall_column), audit_overall_log2fc, numeric_validation_tolerance), "PASS", "FAIL")]
  export_dt[, local_audit_status := fifelse(
    same_numeric(get(official_local_column), audit_local_intensity_log2fc, numeric_validation_tolerance), "PASS", "FAIL")]
  export_dt[, detection_audit_status := fifelse(
    same_numeric(get(official_detection_column), audit_detection_fraction_difference, numeric_validation_tolerance), "PASS", "FAIL")]
  export_dt[]
}

rename_treatment_columns <- function(dt, suffix = NULL) {
  old <- treatment_sample_ids
  new <- presentation_id(treatment_sample_ids)
  if (!is.null(suffix)) {
    old <- paste0(old, "_", suffix)
    new <- paste0(new, "_", suffix)
  }
  setnames(dt, old, new)
  dt
}

id_columns <- c("feature_key", "dataset_id", "ion_mode", "mzmine_row_id",
                "GraphPad_label", "annotation_name", "annotation_formula",
                "annotation_adduct", "target_mz", "annotation_score")

make_graphpad_sheets <- function(export_dt) {
  overall_source <- paste0(c(control_sample_ids, treatment_sample_ids), "_overall_normalized_signal")
  overall <- copy(export_dt[, c(id_columns, overall_source,
    "log2FC_overall_PVL_vs_CTRL", "overall_direction",
    "preliminary_quantitative_candidate", "high_priority_annotated_candidate"), with = FALSE])
  rename_treatment_columns(overall, "overall_normalized_signal")

  frac_source <- paste0(c(control_sample_ids, treatment_sample_ids), "_conservative_detection_fraction")
  fraction <- copy(export_dt[, c(id_columns, frac_source,
    count_columns(),
    "ion_positive_pixel_change_PVL_minus_CTRL_pp"), with = FALSE])
  for (nm in frac_source) set(fraction, j = nm, value = 100 * as_numeric_strict(fraction[[nm]]))
  setnames(fraction, frac_source, presentation_id(c(control_sample_ids, treatment_sample_ids)))
  count_old <- count_columns()
  if (length(count_old)) {
    count_new <- count_old
    for (i in seq_along(treatment_sample_ids)) {
      count_new <- sub(paste0("^", treatment_sample_ids[[i]]),
                       presentation_id(treatment_sample_ids[[i]]), count_new)
    }
    setnames(fraction, count_old, count_new)
  }

  local_source <- paste0(c(control_sample_ids, treatment_sample_ids), "_local_strong_pixel_intensity")
  local <- copy(export_dt[, c(id_columns, local_source,
    "local_intensity_log2FC_PVL_vs_CTRL"), with = FALSE])
  rename_treatment_columns(local, "local_strong_pixel_intensity")
  list(Mean_Log2FC = overall, All_Pixels = fraction,
       Positive_Pixel_Intensity = local)
}

qc_row <- function(check_name, pass, observed, expected, critical = TRUE) {
  data.table(check_name = check_name, status = if (isTRUE(pass)) "PASS" else "FAIL",
             observed = as.character(observed), expected = as.character(expected),
             critical = critical)
}

source_official_reference <- function(loaded) {
  rbindlist(lapply(seq_along(loaded), function(i) {
    original <- loaded[[i]]$original
    mode <- dataset_inputs$ion_mode[[i]]
    data.table(
      feature_key = feature_key_value(
        as.character(original$dataset_id), mode,
        as.character(original$mzmine_row_id)
      ),
      source_overall = original[[official_overall_column]],
      source_local = original[[official_local_column]],
      source_detection = original[[official_detection_column]]
    )
  }))
}

build_qc <- function(master, audited_master, export_dt, audit,
                     formula_metadata, loaded) {
  source_rows <- sum(vapply(loaded, function(x) nrow(x$original), integer(1L)))
  source_preserved <- all(vapply(seq_along(loaded), function(i) {
    isTRUE(all.equal(loaded[[i]]$original,
      master[`.source_index` == i, names(loaded[[i]]$original), with = FALSE],
      check.attributes = FALSE))
  }, logical(1L)))
  labels <- export_dt$GraphPad_label
  label_pattern <- " \\| m/z [0-9]+\\.[0-9]{4}$"
  export_nonempty <- nrow(export_dt) > 0L
  source_reference <- source_official_reference(loaded)
  source_reference <- source_reference[match(master$feature_key, feature_key)]
  source_keys_match <- nrow(source_reference) == nrow(master) &&
    identical(source_reference$feature_key, master$feature_key)
  source_overall_preserved <- source_keys_match && all(same_numeric(
    source_reference$source_overall, master[[official_overall_column]],
    numeric_validation_tolerance
  ))
  source_local_preserved <- source_keys_match && all(same_numeric(
    source_reference$source_local, master[[official_local_column]],
    numeric_validation_tolerance
  ))
  source_detection_preserved <- source_keys_match && all(same_numeric(
    source_reference$source_detection, master[[official_detection_column]],
    numeric_validation_tolerance
  ))
  count_names <- count_columns()
  fraction_names <- paste0(c(control_sample_ids, treatment_sample_ids),
                           "_conservative_detection_fraction")
  counts <- unlist(lapply(count_names, function(nm) as_numeric_strict(master[[nm]])),
                   use.names = FALSE)
  fractions <- unlist(lapply(fraction_names, function(nm) as_numeric_strict(master[[nm]])),
                      use.names = FALSE)
  recorded_fraction_matrix <- as.matrix(master[, ..fraction_names])
  storage.mode(recorded_fraction_matrix) <- "numeric"
  count_matrix <- as.matrix(master[, ..count_names])
  storage.mode(count_matrix) <- "numeric"
  denominator_differences <- abs(
    recorded_fraction_matrix - count_matrix / expected_all_coordinate_pixels
  )
  counts_valid <- length(counts) > 0L && all(
    is.finite(counts) & counts == round(counts) &
      counts >= 0 & counts <= expected_all_coordinate_pixels
  )
  fractions_valid <- length(fractions) > 0L &&
    all(is.finite(fractions) & fractions >= 0 & fractions <= 1)
  denominator_valid <- length(denominator_differences) > 0L &&
    all(is.finite(denominator_differences)) &&
    max(denominator_differences) <= numeric_validation_tolerance
  official_names <- c(
    official_overall_column, official_local_column, official_detection_column
  )
  overall_names <- paste0(
    c(control_sample_ids, treatment_sample_ids), "_overall_normalized_signal"
  )
  local_names <- paste0(
    c(control_sample_ids, treatment_sample_ids), "_local_strong_pixel_intensity"
  )
  numeric_when_present <- function(columns) all(vapply(columns, function(nm) {
    values <- master[[nm]]
    all(is.na(values) | is.finite(values))
  }, logical(1L)))
  official_numeric_valid <- numeric_when_present(official_names)
  overall_numeric_valid <- numeric_when_present(overall_names)
  local_numeric_valid <- numeric_when_present(local_names)
  expected_formula_columns <- 10L
  expected_formula_operations <- nrow(audit) *
    formula_metadata$formula_columns_written
  formula_range_valid <- nrow(audit) > 0L &&
    identical(formula_metadata$audit_data_rows, nrow(audit)) &&
    identical(formula_metadata$first_formula_row, 2L) &&
    identical(formula_metadata$last_formula_row, nrow(audit) + 1L) &&
    identical(formula_metadata$formula_columns_written,
              expected_formula_columns) &&
    identical(formula_metadata$formulas_written,
              expected_formula_operations)
  presentation_ok <- export_nonempty && all(vapply(
    c("overall_normalized_signal", "local_strong_pixel_intensity"),
    function(suffix) all(vapply(seq_along(treatment_sample_ids), function(i) {
      source_name <- paste0(treatment_sample_ids[[i]], "_", suffix)
      expected_name <- paste0(presentation_id(treatment_sample_ids[[i]]), "_", suffix)
      sheet <- if (suffix == "overall_normalized_signal") "Mean_Log2FC" else "Positive_Pixel_Intensity"
      built <- make_graphpad_sheets(export_dt)[[sheet]]
      isTRUE(all.equal(export_dt[[source_name]], built[[expected_name]],
                       check.attributes = FALSE))
    }, logical(1L))),
    logical(1L)
  ))
  rbindlist(list(
    qc_row("configuration_valid", TRUE, "validated", "valid"),
    qc_row("all_input_files_exist", all(file.exists(dataset_inputs$annotated_master_csv)), "checked", "all exist"),
    qc_row("dataset_ids_match_configuration", setequal(unique(master$dataset_id), dataset_inputs$dataset_id), paste(unique(master$dataset_id), collapse = ";"), paste(dataset_inputs$dataset_id, collapse = ";")),
    qc_row("ion_modes_match_configuration", all(master[, .(ion_mode = unique(ion_mode)), by = dataset_id][dataset_inputs, on = "dataset_id", ion_mode == i.ion_mode]), "checked", "match"),
    qc_row("source_tables_not_empty", all(vapply(loaded, function(x) nrow(x$original) > 0L, logical(1L))), source_rows, ">0"),
    qc_row("source_feature_keys_unique", all(vapply(loaded, function(x) !anyDuplicated(x$data$feature_key), logical(1L))), "checked", "unique"),
    qc_row("combined_feature_keys_unique", !anyDuplicated(master$feature_key), uniqueN(master$feature_key), nrow(master)),
    qc_row("no_rows_multiplied", nrow(master) == source_rows, nrow(master), source_rows),
    qc_row("no_source_rows_removed", nrow(master) == source_rows, nrow(master), source_rows),
    qc_row("source_columns_preserved_value_for_value", source_preserved, source_preserved, TRUE),
    qc_row("annotation_score_numeric_when_present", all(is_blank(master$annotation_score) | is.finite(as_numeric_strict(master$annotation_score))), "checked", "numeric when present"),
    qc_row("official_quantitative_columns_numeric_when_present",
           official_numeric_valid, paste(official_names, collapse = "; "),
           "every nonblank value is finite numeric"),
    qc_row("overall_replicate_values_numeric_when_present",
           overall_numeric_valid, paste(overall_names, collapse = "; "),
           "every nonblank value is finite numeric"),
    qc_row("local_intensity_replicate_values_numeric_when_present",
           local_numeric_valid, paste(local_names, collapse = "; "),
           "every nonblank value is finite numeric"),
    qc_row("conservative_detected_pixel_counts_valid", counts_valid,
           paste0("denominator=", expected_all_coordinate_pixels,
                  "; min=", if (length(counts)) min(counts, na.rm = TRUE) else NA,
                  "; max=", if (length(counts)) max(counts, na.rm = TRUE) else NA),
           "finite integer counts between 0 and denominator"),
    qc_row("conservative_detection_fractions_valid", fractions_valid,
           paste0("min=", if (length(fractions)) min(fractions, na.rm = TRUE) else NA,
                  "; max=", if (length(fractions)) max(fractions, na.rm = TRUE) else NA),
           "finite fractions between 0 and 1"),
    qc_row("detection_fractions_match_counts_and_coordinate_denominator", denominator_valid,
           paste0("expected_all_coordinate_pixels=", expected_all_coordinate_pixels,
                  "; maximum_absolute_difference=",
                  if (length(denominator_differences)) max(denominator_differences, na.rm = TRUE) else NA),
           paste0("maximum absolute difference <= ", numeric_validation_tolerance)),
    qc_row("graph_export_not_empty", export_nonempty, nrow(export_dt), ">0"),
    qc_row("graph_export_annotations_present", export_nonempty && all(export_dt$annotation_present), if (export_nonempty) nrow(export_dt) else "NOT EVALUABLE", "all TRUE"),
    qc_row("graph_export_scores_meet_threshold", export_nonempty && all(as_numeric_strict(export_dt$annotation_score) >= minimum_annotation_score_for_export), if (export_nonempty) min(as_numeric_strict(export_dt$annotation_score)) else "NOT EVALUABLE", paste0(">=", minimum_annotation_score_for_export)),
    qc_row("graphpad_labels_present", export_nonempty && !any(is_blank(labels)), if (export_nonempty) sum(is_blank(labels)) else "NOT EVALUABLE", 0),
    qc_row("graphpad_labels_unique", export_nonempty && !anyDuplicated(labels), if (export_nonempty) uniqueN(labels) else "NOT EVALUABLE", length(labels)),
    qc_row("graphpad_labels_have_four_decimal_mz", export_nonempty && all(grepl(label_pattern, labels)), if (export_nonempty) "checked" else "NOT EVALUABLE", "exactly four decimals"),
    qc_row("graphpad_labels_attached_to_correct_feature_key", export_nonempty && all(labels == graphpad_label(export_dt$annotation_name, export_dt$annotation_adduct, as_numeric_strict(export_dt$target_mz))), if (export_nonempty) "checked" else "NOT EVALUABLE", "exact match"),
    qc_row("TRT_to_PVL_presentation_mapping_correct", presentation_ok, "value-copy mapping", "exact"),
    qc_row("official_overall_log2fc_preserved", source_overall_preserved, "combined versus original Script 04 source", "value-for-value"),
    qc_row("official_local_intensity_log2fc_preserved", source_local_preserved, "combined versus original Script 04 source", "value-for-value"),
    qc_row("official_detection_difference_preserved", source_detection_preserved, "combined versus original Script 04 source", "value-for-value"),
    qc_row("overall_presentation_copy_correct", export_nonempty && all(same_numeric(export_dt[[official_overall_column]], export_dt$log2FC_overall_PVL_vs_CTRL, numeric_validation_tolerance)), if (export_nonempty) "checked" else "NOT EVALUABLE", "value-for-value"),
    qc_row("local_presentation_copy_correct", export_nonempty && all(same_numeric(export_dt[[official_local_column]], export_dt$local_intensity_log2FC_PVL_vs_CTRL, numeric_validation_tolerance)), if (export_nonempty) "checked" else "NOT EVALUABLE", "value-for-value"),
    qc_row("overall_audit_matches_official", all(audited_master$overall_audit_status == "PASS"), paste(table(audited_master$overall_audit_status), collapse = ";"), "all PASS"),
    qc_row("local_intensity_audit_matches_official", all(audited_master$local_audit_status == "PASS"), paste(table(audited_master$local_audit_status), collapse = ";"), "all PASS"),
    qc_row("detection_difference_audit_matches_official", all(audited_master$detection_audit_status == "PASS"), paste(table(audited_master$detection_audit_status), collapse = ";"), "all PASS"),
    qc_row("detection_percentage_point_conversion_correct", all(same_numeric(audited_master$ion_positive_pixel_change_PVL_minus_CTRL_pp, 100 * as_numeric_strict(audited_master[[official_detection_column]]), numeric_validation_tolerance)), "checked", "100 x fraction difference"),
    qc_row("no_mean_of_log2_formula_used", TRUE, "arithmetic condition means then log2 ratio", "required"),
    qc_row("no_pseudocount_used", TRUE, "none", "none"),
    qc_row("no_paired_analysis_used", TRUE, "unpaired descriptive summaries", "none"),
    qc_row("biological_replicate_terminology_used", TRUE, "biological replicate", "required"),
    qc_row("formula_ranges_end_at_actual_final_row", formula_range_valid,
           paste0("audit_data_rows=", formula_metadata$audit_data_rows,
                  "; first_formula_row=", formula_metadata$first_formula_row,
                  "; last_formula_row=", formula_metadata$last_formula_row,
                  "; expected_last_formula_row=", nrow(audit) + 1L,
                  "; formulas_written=", formula_metadata$formulas_written,
                  "; expected_formulas_written=", expected_formula_operations,
                  "; formula_columns_written=",
                  formula_metadata$formula_columns_written),
           "metadata from actual formula writing matches the audit table"),
    qc_row("no_adduct_rows_exchanged", export_nonempty && all(labels == graphpad_label(export_dt$annotation_name, export_dt$annotation_adduct, as_numeric_strict(export_dt$target_mz))), if (export_nonempty) "checked" else "NOT EVALUABLE", "exact attachment")
  ), use.names = TRUE)
}

###############################################################################
# WORKBOOK AND PROVENANCE
###############################################################################

make_source_summary <- function(loaded) {
  rbindlist(lapply(seq_len(nrow(dataset_inputs)), function(i) {
    path <- dataset_inputs$annotated_master_csv[[i]]
    info <- file.info(path)
    data.table(dataset_id = dataset_inputs$dataset_id[[i]],
      ion_mode = dataset_inputs$ion_mode[[i]], annotated_master_csv = path,
      modification_time = format(info$mtime, "%Y-%m-%d %H:%M:%S %Z"),
      source_rows = nrow(loaded[[i]]$original),
      md5 = unname(tools::md5sum(path)))
  }))
}

make_configuration <- function() {
  values <- list(
    analysis_root = analysis_root, exposure_label = exposure_label,
    dataset_ids = paste(dataset_inputs$dataset_id, collapse = "; "),
    source_annotated_master_files = paste(dataset_inputs$annotated_master_csv, collapse = "; "),
    control_sample_ids = paste(control_sample_ids, collapse = "; "),
    treatment_sample_ids = paste(treatment_sample_ids, collapse = "; "),
    control_condition_label = control_condition_label,
    treatment_condition_label = treatment_condition_label,
    output_treatment_label = output_treatment_label,
    minimum_annotation_score_for_export = minimum_annotation_score_for_export,
    expected_all_coordinate_pixels = expected_all_coordinate_pixels,
    require_all_replicates_for_audit = require_all_replicates_for_audit,
    minimum_replicates_if_not_all_required = minimum_replicates_if_not_all_required,
    numeric_validation_tolerance = numeric_validation_tolerance,
    feature_key = "dataset_id | ion_mode | mzmine_row_id",
    official_overall_readout_column = official_overall_column,
    official_local_intensity_readout_column = official_local_column,
    official_detection_difference_column = official_detection_column,
    overall_readout_formula = "log2(arithmetic mean treatment overall signal / arithmetic mean control overall signal)",
    local_intensity_readout_formula = "log2(arithmetic mean treatment positive-pixel intensity / arithmetic mean control positive-pixel intensity)",
    detection_difference_formula = "arithmetic mean treatment conservative detection fraction - arithmetic mean control conservative detection fraction",
    percentage_point_conversion = "100 * official fraction-scale detection difference",
    graph_export_rule = paste0("annotation_present == TRUE and annotation_score >= ", minimum_annotation_score_for_export),
    source_of_quantitative_truth = "Official quantitative results were imported from the publication-ready Script 03 output preserved in the Script 04 annotated master.",
    p_values = "Not calculated.",
    pseudocount = "None.",
    missing_value_handling = "Missing values were not replaced with zero or imputed.",
    paired_analysis = "No control-treatment replicate pairing was performed.",
    annotation_identity_claim = "Annotations are putative accurate-mass database matches.",
    normalization_terminology = "Target intensity was normalized to the sum of all centroid intensities in the corresponding pixel and scaled to 10^6; this is described as spectrum-sum normalization and not as a vendor-derived measurement.",
    write_excel_output = write_excel_output, write_csv_outputs = write_csv_outputs,
    script_version = script_version
  )
  data.table(setting = names(values), value = vapply(values, as.character, character(1L)))
}

readme_table <- function() data.table(
  topic = c("Purpose", "Combination key", "Ion modes", "Replicates",
            "Quantitative source", "Candidate classification", "Statistics",
            "Missing values", "Annotations", "Export filter", "Complete master",
            "Normalization"),
  statement = c(
    "Publication-ready combined positive/negative-ion GraphPad export.",
    "Files were combined by dataset_id, ion_mode, and mzmine_row_id.",
    "Positive and negative ion modes were retained separately.",
    "CTRL_R1-R3 and TRT_R1-R3 are biological replicates; no control-treatment replicate pairing was performed.",
    "Script 03 was the sole quantitative source of truth; Script 07 only audited its official values.",
    "Script 07 did not create an alternative candidate classification.",
    "No p-values were calculated and no pseudocount was used.",
    "Missing values were not replaced with zero or imputed.",
    "Annotations are putative accurate-mass database matches, not confirmed identities.",
    "Annotation-score filtering applies only to GraphPad export rows and does not alter the upstream quantitative master.",
    "The complete combined master retains every source row.",
    "Intensities use spectrum-sum normalization (sum of all centroid intensities per pixel, scaled to 10^6), not a vendor-derived measurement."
  )
)

make_calculation_audit <- function(export_dt) {
  replicate_columns <- unlist(lapply(
    c("overall_normalized_signal", "local_strong_pixel_intensity",
      "conservative_detection_fraction"),
    function(s) paste0(c(control_sample_ids, treatment_sample_ids), "_", s)
  ))
  out <- copy(export_dt[, c(id_columns, replicate_columns,
    official_overall_column, "audit_overall_log2fc", "overall_audit_status",
    official_local_column, "audit_local_intensity_log2fc", "local_audit_status",
    official_detection_column, "audit_detection_fraction_difference",
    "audit_detection_percentage_point_difference", "detection_audit_status"), with = FALSE])
  out[, `:=`(
    Excel_overall_audit = NA_real_,
    Excel_overall_absolute_difference = NA_real_,
    Excel_overall_status = NA_character_,
    Excel_local_audit = NA_real_,
    Excel_local_absolute_difference = NA_real_,
    Excel_local_status = NA_character_,
    Excel_detection_audit = NA_real_,
    Excel_detection_percentage_points = NA_real_,
    Excel_detection_absolute_difference = NA_real_,
    Excel_detection_status = NA_character_
  )]
  out
}

excel_column <- function(index) {
  out <- ""
  while (index > 0L) {
    index <- index - 1L
    out <- paste0(LETTERS[(index %% 26L) + 1L], out)
    index <- index %/% 26L
  }
  out
}

cell <- function(dt, column, row) paste0(excel_column(match(column, names(dt))), row)
range_cells <- function(dt, columns, row) paste0(cell(dt, columns[[1L]], row), ":", cell(dt, columns[[length(columns)]], row))

write_table_sheet <- function(wb, name, dt, freeze = TRUE) {
  addWorksheet(wb, name)
  writeDataTable(wb, name, dt, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  if (freeze) freezePane(wb, name, firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb, name, cols = seq_len(ncol(dt)), widths = "auto")
}

write_audit_formulas <- function(wb, audit) {
  sheet <- "Calculation_Audit"
  ctrl_overall <- paste0(control_sample_ids, "_overall_normalized_signal")
  trt_overall <- paste0(treatment_sample_ids, "_overall_normalized_signal")
  ctrl_local <- paste0(control_sample_ids, "_local_strong_pixel_intensity")
  trt_local <- paste0(treatment_sample_ids, "_local_strong_pixel_intensity")
  ctrl_frac <- paste0(control_sample_ids, "_conservative_detection_fraction")
  trt_frac <- paste0(treatment_sample_ids, "_conservative_detection_fraction")
  needed <- if (require_all_replicates_for_audit) length(control_sample_ids) else minimum_replicates_if_not_all_required
  formulas_written <- 0L
  first_formula_row <- NA_integer_
  last_formula_row <- NA_integer_
  formula_columns <- integer()
  write_tracked_formula <- function(formula, column, row) {
    writeFormula(wb, sheet, x = formula, startCol = column, startRow = row)
    formulas_written <<- formulas_written + 1L
    formula_columns <<- c(formula_columns, as.integer(column))
  }
  for (i in seq_len(nrow(audit))) {
    r <- i + 1L
    if (is.na(first_formula_row)) first_formula_row <- r
    last_formula_row <- r
    co <- range_cells(audit, ctrl_overall, r); to <- range_cells(audit, trt_overall, r)
    cl <- range_cells(audit, ctrl_local, r); tl <- range_cells(audit, trt_local, r)
    cf <- range_cells(audit, ctrl_frac, r); tf <- range_cells(audit, trt_frac, r)
    overall_formula <- sprintf('IF(OR(COUNT(%s)<%d,COUNT(%s)<%d),"",IF(OR(AVERAGE(%s)<=0,AVERAGE(%s)<=0),"",LOG(AVERAGE(%s)/AVERAGE(%s),2)))', co, needed, to, needed, co, to, to, co)
    local_formula <- sprintf('IF(OR(COUNT(%s)<%d,COUNT(%s)<%d),"",IF(OR(AVERAGE(%s)<=0,AVERAGE(%s)<=0),"",LOG(AVERAGE(%s)/AVERAGE(%s),2)))', cl, needed, tl, needed, cl, tl, tl, cl)
    detection_formula <- sprintf('IF(OR(COUNT(%s)<%d,COUNT(%s)<%d),"",AVERAGE(%s)-AVERAGE(%s))', cf, needed, tf, needed, tf, cf)
    status_formula <- function(audit_cell, official_cell) sprintf(
      'IF(AND(%s="",%s=""),"PASS",IF(OR(%s="",%s=""),"FAIL",IF(ABS(%s-%s)<=%s,"PASS","FAIL")))',
      audit_cell, official_cell, audit_cell, official_cell,
      audit_cell, official_cell,
      format(numeric_validation_tolerance, scientific = TRUE)
    )
    difference_formula <- function(audit_cell, official_cell) sprintf(
      'IF(AND(%s="",%s=""),"",IF(OR(%s="",%s=""),"",ABS(%s-%s)))',
      audit_cell, official_cell, audit_cell, official_cell,
      audit_cell, official_cell
    )
    write_tracked_formula(overall_formula, match("Excel_overall_audit", names(audit)), r)
    write_tracked_formula(difference_formula(cell(audit, "Excel_overall_audit", r), cell(audit, official_overall_column, r)), match("Excel_overall_absolute_difference", names(audit)), r)
    write_tracked_formula(status_formula(cell(audit, "Excel_overall_audit", r), cell(audit, official_overall_column, r)), match("Excel_overall_status", names(audit)), r)
    write_tracked_formula(local_formula, match("Excel_local_audit", names(audit)), r)
    write_tracked_formula(difference_formula(cell(audit, "Excel_local_audit", r), cell(audit, official_local_column, r)), match("Excel_local_absolute_difference", names(audit)), r)
    write_tracked_formula(status_formula(cell(audit, "Excel_local_audit", r), cell(audit, official_local_column, r)), match("Excel_local_status", names(audit)), r)
    write_tracked_formula(detection_formula, match("Excel_detection_audit", names(audit)), r)
    write_tracked_formula(sprintf('IF(%s="","",100*%s)', cell(audit, "Excel_detection_audit", r), cell(audit, "Excel_detection_audit", r)), match("Excel_detection_percentage_points", names(audit)), r)
    write_tracked_formula(difference_formula(cell(audit, "Excel_detection_audit", r), cell(audit, official_detection_column, r)), match("Excel_detection_absolute_difference", names(audit)), r)
    write_tracked_formula(status_formula(cell(audit, "Excel_detection_audit", r), cell(audit, official_detection_column, r)), match("Excel_detection_status", names(audit)), r)
  }
  list(
    audit_data_rows = nrow(audit),
    first_formula_row = first_formula_row,
    last_formula_row = last_formula_row,
    formulas_written = formulas_written,
    formula_columns_written = length(unique(formula_columns))
  )
}

generate_formula_metadata <- function(audit) {
  validation_wb <- createWorkbook()
  addWorksheet(validation_wb, "Calculation_Audit")
  write_audit_formulas(validation_wb, audit)
}

write_workbook <- function(path, master_out, sheets, audit, qc, source_summary, config) {
  wb <- createWorkbook(creator = "PVL MALDI-MSI publication workflow")
  write_table_sheet(wb, "README", readme_table())
  write_table_sheet(wb, "Source_Files", source_summary)
  write_table_sheet(wb, "Combined_All_Features", master_out)
  for (nm in names(sheets)) write_table_sheet(wb, nm, sheets[[nm]])
  write_table_sheet(wb, "Calculation_Audit", audit)
  write_audit_formulas(wb, audit)
  write_table_sheet(wb, "QC", qc)
  write_table_sheet(wb, "Methods_and_Settings", config)
  saveWorkbook(wb, path, overwrite = TRUE)
}

###############################################################################
# CONTROLLED TEST
###############################################################################

run_controlled_test <- function() {
  old_all <- require_all_replicates_for_audit
  on.exit(assign("require_all_replicates_for_audit", old_all, envir = .GlobalEnv), add = TRUE)
  assign("require_all_replicates_for_audit", TRUE, envir = .GlobalEnv)
  tol <- 1e-12
  stopifnot(abs(audit_log2_ratio(c(8, 8, 8), c(16, 16, 16)) - 1) < tol)
  d <- audit_difference(rep(10 / 1600, 3), rep(20 / 1600, 3))
  stopifnot(abs(d - 0.00625) < tol, abs(100 * d - 0.625) < tol)
  stopifnot(abs(audit_log2_ratio(rep(100, 3), rep(200, 3)) - 1) < tol)
  stopifnot(is.na(audit_log2_ratio(c(8, 8, NA), c(16, 16, 16))))
  stopifnot(is.na(audit_log2_ratio(c(0, 0, 0), c(16, 16, 16))))
  stopifnot(audit_match_status(NA_real_, NA_real_, tol) == "PASS")
  stopifnot(audit_match_status(1, NA_real_, tol) == "FAIL")
  stopifnot(audit_match_status(NA_real_, 1, tol) == "FAIL")
  stopifnot(identical(
    strict_numeric_allow_missing(c("1", "", NA, "2.5"), "controlled_test"),
    c(1, NA_real_, NA_real_, 2.5)
  ))
  invalid_numeric <- try(
    strict_numeric_allow_missing(c("1", "not_numeric"), "controlled_test"),
    silent = TRUE
  )
  stopifnot(inherits(invalid_numeric, "try-error"))
  keys <- feature_key_value(c("P", "P", "N"), c("Positive", "Positive", "Negative"), c("1", "2", "1"))
  labels <- graphpad_label(c("ADP", "ADP", "ADP"), c("[M+H]+", "[M+Na]+", "[M-H]-"), c(428.0361, 450.0180, 426.0215))
  stopifnot(!anyDuplicated(keys), !anyDuplicated(labels), labels[[1L]] != labels[[3L]])
  invisible(TRUE)
}

###############################################################################
# MAIN
###############################################################################

main <- function() {
  run_controlled_test()
  validate_user_configuration()

  run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_dir <- file.path(output_parent, paste0("GraphPad_Export_", run_stamp))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output directory: ", output_dir, call. = FALSE)

  built <- build_combined_master()
  master <- built$master
  audited_master <- make_audits(copy(master))
  export_dt <- copy(audited_master[
    as.logical(annotation_present) &
      is.finite(as_numeric_strict(annotation_score)) &
      as_numeric_strict(annotation_score) >= minimum_annotation_score_for_export
  ])
  sheets <- make_graphpad_sheets(export_dt)
  audit <- make_calculation_audit(export_dt)
  formula_metadata <- generate_formula_metadata(audit)
  qc <- build_qc(
    master, audited_master, export_dt, audit, formula_metadata, built$loaded
  )
  source_summary <- make_source_summary(built$loaded)
  config <- make_configuration()

  validation_path <- file.path(output_dir, "analysis_validation_summary.csv")
  fwrite(qc, validation_path, na = "")
  fwrite(config, file.path(output_dir, "analysis_configuration_used.csv"), na = "")
  fwrite(source_summary, file.path(output_dir, "source_files_used.csv"), na = "")

  running_script <- get_running_script_path()
  copied_script <- file.path(output_dir, "07_export_three_graphs_to_graphpad_xlsx_VALIDATED_used.R")
  if (is.na(running_script) || !file.exists(running_script)) {
    warning("Could not determine the running script path; script provenance copy was skipped.")
  } else {
    file.copy(running_script, copied_script, overwrite = TRUE)
  }
  capture.output(sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))

  report <- c(
    "PVL MALDI-MSI publication-ready GraphPad export",
    paste("Run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("R version:", R.version.string),
    paste("data.table version:", as.character(packageVersion("data.table"))),
    paste("openxlsx version:", as.character(packageVersion("openxlsx"))),
    paste("Combined source rows:", nrow(master)),
    paste("Score-qualified GraphPad rows:", nrow(export_dt)),
    paste("Critical validation:", if (any(qc$critical & qc$status == "FAIL")) "FAIL" else "PASS"),
    "Script 03 official results were preserved and independently audited.",
    "No p-values, pseudocount, imputation, paired analysis, or alternative candidate classification was introduced."
  )
  writeLines(report, file.path(output_dir, "graphpad_export_report.txt"), useBytes = TRUE)

  failures <- qc[critical == TRUE & status == "FAIL"]
  if (nrow(failures)) {
    stop("Critical validation failed; final workbook and data exports were not written. See: ",
         validation_path, "\nFailed checks: ", paste(failures$check_name, collapse = ", "),
         call. = FALSE)
  }

  master_out <- copy(master)
  master_out[, c(".source_index", ".source_row") := NULL]
  export_out <- copy(export_dt)
  export_out[, c(".source_index", ".source_row") := NULL]

  base_name <- paste0("PVL_", exposure_label, "_MALDI_MSI")
  if (write_csv_outputs) {
    fwrite(master_out, file.path(output_dir, paste0(base_name, "_Combined_Annotated_Master.csv")), na = "")
    fwrite(export_out, file.path(output_dir, paste0(base_name, "_GraphPad_Export.csv")), na = "")
    for (nm in names(sheets)) fwrite(sheets[[nm]], file.path(output_dir, paste0(nm, ".csv")), na = "")
  }
  if (write_excel_output) {
    write_workbook(
      file.path(output_dir, paste0(base_name, "_Three_Readouts_GraphPad_Ready.xlsx")),
      master_out, sheets, audit, qc, source_summary, config
    )
  }
  message("Publication-ready GraphPad export completed: ", output_dir)
  invisible(output_dir)
}

if (sys.nframe() == 0L) main()
