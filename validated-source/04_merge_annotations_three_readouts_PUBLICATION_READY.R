###############################################################################
# Merge MALDI-MSI three-readout results with aligned MZmine annotations
###############################################################################

options(stringsAsFactors = FALSE)

###############################################################################
# USER CONFIGURATION PANEL
###############################################################################

dataset_id <- "PVL_Single_Negative"

quantitative_csv <-
  "C:/MALDI-MSI/PVL_MSI_Analysis/03_Replicate_Summaries/PVL_Single_Negative/ThreeReadout_ReplicateSummary_YYYYMMDD_HHMMSS/PVL_Single_Negative_ThreeReadout_Summary.csv"

annotation_csv <-
  "C:/MALDI-MSI/Single Exposure/Negative Mode/MZMINE output/PVL_Single_Negative_40um_Aligned_Annotated_Feature_List.csv"

analysis_root <- "C:/MALDI-MSI/PVL_MSI_Analysis"

output_parent <- file.path(
  analysis_root,
  "04_Annotation_Merge",
  dataset_id
)

expected_ion_mode <- "Negative"

control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "PVL"

accepted_adducts <- c("[M-H]-")

mz_alignment_tolerance_da <- 0.00001
annotation_score_consistency_tolerance <- 0.000001

annotation_score_thresholds <- c(
  0.6,
  0.7,
  0.8
)

minimum_annotation_score_for_high_priority <- 0.6

supporting_local_abs_log2fc_threshold <- 1
supporting_detection_fraction_difference_threshold <- 0.10

write_excel_output <- TRUE

###############################################################################

required_packages <- c("data.table", "openxlsx")

state <- new.env(parent = emptyenv())
state$warnings <- character()

log_message <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", paste(...))
}

add_warning <- function(...) {
  text <- paste(...)
  state$warnings <- c(state$warnings, text)
  warning(text, call. = FALSE)
}

stop_critical <- function(...) {
  stop(paste(...), call. = FALSE)
}

###############################################################################
# CONFIGURATION AND PACKAGE VALIDATION
###############################################################################

infer_dataset_ion_mode <- function(dataset_id_value) {
  if (grepl("_Positive$", dataset_id_value, ignore.case = TRUE)) {
    return("Positive")
  }
  if (grepl("_Negative$", dataset_id_value, ignore.case = TRUE)) {
    return("Negative")
  }
  NA_character_
}

validate_user_configuration <- function() {
  scalar_text <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  scalar_number <- function(x, minimum = 0) {
    is.numeric(x) && length(x) == 1L && !is.na(x) &&
      is.finite(x) && x >= minimum
  }
  if (!scalar_text(dataset_id)) {
    stop_critical("Configuration error: dataset_id must be nonblank.")
  }
  if (!scalar_text(quantitative_csv) || !file.exists(quantitative_csv)) {
    stop_critical(
      "Configuration error: quantitative_csv does not exist:",
      quantitative_csv
    )
  }
  if (!scalar_text(annotation_csv) || !file.exists(annotation_csv)) {
    stop_critical(
      "Configuration error: annotation_csv does not exist:",
      annotation_csv
    )
  }
  if (!scalar_text(analysis_root) || !scalar_text(output_parent)) {
    stop_critical(
      "Configuration error: analysis_root and output_parent must be nonblank."
    )
  }
  if (!scalar_text(expected_ion_mode) ||
      !expected_ion_mode %in% c("Positive", "Negative")) {
    stop_critical(
      "Configuration error: expected_ion_mode must be Positive or Negative."
    )
  }
  inferred_ion_mode <- infer_dataset_ion_mode(dataset_id)
  if (is.na(inferred_ion_mode)) {
    stop_critical(
      "Configuration error: ion mode cannot be inferred from dataset_id."
    )
  }
  if (!identical(inferred_ion_mode, expected_ion_mode)) {
    stop_critical(
      paste(
        "Configuration error: expected_ion_mode does not match the ion mode",
        "encoded in dataset_id."
      )
    )
  }
  if (!scalar_text(control_condition_label) ||
      !scalar_text(treatment_condition_label) ||
      identical(control_condition_label, treatment_condition_label)) {
    stop_critical(
      "Configuration error: condition labels must be nonblank and different."
    )
  }
  if (!scalar_text(output_treatment_label)) {
    stop_critical("Configuration error: output_treatment_label must be nonblank.")
  }
  if (!is.character(accepted_adducts) || !length(accepted_adducts) ||
      anyNA(accepted_adducts) || any(!nzchar(trimws(accepted_adducts))) ||
      anyDuplicated(trimws(accepted_adducts))) {
    stop_critical(
      "Configuration error: accepted_adducts must contain unique nonblank values."
    )
  }
  if (!scalar_number(mz_alignment_tolerance_da) ||
      !scalar_number(annotation_score_consistency_tolerance)) {
    stop_critical(
      "Configuration error: mass and score tolerances must be finite and nonnegative."
    )
  }
  if (!is.numeric(annotation_score_thresholds) ||
      !length(annotation_score_thresholds) ||
      anyNA(annotation_score_thresholds) ||
      any(!is.finite(annotation_score_thresholds)) ||
      any(annotation_score_thresholds < 0) ||
      anyDuplicated(annotation_score_thresholds) ||
      is.unsorted(annotation_score_thresholds, strictly = TRUE)) {
    stop_critical(
      "Configuration error: annotation_score_thresholds must be unique, finite, nonnegative, and strictly increasing."
    )
  }
  if (!scalar_number(minimum_annotation_score_for_high_priority) ||
      !minimum_annotation_score_for_high_priority %in%
        annotation_score_thresholds) {
    stop_critical(
      "Configuration error: the high-priority score must occur in annotation_score_thresholds."
    )
  }
  if (!scalar_number(supporting_local_abs_log2fc_threshold) ||
      !scalar_number(
        supporting_detection_fraction_difference_threshold
      )) {
    stop_critical(
      "Configuration error: supporting-readout thresholds must be finite and nonnegative."
    )
  }
  if (!is.logical(write_excel_output) || length(write_excel_output) != 1L ||
      is.na(write_excel_output)) {
    stop_critical(
      "Configuration error: write_excel_output must be a single TRUE or FALSE."
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
  output_dir <- file.path(
    output_parent,
    paste0("AnnotationMerge_", format(Sys.time(), "%Y%m%d_%H%M%S"))
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

###############################################################################
# SHARED PARSERS AND COLUMN NAMES
###############################################################################

is_present <- function(values) {
  !is.na(values) & nzchar(trimws(as.character(values)))
}

strict_logical <- function(values, column_name) {
  if (is.logical(values)) return(values)
  raw <- trimws(toupper(as.character(values)))
  missing <- is.na(values) | raw == ""
  result <- rep(NA, length(values))
  result[raw %in% c("TRUE", "T", "1", "YES")] <- TRUE
  result[raw %in% c("FALSE", "F", "0", "NO")] <- FALSE
  invalid <- !missing & is.na(result)
  if (any(invalid)) {
    stop_critical(
      "Column",
      column_name,
      "contains unrecognized logical values:",
      paste(unique(raw[invalid]), collapse = ", ")
    )
  }
  result
}

strict_numeric <- function(values, column_name, allow_missing = TRUE) {
  present <- is_present(values)
  numeric_values <- suppressWarnings(as.numeric(values))
  invalid <- present & !is.finite(numeric_values)
  if (any(invalid)) {
    stop_critical(
      "Column",
      column_name,
      "contains nonnumeric or nonfinite values."
    )
  }
  if (!allow_missing && any(!present)) {
    stop_critical("Column", column_name, "contains missing values.")
  }
  numeric_values
}

quantitative_column_names <- function() {
  list(
    overall_log2fc = paste0(
      "log2FC_overall_", treatment_condition_label,
      "_vs_", control_condition_label
    ),
    local_log2fc = paste0(
      "local_intensity_log2FC_", treatment_condition_label,
      "_vs_", control_condition_label
    ),
    detection_difference = paste0(
      "detection_fraction_difference_", treatment_condition_label,
      "_minus_", control_condition_label
    ),
    control_cv = paste0(
      "cv_", control_condition_label, "_overall_signal_percent"
    ),
    treatment_cv = paste0(
      "cv_", treatment_condition_label, "_overall_signal_percent"
    ),
    control_supported = paste0(
      control_condition_label, "_supported_replicates"
    ),
    treatment_supported = paste0(
      treatment_condition_label, "_supported_replicates"
    )
  )
}

legacy_variability_from_replicate <- function(values) {
  mapping <- c(
    high_CV_in_both_groups = "high_CV_both",
    high_control_group_CV = "high_CTRL_CV",
    high_treatment_group_CV = "high_TRT_CV",
    unavailable = "unavailable",
    reasonable = "reasonable"
  )
  result <- unname(mapping[values])
  invalid <- !is.na(values) & is.na(result)
  if (any(invalid)) {
    stop_critical(
      "replicate_variability_flag contains unexpected values:",
      paste(unique(values[invalid]), collapse = ", ")
    )
  }
  result
}

score_threshold_column <- function(threshold) {
  label <- format(threshold, scientific = FALSE, trim = TRUE)
  label <- gsub("-", "minus_", label, fixed = TRUE)
  label <- gsub(".", "_", label, fixed = TRUE)
  paste0("annotation_score_ge_", label)
}

###############################################################################
# STRICT INPUT VALIDATION
###############################################################################

read_and_validate_quantitative <- function() {
  log_message("Reading quantitative summary:", quantitative_csv)
  quantitative <- data.table::fread(
    quantitative_csv,
    na.strings = c("", "NA", "NaN"),
    showProgress = FALSE
  )
  if (!nrow(quantitative)) stop_critical("Quantitative input is empty.")
  dynamic <- quantitative_column_names()
  required <- c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    "preliminary_quantitative_candidate", "overall_direction",
    "overall_abs_log2FC_meets_candidate_threshold",
    dynamic$overall_log2fc, dynamic$local_log2fc,
    dynamic$detection_difference, "replicate_variability_flag",
    dynamic$control_cv, dynamic$treatment_cv,
    dynamic$control_supported, dynamic$treatment_supported,
    "strong_signal_support_pass", "strong_signal_support_category"
  )
  missing <- setdiff(required, names(quantitative))
  if (length(missing)) {
    stop_critical(
      "Quantitative table is missing columns:",
      paste(missing, collapse = ", ")
    )
  }
  numeric_columns_allowing_missing <- c(
    dynamic$overall_log2fc,
    dynamic$local_log2fc,
    dynamic$detection_difference,
    dynamic$control_cv,
    dynamic$treatment_cv
  )
  for (column in numeric_columns_allowing_missing) {
    quantitative[, (column) := strict_numeric(
      get(column),
      column,
      allow_missing = TRUE
    )]
  }
  supported_replicate_columns <- c(
    dynamic$control_supported,
    dynamic$treatment_supported
  )
  for (column in supported_replicate_columns) {
    quantitative[, (column) := strict_numeric(
      get(column),
      column,
      allow_missing = FALSE
    )]
    values <- quantitative[[column]]
    if (any(values < 0) || any(values != floor(values))) {
      stop_critical(
        "Column",
        column,
        "must contain nonnegative integer replicate counts."
      )
    }
  }
  quantitative[, dataset_id := as.character(dataset_id)]
  quantitative[, target_id := as.character(target_id)]
  quantitative[, mzmine_row_id := as.character(mzmine_row_id)]
  observed_ids <- unique(
    quantitative$dataset_id[is_present(quantitative$dataset_id)]
  )
  if (length(observed_ids) != 1L ||
      any(!is_present(quantitative$dataset_id))) {
    stop_critical(
      "Quantitative input must contain exactly one nonmissing dataset ID."
    )
  }
  if (!identical(observed_ids, dataset_id)) {
    stop_critical(
      "Configured dataset_id does not match quantitative input. Configured:",
      dataset_id,
      "Observed:",
      observed_ids
    )
  }
  if (any(!is_present(quantitative$target_id)) ||
      any(!is_present(quantitative$mzmine_row_id))) {
    stop_critical(
      "Quantitative target_id and mzmine_row_id values must be nonblank."
    )
  }
  quantitative[, target_mz := strict_numeric(
    target_mz, "target_mz", allow_missing = FALSE
  )]
  if (any(quantitative$target_mz <= 0)) {
    stop_critical("Quantitative target_mz values must be positive.")
  }
  if (anyDuplicated(quantitative$target_id)) {
    stop_critical("Quantitative target_id values are not globally unique.")
  }
  if (anyDuplicated(quantitative$mzmine_row_id)) {
    stop_critical(
      "Quantitative mzmine_row_id values are not globally unique."
    )
  }
  key_check <- quantitative[, .(
    row_ids = data.table::uniqueN(mzmine_row_id),
    masses = data.table::uniqueN(signif(target_mz, 12))
  ), by = target_id]
  reverse_key_check <- quantitative[
    , .(targets = data.table::uniqueN(target_id)),
    by = mzmine_row_id
  ]
  if (any(key_check$row_ids != 1L) ||
      any(reverse_key_check$targets != 1L) ||
      any(key_check$masses != 1L)) {
    stop_critical("Quantitative target keys are not one-to-one and consistent.")
  }
  quantitative[, preliminary_quantitative_candidate := strict_logical(
    preliminary_quantitative_candidate,
    "preliminary_quantitative_candidate"
  )]
  if (anyNA(quantitative$preliminary_quantitative_candidate)) {
    stop_critical(
      "preliminary_quantitative_candidate contains missing values."
    )
  }
  quantitative[, overall_abs_log2FC_meets_candidate_threshold :=
    strict_logical(
      overall_abs_log2FC_meets_candidate_threshold,
      "overall_abs_log2FC_meets_candidate_threshold"
    )]
  quantitative[, strong_signal_support_pass := strict_logical(
    strong_signal_support_pass,
    "strong_signal_support_pass"
  )]
  if (anyNA(quantitative$overall_abs_log2FC_meets_candidate_threshold) ||
      anyNA(quantitative$strong_signal_support_pass)) {
    stop_critical("Required quantitative logical classifications are missing.")
  }
  quantitative[, replicate_variability_flag :=
    as.character(replicate_variability_flag)]
  expected_legacy <- legacy_variability_from_replicate(
    quantitative$replicate_variability_flag
  )
  if ("technical_variability_flag" %in% names(quantitative)) {
    observed_legacy <- as.character(
      quantitative$technical_variability_flag
    )
    equal <- (is.na(observed_legacy) & is.na(expected_legacy)) |
      (!is.na(observed_legacy) & !is.na(expected_legacy) &
         observed_legacy == expected_legacy)
    if (!all(equal)) {
      stop_critical(
        "technical_variability_flag does not correspond to replicate_variability_flag."
      )
    }
  } else {
    # Compatibility alias for older downstream scripts.
    quantitative[, technical_variability_flag := expected_legacy]
  }
  quantitative[]
}

annotation_required_columns <- function() {
  c(
    "id", "mz", "preferred_annotation:compound_name",
    "preferred_annotation:mol_formula", "preferred_annotation:adduct",
    "preferred_annotation:score", "preferred_annotation:precursor_mz",
    "preferred_annotation:annotation_method",
    "preferred_annotation:comment", "preferred_annotation:neutral_mass",
    "preferred_annotation:inchi", "preferred_annotation:inchi_key",
    "preferred_annotation:isomeric_smiles", "preferred_annotation:smiles",
    "preferred_annotation:classyfire_superclass",
    "preferred_annotation:classyfire_class",
    "preferred_annotation:classyfire_subclass",
    "preferred_annotation:classyfire_parent",
    "preferred_annotation:pubchem_cid",
    "preferred_annotation:compound_annotation_score",
    "preferred_annotation:database_name", "preferred_annotation:mz_diff",
    "preferred_annotation:mz_diff_ppm"
  )
}

read_and_validate_annotation <- function(quantitative) {
  log_message("Reading annotation export:", annotation_csv)
  annotation <- data.table::fread(
    annotation_csv,
    na.strings = c("", "NA", "NaN"),
    showProgress = FALSE
  )
  if (!nrow(annotation)) stop_critical("Annotation input is empty.")
  missing <- setdiff(annotation_required_columns(), names(annotation))
  if (length(missing)) {
    stop_critical(
      "Annotation table is missing columns:",
      paste(missing, collapse = ", ")
    )
  }
  annotation[, id := as.character(id)]
  if (any(!is_present(annotation$id))) {
    stop_critical("Annotation id values must be nonmissing and nonblank.")
  }
  if (anyDuplicated(annotation$id)) {
    stop_critical("Annotation id values are not globally unique.")
  }
  annotation[, mz := strict_numeric(mz, "annotation mz", FALSE)]
  if (any(annotation$mz <= 0)) {
    stop_critical("Annotation mz values must be positive.")
  }
  if (nrow(annotation) != nrow(quantitative)) {
    stop_critical(
      "Annotation and quantitative row counts differ. Annotation:",
      nrow(annotation),
      "Quantitative:",
      nrow(quantitative)
    )
  }
  missing_annotation_ids <- setdiff(
    quantitative$mzmine_row_id, annotation$id
  )
  missing_quantitative_ids <- setdiff(
    annotation$id, quantitative$mzmine_row_id
  )
  if (length(missing_annotation_ids) || length(missing_quantitative_ids)) {
    stop_critical(
      "Annotation and quantitative identifier sets differ. Missing from annotation:",
      paste(missing_annotation_ids, collapse = ", "),
      "Missing from quantitative:",
      paste(missing_quantitative_ids, collapse = ", ")
    )
  }
  if (anyDuplicated(annotation)) {
    stop_critical("Duplicated annotation rows are present.")
  }

  score <- strict_numeric(
    annotation[["preferred_annotation:score"]],
    "preferred_annotation:score"
  )
  compound_score <- strict_numeric(
    annotation[["preferred_annotation:compound_annotation_score"]],
    "preferred_annotation:compound_annotation_score"
  )
  both_scores <- is.finite(score) & is.finite(compound_score)
  score_agreement <- both_scores &
    abs(score - compound_score) <=
      annotation_score_consistency_tolerance
  if (any(both_scores & !score_agreement)) {
    stop_critical(
      "Preferred annotation score columns disagree beyond the configured tolerance."
    )
  }

  adduct <- trimws(
    as.character(annotation[["preferred_annotation:adduct"]])
  )
  adduct[is.na(adduct)] <- ""
  unexpected_adduct_rows <- nzchar(adduct) &
    !adduct %in% accepted_adducts
  unexpected_adduct_values <- unique(
    adduct[unexpected_adduct_rows]
  )
  unexpected_adduct_row_count <- sum(unexpected_adduct_rows)
  if (unexpected_adduct_row_count > 0L) {
    stop_critical(
      "Unexpected nonblank annotation adducts:",
      paste(unexpected_adduct_values, collapse = ", ")
    )
  }

  cleaned <- data.table::data.table(
    mzmine_row_id = annotation$id,
    annotation_mz = annotation$mz,
    annotation_name =
      annotation[["preferred_annotation:compound_name"]],
    annotation_formula =
      annotation[["preferred_annotation:mol_formula"]],
    annotation_adduct = data.table::fifelse(
      nzchar(adduct), adduct, NA_character_
    ),
    annotation_score = score,
    annotation_compound_annotation_score = compound_score,
    annotation_precursor_mz = strict_numeric(
      annotation[["preferred_annotation:precursor_mz"]],
      "preferred_annotation:precursor_mz"
    ),
    annotation_method =
      annotation[["preferred_annotation:annotation_method"]],
    annotation_comment =
      annotation[["preferred_annotation:comment"]],
    annotation_neutral_mass = strict_numeric(
      annotation[["preferred_annotation:neutral_mass"]],
      "preferred_annotation:neutral_mass"
    ),
    annotation_inchi = annotation[["preferred_annotation:inchi"]],
    annotation_inchi_key = annotation[["preferred_annotation:inchi_key"]],
    annotation_isomeric_smiles =
      annotation[["preferred_annotation:isomeric_smiles"]],
    annotation_smiles = annotation[["preferred_annotation:smiles"]],
    annotation_classyfire_superclass =
      annotation[["preferred_annotation:classyfire_superclass"]],
    annotation_classyfire_class =
      annotation[["preferred_annotation:classyfire_class"]],
    annotation_classyfire_subclass =
      annotation[["preferred_annotation:classyfire_subclass"]],
    annotation_classyfire_parent =
      annotation[["preferred_annotation:classyfire_parent"]],
    annotation_pubchem_cid =
      annotation[["preferred_annotation:pubchem_cid"]],
    annotation_database =
      annotation[["preferred_annotation:database_name"]],
    annotation_reported_mz_difference_da = strict_numeric(
      annotation[["preferred_annotation:mz_diff"]],
      "preferred_annotation:mz_diff"
    ),
    annotation_reported_mz_difference_ppm = strict_numeric(
      annotation[["preferred_annotation:mz_diff_ppm"]],
      "preferred_annotation:mz_diff_ppm"
    )
  )
  list(
    raw = annotation,
    cleaned = cleaned,
    scores = list(
      both_present = sum(both_scores),
      both_agree = sum(score_agreement),
      only_one_present = sum(xor(is.finite(score), is.finite(compound_score))),
      neither_present = sum(!is.finite(score) & !is.finite(compound_score)),
      consistent = !any(both_scores & !score_agreement)
    ),
    adducts = list(
      values = adduct,
      blank_count = sum(!nzchar(adduct)),
      unexpected_count = unexpected_adduct_row_count,
      unexpected_values = unexpected_adduct_values
    )
  )
}

###############################################################################
# ONE-TO-ONE MERGE AND DESCRIPTIVE ANNOTATION
###############################################################################

primary_direction_from_upstream <- function(direction) {
  if (is.na(direction) || !nzchar(trimws(direction))) {
    stop_critical("overall_direction contains a missing or blank value.")
  }
  switch(
    direction,
    increased = paste0("increased_after_", output_treatment_label),
    decreased = paste0("decreased_after_", output_treatment_label),
    unchanged_or_small = "small_or_no_overall_change",
    unavailable = "unavailable",
    stop_critical("Unexpected upstream overall_direction:", direction)
  )
}

supporting_pattern <- function(local_log2fc, detection_difference) {
  if (!is.finite(local_log2fc)) return("local_intensity_unavailable")
  if (!is.finite(detection_difference)) {
    return("mixed_or_small_supporting_changes")
  }
  local_up <- local_log2fc >= supporting_local_abs_log2fc_threshold
  local_down <- local_log2fc <= -supporting_local_abs_log2fc_threshold
  coverage_up <- detection_difference >=
    supporting_detection_fraction_difference_threshold
  coverage_down <- detection_difference <=
    -supporting_detection_fraction_difference_threshold
  if (local_up && coverage_up) {
    return("local_intensity_and_coverage_increased")
  }
  if (local_up && coverage_down) {
    return("local_intensity_increased_coverage_decreased")
  }
  if (local_down && coverage_up) {
    return("local_intensity_decreased_coverage_increased")
  }
  if (local_down && coverage_down) {
    return("local_intensity_and_coverage_decreased")
  }
  "mixed_or_small_supporting_changes"
}

merge_and_classify <- function(quantitative, annotation_info) {
  annotation <- annotation_info$cleaned
  annotation_index <- match(
    quantitative$mzmine_row_id,
    annotation$mzmine_row_id
  )
  if (anyNA(annotation_index)) {
    stop_critical("At least one quantitative row lacks an annotation-table row.")
  }
  annotation_columns <- setdiff(names(annotation), "mzmine_row_id")
  merged <- cbind(
    data.table::copy(quantitative),
    annotation[annotation_index, ..annotation_columns]
  )
  if (!identical(merged$mzmine_row_id, quantitative$mzmine_row_id)) {
    stop_critical("Merge changed quantitative row order or identifiers.")
  }
  merged[, annotation_to_target_mz_difference_da :=
    annotation_mz - target_mz]
  merged[, annotation_to_target_mz_difference_ppm :=
    (annotation_to_target_mz_difference_da / target_mz) * 1000000]
  # Compatibility aliases retained for prior consumers.
  merged[, annotation_mz_difference_da :=
    annotation_to_target_mz_difference_da]
  merged[, annotation_mz_difference_ppm :=
    annotation_to_target_mz_difference_ppm]
  merged[, annotation_present := is_present(annotation_name)]

  threshold_columns <- vapply(
    annotation_score_thresholds,
    score_threshold_column,
    character(1)
  )
  for (index in seq_along(annotation_score_thresholds)) {
    threshold <- annotation_score_thresholds[index]
    column <- threshold_columns[index]
    merged[, (column) := annotation_present &
      is.finite(annotation_score) &
      annotation_score >= threshold]
  }

  annotation_note <- paste(
    "Tentative accurate-mass database annotation for",
    expected_ion_mode,
    "ion mode and configured adduct",
    paste(accepted_adducts, collapse = ", "),
    "; identity not confirmed using tandem mass spectrometry or an authentic reference standard."
  )
  merged[, annotation_confidence_note := data.table::fifelse(
    annotation_present, annotation_note, ""
  )]
  merged[, high_priority_annotated_candidate :=
    preliminary_quantitative_candidate &
      annotation_present &
      is.finite(annotation_score) &
      annotation_score >= minimum_annotation_score_for_high_priority]
  merged[, candidate_review_category := data.table::fifelse(
    high_priority_annotated_candidate,
    "high_priority_annotated_candidate",
    data.table::fifelse(
      preliminary_quantitative_candidate & annotation_present,
      "quantitative_candidate_lower_score_annotation",
      data.table::fifelse(
        preliminary_quantitative_candidate & !annotation_present,
        "quantitative_candidate_unannotated",
        data.table::fifelse(
          annotation_present,
          "annotated_not_quantitative_candidate",
          "other_feature"
        )
      )
    )
  )]
  merged[, primary_change_direction := vapply(
    as.character(overall_direction),
    primary_direction_from_upstream,
    character(1)
  )]
  dynamic <- quantitative_column_names()
  merged[, supporting_readout_pattern := mapply(
    supporting_pattern,
    get(dynamic$local_log2fc),
    get(dynamic$detection_difference),
    USE.NAMES = FALSE
  )]

  leading <- c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    "annotation_mz", "annotation_to_target_mz_difference_da",
    "annotation_to_target_mz_difference_ppm",
    "annotation_reported_mz_difference_da",
    "annotation_reported_mz_difference_ppm",
    "annotation_mz_difference_da", "annotation_mz_difference_ppm",
    "annotation_present", "annotation_name", "annotation_formula",
    "annotation_adduct", "annotation_score",
    "annotation_compound_annotation_score", threshold_columns,
    "annotation_confidence_note"
  )
  data.table::setcolorder(
    merged,
    c(leading, setdiff(names(merged), leading))
  )
  list(merged = merged, threshold_columns = threshold_columns)
}

###############################################################################
# VALUE-FOR-VALUE AND CRITICAL OUTPUT VALIDATION
###############################################################################

numeric_equal <- function(observed, expected, tolerance = 1e-12) {
  if (length(observed) != length(expected)) return(FALSE)
  both_missing <- is.na(observed) & is.na(expected)
  both_finite <- is.finite(observed) & is.finite(expected)
  close <- both_finite &
    abs(observed - expected) <=
      tolerance * pmax(1, abs(observed), abs(expected))
  all(both_missing | close)
}

exact_equal_with_missing <- function(observed, expected) {
  if (length(observed) != length(expected)) return(FALSE)
  all(
    (is.na(observed) & is.na(expected)) |
      (!is.na(observed) & !is.na(expected) & observed == expected)
  )
}

quantitative_columns_preserved <- function(quantitative, merged) {
  matched <- merged[match(quantitative$mzmine_row_id, mzmine_row_id)]
  if (!identical(
    matched$mzmine_row_id,
    quantitative$mzmine_row_id
  )) return(FALSE)
  for (column in names(quantitative)) {
    observed <- matched[[column]]
    expected <- quantitative[[column]]
    equal <- if (is.numeric(expected) || is.integer(expected)) {
      numeric_equal(as.numeric(observed), as.numeric(expected))
    } else {
      exact_equal_with_missing(observed, expected)
    }
    if (!equal) return(FALSE)
  }
  TRUE
}

create_validation_summary <- function(
    quantitative, annotation_info, merge_info) {
  annotation <- annotation_info$cleaned
  merged <- merge_info$merged
  max_mz_difference <- max(
    abs(merged$annotation_to_target_mz_difference_da),
    na.rm = TRUE
  )
  allowed_categories <- c(
    "high_priority_annotated_candidate",
    "quantitative_candidate_lower_score_annotation",
    "quantitative_candidate_unannotated",
    "annotated_not_quantitative_candidate",
    "other_feature"
  )
  candidate_partition <- sum(
    merged$candidate_review_category %in% c(
      "high_priority_annotated_candidate",
      "quantitative_candidate_lower_score_annotation",
      "quantitative_candidate_unannotated"
    )
  ) == sum(merged$preliminary_quantitative_candidate)
  expected_primary_direction <- vapply(
    as.character(quantitative$overall_direction),
    primary_direction_from_upstream,
    character(1)
  )
  expected_high_priority <- merged$preliminary_quantitative_candidate &
    merged$annotation_present &
    is.finite(merged$annotation_score) &
    merged$annotation_score >= minimum_annotation_score_for_high_priority
  expected_candidate_category <- data.table::fifelse(
    expected_high_priority,
    "high_priority_annotated_candidate",
    data.table::fifelse(
      merged$preliminary_quantitative_candidate &
        merged$annotation_present,
      "quantitative_candidate_lower_score_annotation",
      data.table::fifelse(
        merged$preliminary_quantitative_candidate &
          !merged$annotation_present,
        "quantitative_candidate_unannotated",
        data.table::fifelse(
          merged$annotation_present,
          "annotated_not_quantitative_candidate",
          "other_feature"
        )
      )
    )
  )
  checks <- data.table::data.table(
    check = c(
      "quantitative_input_not_empty",
      "annotation_input_not_empty",
      "dataset_id_matches_configuration",
      "quantitative_mzmine_row_ids_unique",
      "annotation_ids_unique",
      "identifier_sets_match",
      "quantitative_and_annotation_row_counts_match",
      "merge_row_count_preserved",
      "no_rows_multiplied",
      "no_quantitative_rows_removed",
      "quantitative_columns_preserved_value_for_value",
      "target_mz_preserved",
      "annotation_mz_matches_target_within_tolerance",
      "annotation_scores_consistent",
      "all_nonmissing_adducts_are_accepted",
      "unannotated_features_retained",
      "candidate_categories_partition_features_correctly",
      "high_priority_rule_applied_correctly",
      "primary_direction_mapped_from_upstream",
      "three_readouts_not_recalculated"
    ),
    passed = c(
      nrow(quantitative) > 0,
      nrow(annotation) > 0,
      identical(unique(quantitative$dataset_id), dataset_id),
      !anyDuplicated(quantitative$mzmine_row_id),
      !anyDuplicated(annotation$mzmine_row_id),
      setequal(quantitative$mzmine_row_id, annotation$mzmine_row_id),
      nrow(quantitative) == nrow(annotation),
      nrow(merged) == nrow(quantitative),
      data.table::uniqueN(merged$mzmine_row_id) == nrow(merged),
      setequal(merged$mzmine_row_id, quantitative$mzmine_row_id),
      quantitative_columns_preserved(quantitative, merged),
      numeric_equal(merged$target_mz, quantitative$target_mz),
      all(is.finite(merged$annotation_mz)) &&
        max_mz_difference <= mz_alignment_tolerance_da,
      annotation_info$scores$consistent,
      annotation_info$adducts$unexpected_count == 0L,
      sum(!merged$annotation_present) ==
        sum(!is_present(annotation$annotation_name)),
      all(merged$candidate_review_category %in% allowed_categories) &&
        candidate_partition &&
        exact_equal_with_missing(
          merged$candidate_review_category,
          expected_candidate_category
        ),
      exact_equal_with_missing(
        merged$high_priority_annotated_candidate,
        expected_high_priority
      ),
      exact_equal_with_missing(
        merged$primary_change_direction,
        expected_primary_direction
      ),
      quantitative_columns_preserved(quantitative, merged)
    )
  )
  checks[, status := ifelse(passed, "PASS", "FAIL")]
  checks[, details := c(
    paste("Rows:", nrow(quantitative)),
    paste("Rows:", nrow(annotation)),
    paste("Configured dataset:", dataset_id),
    "Quantitative merge keys are unique.",
    "Annotation merge keys are unique.",
    "The identifier sets are identical.",
    "Input tables contain equal row counts.",
    "The merge preserved the quantitative row count.",
    "Every merged MZmine row ID occurs once.",
    "Every quantitative merge key remains present.",
    "Every original quantitative column was compared.",
    "Target m/z values remain unchanged.",
    paste("Maximum absolute difference:", max_mz_difference),
    paste("Rows with both scores:", annotation_info$scores$both_present),
    paste("Configured adducts:", paste(accepted_adducts, collapse = ", ")),
    "Rows with blank annotation names remain in the master.",
    paste(
      "Every category was validated value-for-value and candidates form",
      "an exact partition."
    ),
    "The configured descriptive annotation-score rule was verified.",
    "Display directions were mapped from upstream overall_direction values.",
    "All Script 03 columns were carried forward value-for-value."
  )]
  checks[]
}

###############################################################################
# OUTPUT TABLES, WORKBOOK, REPORT, AND PROVENANCE
###############################################################################

create_output_tables <- function(merged) {
  list(
    high_priority = merged[high_priority_annotated_candidate == TRUE],
    quantitative_unannotated = merged[
      preliminary_quantitative_candidate == TRUE &
        annotation_present == FALSE
    ],
    lower_score = merged[
      preliminary_quantitative_candidate == TRUE &
        annotation_present == TRUE &
        high_priority_annotated_candidate == FALSE
    ],
    all_annotated = merged[annotation_present == TRUE],
    all_candidates = merged[preliminary_quantitative_candidate == TRUE]
  )
}

create_status_summary <- function(
    quantitative, annotation_info, merge_info, tables) {
  merged <- merge_info$merged
  threshold_rows <- data.table::rbindlist(lapply(
    seq_along(annotation_score_thresholds),
    function(index) {
      data.table::data.table(
        metric = paste0(
          "annotation_score_at_least_",
          format(annotation_score_thresholds[index], trim = TRUE)
        ),
        value = sum(
          merged[[merge_info$threshold_columns[index]]],
          na.rm = TRUE
        )
      )
    }
  ))
  adduct_counts <- data.table::as.data.table(
    table(
      factor(
        annotation_info$adducts$values[
          nzchar(annotation_info$adducts$values)
        ],
        levels = accepted_adducts
      ),
      useNA = "no"
    )
  )
  data.table::setnames(adduct_counts, c("adduct", "value"))
  adduct_counts[, metric := paste0("annotated_rows_adduct_", adduct)]
  adduct_counts[, adduct := NULL]
  reported_compared <- is.finite(
    merged$annotation_reported_mz_difference_da
  ) & is.finite(merged$annotation_to_target_mz_difference_da)
  reported_agree <- reported_compared &
    abs(
      merged$annotation_reported_mz_difference_da -
        merged$annotation_to_target_mz_difference_da
    ) <= mz_alignment_tolerance_da
  base <- data.table::data.table(
    metric = c(
      "quantitative_rows_before_merging",
      "annotation_rows_before_merging",
      "final_merged_rows",
      "unique_mzmine_row_ids",
      "unmatched_ids",
      "maximum_absolute_annotation_to_target_mz_difference_da",
      "annotated_features",
      "unannotated_features",
      "blank_adduct_rows",
      "unexpected_adduct_rows",
      "rows_with_both_annotation_scores",
      "rows_where_both_annotation_scores_agree",
      "rows_with_only_one_annotation_score",
      "unannotated_rows_without_scores",
      "reported_vs_calculated_mz_difference_rows_compared",
      "reported_vs_calculated_mz_difference_rows_within_tolerance",
      "preliminary_quantitative_candidates",
      "high_priority_annotated_candidates",
      "quantitative_candidates_lower_score_annotations",
      "quantitative_candidates_without_annotations"
    ),
    value = c(
      nrow(quantitative),
      nrow(annotation_info$raw),
      nrow(merged),
      data.table::uniqueN(merged$mzmine_row_id),
      0,
      max(abs(merged$annotation_to_target_mz_difference_da)),
      sum(merged$annotation_present),
      sum(!merged$annotation_present),
      annotation_info$adducts$blank_count,
      annotation_info$adducts$unexpected_count,
      annotation_info$scores$both_present,
      annotation_info$scores$both_agree,
      annotation_info$scores$only_one_present,
      sum(!merged$annotation_present &
            !is.finite(merged$annotation_score) &
            !is.finite(merged$annotation_compound_annotation_score)),
      sum(reported_compared),
      sum(reported_agree),
      sum(merged$preliminary_quantitative_candidate),
      nrow(tables$high_priority),
      nrow(tables$lower_score),
      nrow(tables$quantitative_unannotated)
    )
  )
  data.table::rbindlist(list(base, threshold_rows, adduct_counts), fill = TRUE)
}

create_methods_table <- function(
    quantitative, annotation_info, merged) {
  data.table::data.table(
    setting = c(
      "dataset_id", "quantitative_input", "annotation_input", "merge_key",
      "expected_ion_mode", "accepted_adducts", "quantitative_rows",
      "annotation_rows", "merged_rows", "mz_alignment_tolerance_da",
      "annotation_score_consistency_tolerance",
      "annotation_score_thresholds",
      "minimum_annotation_score_for_high_priority",
      "high_priority_candidate_rule",
      "supporting_local_abs_log2fc_threshold",
      "supporting_detection_fraction_difference_threshold",
      "supporting_detection_fraction_threshold_interpretation",
      "supporting_readout_pattern_interpretation",
      "primary_direction_source", "three_readouts_recalculated",
      "p_values", "pseudocount", "annotation_filtering",
      "replicate_cv_exclusion", "unannotated_features_retained",
      "annotation_identity_claim", "candidate_category_interpretation"
    ),
    value = c(
      dataset_id,
      quantitative_csv,
      annotation_csv,
      "quantitative mzmine_row_id = annotation id",
      expected_ion_mode,
      paste(accepted_adducts, collapse = ", "),
      nrow(quantitative),
      nrow(annotation_info$raw),
      nrow(merged),
      mz_alignment_tolerance_da,
      annotation_score_consistency_tolerance,
      paste(annotation_score_thresholds, collapse = ", "),
      minimum_annotation_score_for_high_priority,
      paste(
        "Upstream preliminary quantitative candidate AND annotation present",
        "AND annotation score at least",
        minimum_annotation_score_for_high_priority
      ),
      supporting_local_abs_log2fc_threshold,
      supporting_detection_fraction_difference_threshold,
      paste(
        supporting_detection_fraction_difference_threshold,
        "as a fraction corresponds to",
        100 * supporting_detection_fraction_difference_threshold,
        "percentage points."
      ),
      paste(
        "Local criterion uses absolute local log2 fold change; coverage uses",
        "the treatment-minus-control detection-fraction difference;",
        "the pattern is descriptive and is not a statistical test."
      ),
      "Mapped from the upstream Script 03 overall_direction field; not recalculated.",
      "No; all quantitative readouts and classifications were retained from Script 03.",
      "No p-values or statistical significance tests were calculated.",
      "No pseudocount was used.",
      "No quantitative row was removed according to annotation status or annotation score.",
      "No; replicate CV was retained for descriptive QC only.",
      "Yes; all quantitative features were retained.",
      paste(
        "Putative accurate-mass database annotation only; identity was not",
        "confirmed using tandem mass spectrometry or an authentic reference standard."
      ),
      paste(
        "Descriptive review categories only; they do not represent statistically",
        "significant effects or confirmed molecular identifications."
      )
    )
  )
}

replicate_qc_columns <- function(merged) {
  dynamic <- quantitative_column_names()
  unique(c(
    "dataset_id", "target_id", "mzmine_row_id", "target_mz",
    "annotation_name", "annotation_score",
    "high_priority_annotated_candidate", "candidate_review_category",
    "replicate_variability_flag", "technical_variability_flag",
    dynamic$control_cv, dynamic$treatment_cv,
    dynamic$control_supported, dynamic$treatment_supported,
    "strong_signal_support_pass", "strong_signal_support_category",
    grep(
      paste0(
        "_(overall_normalized_signal|local_strong_pixel_intensity|",
        "conservative_detected_pixel_count|conservative_detection_fraction)$"
      ),
      names(merged),
      value = TRUE
    )
  ))
}

get_running_script_path <- function() {
  command_arguments <- commandArgs(trailingOnly = FALSE)
  file_arguments <- sub(
    "^--file=", "",
    grep("^--file=", command_arguments, value = TRUE)
  )
  frame_files <- unlist(lapply(
    sys.frames(),
    function(frame) {
      if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
    }
  ), use.names = FALSE)
  candidates <- unique(c(file_arguments, frame_files))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) return(NA_character_)
  normalizePath(tail(candidates, 1L), winslash = "/", mustWork = TRUE)
}

write_configuration <- function(output_dir, package_versions, start_time) {
  values <- c(
    dataset_id = dataset_id,
    quantitative_csv = quantitative_csv,
    annotation_csv = annotation_csv,
    analysis_root = analysis_root,
    output_parent = output_parent,
    expected_ion_mode = expected_ion_mode,
    control_condition_label = control_condition_label,
    treatment_condition_label = treatment_condition_label,
    output_treatment_label = output_treatment_label,
    accepted_adducts = paste(accepted_adducts, collapse = ","),
    mz_alignment_tolerance_da = as.character(mz_alignment_tolerance_da),
    annotation_score_consistency_tolerance =
      as.character(annotation_score_consistency_tolerance),
    annotation_score_thresholds =
      paste(annotation_score_thresholds, collapse = ","),
    minimum_annotation_score_for_high_priority =
      as.character(minimum_annotation_score_for_high_priority),
    supporting_local_abs_log2fc_threshold =
      as.character(supporting_local_abs_log2fc_threshold),
    supporting_detection_fraction_difference_threshold =
      as.character(supporting_detection_fraction_difference_threshold),
    write_excel_output = as.character(write_excel_output),
    r_version = R.version.string,
    run_date_time = format(start_time, "%Y-%m-%d %H:%M:%S %Z")
  )
  configuration <- data.table::data.table(
    setting = names(values), value = unname(values)
  )
  versions <- data.table::data.table(
    setting = paste0("package_version_", package_versions$package),
    value = package_versions$version
  )
  data.table::fwrite(
    data.table::rbindlist(list(configuration, versions)),
    file.path(output_dir, "analysis_configuration_used.csv")
  )
}

add_workbook_sheet <- function(workbook, sheet, data) {
  display <- data.table::copy(data)
  names(display) <- make.unique(names(display), sep = "_")
  openxlsx::addWorksheet(workbook, sheet)
  if (nrow(display)) {
    openxlsx::writeDataTable(
      workbook, sheet, display, withFilter = TRUE,
      tableStyle = "TableStyleLight9", keepNA = FALSE
    )
  } else {
    openxlsx::writeData(workbook, sheet, display, colNames = TRUE)
  }
  openxlsx::freezePane(workbook, sheet, firstRow = TRUE)
  widths <- vapply(seq_along(display), function(index) {
    values <- as.character(display[[index]])
    observed <- max(
      nchar(c(names(display)[index], values), type = "width"),
      na.rm = TRUE
    )
    min(max(observed + 2, 10), 45)
  }, numeric(1))
  openxlsx::setColWidths(
    workbook, sheet, cols = seq_along(display), widths = widths
  )
  long_columns <- grep(
    "inchi|smiles|comment|confidence_note|input|path",
    names(display),
    ignore.case = TRUE
  )
  if (length(long_columns) && nrow(display)) {
    wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
    openxlsx::addStyle(
      workbook, sheet, style = wrap_style,
      rows = seq_len(nrow(display)) + 1L,
      cols = long_columns,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

write_workbook <- function(
    output_path, merged, tables, status, validation, methods) {
  workbook <- openxlsx::createWorkbook()
  add_workbook_sheet(workbook, "Master_All_Features", merged)
  add_workbook_sheet(
    workbook, "High_Priority_Annotated", tables$high_priority
  )
  add_workbook_sheet(
    workbook, "Quantitative_Unannotated",
    tables$quantitative_unannotated
  )
  add_workbook_sheet(
    workbook, "Lower_Score_Annotated", tables$lower_score
  )
  add_workbook_sheet(workbook, "All_Annotated", tables$all_annotated)
  add_workbook_sheet(
    workbook, "Three_Readout_Candidates", tables$all_candidates
  )
  qc_columns <- replicate_qc_columns(merged)
  add_workbook_sheet(
    workbook, "Replicate_QC", merged[, ..qc_columns]
  )
  add_workbook_sheet(workbook, "Status_Summary", status)
  add_workbook_sheet(workbook, "Validation_Summary", validation)
  add_workbook_sheet(workbook, "Methods_and_Settings", methods)
  openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)
}

write_report <- function(
    output_dir, quantitative, annotation_info, merge_info,
    tables, start_time) {
  merged <- merge_info$merged
  high_priority_replicate_variability <- tables$high_priority[
    , .N, by = replicate_variability_flag
  ][order(replicate_variability_flag)]
  high_priority_support <- tables$high_priority[
    , .N, by = strong_signal_support_category
  ][order(strong_signal_support_category)]
  adduct_counts <- table(
    factor(
      annotation_info$adducts$values[
        nzchar(annotation_info$adducts$values)
      ],
      levels = accepted_adducts
    )
  )
  threshold_lines <- vapply(
    seq_along(annotation_score_thresholds),
    function(index) {
      paste(
        "- Annotation score at least",
        annotation_score_thresholds[index],
        ":",
        sum(merged[[merge_info$threshold_columns[index]]], na.rm = TRUE)
      )
    },
    character(1)
  )
  direction_counts <- table(
    factor(
      tables$high_priority$primary_change_direction,
      levels = c(
        paste0("increased_after_", output_treatment_label),
        paste0("decreased_after_", output_treatment_label),
        "small_or_no_overall_change", "unavailable"
      )
    )
  )
  ion_adduct_statement <- paste0(
    "All nonblank annotation adducts matched the configured ",
    tolower(expected_ion_mode),
    "-ion adduct list."
  )
  report <- c(
    "Annotation merge with three-readout quantitative summary",
    paste("Dataset ID:", dataset_id),
    paste("Expected ion mode:", expected_ion_mode),
    paste("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste(
      "Runtime seconds:",
      round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
    ),
    paste("Quantitative input:", quantitative_csv),
    paste("Annotation input:", annotation_csv),
    paste("Output directory:", output_dir),
    "",
    "Merge validation:",
    paste("- Quantitative rows:", nrow(quantitative)),
    paste("- Annotation rows:", nrow(annotation_info$raw)),
    paste("- Merged rows:", nrow(merged)),
    paste("- Unique MZmine row IDs:", data.table::uniqueN(merged$mzmine_row_id)),
    "- Unmatched identifiers: 0",
    paste(
      "- Maximum absolute annotation-to-target m/z difference:",
      max(abs(merged$annotation_to_target_mz_difference_da)),
      "Da"
    ),
    "- Every quantitative row was retained.",
    "- Unannotated quantitative rows were retained.",
    "- Every quantitative column was preserved value-for-value.",
    "",
    "Adduct validation:",
    paste("- Configured accepted adducts:", paste(accepted_adducts, collapse = ", ")),
    paste(
      "- Counts per accepted adduct:",
      paste(names(adduct_counts), as.integer(adduct_counts),
            sep = "=", collapse = "; ")
    ),
    paste("- Blank adduct rows:", annotation_info$adducts$blank_count),
    paste("- Unexpected adduct rows:", annotation_info$adducts$unexpected_count),
    paste("-", ion_adduct_statement),
    "",
    "Annotation-score audit:",
    paste("- Rows with both scores:", annotation_info$scores$both_present),
    paste("- Rows where both scores agree:", annotation_info$scores$both_agree),
    paste("- Rows with only one score:", annotation_info$scores$only_one_present),
    paste("- Rows without either score:", annotation_info$scores$neither_present),
    "",
    "Annotation counts:",
    paste("- Annotated features:", nrow(tables$all_annotated)),
    paste("- Unannotated features:", nrow(merged) - nrow(tables$all_annotated)),
    threshold_lines,
    "",
    "Descriptive candidate counts:",
    paste(
      "- Preliminary quantitative candidates:",
      sum(merged$preliminary_quantitative_candidate)
    ),
    paste(
      "- High-priority annotated candidates:",
      nrow(tables$high_priority)
    ),
    paste(
      "- Lower-score annotated quantitative candidates:",
      nrow(tables$lower_score)
    ),
    paste(
      "- Unannotated quantitative candidates:",
      nrow(tables$quantitative_unannotated)
    ),
    paste(
      "- Increased high-priority candidates:",
      direction_counts[1]
    ),
    paste(
      "- Decreased high-priority candidates:",
      direction_counts[2]
    ),
    paste(
      "- Small/no-change high-priority candidates:",
      direction_counts[3]
    ),
    "",
    "Biological-replicate variability among high-priority candidates:",
    utils::capture.output(print(high_priority_replicate_variability)),
    "",
    "Strong-signal support among high-priority candidates:",
    utils::capture.output(print(high_priority_support)),
    "",
    "Explicit confirmations:",
    "- The three quantitative readouts and primary classifications were not recalculated.",
    "- No p-values or statistical significance tests were calculated.",
    "- No pseudocount was used and missing values were not replaced with zero.",
    "- Replicate CV was not used as an exclusion criterion.",
    "- Candidate categories are descriptive review categories.",
    "- Annotations are putative accurate-mass database matches.",
    "- Annotation identities were not confirmed using tandem mass spectrometry or an authentic reference standard.",
    paste(
      "Warnings:",
      if (length(state$warnings)) paste(state$warnings, collapse = " || ")
      else "none"
    )
  )
  writeLines(report, file.path(output_dir, "annotation_merge_report.txt"))
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
      file.path(output_dir, "04_merge_annotations_three_readouts_used.R"),
      overwrite = TRUE
    )
    if (!isTRUE(copied)) {
      add_warning("Could not copy the running script:", running_script)
    }
  }

  quantitative <- read_and_validate_quantitative()
  annotation_info <- read_and_validate_annotation(quantitative)
  merge_info <- merge_and_classify(quantitative, annotation_info)
  validation <- create_validation_summary(
    quantitative, annotation_info, merge_info
  )
  data.table::fwrite(
    validation,
    file.path(output_dir, "analysis_validation_summary.csv")
  )
  if (any(validation$status != "PASS")) {
    stop_critical(
      "Critical validation failed:",
      paste(validation[status != "PASS", check], collapse = ", ")
    )
  }

  merged <- merge_info$merged
  tables <- create_output_tables(merged)
  status <- create_status_summary(
    quantitative, annotation_info, merge_info, tables
  )
  methods <- create_methods_table(
    quantitative, annotation_info, merged
  )

  master_csv <- file.path(
    output_dir,
    paste0(dataset_id, "_Annotated_ThreeReadout_Master.csv")
  )
  master_xlsx <- file.path(
    output_dir,
    paste0(dataset_id, "_Annotated_ThreeReadout_Master.xlsx")
  )
  data.table::fwrite(merged, master_csv, na = "")
  data.table::fwrite(
    tables$high_priority,
    file.path(
      output_dir,
      paste0(dataset_id, "_HighPriority_Annotated_Candidates.csv")
    ),
    na = ""
  )
  data.table::fwrite(
    tables$quantitative_unannotated,
    file.path(
      output_dir,
      paste0(dataset_id, "_Quantitative_Unannotated_Candidates.csv")
    ),
    na = ""
  )
  data.table::fwrite(
    tables$lower_score,
    file.path(
      output_dir,
      paste0(
        dataset_id,
        "_Quantitative_LowerScore_Annotated_Candidates.csv"
      )
    ),
    na = ""
  )
  data.table::fwrite(
    tables$all_annotated,
    file.path(
      output_dir,
      paste0(dataset_id, "_All_Annotated_Features.csv")
    ),
    na = ""
  )
  data.table::fwrite(
    status,
    file.path(output_dir, "annotation_merge_status_summary.csv"),
    na = ""
  )
  if (isTRUE(write_excel_output)) {
    write_workbook(
      master_xlsx, merged, tables, status, validation, methods
    )
  }
  write_report(
    output_dir, quantitative, annotation_info, merge_info,
    tables, start_time
  )
  utils::capture.output(
    utils::sessionInfo(),
    file = file.path(output_dir, "sessionInfo.txt")
  )
  log_message("Wrote:", master_csv)
  if (isTRUE(write_excel_output)) log_message("Wrote:", master_xlsx)
  log_message("Complete. Output directory:", output_dir)
  invisible(list(
    output_dir = output_dir,
    quantitative = quantitative,
    annotation = annotation_info,
    merged = merged,
    tables = tables,
    status = status,
    validation = validation
  ))
}

main()
