fpq_quantify_only_final_workbook_sheets <- function() {
  c("Normalized_Intensity", "Detected_Pixel_Intensity",
    "Detection", "QC", "README")
}

fpq_validate_quantify_only_final_results_workbook <- function(path) {
  errors <- character()
  if (!file.exists(path))
    return(list(status = "FAIL", errors = "Workbook is missing.",
                workbook_type = "quantify_only"))

  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
    errors <<- c(errors, paste("openxlsx:", conditionMessage(e)))
    character()
  })
  expected <- fpq_quantify_only_final_workbook_sheets()
  if (!identical(sheets, expected))
    errors <- c(errors,
      "Worksheet names or order do not match the Quantify Samples Only contract.")

  header_contract <- list(
    Normalized_Intensity = c("_overall_normalized_signal"),
    Detected_Pixel_Intensity = c("_local_strong_pixel_intensity"),
    Detection = c("_detection_fraction_percent",
                  "_conservative_detected_pixel_count")
  )
  forbidden_tokens <- c(
    "log2", "fold_change", "fold change", "p_value", "pvalue",
    "trt_minus_ctrl", "difference_trt", "group_mean", "ctrl_mean",
    "pvl_mean", "mean_log2fc"
  )

  for (sheet in names(header_contract)) {
    frame <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
      path, sheet = sheet, rows = 1L, colNames = TRUE,
      check.names = FALSE)), error = function(e) NULL)
    headers <- if (is.null(frame)) character() else names(frame)

    if (!length(headers)) {
      errors <- c(errors, paste("Header contract failed for", sheet))
      next
    }
    lower <- tolower(headers)
    forbidden <- headers[vapply(forbidden_tokens, function(token)
      any(grepl(token, lower, fixed = TRUE)), logical(1L))]
    if (length(forbidden))
      errors <- c(errors, paste(
        "Comparison-specific headers appeared in", sheet))

    for (suffix in header_contract[[sheet]]) {
      if (!any(endsWith(headers, suffix)))
        errors <- c(errors, paste(
          "Required individual-sample columns are missing from", sheet,
          "for suffix", suffix))
    }
  }

  qc <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "QC", check.names = FALSE)),
    error = function(e) data.frame())
  if (!is.data.frame(qc) || !nrow(qc) ||
      !all(c("check", "status") %in% names(qc))) {
    errors <- c(errors, "Quantify-only QC sheet is missing or malformed.")
  } else if (any(toupper(trimws(as.character(qc$status))) == "FAIL",
                 na.rm = TRUE)) {
    errors <- c(errors, "One or more quantify-only QC checks failed.")
  }

  readme <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "README", check.names = FALSE)),
    error = function(e) data.frame())
  if (!is.data.frame(readme) || ncol(readme) < 2L || !nrow(readme)) {
    errors <- c(errors, "Quantify-only README sheet is missing or malformed.")
  } else {
    keys <- tolower(trimws(as.character(readme[[1L]])))
    values <- trimws(as.character(readme[[2L]]))
    analysis_index <- which(keys == "analysis type")
    comparison_index <- which(keys == "comparison statistics")
    if (!length(analysis_index) ||
        !identical(tolower(values[[analysis_index[[1L]]]]),
                   "quantify samples only"))
      errors <- c(errors,
        "README does not identify the Quantify Samples Only workflow.")
    if (!length(comparison_index) ||
        !startsWith(tolower(values[[comparison_index[[1L]]]]), "none"))
      errors <- c(errors,
        "README does not confirm that comparison statistics are absent.")
  }

  list(status = if (length(errors)) "FAIL" else "PASS",
       errors = errors, sheets = sheets, workbook_type = "quantify_only")
}

fpq_generic_two_group_final_workbook_sheets <- function() {
  c("Mean_Log2FC", "All_Pixels", "TIC_normalized_intensity", "QC", "README")
}

fpq_primary_intensity_sheet_from_names <- function(sheets, position = 3L) {
  if (length(sheets) < position)
    return(NA_character_)
  candidate <- sheets[[position]]
  allowed <- c(
    "TIC_normalized_intensity", "Primary_RAW_Intensity",
    "Primary_RMS_Intensity", "Primary_MEDIAN_Intensity"
  )
  if (candidate %in% allowed) candidate else NA_character_
}

fpq_all_features_sheet_from_names <- function(sheets, position = 4L) {
  if (length(sheets) < position)
    return(NA_character_)
  candidate <- sheets[[position]]
  allowed <- c(
    "All_Features_TIC_Normalized", "All_Features_RAW",
    "All_Features_RMS", "All_Features_MEDIAN"
  )
  if (candidate %in% allowed) candidate else NA_character_
}

fpq_validate_generic_two_group_final_results_workbook <- function(path) {
  errors <- character()
  if (!file.exists(path))
    return(list(status = "FAIL", errors = "Workbook is missing.",
                workbook_type = "generic_two_group_comparison"))

  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
    errors <<- c(errors, paste("openxlsx:", conditionMessage(e)))
    character()
  })
  intensity_sheet <- fpq_primary_intensity_sheet_from_names(sheets)
  expected <- c("Mean_Log2FC", "All_Pixels", intensity_sheet, "QC", "README")
  if (!identical(sheets, expected))
    errors <- c(errors,
      "Worksheet names or order do not match the generic two-group contract.")

  prohibited <- intersect(sheets, c(
    "Mean_Log2FC R2", "Calculation_Audit", "Source_Files",
    "Combined_All_Features", "Methods_and_Settings"
  ))
  if (length(prohibited))
    errors <- c(errors, paste(
      "Prohibited worksheets:", paste(prohibited, collapse = ", ")))

  read_headers <- function(sheet) {
    frame <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
      path, sheet = sheet, rows = 1L, colNames = TRUE,
      check.names = FALSE)), error = function(e) NULL)
    if (is.null(frame)) character() else names(frame)
  }

  common_required <- c(
    "dataset_id", "ion_mode", "target_mz",
    "annotation_name", "GraphPad_label"
  )
  mean_headers <- read_headers("Mean_Log2FC")
  pixel_headers <- read_headers("All_Pixels")
  local_headers <- if (is.na(intensity_sheet)) character() else
    read_headers(intensity_sheet)

  header_sets <- list(
    Mean_Log2FC = mean_headers,
    All_Pixels = pixel_headers,
    primary_intensity = local_headers
  )
  for (sheet in names(header_sets)) {
    headers <- header_sets[[sheet]]
    if (!length(headers)) {
      errors <- c(errors, paste("Header contract failed for", sheet))
    } else {
      missing <- setdiff(common_required, headers)
      if (length(missing))
        errors <- c(errors, paste(
          sheet, "is missing required metadata columns:",
          paste(missing, collapse = ", ")))
    }
  }

  mean_sample <- mean_headers[endsWith(
    mean_headers, "_overall_normalized_signal")]
  mean_change <- mean_headers[startsWith(mean_headers, "log2FC_")]

  pixel_fraction <- pixel_headers[endsWith(
    pixel_headers, "_conservative_detection_fraction")]
  pixel_count <- pixel_headers[endsWith(
    pixel_headers, "_conservative_detected_pixel_count")]
  pixel_change <- pixel_headers[
    startsWith(pixel_headers, "ion_positive_pixel_change_") &
      endsWith(pixel_headers, "_pp")]

  local_sample <- local_headers[endsWith(
    local_headers, "_local_strong_pixel_intensity")]
  local_change <- local_headers[startsWith(
    local_headers, "local_intensity_log2FC_")]

  sample_counts <- c(
    overall = length(mean_sample),
    detection_fraction = length(pixel_fraction),
    detection_count = length(pixel_count),
    local_intensity = length(local_sample)
  )

  if (any(sample_counts < 4L))
    errors <- c(errors,
      "The generic two-group workbook must contain at least four individual sample columns.")
  if (length(unique(sample_counts)) != 1L)
    errors <- c(errors, paste(
      "Individual sample-column counts disagree across readouts:",
      paste(names(sample_counts), sample_counts, sep = "=", collapse = ", ")))
  if (length(mean_sample) %% 2L != 0L)
    errors <- c(errors,
      "The generic two-group workbook must contain equal-sized sample groups.")

  if (length(mean_change) != 1L)
    errors <- c(errors,
      "Mean_Log2FC must contain exactly one dynamic two-group log2FC column.")
  if (length(pixel_change) != 1L)
    errors <- c(errors,
      "All_Pixels must contain exactly one dynamic detection-change column.")
  if (length(local_change) != 1L)
    errors <- c(errors,
      "The primary-intensity sheet must contain exactly one dynamic local-intensity log2FC column.")

  inferential_tokens <- c(
    "p_value", "pvalue", "q_value", "qvalue", "fdr",
    "adjusted_p", "paired_test", "imputed"
  )
  all_headers <- tolower(c(mean_headers, pixel_headers, local_headers))
  for (token in inferential_tokens) {
    if (any(grepl(token, all_headers, fixed = TRUE)))
      errors <- c(errors, paste(
        "An unsupported inferential-statistics column was found:", token))
  }

  qc <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "QC", check.names = FALSE)),
    error = function(e) data.frame())
  if (!is.data.frame(qc) || !nrow(qc) ||
      !all(c("check", "status") %in% names(qc))) {
    errors <- c(errors, "Generic two-group QC sheet is missing or malformed.")
  } else if (any(toupper(trimws(as.character(qc$status))) == "FAIL",
                 na.rm = TRUE)) {
    errors <- c(errors,
      "One or more generic two-group QC checks failed.")
  }

  readme <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "README", check.names = FALSE)),
    error = function(e) data.frame())
  if (!is.data.frame(readme) || ncol(readme) < 2L || !nrow(readme)) {
    errors <- c(errors,
      "Generic two-group README sheet is missing or malformed.")
  } else {
    keys <- tolower(trimws(as.character(readme[[1L]])))
    values <- trimws(as.character(readme[[2L]]))

    required_keys <- c(
      "reference condition", "comparison condition",
      "replicates per condition", "statistical interpretation",
      "validation status"
    )
    missing_keys <- setdiff(required_keys, keys)
    if (length(missing_keys))
      errors <- c(errors, paste(
        "README is missing:", paste(missing_keys, collapse = ", ")))

    replicate_index <- which(keys == "replicates per condition")
    if (length(replicate_index)) {
      replicates <- suppressWarnings(as.integer(
        values[[replicate_index[[1L]]]]))
      if (is.na(replicates) || replicates < 2L)
        errors <- c(errors,
          "README does not confirm at least two replicates per condition.")
      if (!is.na(replicates) &&
          length(mean_sample) != 2L * replicates)
        errors <- c(errors,
          "README replicate count does not match the workbook sample columns.")
    }

    interpretation_index <- which(keys == "statistical interpretation")
    if (length(interpretation_index) &&
        !grepl("descriptive two-group",
               tolower(values[[interpretation_index[[1L]]]]),
               fixed = TRUE))
      errors <- c(errors,
        "README does not identify descriptive two-group readouts.")

    status_index <- which(keys == "validation status")
    if (length(status_index) &&
        !grepl("technical validation candidate",
               tolower(values[[status_index[[1L]]]]),
               fixed = TRUE))
      errors <- c(errors,
        "README does not identify Technical Validation Candidate status.")
  }

  list(
    status = if (length(errors)) "FAIL" else "PASS",
    errors = errors,
    sheets = sheets,
    workbook_type = "generic_two_group_comparison",
    individual_sample_columns = sample_counts
  )
}


fpq_all_features_comparison_workbook_sheets <- function() {
  c("Mean_Log2FC", "All_Pixels", "TIC_normalized_intensity",
    "All_Features_TIC_Normalized", "QC", "README")
}

fpq_validate_all_features_comparison_workbook <- function(path) {
  errors <- character()
  if (!file.exists(path))
    return(list(status = "FAIL", errors = "Workbook is missing.",
                workbook_type = "two_group_comparison_all_features"))

  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
    errors <<- c(errors, paste("openxlsx:", conditionMessage(e)))
    character()
  })
  intensity_sheet <- fpq_primary_intensity_sheet_from_names(sheets)
  all_features_sheet <- fpq_all_features_sheet_from_names(sheets)
  expected <- c("Mean_Log2FC", "All_Pixels", intensity_sheet,
                all_features_sheet, "QC", "README")
  if (!identical(sheets, expected))
    errors <- c(errors,
      "Worksheet names or order do not match the extended comparison contract.")

  read_headers <- function(sheet) {
    frame <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
      path, sheet = sheet, rows = 1L, colNames = TRUE,
      check.names = FALSE)), error = function(e) NULL)
    if (is.null(frame)) character() else names(frame)
  }

  base_headers <- list(
    Mean_Log2FC = read_headers("Mean_Log2FC"),
    All_Pixels = read_headers("All_Pixels"),
    primary_intensity = if (is.na(intensity_sheet)) character() else
      read_headers(intensity_sheet)
  )
  for (sheet in names(base_headers)) {
    headers <- base_headers[[sheet]]
    if (!length(headers))
      errors <- c(errors, paste("Header contract failed for", sheet))
    if (!all(c("dataset_id", "ion_mode", "target_mz") %in% headers))
      errors <- c(errors, paste(
        sheet, "is missing dataset_id, ion_mode, or target_mz."))
  }

  mean_headers <- base_headers$Mean_Log2FC
  base_sample_columns <- c(
    mean_headers[endsWith(mean_headers, "_overall_normalized_signal")],
    mean_headers[endsWith(mean_headers, "_TIC_normalized_signal")]
  )
  base_log2_columns <- mean_headers[
    startsWith(mean_headers, "log2FC_") |
      grepl("log2FC", mean_headers, fixed = TRUE)
  ]
  if (length(base_sample_columns) < 4L ||
      length(base_sample_columns) %% 2L != 0L)
    errors <- c(errors,
      "The comparison workbook must contain equal two-group sample columns.")
  if (!length(base_log2_columns))
    errors <- c(errors,
      "Mean_Log2FC does not contain a descriptive log2 fold-change column.")

  all_features <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = all_features_sheet,
    check.names = FALSE)), error = function(e) NULL)
  if (is.null(all_features) || !is.data.frame(all_features) ||
      !nrow(all_features)) {
    errors <- c(errors,
      "The all-features primary-intensity sheet is missing or empty.")
  } else {
    required <- c(
      "dataset_id", "ion_mode", "mzmine_row_id", "target_mz",
      "annotation_status", "annotation_name", "annotation_score"
    )
    missing <- setdiff(required, names(all_features))
    if (length(missing))
      errors <- c(errors, paste(
        "The all-features primary-intensity sheet is missing:",
        paste(missing, collapse = ", ")))

    intensity_suffix <- if (identical(all_features_sheet,
                                      "All_Features_TIC_Normalized")) {
      "_TIC_normalized_intensity"
    } else if (identical(all_features_sheet, "All_Features_RAW")) {
      "_RAW_intensity"
    } else {
      paste0("_", sub("All_Features_", "", all_features_sheet),
             "_normalized_intensity")
    }
    sample_columns <- names(all_features)[endsWith(
      names(all_features), intensity_suffix)]
    mean_columns <- sample_columns[startsWith(sample_columns, "mean_")]
    individual_columns <- setdiff(sample_columns, mean_columns)
    fold_columns <- names(all_features)[startsWith(
      names(all_features), "fold_change_")]
    log2_columns <- names(all_features)[startsWith(
      names(all_features), "log2FC_")]

    if (length(individual_columns) < 4L ||
        length(individual_columns) %% 2L != 0L)
      errors <- c(errors,
        "All-features sheet must contain equal two-group replicate columns.")
    if (length(mean_columns) != 2L)
      errors <- c(errors,
        "All-features sheet must contain exactly two condition-mean columns.")
    if (length(fold_columns) != 1L)
      errors <- c(errors,
        "All-features sheet must contain exactly one linear fold-change column.")
    if (length(log2_columns) != 1L)
      errors <- c(errors,
        "All-features sheet must contain exactly one log2 fold-change column.")

    status_values <- unique(trimws(as.character(
      all_features$annotation_status)))
    if (any(!status_values %in% c("Annotated", "Unannotated")))
      errors <- c(errors,
        "annotation_status contains unsupported values.")

    annotated <- trimws(as.character(
      all_features$annotation_status)) == "Annotated"
    scores <- trimws(as.character(all_features$annotation_score))
    if (any(annotated, na.rm = TRUE) &&
        !any(annotated & nzchar(scores), na.rm = TRUE))
      errors <- c(errors,
        "Annotated rows do not retain a nonblank MZmine annotation score.")

    base_rows <- vapply(names(base_headers), function(sheet) {
      frame <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
        path, sheet = sheet, cols = 1L, check.names = FALSE)),
        error = function(e) data.frame())
      nrow(frame)
    }, integer(1L))
    if (nrow(all_features) < max(base_rows))
      errors <- c(errors,
        "All-features sheet contains fewer rows than an annotation-focused sheet.")

    inferential_tokens <- c(
      "p_value", "pvalue", "q_value", "qvalue", "fdr",
      "adjusted_p", "paired_test", "imputed"
    )
    lower_headers <- tolower(names(all_features))
    for (token in inferential_tokens) {
      if (any(grepl(token, lower_headers, fixed = TRUE)))
        errors <- c(errors, paste(
          "Unsupported inferential-statistics column:", token))
    }
  }

  qc <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "QC", check.names = FALSE)),
    error = function(e) data.frame())
  if (!is.data.frame(qc) || !nrow(qc) ||
      !all(c("check", "status") %in% names(qc))) {
    errors <- c(errors, "QC sheet is missing or malformed.")
  } else if (any(toupper(trimws(as.character(qc$status))) == "FAIL",
                 na.rm = TRUE)) {
    errors <- c(errors, "One or more workbook QC checks failed.")
  }

  readme <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "README", check.names = FALSE)),
    error = function(e) data.frame())
  if (!is.data.frame(readme) || ncol(readme) < 2L || !nrow(readme))
    errors <- c(errors, "README sheet is missing or malformed.")

  list(
    status = if (length(errors)) "FAIL" else "PASS",
    errors = unique(errors),
    sheets = sheets,
    workbook_type = "two_group_comparison_all_features",
    all_features_rows = if (is.data.frame(all_features))
      nrow(all_features) else 0L
  )
}

fpq_validate_published_final_results_workbook <- function(path) {
  sheets <- tryCatch(openxlsx::getSheetNames(path),
                     error = function(e) character())

  all_features_contract <- length(sheets) == 6L &&
    identical(sheets[c(1L, 2L, 5L, 6L)],
              c("Mean_Log2FC", "All_Pixels", "QC", "README")) &&
    !is.na(fpq_primary_intensity_sheet_from_names(sheets)) &&
    !is.na(fpq_all_features_sheet_from_names(sheets))
  if (all_features_contract)
    return(fpq_validate_all_features_comparison_workbook(path))

  if (identical(sheets, fpq_quantify_only_final_workbook_sheets()))
    return(fpq_validate_quantify_only_final_results_workbook(path))

  # Preserve compatibility with the original fixed PVL workbook contract.
  legacy <- tryCatch(
    fpq_validate_final_results_workbook(path),
    error = function(e) list(status = "FAIL",
      errors = conditionMessage(e)))
  if (identical(legacy$status, "PASS")) {
    legacy$workbook_type <- "two_group_comparison_legacy"
    return(legacy)
  }

  # Generic v1 uses arbitrary condition/sample names and dynamic headers.
  generic <- fpq_validate_generic_two_group_final_results_workbook(path)
  if (identical(generic$status, "PASS"))
    return(generic)

  generic$errors <- unique(c(
    generic$errors,
    paste0("Legacy-contract validation: ",
           paste(legacy$errors %||% "failed", collapse = "; "))
  ))
  generic
}

fpq_current_final_results_workbook <- function(repository_root, project = NULL) {
  if (!is.null(project)) {
    project_id <- project$manifest$project_id %||% ""
    artifacts <- Filter(function(x)
      identical(x$artifact_type, "final_results_workbook") &&
        identical(x$active_status, "active") &&
        identical(x$validation_status, "PASS") &&
        identical(x$project_id %||% "", project_id),
      fpq_list_artifacts(project))

    if (!length(artifacts))
      return(list(available = FALSE,
        reason = paste(
          "This project does not have an active validated final workbook.",
          "No fallback workbook from another project will be offered.")))

    timestamps <- vapply(artifacts, function(x)
      x$registered_at %||% x$created_at %||% "", character(1L))
    artifacts <- artifacts[order(timestamps, decreasing = TRUE)]

    failures <- character()
    for (artifact in artifacts) {
      path <- artifact$path %||% ""
      if (!nzchar(path) || !file.exists(path)) {
        failures <- c(failures,
          paste0(artifact$display_name %||% artifact$artifact_id,
                 ": file is unavailable"))
        next
      }

      expected_hash <- toupper(artifact$sha256 %||% "")
      if (nzchar(expected_hash)) {
        actual_hash <- tryCatch(toupper(fpq_hash_file(path)),
                                error = function(e) "")
        if (!identical(actual_hash, expected_hash)) {
          failures <- c(failures,
            paste0(basename(path), ": SHA-256 identity check failed"))
          next
        }
      }

      validation <- tryCatch(
        fpq_validate_published_final_results_workbook(path),
        error = function(e) list(status = "FAIL",
          errors = conditionMessage(e), workbook_type = "unknown"))
      if (identical(validation$status, "PASS"))
        return(list(available = TRUE, path = path,
          artifact = artifact, validation = validation,
          scientific_status = "technical_validation_candidate"))

      failures <- c(failures, paste0(
        basename(path), ": ",
        paste(validation$errors %||% "validation failed", collapse = "; ")))
    }

    return(list(available = FALSE, reason = paste(
      "The active workbook for this project was not publishable.",
      paste(failures, collapse = " | "),
      "No fallback workbook from another project was used.")))
  }

  # Development-only fallback when no project is open. A loaded project never
  # falls back to a workbook belonging to another project or test fixture.
  report_path <- file.path(repository_root, "tests", "fixtures", "reports",
                           "phase6_presentation_adapter_report.json")
  if (!file.exists(report_path))
    return(list(available = FALSE,
      reason = "The final results workbook has not been created and validated."))
  report <- tryCatch(fpq_read_json(report_path), error = function(e) NULL)
  if (is.null(report) || !identical(report$status, "PASS"))
    return(list(available = FALSE,
      reason = "Final workbook validation did not pass."))
  path <- report$output_workbook %||% ""
  validation <- tryCatch(
    fpq_validate_published_final_results_workbook(path),
    error = function(e) list(status = "FAIL", errors = conditionMessage(e)))
  if (!identical(validation$status, "PASS"))
    return(list(available = FALSE,
      reason = paste("Final workbook validation failed:",
                     paste(validation$errors, collapse = "; "))))
  list(available = TRUE, path = path, report = report, validation = validation)
}

fpq_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$h2("Results"),
    shiny::uiOutput(ns("final_workbook")),
    htmltools::tags$p(
      "The detailed nine-sheet scientific-audit workbook remains an internal artifact."))
}

fpq_results_project_refresh_token <- function(project) {
  if (is.null(project))
    return("no-project")

  project_id <- project$manifest$project_id %||%
    project$root %||% "unknown-project"

  registry_path <- tryCatch(
    fpq_project_paths(project$root)$artifacts,
    error = function(e) file.path(
      project$root, "registry", "artifacts.json")
  )

  registry_info <- suppressWarnings(file.info(registry_path))
  if (!nrow(registry_info) || is.na(registry_info$size[[1L]]))
    return(paste(project_id, "registry-missing", sep = "|"))

  paste(
    project_id,
    as.character(registry_info$size[[1L]]),
    format(
      registry_info$mtime[[1L]],
      "%Y%m%d%H%M%OS6",
      tz = "UTC"
    ),
    sep = "|"
  )
}

fpq_results_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    active_project <- shiny::reactive(state$project())

    # Poll the small artifact registry, but only invalidate the Results UI when
    # its identity, size, or modification time changes. The previous
    # invalidateLater() implementation rebuilt the download button every two
    # seconds, causing visible grey/blue flicker.
    result <- shiny::reactivePoll(
      intervalMillis = 2000,
      session = session,
      checkFunc = function() {
        fpq_results_project_refresh_token(active_project())
      },
      valueFunc = function() {
        fpq_current_final_results_workbook(
          state$repository_root,
          active_project()
        )
      }
    )

    shiny::observeEvent(result()$available, {
      if (isTRUE(result()$available))
        shiny::showNotification("Final results workbook created",
                                type = "message", duration = 5)
    }, ignoreInit = FALSE, once = TRUE)

    output$final_workbook <- shiny::renderUI({
      current <- result()
      if (!isTRUE(current$available))
        return(htmltools::tags$div(class = "fpq-error",
          htmltools::tags$p(current$reason),
          htmltools::tags$p("No workbook download has been published.")))

      quantify_only <- identical(
        current$validation$workbook_type %||% "",
        "quantify_only")
      button_label <- if (quantify_only)
        "Download Quantified Samples Workbook"
      else
        "Download Final Results Workbook"

      shiny::tagList(
        htmltools::tags$p(
          "Scientific status: Technical Validation Candidate"),
        htmltools::tags$p(
          htmltools::tags$strong("Published workbook: "),
          basename(current$path)),
        shiny::downloadButton(session$ns("download_final_workbook"),
                              button_label, class = "btn-primary"),
        shiny::actionButton(session$ns("open_results_folder"),
                            "Open Results Folder"))
    })

    shiny::observeEvent(input$open_results_folder, {
      current <- result()
      if (isTRUE(current$available))
        try(shell.exec(normalizePath(dirname(current$path))), silent = TRUE)
    })

    output$download_final_workbook <- shiny::downloadHandler(
      filename = function() {
        current <- result()
        if (!isTRUE(current$available)) "Final_Workbook_Unavailable.xlsx"
        else basename(current$path)
      },
      content = function(file) {
        current <- result()
        if (!isTRUE(current$available) ||
            !file.copy(current$path, file, overwrite = TRUE)) {
          shiny::showNotification(
            "Final workbook validation failed; no download was published.",
            type = "error", duration = NULL)
          fpq_stop("results.download_unavailable",
                   "The validated final workbook is unavailable.",
                   "infrastructure")
        }
      }
    )
  })
}
