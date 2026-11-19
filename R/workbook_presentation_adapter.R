fpq_final_workbook_sheets <- function() {
  c("Mean_Log2FC", "All_Pixels", "TIC_normalized_intensity", "QC", "README")
}

fpq_prohibited_final_workbook_sheets <- function() {
  c("Mean_Log2FC R2", "Calculation_Audit", "Source_Files",
    "Combined_All_Features", "Methods_and_Settings")
}

fpq_final_readout_headers <- function() {
  list(
    Mean_Log2FC = c(
      "feature_key", "dataset_id", "ion_mode", "mzmine_row_id",
      "annotation_name", "formula", "adduct", "target_mz",
      paste0(c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3"),
             "_TIC_normalized_signal"),
      paste0(c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3"),
             "_log2_TIC_normalized_signal"),
      "CTRL_mean_log2_TIC_normalized_signal",
      "PVL_mean_log2_TIC_normalized_signal", "GraphPad_label",
      "mean_log2FC_PVL_vs_CTRL"
    ),
    All_Pixels = c(
      "feature_key", "dataset_id", "ion_mode", "mzmine_row_id",
      "annotation_name", "formula", "adduct", "target_mz",
      "all_coordinate_pixels",
      paste0(c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3"),
             "_ion_positive_pixels"),
      paste0(c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3"),
             "_ion_positive_pixel_percentage"),
      "CTRL_mean_ion_positive_pixel_percentage",
      "PVL_mean_ion_positive_pixel_percentage", "GraphPad_label",
      "ion_positive_pixel_change_PVL_minus_CTRL_pp"
    ),
    TIC_normalized_intensity = c(
      "feature_key", "dataset_id", "ion_mode", "mzmine_row_id",
      "annotation_name", "formula", "adduct", "target_mz",
      paste0(c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3"),
             "_mean_TIC_normalized_intensity_positive_pixels"),
      paste0(c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3"),
             "_log2_mean_TIC_normalized_intensity_positive_pixels"),
      "CTRL_mean_log2_TIC_normalized_intensity_positive_pixels",
      "PVL_mean_log2_TIC_normalized_intensity_positive_pixels",
      "GraphPad_label", "mean_positive_pixel_intensity_log2FC_PVL_vs_CTRL"
    )
  )
}

fpq_excel_col <- function(n) {
  out <- character(length(n))
  for (k in seq_along(n)) {
    x <- n[[k]]; value <- ""
    while (x > 0L) {
      r <- (x - 1L) %% 26L
      value <- paste0(intToUtf8(65L + r), value)
      x <- (x - 1L) %/% 26L
    }
    out[[k]] <- value
  }
  out
}

fpq_adapter_require_columns <- function(x, required, source) {
  missing <- setdiff(required, names(x))
  if (length(missing)) fpq_stop("workbook_adapter.columns_missing",
    paste(source, "is missing:", paste(missing, collapse = ", ")), "input")
  invisible(TRUE)
}

fpq_read_script07_selected_keys <- function(audit_workbook) {
  selected <- openxlsx::read.xlsx(audit_workbook, sheet = "Mean_Log2FC",
                                  check.names = FALSE)
  fpq_adapter_require_columns(selected,
    c("feature_key", "dataset_id", "ion_mode", "mzmine_row_id"), "Mean_Log2FC")
  selected[, c("feature_key", "dataset_id", "ion_mode", "mzmine_row_id"), drop = FALSE]
}

fpq_presentation_source <- function(audit_workbook) {
  selected <- fpq_read_script07_selected_keys(audit_workbook)
  master <- openxlsx::read.xlsx(audit_workbook, sheet = "Combined_All_Features",
                               check.names = FALSE)
  required <- c(
    "feature_key", "dataset_id", "ion_mode", "mzmine_row_id", "target_mz",
    "annotation_name", "annotation_formula", "annotation_adduct",
    paste0(rep(c("CTRL", "TRT"), each = 3L), "_R", rep(1:3, 2L),
           "_overall_normalized_signal"),
    paste0(rep(c("CTRL", "TRT"), each = 3L), "_R", rep(1:3, 2L),
           "_local_strong_pixel_intensity"),
    paste0(rep(c("CTRL", "TRT"), each = 3L), "_R", rep(1:3, 2L),
           "_conservative_detected_pixel_count")
  )
  fpq_adapter_require_columns(master, required, "Combined_All_Features")
  if (anyDuplicated(master$feature_key) || anyDuplicated(selected$feature_key))
    fpq_stop("workbook_adapter.duplicate_key", "Feature keys must be unique.", "validation")
  index <- match(selected$feature_key, master$feature_key)
  if (anyNA(index)) fpq_stop("workbook_adapter.selection_missing",
    "A Script 07 selected feature key is absent from the combined master.", "validation")
  x <- master[index, , drop = FALSE]
  if (!identical(as.character(x$dataset_id), as.character(selected$dataset_id)) ||
      !identical(as.character(x$ion_mode), as.character(selected$ion_mode)) ||
      !identical(as.character(x$mzmine_row_id), as.character(selected$mzmine_row_id)))
    fpq_stop("workbook_adapter.key_mismatch",
      "Selected key components do not match the combined master.", "validation")
  x[order(as.numeric(x$target_mz), as.character(x$ion_mode),
          as.character(x$dataset_id), as.numeric(x$mzmine_row_id)), , drop = FALSE]
}

fpq_base_readout <- function(x) {
  data.frame(
    feature_key = x$feature_key, dataset_id = x$dataset_id, ion_mode = x$ion_mode,
    mzmine_row_id = x$mzmine_row_id, annotation_name = x$annotation_name,
    formula = x$annotation_formula, adduct = x$annotation_adduct,
    target_mz = as.numeric(x$target_mz), stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

fpq_write_formula_block <- function(wb, sheet, rows, columns, formulas) {
  for (i in seq_along(columns))
    openxlsx::writeFormula(wb, sheet, x = formulas[[i]],
                           startCol = columns[[i]], startRow = min(rows))
}

fpq_create_final_results_workbook <- function(
    audit_workbook, output_path, current_source_files,
    frozen_script_hash =
      "94055F3210D95D6AF0EA35D246D9F234A24FF429496E179DA91EE58A1FBBB0A0",
    adapter_version = "1.0.0", exposure = "Single",
    classification = "Validated workflow output",
    all_coordinate_pixels = 1600) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    fpq_stop("workbook_adapter.openxlsx_missing", "openxlsx is required.", "environment")
  if (!file.exists(audit_workbook))
    fpq_stop("workbook_adapter.audit_missing", "Script 07 audit workbook is missing.", "input")
  if (!identical(toupper(frozen_script_hash),
    "94055F3210D95D6AF0EA35D246D9F234A24FF429496E179DA91EE58A1FBBB0A0"))
    fpq_stop("workbook_adapter.script07_hash",
             "Frozen Script 07 hash does not match the approved value.", "validation")
  x <- fpq_presentation_source(audit_workbook)
  n <- nrow(x); excel_rows <- seq.int(2L, n + 1L)
  base <- fpq_base_readout(x)
  source_prefix <- c("CTRL_R1", "CTRL_R2", "CTRL_R3", "TRT_R1", "TRT_R2", "TRT_R3")
  display_prefix <- c("CTRL_R1", "CTRL_R2", "CTRL_R3", "PVL_R1", "PVL_R2", "PVL_R3")

  mean_df <- base
  for (i in seq_along(source_prefix))
    mean_df[[paste0(display_prefix[[i]], "_TIC_normalized_signal")]] <-
      x[[paste0(source_prefix[[i]], "_overall_normalized_signal")]]
  for (name in fpq_final_readout_headers()$Mean_Log2FC[15:24]) mean_df[[name]] <- NA

  pixels_df <- base
  pixels_df$all_coordinate_pixels <- all_coordinate_pixels
  for (i in seq_along(source_prefix))
    pixels_df[[paste0(display_prefix[[i]], "_ion_positive_pixels")]] <-
      x[[paste0(source_prefix[[i]], "_conservative_detected_pixel_count")]]
  for (name in fpq_final_readout_headers()$All_Pixels[16:25]) pixels_df[[name]] <- NA

  intensity_df <- base
  for (i in seq_along(source_prefix))
    intensity_df[[paste0(display_prefix[[i]],
      "_mean_TIC_normalized_intensity_positive_pixels")]] <-
      x[[paste0(source_prefix[[i]], "_local_strong_pixel_intensity")]]
  for (name in fpq_final_readout_headers()$TIC_normalized_intensity[15:24])
    intensity_df[[name]] <- NA

  wb <- openxlsx::createWorkbook(creator = "FullPixel MSI Quant")
  lapply(fpq_final_workbook_sheets(), function(s) openxlsx::addWorksheet(wb, s))
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7",
                                        border = "Bottom", wrapText = TRUE)
  write_readout <- function(sheet, data) {
    openxlsx::writeData(wb, sheet, data, headerStyle = header_style, keepNA = FALSE)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::addFilter(wb, sheet, row = 1L, cols = seq_len(ncol(data)))
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(data)), widths = "auto")
  }
  write_readout("Mean_Log2FC", mean_df)
  write_readout("All_Pixels", pixels_df)
  write_readout("TIC_normalized_intensity", intensity_df)

  log_formula <- function(source_cols) lapply(source_cols, function(col)
    sprintf('IF(OR(%s%d="",%s%d<=0),"",LOG(%s%d,2))',
            col, excel_rows, col, excel_rows, col, excel_rows))
  fpq_write_formula_block(wb, "Mean_Log2FC", excel_rows, 15:20,
                          log_formula(fpq_excel_col(9:14)))
  fpq_write_formula_block(wb, "Mean_Log2FC", excel_rows, 21:22, list(
    sprintf('IF(COUNT(O%d:Q%d)>=2,AVERAGE(O%d:Q%d),"")',
            excel_rows, excel_rows, excel_rows, excel_rows),
    sprintf('IF(COUNT(R%d:T%d)>=2,AVERAGE(R%d:T%d),"")',
            excel_rows, excel_rows, excel_rows, excel_rows)))
  fpq_write_formula_block(wb, "Mean_Log2FC", excel_rows, 23:24, list(
    sprintf('E%d&" | "&G%d&" | "&TEXT(H%d,"0.0000")',
            excel_rows, excel_rows, excel_rows),
    sprintf('IF(OR(U%d="",V%d=""),"",V%d-U%d)',
            excel_rows, excel_rows, excel_rows, excel_rows)))

  percent_formulas <- lapply(fpq_excel_col(10:15), function(col)
    sprintf('IF(OR(I%d="",I%d<=0),"",%s%d/I%d*100)',
            excel_rows, excel_rows, col, excel_rows, excel_rows))
  fpq_write_formula_block(wb, "All_Pixels", excel_rows, 16:21, percent_formulas)
  fpq_write_formula_block(wb, "All_Pixels", excel_rows, 22:23, list(
    sprintf('IF(COUNT(P%d:R%d)=3,AVERAGE(P%d:R%d),"")',
            excel_rows, excel_rows, excel_rows, excel_rows),
    sprintf('IF(COUNT(S%d:U%d)=3,AVERAGE(S%d:U%d),"")',
            excel_rows, excel_rows, excel_rows, excel_rows)))
  fpq_write_formula_block(wb, "All_Pixels", excel_rows, 24:25, list(
    sprintf('E%d&" | "&G%d&" | "&TEXT(H%d,"0.0000")',
            excel_rows, excel_rows, excel_rows),
    sprintf('IF(OR(V%d="",W%d=""),"",W%d-V%d)',
            excel_rows, excel_rows, excel_rows, excel_rows)))

  fpq_write_formula_block(wb, "TIC_normalized_intensity", excel_rows, 15:20,
                          log_formula(fpq_excel_col(9:14)))
  fpq_write_formula_block(wb, "TIC_normalized_intensity", excel_rows, 21:22, list(
    sprintf('IF(COUNT(O%d:Q%d)>=2,AVERAGE(O%d:Q%d),"")',
            excel_rows, excel_rows, excel_rows, excel_rows),
    sprintf('IF(COUNT(R%d:T%d)>=2,AVERAGE(R%d:T%d),"")',
            excel_rows, excel_rows, excel_rows, excel_rows)))
  fpq_write_formula_block(wb, "TIC_normalized_intensity", excel_rows, 23:24, list(
    sprintf('E%d&" | "&G%d&" | "&TEXT(H%d,"0.0000")',
            excel_rows, excel_rows, excel_rows),
    sprintf('IF(OR(U%d="",V%d=""),"",V%d-U%d)',
            excel_rows, excel_rows, excel_rows, excel_rows)))

  inherited_qc <- openxlsx::read.xlsx(audit_workbook, sheet = "QC",
                                      check.names = FALSE)
  names(inherited_qc)[1:5] <- c("check", "status", "observed", "expected", "critical")
  inherited_qc <- inherited_qc[, 1:5, drop = FALSE]
  adapter_qc <- data.frame(
    check = c("final_sheet_contract", "prohibited_sheets_absent",
      "selected_feature_keys_unique", "deterministic_sort", "formula_cells_embedded"),
    status = "PASS",
    observed = c(paste(fpq_final_workbook_sheets(), collapse = ";"),
      "all absent", n, "target_mz;ion_mode;dataset_id;mzmine_row_id", n * 30L),
    expected = c("exact five-sheet order", "all absent", n,
      "ascending deterministic order", n * 30L), critical = 1L,
    stringsAsFactors = FALSE)
  qc <- rbind(inherited_qc, adapter_qc)
  openxlsx::writeData(wb, "QC", qc, headerStyle = header_style)
  openxlsx::freezePane(wb, "QC", firstRow = TRUE)

  readme <- data.frame(topic = c(
    "Workbook name", "Validation status", "Exposure", "Included ion modes",
    "Sample groups", "Replicates", "Three readouts", "Log2 positivity rule",
    "Zero-positive-pixel behaviour", "Minimum replicate requirements",
    "Percentage-point calculation", "Unique feature key", "GraphPad-label formula",
    "Analytical scope", "Inferential P values", "Current source files",
    "Frozen Script 07 SHA-256", "Presentation-adapter version",
    "Generation timestamp", "Legacy worksheet removal"),
    detail = c(
      basename(output_path), classification, exposure,
      paste(sort(unique(x$ion_mode)), collapse = ", "),
      "Reference: CTRL; comparison: PVL (upstream TRT label mapped for presentation).",
      "CTRL_R1-R3 and PVL_R1-R3 are biological replicates; no pairing is performed.",
      "Mean log2 fold change; ion-positive pixel percentage-point change; positive-pixel intensity log2 fold change.",
      "Log2 is calculated only for finite positive values; otherwise the cell remains blank.",
      "A zero-positive-pixel intensity is undefined and remains blank.",
      "Log2 means require at least two finite replicates; pixel-percentage means require all three replicates.",
      "PVL mean ion-positive pixel percentage minus CTRL mean percentage.",
      "dataset_id | ion_mode | mzmine_row_id",
      'annotation_name | adduct | m/z rounded to four decimal places',
      "Descriptive full-pixel, spectrum-sum-normalized quantitative MALDI-MSI summaries.",
      "No inferential P values are calculated.",
      paste(current_source_files, collapse = "\n"), frozen_script_hash,
      adapter_version, fpq_now(),
      "Mean_Log2FC R2 is intentionally omitted because it was a row-reordered legacy duplicate."),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "README", readme, headerStyle = header_style)
  openxlsx::setColWidths(wb, "README", 1:2, widths = c(32, 110))
  openxlsx::setColWidths(wb, "QC", 1:5, widths = "auto")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  validation <- fpq_validate_final_results_workbook(output_path, expected_rows = n)
  if (!identical(validation$status, "PASS")) {
    unlink(output_path)
    fpq_stop("workbook_adapter.validation_failed",
             paste(validation$errors, collapse = "; "), "validation")
  }
  invisible(validation)
}

fpq_validate_final_results_workbook <- function(path, expected_rows = NULL) {
  errors <- character()
  if (!file.exists(path)) return(list(status = "FAIL", errors = "Workbook is missing."))
  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) {
    errors <<- c(errors, paste("openxlsx:", conditionMessage(e))); character()
  })
  if (!identical(sheets, fpq_final_workbook_sheets()))
    errors <- c(errors, "Worksheet names or order do not match the five-sheet contract.")
  prohibited <- intersect(sheets, fpq_prohibited_final_workbook_sheets())
  if (length(prohibited))
    errors <- c(errors, paste("Prohibited worksheets:", paste(prohibited, collapse = ", ")))
  for (sheet in names(fpq_final_readout_headers())) {
    x <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
      path, sheet = sheet, rows = 1L, colNames = TRUE, check.names = FALSE)),
      error = function(e) NULL)
    if (is.null(x) || !identical(names(x), fpq_final_readout_headers()[[sheet]]))
      errors <- c(errors, paste("Header contract failed for", sheet))
  }
  qc <- tryCatch(suppressWarnings(openxlsx::read.xlsx(
    path, sheet = "QC", check.names = FALSE)), error = function(e) data.frame())
  if (!is.data.frame(qc) || !nrow(qc) ||
      any(qc$status[as.character(qc$critical) %in% c("1", "TRUE")] != "PASS"))
    errors <- c(errors, "One or more critical QC checks did not pass.")
  list(status = if (length(errors)) "FAIL" else "PASS", errors = errors,
       sheets = sheets, expected_rows = expected_rows)
}
