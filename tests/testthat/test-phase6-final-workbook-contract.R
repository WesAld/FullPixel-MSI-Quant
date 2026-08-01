test_that("final workbook contract rejects legacy and internal sheets", {
  expect_identical(fpq_final_workbook_sheets(),
    c("Mean_Log2FC", "All_Pixels", "TIC_normalized_intensity", "QC", "README"))
  expect_true(all(c("Mean_Log2FC R2", "Calculation_Audit", "Source_Files",
    "Combined_All_Features", "Methods_and_Settings") %in%
      fpq_prohibited_final_workbook_sheets()))

  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  for (sheet in c(fpq_final_workbook_sheets(), "Calculation_Audit"))
    openxlsx::addWorksheet(wb, sheet)
  openxlsx::saveWorkbook(wb, path)
  result <- fpq_validate_final_results_workbook(path)
  expect_identical(result$status, "FAIL")
  expect_true(any(grepl("Worksheet names or order", result$errors)))
  expect_true(any(grepl("Calculation_Audit", result$errors)))
})

test_that("readout header contracts match the authority structure", {
  headers <- fpq_final_readout_headers()
  expect_length(headers$Mean_Log2FC, 24L)
  expect_length(headers$All_Pixels, 25L)
  expect_length(headers$TIC_normalized_intensity, 24L)
  expect_identical(tail(headers$Mean_Log2FC, 2),
    c("GraphPad_label", "mean_log2FC_PVL_vs_CTRL"))
  expect_identical(tail(headers$All_Pixels, 2),
    c("GraphPad_label", "ion_positive_pixel_change_PVL_minus_CTRL_pp"))
  expect_identical(tail(headers$TIC_normalized_intensity, 2),
    c("GraphPad_label", "mean_positive_pixel_intensity_log2FC_PVL_vs_CTRL"))
})

test_that("Results download is published only for a validated workbook", {
  root <- tempfile("fpq-results-")
  dir.create(file.path(root, "tests", "fixtures", "reports"),
             recursive = TRUE)
  unavailable <- fpq_current_final_results_workbook(root)
  expect_false(unavailable$available)
  expect_match(unavailable$reason, "not been created")
})

test_that("Results UI uses the single user-facing workbook label", {
  server_source <- paste(deparse(body(fpq_results_server)), collapse = "\n")
  expect_match(server_source, "Download Final Results Workbook")
  expect_match(server_source, "Final results workbook created")
  expect_false(grepl("CSV", server_source, fixed = TRUE))
})

test_that("generated Phase 6 workbook satisfies the public-sheet gate", {
  report_path <- file.path(fpq_repo_root(), "tests", "fixtures", "reports",
                           "phase6_presentation_adapter_report.json")
  skip_if_not(file.exists(report_path), "Phase 6 generated artifact is unavailable")
  report <- jsonlite::fromJSON(report_path)
  result <- fpq_validate_final_results_workbook(report$output_workbook,
                                                expected_rows = 221L)
  expect_identical(result$status, "PASS")
  expect_identical(result$sheets, fpq_final_workbook_sheets())
})
