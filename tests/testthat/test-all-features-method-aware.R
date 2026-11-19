test_that("all-features export metadata follows the selected normalization", {
  workflows <- list(
    raw = list(display_metadata = list(normalization = list(
      primary_normalization_method = "raw"))),
    spectrum_sum = list(display_metadata = list(normalization = list(
      primary_normalization_method = "spectrum_sum"))),
    rms = list(display_metadata = list(normalization = list(
      primary_normalization_method = "rms"))),
    median = list(display_metadata = list(normalization = list(
      primary_normalization_method = "median")))
  )

  expect_identical(fpq_all_features_sheet_name(workflows$raw),
                   "All_Features_RAW")
  expect_identical(fpq_all_features_sheet_name(workflows$spectrum_sum),
                   "All_Features_TIC_Normalized")
  expect_identical(fpq_all_features_sheet_name(workflows$rms),
                   "All_Features_RMS")
  expect_identical(fpq_all_features_sheet_name(workflows$median),
                   "All_Features_MEDIAN")

  expect_identical(fpq_all_features_intensity_description(workflows$raw),
                   "individual raw intensities")
  expect_identical(
    fpq_all_features_intensity_description(workflows$spectrum_sum),
    "individual TIC-normalized intensities")
  expect_identical(fpq_all_features_intensity_description(workflows$rms),
                   "individual RMS-normalized intensities")
  expect_identical(fpq_all_features_intensity_description(workflows$median),
                   "individual median-normalized intensities")
})

test_that("all-features metadata retains the legacy spectrum-sum default", {
  expect_identical(fpq_all_features_sheet_name(list()),
                   "All_Features_TIC_Normalized")
})

test_that("explicit methods never fall back to spectrum-sum", {
  for (method in c("raw", "rms", "median")) {
    workflow <- list(display_metadata = list(normalization = list(
      primary_normalization_method = method)))
    expect_identical(fpq_all_features_normalization_method(workflow), method)
  }
})

test_that("completed generic export is persisted before all-features work", {
  source_text <- paste(readLines(file.path(
    fpq_repo_root(), "R", "zzz_all_features_comparison_extension.R")),
    collapse = "\n")
  generic_return <- regexpr(
    "fpq_run_script07_and_presentation_before_all_features\\(",
    source_text)[[1L]]
  completed_boundary <- regexpr(
    'fpq_set_workflow_stage\\([\\s\\S]*?"combined_export"[\\s\\S]*?"completed"',
    source_text, perl = TRUE)[[1L]]
  all_features_call <- gregexpr(
    "fpq_add_all_features_comparison_sheet\\(", source_text)[[1L]]
  all_features_call <- all_features_call[[length(all_features_call)]]

  expect_gt(generic_return, 0L)
  expect_gt(completed_boundary, generic_return)
  expect_gt(all_features_call, completed_boundary)
})
