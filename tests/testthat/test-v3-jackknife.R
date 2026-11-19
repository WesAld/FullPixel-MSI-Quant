test_that("jackknife defaults off and persists explicitly", {
  config <- make_design(c("CTRL", "TRT"), c(3, 3))
  config$analysis_type <- "compare_two_groups"
  expect_false(fpq_jackknife_enabled(config))
  config <- fpq_set_jackknife_enabled(config, TRUE)
  expect_true(fpq_jackknife_enabled(config))
  expect_identical(config$statistical_analyses$jackknife$resampling_unit,
    "matched_biological_replicate_pairs")
  expect_false(fpq_jackknife_enabled(fpq_set_jackknife_enabled(config, FALSE)))
})

test_that("jackknife export resolves analysis type from persisted configuration", {
  expect_identical(fpq_jackknife_analysis_type(
    workflow = list(), config = list(analysis_type = "compare_two_groups"),
    manifest = list()), "compare_two_groups")
  expect_identical(fpq_jackknife_analysis_type(
    workflow = list(), config = list(),
    manifest = list(project_preferences = list(
      analysis_type = "compare_two_groups"))), "compare_two_groups")
  expect_identical(fpq_jackknife_analysis_type(
    workflow = list(analysis_type = "quantify_only"),
    config = list(analysis_type = "compare_two_groups"), manifest = list()),
    "quantify_only")
})

test_that("eligibility uses explicit matched biological replicate IDs", {
  config <- make_design(c("CTRL", "TRT"), c(3, 3))
  config$analysis_type <- "compare_two_groups"
  design <- fpq_jackknife_design(config)
  expect_true(design$eligible)
  expect_identical(design$replicate_ids, c("R1", "R2", "R3"))
  expect_equal(design$n_pairs, 3L)

  config$samples[[6]]$replicate_id <- "R4"
  expect_false(fpq_jackknife_design(config)$eligible)
  expect_match(fpq_jackknife_design(config)$reason, "identical matched")
})

test_that("ineligible jackknife does not block the ordinary workflow", {
  config <- make_design(c("CTRL", "TRT"), c(2, 2))
  config$analysis_type <- "compare_two_groups"
  design <- fpq_jackknife_design(config)
  expect_false(design$eligible)
  expect_match(design$reason, "At least 3")
  expect_false(fpq_jackknife_enabled(config))
  expect_length(config$samples, 4L)
})

test_that("V3 GUI exposes deliberate include and remove controls", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_project.R"), warn = FALSE), collapse = "\n")
  service <- paste(readLines(file.path(fpq_repo_root(), "R",
    "jackknife_service.R"), warn = FALSE), collapse = "\n")
  expect_match(service, "+ Include Jackknife Statistics", fixed = TRUE)
  expect_match(service, "Remove Jackknife Statistics", fixed = TRUE)
  expect_match(service, "never individual MSI pixels", fixed = TRUE)
  expect_match(source, "fpq_set_jackknife_enabled", fixed = TRUE)
})

test_that("results card and workbook validation are conditional", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_results.R"), warn = FALSE), collapse = "\n")
  expect_match(source, "Jackknife robustness", fixed = TRUE)
  expect_match(source, '"Jackknife_Log2FC" %in% sheets', fixed = TRUE)
  expect_match(source, "JK_Omit_.+_Log2FC", fixed = TRUE)
})

test_that("published workbook discovery tolerates the inserted jackknife sheet", {
  sheets <- c(
    "Mean_Log2FC", "Jackknife_Log2FC", "All_Pixels",
    "TIC_normalized_intensity", "All_Features_TIC_Normalized", "QC", "README"
  )
  expect_identical(
    fpq_primary_intensity_sheet_from_names(sheets),
    "TIC_normalized_intensity"
  )
  expect_identical(
    fpq_all_features_sheet_from_names(sheets),
    "All_Features_TIC_Normalized"
  )
})
