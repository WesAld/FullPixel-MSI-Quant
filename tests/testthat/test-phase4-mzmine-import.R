test_that("standard aligned annotated MZmine columns are detected", {
  columns <- c("row ID", "row m/z", "Compound name", "Molecular formula",
               "Adduct", "row retention time", "Annotation score", "Area sample")
  result <- fpq_detect_mzmine_columns(columns)
  expect_identical(result$status, "PASS")
  expect_identical(result$mappings$feature_id, "row ID")
  expect_identical(result$mappings$mz, "row m/z")
  expect_identical(result$mappings$annotation, "Compound name")
  expect_identical(result$mappings$formula, "Molecular formula")
  expect_identical(result$mappings$adduct, "Adduct")
})

test_that("multiple plausible m/z columns are never selected silently", {
  result <- fpq_detect_mzmine_columns(c("row ID", "m/z", "average m/z", "Name"))
  expect_identical(result$status, "NEEDS_REVIEW")
  expect_true("mz" %in% names(result$ambiguous))
  expect_null(result$mappings$mz)
  expect_error(fpq_confirm_mzmine_mapping(result),
               class = "fpq_configuration_error")
  result$mappings$mz <- "m/z"
  confirmed <- fpq_confirm_mzmine_mapping(result)
  expect_identical(confirmed$status, "PASS")
  expect_identical(confirmed$mappings$mz, "m/z")
})

test_that("one MZmine CSV supplies targets and annotation metadata", {
  path <- tempfile(fileext = ".csv")
  data <- data.frame("row ID" = c(1, 2), "row m/z" = c(101.1, 202.2),
    "Compound name" = c("A", "B"), "Molecular formula" = c("CH4", "C2H6"),
    "Adduct" = c("[M-H]-", "[M-H]-"), check.names = FALSE)
  utils::write.csv(data, path, row.names = FALSE)
  inspection <- fpq_inspect_mzmine_csv(path)
  expect_identical(inspection$feature_count, 2L)
  expect_identical(inspection$status, "PASS")
  expect_identical(inspection$path, normalizePath(path))
  expect_true(all(c("mz", "annotation", "formula", "adduct") %in%
                    names(Filter(Negate(is.null), inspection$mappings))))
})

test_that("required MZmine fields are reported when absent", {
  result <- fpq_detect_mzmine_columns(c("Name", "Formula", "Adduct"))
  expect_setequal(unlist(result$missing_required), c("feature_id", "mz"))
  expect_identical(result$status, "NEEDS_REVIEW")
})
