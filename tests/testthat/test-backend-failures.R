test_that("generic identity and reference failures are typed", {
  duplicate_conditions <- make_design(c("Control", "Treatment"), c(2, 2))
  duplicate_conditions$conditions[[2]]$condition_id <- "Control"
  expect_error(fpq_validate_dynamic_configuration(duplicate_conditions),
               class = "fpq_configuration_error")
  duplicate_samples <- make_design(c("Control", "Treatment"), c(2, 2))
  duplicate_samples$samples[[2]]$sample_id <- duplicate_samples$samples[[1]]$sample_id
  expect_error(fpq_validate_dynamic_configuration(duplicate_samples),
               class = "fpq_configuration_error")
  bad_comparison <- make_design(c("Control", "Treatment"), c(2, 2))
  bad_comparison$comparisons[[1]]$comparison_condition_id <- "missing"
  expect_error(fpq_validate_dynamic_configuration(bad_comparison),
               class = "fpq_configuration_error")
  missing_replicate <- make_design(c("Control", "Treatment"), c(2, 2))
  missing_replicate$samples[[1]]$replicate_id <- ""
  expect_error(fpq_validate_dynamic_configuration(missing_replicate),
               class = "fpq_configuration_error")
})

test_that("malformed records and unsupported schema versions fail closed", {
  malformed <- tempfile(fileext = ".json"); writeLines("{bad", malformed)
  expect_error(fpq_read_json(malformed), class = "fpq_configuration_error")
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-schema-test", "Schema")
  value <- make_design(c("Control", "Treatment"), c(2, 2))
  value$schema_version <- "99.0.0"
  expect_error(fpq_save_configuration(project, "datasets", "bad", value),
               class = "fpq_configuration_error")
})

test_that("profile validation separates invalid designs from candidate status", {
  design <- make_design(c("Control", "Treatment"), c(3, 3))
  design$ion_mode <- "POS"
  expect_identical(fpq_validate_profile_compatibility(design,
    "pvl-fullpixel-reference-v1")$classification,
    "profile_incompatible")
  design$ion_mode <- "NEG"; design$exposure_label <- "Sequential"
  expect_identical(fpq_validate_profile_compatibility(design,
    "pvl-fullpixel-reference-v1")$classification, "profile_incompatible")
})
