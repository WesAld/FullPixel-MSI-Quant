test_that("generic two-group records translate to exact validated internal roles", {
  data_root <- tempfile("fpq-six-sample-")
  dir.create(data_root)
  groups <- c("Healthy", "Diseased")
  samples <- unlist(lapply(groups, function(group) lapply(1:3, function(i) {
    stem <- paste0(group, "_R", i)
    imzml <- file.path(data_root, paste0(stem, ".imzML"))
    ibd <- file.path(data_root, paste0(stem, ".ibd"))
    file.create(imzml); file.create(ibd)
    list(sample_id = stem, sample_label = stem, condition_id = group,
      replicate_id = paste0("R", i), inclusion_status = "included",
      imzml_path = imzml, ibd_path = ibd, notes = "")
  })), recursive = FALSE)
  mzmine_path <- file.path(data_root, "aligned_annotated.csv")
  utils::write.csv(data.frame(id = 1:2, mz = c(101, 202),
                              annotation = c("A", "B")),
                   mzmine_path, row.names = FALSE)
  mzmine <- fpq_confirm_mzmine_mapping(fpq_inspect_mzmine_csv(mzmine_path))
  config <- list(schema_version = "1.0.0", dataset_id = "dset-generic",
    analysis_type = "compare_two_groups", ion_mode = "NEG",
    exposure_label = "Single", samples = samples,
    conditions = list(make_condition("Healthy", "reference", TRUE, 1L),
                      make_condition("Diseased", "comparison", FALSE, 2L)),
    comparisons = list(make_comparison("cmp-1", "Healthy", "Diseased")))
  project_root <- tempfile("fpq-simple-adapter-project-")
  project <- fpq_create_project(project_root, "prj-simple-adapter", "Simple adapter")
  request <- fpq_prepare_validated_negative_request(project, config, mzmine,
    normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  expect_identical(vapply(request$samples, `[[`, "", "sample_id"),
    c("CTRL_R1", "CTRL_R2", "CTRL_R3", "TRT_R1", "TRT_R2", "TRT_R3"))
  expect_identical(request$comparison$reference_condition_id, "CTRL")
  expect_identical(request$comparison$comparison_condition_id, "TRT")
  expect_identical(request$column_mappings$target_mz, "mz")
  expect_identical(request$registered_inputs$aligned_target_table$path,
                   normalizePath(mzmine_path))
})

test_that("validated Negative adapter blocks unsupported designs honestly", {
  root <- tempfile("fpq-blocked-adapter-project-")
  project <- fpq_create_project(root, "prj-blocked-adapter", "Blocked adapter")
  config <- make_design(c("A", "B"), c(1, 1))
  config$analysis_type <- "compare_two_groups"
  mzmine <- list(status = "PASS")
  expect_error(fpq_prepare_validated_negative_request(project, config, mzmine,
    normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT"))),
    class = "fpq_scientific_error")
  config$ion_mode <- "POS"
  expect_error(fpq_prepare_validated_negative_request(project, config, mzmine,
    normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT"))),
    class = "fpq_scientific_error")
})
