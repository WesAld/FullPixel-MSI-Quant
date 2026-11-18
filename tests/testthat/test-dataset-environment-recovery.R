test_that("dataset files are hashed without modification and pair ambiguity fails", {
  root <- tempfile("fpq-data-"); dir.create(root)
  imzml <- file.path(root, "sample.imzML"); ibd <- file.path(root, "sample.ibd")
  writeLines("xml", imzml); writeBin(charToRaw("binary"), ibd)
  before <- c(fpq_hash_file(imzml), fpq_hash_file(ibd))
  expect_silent(fpq_validate_pair(imzml, ibd))
  expect_equal(before, c(fpq_hash_file(imzml), fpq_hash_file(ibd)))
  other <- file.path(root, "other.ibd"); writeBin(charToRaw("binary"), other)
  expect_error(fpq_validate_pair(imzml, other), class = "fpq_input_error")
  expect_error(fpq_validate_pair(file.path(root, "missing.imzML"), ibd),
               class = "fpq_input_error")
})

test_that("dataset registration persists exact external files and rejects duplicate IDs", {
  project_root <- tempfile("fpq-project-")
  project <- fpq_create_project(project_root, "prj-dataset-test", "Dataset")
  data_root <- tempfile("fpq-data-"); dir.create(data_root)
  imzml <- file.path(data_root, "sample.imzML")
  ibd <- file.path(data_root, "sample.ibd")
  targets <- file.path(data_root, "targets.csv")
  writeLines("<imzML/>", imzml); writeBin(charToRaw("binary"), ibd)
  writeLines(c("id,mz", "1,100.1"), targets)
  dataset <- make_design(c("Control"), 1)
  dataset$schema_version <- "1.0.0"
  dataset$dataset_id <- "dset-registration-test"
  dataset$raw_data_pairs <- list(list(sample_id = dataset$samples[[1]]$sample_id,
                                      imzml_path = imzml, ibd_path = ibd))
  dataset$aligned_feature_list <- list(path = targets)
  registered <- fpq_register_dataset(project, dataset)
  loaded <- fpq_load_configuration(registered, "datasets", dataset$dataset_id)
  expect_identical(loaded$registered_raw_data_pairs[[1]]$imzml$sha256,
                   fpq_hash_file(imzml))
  expect_identical(loaded$registered_aligned_feature_list$artifact_type,
                   "aligned_mzmine_target_list")
  expect_length(fpq_list_artifacts(registered), 3)
  expect_true(all(vapply(fpq_list_artifacts(registered),
    function(x) x$validation_status == "PASS", logical(1))))
  expect_error(fpq_register_dataset(registered, dataset),
               class = "fpq_configuration_error")
})

test_that("environment and frozen integrity checks are non-destructive", {
  integrity <- fpq_verify_scientific_core()
  expect_identical(integrity$status, "compatible")
  environment <- fpq_check_environment()
  expect_true(environment$status %in%
                c("compatible", "compatible_with_warning", "unresolved"))
  request <- list(environment_requirements = list(r_executable = "Z:/missing/Rscript.exe",
                                                  required_packages = c("notARealPackage")))
  expect_identical(fpq_check_environment(request)$status, "incompatible")
})

test_that("stale lock requires explicit recovery", {
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-lock-test", "Lock")
  lock <- file.path(fpq_project_paths(root)$internal_root,
                    ".fpq-project.lock"); writeLines("stale", lock)
  Sys.setFileTime(lock, Sys.time() - 7200)
  expect_error(fpq_with_project_lock(root, TRUE), class = "fpq_infrastructure_error")
  result <- fpq_recover_project(project, "remove_stale_lock", stale_seconds = 3600)
  expect_true(result$lock_removed)
  expect_false(file.exists(lock))
})
