test_that("MSI discovery is recursive and extension matching is case-insensitive", {
  root <- tempfile("fpq-msi-tree-")
  dir.create(file.path(root, "nested", "Leaf_R3"), recursive = TRUE)
  file.create(file.path(root, "nested", "Leaf_R3", "scan.imzML"))
  file.create(file.path(root, "nested", "Leaf_R3", "scan.IBD"))
  file.create(file.path(root, "Plant17.imzml"))
  file.create(file.path(root, "Plant17.ibd"))

  rows <- fpq_pair_msi_files(root, "NEG")
  expect_length(rows, 2L)
  expect_true(all(vapply(rows, function(x) x$pair_status == "complete", logical(1))))
  leaf <- Filter(function(x) x$biological_sample == "Leaf_R3", rows)[[1]]
  expect_identical(leaf$sample_group, "Leaf")
  expect_identical(leaf$replicate, "R3")
  plant <- Filter(function(x) x$biological_sample == "Plant17", rows)[[1]]
  expect_identical(plant$sample_group, "")
  expect_identical(plant$replicate, "")
})

test_that("missing counterparts are reported instead of silently discarded", {
  root <- tempfile("fpq-msi-incomplete-")
  dir.create(root)
  file.create(file.path(root, "complete.imzML"))
  file.create(file.path(root, "complete.ibd"))
  file.create(file.path(root, "missing.imzML"))
  rows <- fpq_pair_msi_files(root, "POS")
  expect_length(rows, 2L)
  incomplete <- Filter(function(x) x$pair_status == "missing_ibd", rows)
  expect_length(incomplete, 1L)
  expect_false(incomplete[[1]]$include)
  expect_identical(incomplete[[1]]$validation_status, "FAIL")
})

test_that("review data hides paths by default and exposes them on request", {
  root <- tempfile("fpq-msi-review-")
  dir.create(root)
  file.create(file.path(root, "sample.imzML"))
  file.create(file.path(root, "sample.ibd"))
  rows <- fpq_pair_msi_files(root, "NEG")
  compact <- fpq_msi_review_data(rows)
  detailed <- fpq_msi_review_data(rows, details = TRUE)
  expect_false("imzml_path" %in% names(compact))
  expect_true(all(c("imzml_path", "ibd_path") %in% names(detailed)))
  expect_identical(compact$imzml_filename, "sample.imzML")
})

test_that("two-group sample import preserves the inferred condition assignment", {
  root <- tempfile("fpq-msi-confirm-two-group-")
  dir.create(root)
  file.create(file.path(root, "Control_R1.imzML"))
  file.create(file.path(root, "Control_R1.ibd"))
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  state$analysis_type("compare_two_groups")
  state$selected_ion_mode("NEG")
  current_configuration <- shiny::isolate(state$configuration())
  state$configuration(fpq_apply_analysis_design(
    current_configuration, "compare_two_groups", "NEG"))
  state$detected_samples(fpq_pair_msi_files(root, "NEG"))
  shiny::testServer(fpq_sample_import_server, args = list(state = state), {
    session$setInputs(confirm = 1)
    session$flushReact()
    expect_length(state$configuration()$samples, 1L)
    expect_identical(state$configuration()$samples[[1]]$imzml_path,
                     state$detected_samples()[[1]]$imzml_path)
    expect_identical(state$configuration()$conditions[[1]]$condition_id, "Control")
    expect_identical(state$configuration()$samples[[1]]$ion_mode, "NEG")
  })
})

test_that("quantify-only sample import uses the independent samples condition", {
  root <- tempfile("fpq-msi-confirm-quantify-")
  dir.create(root)
  file.create(file.path(root, "Control_R1.imzML"))
  file.create(file.path(root, "Control_R1.ibd"))
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  state$detected_samples(fpq_pair_msi_files(root, "NEG"))
  shiny::testServer(fpq_sample_import_server, args = list(state = state), {
    session$setInputs(confirm = 1)
    session$flushReact()
    expect_length(state$configuration()$samples, 1L)
    expect_identical(state$configuration()$conditions[[1]]$condition_id, "samples")
    expect_identical(state$configuration()$samples[[1]]$condition_id, "samples")
  })
})
