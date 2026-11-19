test_that("V2 normalization configuration is strict and backward compatible", {
  expect_identical(fpq_normalization_methods(),
                   c("raw", "spectrum_sum", "rms", "median"))
  expect_identical(fpq_normalization_method(list()), "spectrum_sum")
  expect_identical(fpq_normalization_method(
    list(primary_normalization_method = "RMS")), "rms")
  expect_error(fpq_normalization_method(
    list(primary_normalization_method = "unsupported")),
    class = "fpq_scientific_error")
  expect_identical(fpq_primary_summary_columns("raw"),
    c(overall = "raw_mean_all_valid_pixels",
      local = "raw_mean_conservative_detected_pixels"))
  expect_identical(fpq_primary_summary_columns("spectrum_sum"),
    c(overall = "normalized_mean_all_valid_pixels",
      local = "normalized_mean_conservative_detected_pixels"))
})

test_that("RMS and Median denominator mathematics are deterministic", {
  environment <- new.env(parent = globalenv())
  sys.source(file.path(fpq_repo_root(), "tools",
    "normalization_execution_extension.R"), envir = environment)
  rms <- environment$fpq_v2_stable_rms
  med <- environment$fpq_v2_median_positive
  expect_equal(rms(c(10, 20, 30, 40)), sqrt(750), tolerance = 1e-12)
  expect_equal(20 / rms(c(10, 20, 30, 40)) * 1e6,
               730296.7433402214, tolerance = 1e-8)
  expect_equal(med(c(10, 20, 30, 40)), 25)
  expect_equal(20 / med(c(10, 20, 30, 40)) * 1e6, 800000)
  expect_equal(rms(c(0, 10, 20, 30, 0)), sqrt(mean(c(10, 20, 30)^2)))
  expect_equal(med(c(0, 10, 20, 30, 0)), 20)
  expect_equal(rms(c(-5, 10, 20)), sqrt(250))
  expect_equal(med(c(-5, 10, 20)), 15)
  expect_equal(rms(c(0, 0, 0)), 0)
  expect_equal(med(c(0, 0, 0)), 0)
  expect_equal(rms(25), 25)
  expect_equal(med(25), 25)
  expect_equal(med(c(10, 20, 30)), 20)
  expect_equal(med(c(10, 20, 30, 40)), 25)
  expect_true(is.finite(rms(c(1e308, 5e307))))
  expect_equal(rms(c(NA, NaN, Inf, -Inf, 10)), 10)
})

test_that("target absence preserves exact zero for every valid strategy", {
  denominators <- c(spectrum_sum = 100, rms = sqrt(750), median = 25)
  expect_identical(0 / denominators * 1e6, c(spectrum_sum = 0,
    rms = 0, median = 0))
  expect_identical(as.numeric(0), 0)
})

test_that("normalization selection persists through project reload", {
  root <- tempfile("fpq-v2-normalization-")
  project <- fpq_create_project(root, "prj-v2-normalization", "V2 normalization",
    project_preferences = list(primary_normalization_method = "median"))
  config <- list(schema_version = "1.0.0", dataset_id = "dset-v2-normalization",
    conditions = list(), samples = list(), comparisons = list(),
    primary_normalization_method = "rms")
  fpq_save_configuration(project, "datasets", config$dataset_id, config,
                         fpq_validate_dynamic_configuration)
  reopened <- fpq_load_project_configuration(fpq_open_project(root))
  expect_identical(reopened$primary_normalization_method, "rms")
})

test_that("V2 UI exposes exactly four normalization choices and identity", {
  expect_identical(unname(fpq_normalization_labels()),
    c("Raw / no normalization", "TIC / spectrum-sum", "RMS", "Median"))
  expect_identical(fpq_normalization_ui_choices(), c(
    "Raw / no normalization" = "raw",
    "TIC / spectrum-sum" = "spectrum_sum",
    "RMS" = "rms",
    "Median" = "median"))
  app_source <- paste(readLines(file.path(fpq_repo_root(), "R", "shiny_app.R")),
                      collapse = "\n")
  expect_match(app_source, "MALDIPixelQuant V2", fixed = TRUE)
  expect_match(app_source, "2.0.1", fixed = TRUE)
  project_source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_project.R")), collapse = "\n")
  expect_match(project_source, "fpq_normalization_descriptions()", fixed = TRUE)
  expect_match(project_source, "fpq_normalization_ui_choices()", fixed = TRUE)
  expect_identical(unname(fpq_normalization_descriptions()), c(
    "Measured target intensity without per-pixel spectral normalization.",
    paste("Target intensity divided by the sum of stored centroid intensities",
      "in the pixel and scaled by 1,000,000.",
      "Existing validated FullPixel normalization."),
    paste("Target intensity divided by the root mean square of positive",
      "stored centroid-peak intensities in the pixel and scaled by 1,000,000."),
    paste("Target intensity divided by the median of positive stored",
      "centroid-peak intensities in the pixel and scaled by 1,000,000.")))
})

test_that("actual project GUI saves and visibly reloads every normalization method", {
  for (method in fpq_normalization_methods()) {
    root <- tempfile(paste0("fpq-gui-normalization-", method, "-"))
    project <- fpq_create_project(root,
      paste0("prj-gui-", gsub("_", "-", method)),
      paste("GUI", method), project_preferences = list(
        primary_normalization_method = "spectrum_sum"))
    state <- fpq_new_app_state(fpq_repo_root())
    state$project(project)
    state$configuration(fpq_load_project_configuration(project))

    shiny::testServer(fpq_project_server, args = list(state = state), {
      session$setInputs(analysis_type = "compare_two_groups",
        selected_ion_mode = "NEG",
        primary_normalization_method = method,
        save_next_step = 1)
      session$flushReact()
      expect_null(state$last_error(), info = method)
      expect_identical(state$configuration()$primary_normalization_method,
        method, info = method)
    })

    reopened_state <- fpq_new_app_state(fpq_repo_root())
    shiny::testServer(fpq_project_server, args = list(state = reopened_state), {
      session$setInputs(open_location = root, open = 1)
      session$flushReact()
      expect_null(reopened_state$last_error(), info = method)
      expect_identical(
        reopened_state$configuration()$primary_normalization_method,
        method, info = method)
      overview <- as.character(output$overview)
      selected_pattern <- paste0('value="', method, '" checked="checked"')
      expect_true(any(grepl(selected_pattern, overview, fixed = TRUE)),
        info = method)
    })
  }
})

test_that("normalization GUI never submits display labels", {
  expect_identical(names(fpq_normalization_ui_choices()),
    unname(fpq_normalization_labels()))
  expect_identical(unname(fpq_normalization_ui_choices()),
    fpq_normalization_methods())
  expect_identical(fpq_normalization_method(list()), "spectrum_sum")
  expect_error(fpq_normalization_method(list(
    primary_normalization_method = "Raw / no normalization")),
    class = "fpq_scientific_error")
  expect_error(fpq_normalization_method(list(
    primary_normalization_method = "TIC / spectrum-sum")),
    class = "fpq_scientific_error")
})

test_that("V2 status strip and About presentation use the V2 identity", {
  app_source <- paste(readLines(file.path(fpq_repo_root(), "R", "shiny_app.R")),
    collapse = "\n")
  expect_match(app_source, "App 2.0.1 - Scientific core 1.0.0",
    fixed = TRUE)
  expect_false(grepl("App 1.1.1 - Scientific core", app_source, fixed = TRUE))
})
