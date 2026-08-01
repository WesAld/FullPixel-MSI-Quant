test_that("mass tolerance defaults to the validated v1.0.0 setting", {
  settings <- fpq_mass_tolerance_settings(list())
  expect_identical(settings$mode, "validated_default")
  expect_equal(settings$ppm, 10)
  expect_equal(settings$absolute_da, 0.002)
  expect_identical(
    settings$rule,
    "maximum_of_absolute_da_and_target_mz_times_ppm"
  )
  expect_true(settings$validated_default)
})

test_that("custom strict ppm and Da tolerances are validated and stored", {
  config <- fpq_set_mass_tolerance(
    list(), "custom_strict_combined", 5.5, 0.0015
  )
  settings <- fpq_mass_tolerance_settings(config)
  expect_identical(settings$mode, "custom_strict_combined")
  expect_equal(settings$ppm, 5.5)
  expect_equal(settings$absolute_da, 0.0015)
  expect_identical(
    settings$rule,
    "minimum_of_absolute_da_and_target_mz_times_ppm"
  )
  expect_false(settings$validated_default)
  expect_silent(fpq_validate_mass_tolerance_configuration(config))
  expect_error(
    fpq_set_mass_tolerance(
      list(), "custom_strict_combined", 0, 0.0015
    ),
    class = "fpq_error"
  )
  expect_error(
    fpq_set_mass_tolerance(
      list(), "custom_strict_combined", 5, 0
    ),
    class = "fpq_error"
  )
})

test_that("validated-default mode restores its complete legacy rule", {
  config <- fpq_set_mass_tolerance(
    list(), "validated_default", 25, 0.5
  )
  settings <- fpq_mass_tolerance_settings(config)
  expect_equal(settings$ppm, 10)
  expect_equal(settings$absolute_da, 0.002)
  expect_identical(
    settings$rule,
    "maximum_of_absolute_da_and_target_mz_times_ppm"
  )
  expect_true(settings$validated_default)
})

test_that("strict effective windows use the smaller ppm or Da value", {
  effective_window <- function(target_mz, ppm, absolute_da)
    pmin(absolute_da, target_mz * ppm / 1000000)
  expect_equal(effective_window(100, 10, 0.002), 0.001)
  expect_equal(effective_window(500, 10, 0.002), 0.002)
  expect_equal(effective_window(1000, 5, 0.003), 0.003)
})

test_that("execution requests receive both tolerances and strict provenance", {
  request <- list(
    approved_scientific_configuration = list(
      absolute_tolerance = 0.002,
      ppm_tolerance = 10
    ),
    scientific_profile = list(
      profile_id = "pvl-fullpixel-reference-v1",
      profile_version = "1.0.0",
      reference_status = "validated_reference"
    ),
    display_metadata = list()
  )
  config <- fpq_set_mass_tolerance(
    list(), "custom_strict_combined", 7.5, 0.0015
  )
  observed <- fpq_apply_mass_tolerance_to_request(request, config)
  expect_equal(
    observed$approved_scientific_configuration$ppm_tolerance,
    7.5
  )
  expect_equal(
    observed$approved_scientific_configuration$absolute_tolerance,
    0.0015
  )
  expect_identical(
    observed$approved_scientific_configuration$tolerance_combination_rule,
    "minimum_of_absolute_da_and_target_mz_times_ppm"
  )
  expect_identical(
    observed$scientific_profile$reference_status,
    "technical_validation_candidate"
  )
  expect_equal(observed$display_metadata$mass_tolerance$ppm, 7.5)
  expect_equal(observed$display_metadata$mass_tolerance$absolute_da, 0.0015)
})

test_that("the Shiny control applies a strict combined tolerance", {
  state <- fpq_new_app_state(fpq_repo_root())
  shiny::testServer(fpq_design_server, args = list(state = state), {
    session$setInputs(
      mass_tolerance_mode = "custom_strict_combined",
      custom_ppm_tolerance = 6,
      custom_absolute_da_tolerance = 0.001,
      apply_mass_tolerance = 1
    )
    session$flushReact()
    settings <- fpq_mass_tolerance_settings(state$configuration())
    expect_identical(settings$mode, "custom_strict_combined")
    expect_equal(settings$ppm, 6)
    expect_equal(settings$absolute_da, 0.001)
    expect_true(state$dirty())
  })
})

test_that("custom strict tolerance persists in saved configuration", {
  root <- tempfile("fpq-tolerance-project-")
  project <- fpq_create_project(root, "prj-tolerance-test", "Tolerance test")
  config <- fpq_set_mass_tolerance(
    list(
      schema_version = "1.0.0",
      dataset_id = "dset-tolerance-test",
      conditions = list(),
      samples = list(),
      comparisons = list()
    ),
    "custom_strict_combined",
    8,
    0.0018
  )
  fpq_save_configuration(
    project,
    "datasets",
    config$dataset_id,
    config,
    fpq_validate_dynamic_configuration
  )
  reopened <- fpq_load_configuration(
    project,
    "datasets",
    config$dataset_id
  )
  settings <- fpq_mass_tolerance_settings(reopened)
  expect_identical(settings$mode, "custom_strict_combined")
  expect_equal(settings$ppm, 8)
  expect_equal(settings$absolute_da, 0.0018)
})
