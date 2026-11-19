test_that("pixel geometry policy defaults to strict and rejects unknown values", {
  expect_identical(fpq_pixel_geometry_policy(), "strict_predefined")
  expect_identical(
    fpq_pixel_geometry_policy(list(pixel_geometry_policy = "sparse_observed")),
    "sparse_observed")
  expect_error(
    fpq_pixel_geometry_policy(list(pixel_geometry_policy = "unknown")),
    class = "fpq_error")
})

test_that("acquisition metadata resolves and persists sparse geometry policy", {
  project <- list(manifest = list(project_preferences = list(
    pixel_geometry_policy = "sparse_observed", full_pixel = TRUE)))
  acquisition <- fpq_mode_acquisition_metadata(project, list(), "NEG")
  expect_identical(acquisition$pixel_geometry_policy, "sparse_observed")
  expect_identical(acquisition$expected_coordinate_count, 1600L)
})
