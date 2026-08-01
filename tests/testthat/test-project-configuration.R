test_that("project lifecycle is persistent and transactional", {
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-test-project", "Test project")
  expect_true(file.exists(file.path(root, "project.json")))
  expect_identical(fpq_open_project(root)$manifest$project_id, "prj-test-project")
  expect_error(fpq_create_project(root, "prj-other-project", "Duplicate"),
               class = "fpq_configuration_error")
  project <- fpq_save_project(project, list(project_name = "Updated"))
  expect_identical(fpq_open_project(root)$manifest$project_name, "Updated")
  project <- fpq_archive_project(project)
  expect_identical(fpq_project_status(project)$status, "archived")
})

test_that("dynamic designs have no fixed maximum", {
  designs <- list(
    make_design(c("Control", "Treatment-A", "Treatment-B", "Treatment-C"), rep(3, 4)),
    make_design(c("Control", "Treatment-A"), c(6, 6)),
    make_design(c("A", "B", "C"), c(4, 5, 3)),
    make_design(c("Vehicle", "Lino-Phe", "Ole-Dop", "A-Lino-Etha", "TUG-891"), rep(2, 5)),
    make_design(c("Negative-control", "Negative-treatment"), c(3, 3))
  )
  for (design in designs) {
    expect_silent(fpq_validate_dynamic_configuration(design))
    expect_identical(fpq_validate_structural(design)$classification, "structurally_valid")
  }
  expect_identical(fpq_validate_profile_compatibility(designs[[1]],
    "pvl-fullpixel-reference-v1")$classification, "profile_incompatible")
})

test_that("referential failures are explicit and cascade is opt-in", {
  design <- make_design(c("Control", "Treatment"), c(2, 2))
  expect_error(fpq_add_sample(design, make_sample("bad", "missing", "R1")),
               class = "fpq_configuration_error")
  expect_error(fpq_remove_condition(design, "Control"), class = "fpq_configuration_error")
  cascaded <- fpq_remove_condition(design, "Control", cascade = TRUE)
  expect_false("Control" %in% vapply(cascaded$conditions, `[[`, "", "condition_id"))
})

test_that("configuration saves atomically and survives reload", {
  root <- tempfile("fpq-project-")
  project <- fpq_create_project(root, "prj-config-test", "Config")
  design <- make_design(c("Control", "Treatment"), c(3, 3))
  result <- fpq_save_configuration(project, "datasets", "dset-config", design,
                                   fpq_validate_dynamic_configuration)
  expect_length(result$sha256, 1)
  expect_equal(fpq_load_configuration(project, "datasets", "dset-config"), design)
  expect_error(fpq_save_configuration(project, "datasets", "dset-interrupt", design,
    fpq_validate_dynamic_configuration, simulate_interrupt = TRUE),
    class = "fpq_infrastructure_error")
  expect_false(file.exists(file.path(root, "config", "datasets", "dset-interrupt.json")))
})
