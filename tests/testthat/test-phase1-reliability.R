test_that("repository root resolution is explicit and independent of working directory", {
  root <- normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT"), mustWork = TRUE)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(tempdir())
  expect_identical(fpq_resolve_repository_root(root), root)
  expect_identical(fpq_repo_root(), root)
  expect_error(fpq_resolve_repository_root(file.path(tempdir(), "missing-repository")),
               class = "fpq_infrastructure_error")
})

test_that("project creation publishes only a readable validated manifest", {
  root <- tempfile("fpq-transactional-project-")
  project <- fpq_create_project(root, "prj-transaction-test", "Transactional project",
    project_description = "Read-back test",
    project_preferences = list(investigator_or_laboratory = "Test laboratory"))
  expect_true(file.exists(fpq_project_paths(root)$manifest))
  reopened <- fpq_open_project(root)
  expect_identical(reopened$manifest$project_id, "prj-transaction-test")
  expect_identical(reopened$manifest$project_description, "Read-back test")

  failed <- tempfile("fpq-invalid-project-")
  expect_error(fpq_create_project(failed, "invalid", "Invalid project"),
               class = "fpq_configuration_error")
  expect_false(dir.exists(failed))
  expect_length(Sys.glob(paste0(failed, ".creating-*")), 0L)
})

test_that("failed project creation leaves Shiny state closed and recoverable", {
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  state$advanced_mode(TRUE)
  root <- tempfile("fpq-ui-failure-")
  shiny::testServer(fpq_project_server, args = list(state = state), {
    session$setInputs(name = "Bad project", project_id = "invalid",
      description = "", location = root, investigator = "", exposure = "Single",
      ion_mode = "NEG", profile = "pvl-fullpixel-reference-v1", check_location = 1)
    session$flushReact()
    session$setInputs(create = 1)
    session$flushReact()
    expect_null(state$project())
    expect_s3_class(state$last_error(), "fpq_error")

    session$setInputs(project_id = "prj-recovered", check_location = 2)
    session$flushReact()
    session$setInputs(create = 2)
    session$flushReact()
    expect_identical(state$project()$manifest$project_id, "prj-recovered")
    expect_null(state$last_error())
  })
})

test_that("replicate records display safely and template data supports zero rows", {
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  state$configuration(make_design("Control", 0))
  shiny::testServer(fpq_design_server, args = list(state = state), {
    session$setInputs(condition = "Control", add_replicate = 1)
    session$flushReact()
    sample <- state$configuration()$samples[[1]]
    expect_identical(sample$notes, "")
    expect_silent(output$samples)
  })

  fields <- c("sample_id", "sample_label", "condition_id", "replicate_id",
              "inclusion_status", "imzml_path", "ibd_path", "notes")
  empty <- as.data.frame(setNames(replicate(length(fields), character(),
                                            simplify = FALSE), fields))
  path <- tempfile(fileext = ".csv")
  expect_silent(utils::write.csv(empty, path, row.names = FALSE, na = ""))
  expect_identical(names(utils::read.csv(path, nrows = 0, check.names = FALSE)), fields)
})

test_that("interactive validation replaces persisted current results", {
  root <- tempfile("fpq-validation-project-")
  project <- fpq_create_project(root, "prj-validation-replace", "Validation project")
  first <- fpq_validation_record("check.one", "WARNING", "First")
  second <- fpq_validation_record("check.two", "PASS", "Second")
  fpq_store_validation(project, list(first), replace = TRUE)
  fpq_store_validation(project, list(second), replace = TRUE)
  records <- fpq_read_collection(fpq_project_paths(root)$validations)
  expect_length(records, 1L)
  expect_identical(records[[1]]$check_id, "check.two")
})

test_that("optional storage check does not make environment unresolved", {
  result <- fpq_check_environment(repository_root =
    normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  expect_identical(result$checks$storage$status, "compatible_with_warning")
  expect_false(identical(result$status, "unresolved"))
})
