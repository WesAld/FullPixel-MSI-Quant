make_environment_case <- function(extra = list()) {
  list(status = "compatible", checks = utils::modifyList(list(
    r = list(status = "compatible", required_for_current_workflow = TRUE),
    packages = list(status = "compatible", required_for_current_workflow = TRUE),
    python = list(status = "compatible", required_for_current_workflow = TRUE),
    scientific_core = list(status = "compatible",
      required_for_current_workflow = TRUE)), extra))
}

test_that("a missing required dependency remains actionable", {
  environment <- make_environment_case(list(
    packages = list(status = "incompatible", observed = "required package missing",
      required_for_current_workflow = TRUE)))
  expect_identical(fpq_environment_workflow_status(environment), "incompatible")
  records <- fpq_environment_detail_records(environment)
  package_record <- Filter(function(x) identical(x$check_id, "environment.packages"),
                           records)[[1L]]
  expect_identical(package_record$status, "FAIL")
  expect_true(package_record$blocking_status)
})

test_that("an unavailable unused optional dependency is technical detail only", {
  environment <- make_environment_case(list(
    optional_reporter = list(status = "incompatible", observed = "not installed",
      required_for_current_workflow = FALSE, impact_on_current_workflow = "none")))
  environment$status <- "incompatible"
  environment$workflow_status <- fpq_environment_workflow_status(environment)
  expect_identical(environment$workflow_status, "compatible")
  records <- fpq_environment_detail_records(environment)
  optional <- Filter(function(x)
    identical(x$check_id, "environment.optional_reporter"), records)[[1L]]
  expect_identical(optional$status, "WARNING")
  expect_false(optional$blocking_status)
  expect_match(optional$message, "Impact on current workflow: None", fixed = TRUE)

  state <- fpq_new_app_state(fpq_repo_root())
  state$validations(list(
    structural = list(classification = "structurally_valid"),
    registration = list(blocking = FALSE),
    profile = list(classification = "profile_compatible"),
    environment = environment, readiness = "ready", records = records))
  shiny::testServer(fpq_validation_server, args = list(state = state), {
    html <- paste(as.character(output$layers), collapse = "")
    expect_match(html, "Software environment", fixed = TRUE)
    expect_match(html, ">Passed<", fixed = TRUE)
    expect_match(html, "Ready to run", fixed = TRUE)
    expect_match(html, ">Ready<", fixed = TRUE)
    expect_false(grepl("Passed with warning", html, fixed = TRUE))
    expect_false(grepl("Ready with warning", html, fixed = TRUE))
  })
})

test_that("a clean environment is Passed and Ready", {
  environment <- make_environment_case()
  expect_identical(fpq_environment_workflow_status(environment), "compatible")
  expect_length(fpq_environment_detail_records(environment), 0L)
})

test_that("Workflow summary receives Ready for optional-only environment issues", {
  root <- tempfile("fpq-workflow-ready-")
  project <- fpq_create_project(root, "prj-workflow-ready", "Workflow Ready")
  state <- fpq_new_app_state(fpq_repo_root())
  state$project(project)
  state$configuration(fpq_load_project_configuration(project))
  state$validations(list(readiness = "ready"))
  shiny::testServer(fpq_v4_workflow_companion_server,
    args = list(state = state), {
      html <- paste(as.character(output$v4_analysis_summary), collapse = "")
      expect_match(html, "Validation", fixed = TRUE)
      expect_match(html, ">Ready<", fixed = TRUE)
      expect_false(grepl("Ready with warning", html, fixed = TRUE))
    })
})
