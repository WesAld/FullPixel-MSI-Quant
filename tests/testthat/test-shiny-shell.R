test_that("Shiny application and navigation UIs construct successfully", {
  app <- fpq_shiny_app(normalizePath("."))
  expect_s3_class(app, "shiny.appobj")
  fragments <- c(
    as.character(fpq_home_ui("home")),
    as.character(fpq_project_ui("project")),
    as.character(fpq_design_ui("design")),
    as.character(fpq_dataset_ui("dataset")),
    as.character(fpq_profile_ui("profile")),
    as.character(fpq_validation_ui("validation")),
    as.character(fpq_workflow_ui("workflow")),
    as.character(fpq_runs_ui("runs")),
    as.character(fpq_artifacts_ui("artifacts")),
    as.character(fpq_logs_ui("logs")),
    as.character(fpq_provenance_ui("provenance")),
    as.character(fpq_about_ui("about")))
  html <- paste(fragments, collapse = "\n")
  home_source <- paste(deparse(body(fpq_home_server)), collapse = "\n")
  for (label in c("Create New Project", "Open Existing Project"))
    expect_match(home_source, label, fixed = TRUE)
  for (label in c("Add Condition",
                  "Add Treatment", "Add Replicate", "Add Comparison",
                  "Files and Columns", "Scientific validation details",
                  "Validation", "Workflow", "Run History", "Artifacts",
                  "Logs", "Provenance", "MALDIPixelQuant"))
    expect_match(html, label, fixed = TRUE)
  sidebar <- htmltools::renderTags(fpq_v4_sidebar())$html
  expect_match(sidebar, "MALDIPixelQuant", fixed = TRUE)
  expect_match(sidebar, "Run History", fixed = TRUE)
})

test_that("no-project module states are safe", {
  state <- fpq_new_app_state(normalizePath("."))
  state$advanced_mode(TRUE)
  shiny::testServer(fpq_home_server, args = list(state = state), {
    expect_null(state$project())
    expect_false(fpq_app_summary(state)$project_open)
  })
  shiny::testServer(fpq_artifacts_server, args = list(state = state), {
    expect_length(fpq_list_artifacts(state$project()), 0)
  })
})

test_that("project UI creates, opens and restores a project", {
  state <- fpq_new_app_state(normalizePath("."))
  state$advanced_mode(TRUE)
  root <- tempfile("fpq-shiny-project-")
  shiny::testServer(fpq_project_server, args = list(state = state), {
    session$setInputs(name = "Shiny project", project_id = "prj-shiny-test",
      description = "UI lifecycle", location = root, investigator = "Test lab",
      exposure = "Single", ion_mode = "NEG",
      check_location = 1)
    session$flushReact()
    session$setInputs(create = 1); session$flushReact()
    expect_identical(state$project()$manifest$project_id, "prj-shiny-test")
    expect_true(file.exists(fpq_project_paths(root)$manifest))
  })
  restarted <- fpq_new_app_state(normalizePath("."))
  shiny::testServer(fpq_project_server, args = list(state = restarted), {
    session$setInputs(open_location = root, open = 1); session$flushReact()
    expect_identical(restarted$project()$manifest$project_name, "Shiny project")
  })
  malformed <- tempfile("fpq-invalid-project-"); dir.create(malformed)
  writeLines("{bad", file.path(malformed, "project.json"))
  expect_error(fpq_open_project(malformed), class = "fpq_configuration_error")
})
