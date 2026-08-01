test_that("Simple Mode has no separate Scientific Profile navigation page", {
  app_source <- paste(deparse(body(fpq_shiny_app)), collapse = "\n")
  expect_false(grepl('tabPanel("Scientific Profile"', app_source,
                     fixed = TRUE))
  expect_match(app_source, 'tabPanel("Advanced Settings"', fixed = TRUE)
  expect_match(app_source, "Scientific validation details", fixed = TRUE)

  project_html <- as.character(fpq_project_ui("project"))
  expect_false(grepl("Scientific profile", project_html, fixed = TRUE))
})

test_that("Advanced scientific details inspect the persisted profile read-only", {
  project <- fpq_create_project(tempfile("fpq-profile-details-"),
    "prj-profile-details", "Profile details")
  state <- fpq_new_app_state(fpq_repo_root())
  state$project(project)
  state$advanced_mode(TRUE)
  shiny::testServer(fpq_profile_server, args = list(state = state), {
    session$flushReact()
    details <- paste(as.character(output$details), collapse = "")
    expect_match(details, project$manifest$scientific_profile_id, fixed = TRUE)
    expect_match(details, "Locked scientific settings", fixed = TRUE)
  })
})

test_that("automatic generic-profile validation remains authoritative without selection", {
  project <- fpq_create_project(tempfile("fpq-profile-validation-"),
    "prj-profile-validation", "Profile validation")
  config <- make_design(c("CTRL", "TRT"), c(3L, 3L))
  config$conditions <- unname(config$conditions)
  config$samples <- unname(config$samples)
  config$analysis_type <- "compare_two_groups"
  config$samples <- lapply(config$samples, function(x) {
    x$ion_mode <- "NEG"
    x
  })
  compatibility <- fpq_validate_profile_compatibility(config,
    project$manifest$scientific_profile_id)
  expect_identical(compatibility$classification,
                   "profile_candidate_compatible")
  expect_identical(compatibility$scientific_status$overall,
                   "technical_validation_candidate")
})

test_that("the explicit PVL reference profile retains approved status", {
  config <- make_design(c("CTRL", "TRT"), c(3L, 3L))
  config$conditions <- unname(config$conditions)
  config$samples <- unname(config$samples)
  config$analysis_type <- "compare_two_groups"
  config$samples <- lapply(config$samples, function(x) {
    x$ion_mode <- "NEG"
    x
  })
  compatibility <- fpq_validate_profile_compatibility(
    config, "pvl-fullpixel-reference-v1")
  expect_identical(compatibility$classification, "profile_compatible")
  expect_identical(compatibility$scientific_status$overall, "approved")
})

test_that("existing projects reopen without profile reselection", {
  root <- tempfile("fpq-profile-reopen-")
  created <- fpq_create_project(root, "prj-profile-reopen", "Profile reopen")
  reopened <- fpq_open_project(root)
  expect_identical(reopened$manifest$scientific_profile_id,
                   created$manifest$scientific_profile_id)

  state <- fpq_new_app_state(fpq_repo_root())
  shiny::testServer(fpq_project_server, args = list(state = state), {
    session$setInputs(open_location = root, open = 1)
    session$flushReact()
    expect_identical(state$project()$manifest$scientific_profile_id,
                     created$manifest$scientific_profile_id)
  })
})
