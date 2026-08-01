test_that("simple project flow generates internal identifiers automatically", {
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  root <- tempfile("fpq-simple-project-")
  shiny::testServer(fpq_project_server, args = list(state = state), {
    session$setInputs(name = "Leaf Quantification Study",
      description = "Simple flow", location = root, investigator = "Plant lab",
      check_location = 1)
    session$flushReact()
    session$setInputs(create = 1)
    session$flushReact()
    expect_match(state$project()$manifest$project_id,
                 "^prj-leaf-quantification-study-[0-9]{14}$")
    expect_identical(
      state$project()$manifest$project_preferences$analysis_type,
      "quantify_only")
  })
})

test_that("analysis type and ion mode are configured once after project creation", {
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  root <- tempfile("fpq-analysis-selection-")
  shiny::testServer(fpq_project_server, args = list(state = state), {
    session$setInputs(name = "Generic comparison", description = "",
      location = root, investigator = "", check_location = 1)
    session$flushReact()
    session$setInputs(create = 1)
    session$flushReact()
    session$setInputs(analysis_type = "compare_multiple_groups",
      selected_ion_mode = "BOTH", save_next_step = 1)
    session$flushReact()
    expect_identical(state$analysis_type(), "compare_multiple_groups")
    expect_identical(state$selected_ion_mode(), "BOTH")
    expect_identical(state$configuration()$analysis_type,
                     "compare_multiple_groups")
    expect_identical(state$configuration()$ion_mode, "BOTH")
    expect_false(state$dirty())
    reopened <- fpq_open_project(root)
    expect_identical(reopened$manifest$project_preferences$analysis_type,
                     "compare_multiple_groups")
    expect_identical(reopened$manifest$project_preferences$ion_mode, "BOTH")
  })
})

test_that("advanced controls remain available without being the default", {
  state <- fpq_new_app_state(normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT")))
  expect_false(shiny::isolate(state$advanced_mode()))
  html <- as.character(fpq_project_ui("project"))
  expect_match(html, "Advanced Mode|advanced_mode")
  expect_match(html, "Internal project identifier", fixed = TRUE)
  expect_s3_class(fpq_shiny_app(
    normalizePath(Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT"))), "shiny.appobj")
})
