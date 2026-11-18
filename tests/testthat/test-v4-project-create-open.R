test_that("V4 Create starts scientifically blank without a description field", {
  ui <- as.character(fpq_project_ui("project"))
  expect_false(grepl("Project description", ui, fixed = TRUE))
  expect_true(grepl("Experiment label", ui, fixed = TRUE))
  expect_true(grepl("Expected pixels per sample", ui, fixed = TRUE))
  expect_true(grepl("Pixel size", ui, fixed = TRUE))
  expect_false(grepl('value="Single"', ui, fixed = TRUE))
  expect_false(grepl('value="40"', ui, fixed = TRUE))
  expect_false(grepl('value="1600"', ui, fixed = TRUE))
  expect_false(grepl('value="DHB"', ui, fixed = TRUE))
  expect_false(grepl('value="9AA"', ui, fixed = TRUE))
  expect_false(grepl('checked="checked"', ui, fixed = TRUE))
})

test_that("V4 Create validates mandatory acquisition context without defaults", {
  errors <- fpq_validate_new_project_inputs("", "", NULL, "", "")
  expect_true(all(c("Enter a project name.", "Enter an experiment label.",
    "Select a pixel geometry policy.",
    "Enter a positive pixel size in micrometres.") %in% errors))
  expect_length(fpq_validate_new_project_inputs(
    "Project", "Single exposure", "sparse_observed", "40", ""), 0L)
  expect_match(fpq_validate_new_project_inputs(
    "Project", "Single exposure", "strict_predefined", "40", "")[[1]],
    "expected pixel count")
})

test_that("Create and Open expose adjacent browser-compatible selectors", {
  ui <- as.character(fpq_project_ui("project"))
  expect_match(ui, "fpq-v4-path-picker", fixed = TRUE)
  expect_true(grepl("project-browse_location", ui, fixed = TRUE))
  expect_true(grepl("project-browse_open_location", ui, fixed = TRUE))
  source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_project.R"), warn = FALSE), collapse = "\n")
  expect_match(source, 'shinyDirChoose(input, "browse_location"', fixed = TRUE)
  expect_match(source, 'shinyDirChoose(input, "browse_open_location"', fixed = TRUE)
  expect_false(grepl('input$positive_matrix %||% "DHB"', source, fixed = TRUE))
  expect_false(grepl('input$negative_matrix %||% "9AA"', source, fixed = TRUE))
})

test_that("folder picker discovers accessible local drives dynamically", {
  roots <- fpq_local_directory_roots()
  expect_true(all(dir.exists(unname(roots))))
  if (.Platform$OS.type == "windows") {
    expected <- paste0(LETTERS, ":/")
    expected <- expected[dir.exists(expected)]
    expect_true(all(tolower(expected) %in% tolower(unname(roots))))
    expect_true("c:/" %in% tolower(unname(roots)))
    expect_true("d:/" %in% tolower(unname(roots)))
    expect_true(dir.exists("D:/Master"))
  }
})

test_that("Open guards empty and invalid paths with standard-mode messages", {
  state <- fpq_new_app_state(fpq_repo_root())
  shiny::testServer(fpq_project_server, args = list(state = state), {
    session$setInputs(open_location = "", open = 1)
    session$flushReact()
    expect_match(paste(as.character(output$open_status), collapse = ""),
                 "Please select a MALDIPixelQuant project folder first.")
    expect_null(state$last_error())

    invalid <- tempfile("not-a-project-")
    dir.create(invalid)
    session$setInputs(open_location = invalid, open = 2)
    session$flushReact()
    html <- paste(as.character(output$open_status), collapse = "")
    expect_match(html, "does not contain a MALDIPixelQuant project")
    expect_false(grepl("record.missing", html, fixed = TRUE))
    expect_null(state$last_error())
  })
})

test_that("technical open details are conditional on Advanced Mode", {
  status <- list(type = "error", message = "Friendly message",
    technical = list(`Error identifier` = "record.missing",
                     `Searched path` = "C:/missing/project.json"))
  standard <- as.character(fpq_project_open_status_ui(status, FALSE))
  advanced <- as.character(fpq_project_open_status_ui(status, TRUE))
  expect_match(standard, "Friendly message")
  expect_false(grepl("record.missing", standard, fixed = TRUE))
  expect_match(advanced, "record.missing", fixed = TRUE)
  expect_match(advanced, "C:/missing/project.json", fixed = TRUE)
})

test_that("new and legacy project roots still reopen", {
  modern_root <- tempfile("modern-project-")
  modern <- fpq_create_project(modern_root, "prj-modern-open", "Modern",
    project_preferences = list(default_exposure = "Actual exposure",
      pixel_geometry_policy = "strict_predefined", pixel_size_um = 25,
      expected_coordinate_count = 1600, positive_matrix = "DHB",
      negative_matrix = "9AA", primary_normalization_method = "rms",
      statistical_analyses = list(jackknife = list(enabled = TRUE))))
  reopened <- fpq_open_project(modern_root)
  expect_identical(reopened$manifest$project_preferences$default_exposure,
                   "Actual exposure")
  expect_equal(reopened$manifest$project_preferences$pixel_size_um, 25)
  expect_identical(reopened$manifest$project_preferences$primary_normalization_method,
                   "rms")

  internal <- fpq_project_paths(modern$root)$internal_root
  legacy_root <- tempfile("legacy-project-")
  dir.create(legacy_root)
  children <- list.files(internal, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  expect_true(all(file.rename(children,
    file.path(legacy_root, basename(children)))))
  expect_identical(fpq_project_paths(fpq_open_project(legacy_root)$root)$layout,
                   "legacy")
})

test_that("V4 Overview is researcher-facing while Advanced retains identifiers", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_project.R"), warn = FALSE), collapse = "\n")
  expect_match(source, '"Experiment label"', fixed = TRUE)
  expect_match(source, '"Save Project Settings"', fixed = TRUE)
  expect_false(grepl("Spectrum-sum is the validated V1 reference", source,
                     fixed = TRUE))
  expect_false(grepl("Unequal / sparse pixel area is a Technical Validation Candidate",
                     source, fixed = TRUE))
  expect_match(source, "isTRUE(state$advanced_mode())", fixed = TRUE)

  root <- tempfile("overview-project-")
  project <- fpq_create_project(root, "prj-overview", "Overview project",
    project_preferences = list(default_exposure = "Saved experiment",
      pixel_geometry_policy = "strict_predefined", pixel_size_um = 25,
      expected_coordinate_count = 1600, positive_matrix = "DHB",
      negative_matrix = "9AA", primary_normalization_method = "median",
      statistical_analyses = list(jackknife = list(enabled = TRUE))))
  state <- fpq_new_app_state(fpq_repo_root())
  state$project(project)
  state$configuration(fpq_load_project_configuration(project))
  shiny::testServer(fpq_project_server, args = list(state = state), {
    standard <- paste(as.character(output$overview), collapse = "")
    expect_false(grepl("prj-overview", standard, fixed = TRUE))
    expect_false(grepl("generic-two-group-v1", standard, fixed = TRUE))
    expect_match(standard, "Experiment label", fixed = TRUE)
    expect_match(standard, "Save Project Settings", fixed = TRUE)
    expect_match(standard, "median", fixed = TRUE)
    state$advanced_mode(TRUE)
    session$flushReact()
    advanced <- paste(as.character(output$overview), collapse = "")
    expect_match(advanced, "prj-overview", fixed = TRUE)
    expect_match(advanced, "generic-two-group-v1", fixed = TRUE)
  })
})
