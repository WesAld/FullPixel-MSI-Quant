test_that("V4 normalization cards are a Shiny-bound radio group", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_pages.R"),
    warn = FALSE), collapse = "\n")
  expect_match(source, 'class = "shiny-input-radiogroup fpq-v4-method-grid"',
    fixed = TRUE)
  expect_match(source, 'id = session$ns("method")', fixed = TRUE)
})

test_that("V4 saves and restores every normalization method in all authorities", {
  for (method in fpq_normalization_methods()) {
    root <- tempfile(paste0("fpq-v4-method-", method, "-"))
    project <- fpq_create_project(root,
      paste0("prj-v4-", gsub("_", "-", method)),
      paste("V4", method), project_preferences = list(
        primary_normalization_method = "spectrum_sum"))
    config <- make_design(c("CTRL", "TRT"), c(3L, 3L))
    config$schema_version <- "1.0.0"
    config$dataset_id <- "dset-project-configuration"
    config$primary_normalization_method <- "spectrum_sum"
    config$statistical_analyses <- list(jackknife = list(enabled = TRUE))
    fpq_persist_msi_configuration(project, config)

    state <- fpq_new_app_state(fpq_repo_root())
    state$project(project); state$configuration(config)
    shiny::testServer(fpq_analysis_settings_server,
      args = list(state = state), {
        session$setInputs(method = method, save = 1)
        session$flushReact()
        expect_identical(state$configuration()$primary_normalization_method,
          method, info = method)
      })

    manifest <- fpq_read_json(fpq_project_paths(root)$manifest)
    expect_identical(
      manifest$project_preferences$primary_normalization_method,
      method, info = method)
    persisted <- fpq_read_json(file.path(fpq_project_paths(root)$datasets,
      "dset-project-configuration.json"))
    expect_identical(persisted$primary_normalization_method, method,
      info = method)

    reopened <- fpq_open_project(root)
    restored <- fpq_load_project_configuration(reopened)
    expect_identical(restored$primary_normalization_method, method,
      info = method)

    restored_state <- fpq_new_app_state(fpq_repo_root())
    restored_state$project(reopened); restored_state$configuration(restored)
    shiny::testServer(fpq_analysis_settings_server,
      args = list(state = restored_state), {
        rendered <- as.character(output$content)
        expect_true(any(grepl(paste0('value="', method,
          '" checked="checked"'), rendered, fixed = TRUE)), info = method)
      })
  }
})

test_that("V4 browse uses the browser-compatible shinyFiles chooser", {
  page <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_pages.R"),
    warn = FALSE), collapse = "\n")
  server <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_project.R"), warn = FALSE), collapse = "\n")
  expect_match(page, "shinyFiles::shinyDirButton", fixed = TRUE)
  expect_match(server, "shinyFiles::shinyDirChoose", fixed = TRUE)
  expect_match(server, "shinyFiles::parseDirPath", fixed = TRUE)
  expect_false(grepl("utils::choose.dir", server, fixed = TRUE))
})

test_that("scientific details render generic locked settings", {
  state <- fpq_new_app_state(fpq_repo_root())
  shiny::testServer(fpq_profile_server, args = list(state = state), {
    details <- paste(as.character(output$details), collapse = "\n")
    expect_match(details, "target_mass_tolerance absolute_da: 0.002",
      fixed = TRUE)
    expect_match(details, "replicate_summary pseudocount: none",
      fixed = TRUE)
    expect_match(details, "Locked settings are inspectable but not editable.",
      fixed = TRUE)
  })
})

test_that("jackknife explanatory text uses an HTML em dash", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_pages.R"),
    warn = FALSE), collapse = "\n")
  expect_match(source, "matched biological replicate pairs &mdash; never MSI pixels",
    fixed = TRUE)
  expect_false(grepl("<U+2014>", source, fixed = TRUE))
})

test_that("Files and Columns snapshots follow authoritative ion-mode registrations", {
  config <- list(samples = list(
    list(sample_id = "P1", ion_mode = "POS", imzml_path = "p.imzML", ibd_path = "p.ibd"),
    list(sample_id = "N1", ion_mode = "NEG", imzml_path = "n.imzML", ibd_path = "n.ibd")),
    positive_matrix = "DHB", negative_matrix = "9AA",
    mzmine_registrations = list(
      POS = list(path = "positive.csv", mappings = list(feature_id = "pid", mz = "pmz")),
      NEG = list(path = "negative.csv", mappings = list(feature_id = "nid", mz = "nmz"))))
  pos <- fpq_v4_dataset_sync_snapshot(config, "POS")
  neg <- fpq_v4_dataset_sync_snapshot(config, "NEG")
  expect_identical(pos$mzmine_path, "positive.csv")
  expect_identical(pos$pair_lines, "P1 | p.imzML | p.ibd")
  expect_identical(pos$matrix, "DHB")
  expect_identical(c(pos$feature_id, pos$target_mz), c("pid", "pmz"))
  expect_identical(neg$mzmine_path, "negative.csv")
  expect_identical(neg$pair_lines, "N1 | n.imzML | n.ibd")
  expect_identical(neg$matrix, "9AA")
  expect_identical(c(neg$feature_id, neg$target_mz), c("nid", "nmz"))
})

test_that("Run History exposes status-appropriate actions", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_pages.R"),
    warn = FALSE), collapse = "\n")
  for (label in c("Download Workbook", "View Run Details",
                  "View Failure Details", "Open Output Folder", "Resume Run"))
    expect_match(source, label, fixed = TRUE)
  expect_match(source, "fpq_full_workflow_worker.R", fixed = TRUE)
})

test_that("V4 Standard Mode removes candidate-only warning presentation", {
  state <- fpq_new_app_state(fpq_repo_root())
  state$validations(list(
    structural = list(classification = "structurally_valid"),
    registration = list(blocking = FALSE),
    profile = list(classification = "profile_candidate_compatible"),
    environment = list(status = "compatible"),
    readiness = "ready",
    records = list()))
  shiny::testServer(fpq_validation_server, args = list(state = state), {
    html <- paste(as.character(output$layers), collapse = "")
    expect_match(html, "Analysis compatibility", fixed = TRUE)
    expect_match(html, "Ready to run", fixed = TRUE)
    expect_false(grepl("Passed with warning", html, fixed = TRUE))
    expect_false(grepl("Ready with warning", html, fixed = TRUE))
  })
})

test_that("V4 final researcher wording is method-neutral and sheet-neutral", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_profile_validation_workflow.R"), warn = FALSE),
    collapse = "\n")
  expect_match(source,
    "Extract target signals across complete full-pixel images.", fixed = TRUE)
  expect_match(source, "Validated final workbook is available.", fixed = TRUE)
  expect_false(grepl("Extract and spectrum-sum-normalize", source, fixed = TRUE))
  expect_false(grepl("Validated final five-sheet workbook", source, fixed = TRUE))
})

test_that("V4 synthetic demonstration control is Advanced-only", {
  state <- fpq_new_app_state(fpq_repo_root())
  shiny::testServer(fpq_design_server, args = list(state = state), {
    expect_null(output$advanced_demo_control)
    state$advanced_mode(TRUE)
    session$flushReact()
    expect_match(paste(as.character(output$advanced_demo_control), collapse = ""),
                 "Load Synthetic Demonstration", fixed = TRUE)
  })
})

test_that("Project Overview does not duplicate the statistical analyses heading", {
  config <- make_design(c("CTRL", "TRT"), c(3L, 3L))
  html <- as.character(fpq_jackknife_ui(shiny::NS("project"), config,
    enabled = TRUE, prefix = "overview_", show_section_heading = FALSE))
  expect_false(grepl("<h3>Statistical analyses</h3>", html, fixed = TRUE))
  expect_match(html, "Jackknife robustness analysis", fixed = TRUE)
})

test_that("Workflow details honor the run selected from Run History", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R",
    "shiny_module_profile_validation_workflow.R"), warn = FALSE), collapse = "\n")
  expect_match(source, "selected_id <- state$workflow_id()", fixed = TRUE)
  expect_match(source, "identical(x$workflow_id, selected_id)", fixed = TRUE)
  expect_match(source, "if (length(selected)) return(selected[[1]])", fixed = TRUE)
})
