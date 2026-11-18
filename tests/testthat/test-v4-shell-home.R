test_that("V4 routing exposes stable parent and child destinations", {
  expect_identical(fpq_v4_routes(), c(
    "home", "project", "msi_samples", "mzmine", "dataset_configuration",
    "validation", "workflow", "runs", "results_internal", "artifacts",
    "logs", "provenance", "scientific_validation_details", "about"))
  expect_identical(fpq_v4_route_child("project_create"),
    list(route = "project", tab = "Create"))
  expect_identical(fpq_v4_route_child("project_open"),
    list(route = "project", tab = "Open"))
  expect_identical(fpq_v4_route_child("project_overview"),
    list(route = "project", tab = "Overview"))
  expect_identical(fpq_v4_route_child("experimental_design"),
    list(route = "dataset_configuration", tab = "Experimental Design"))
  expect_identical(fpq_v4_route_child("files_columns"),
    list(route = "dataset_configuration", tab = "Files and Columns"))
})

test_that("Standard and Advanced navigation are deliberately separated", {
  expect_false("results_internal" %in% fpq_v4_standard_routes())
  expect_false(any(fpq_v4_advanced_routes() %in% fpq_v4_standard_routes()))
  expect_identical(fpq_v4_advanced_routes(), c(
    "artifacts", "logs", "provenance", "scientific_validation_details"))
  sidebar <- htmltools::renderTags(fpq_v4_sidebar())$html
  expect_match(sidebar, "Run History", fixed = TRUE)
  expect_false(grepl("results_internal", sidebar, fixed = TRUE))
  expect_match(sidebar, "Advanced Mode", fixed = TRUE)
})

test_that("V4 shell retains every existing server module", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_shell.R"),
    warn = FALSE), collapse = "\n")
  module_calls <- c(
    'fpq_project_server("project", state)',
    'fpq_sample_import_server("sample_import", state)',
    'fpq_mzmine_import_server("mzmine_import", state)',
    'fpq_design_server("design", state)',
    'fpq_dataset_server("dataset", state)',
    'fpq_validation_server("validation", state)',
    'fpq_workflow_server("workflow", state)',
    'fpq_runs_server("runs", state)',
    'fpq_results_server("results", state)',
    'fpq_artifacts_server("artifacts", state)',
    'fpq_logs_server("logs", state)',
    'fpq_provenance_server("provenance", state)',
    'fpq_profile_server("profile", state)')
  for (call in module_calls) expect_match(source, call, fixed = TRUE)
  expect_match(source, 'fpq_v4_route_panel("results_internal"', fixed = TRUE)
})

test_that("Home actions target Project Create and Open", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_shell.R"),
    warn = FALSE), collapse = "\n")
  expect_match(source,
    'input$create_project,\n      fpq_v4_navigate(session$rootScope(), "project_create")',
    fixed = TRUE)
  expect_match(source,
    'input$open_project,\n      fpq_v4_navigate(session$rootScope(), "project_open")',
    fixed = TRUE)
  expect_match(source, "Welcome to MALDIPixelQuant", fixed = TRUE)
  expect_match(source, "4.0.0-dev", fixed = TRUE)
})

test_that("Home presents the approved scientific workflow summary", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_shell.R"),
    warn = FALSE), collapse = "\n")
  for (label in c(
    "Pixel-level quantitative analysis of MALDI mass spectrometry imaging data.",
    "Raw, TIC, RMS and Median",
    "Group comparison with optional jackknife",
    "From MSI data to publication-ready output",
    "All results in a single file",
    "Application status",
    "Getting started",
    "Register MSI samples",
    "Configure dataset",
    "Jackknife robustness analysis"))
    expect_match(source, label, fixed = TRUE)
  expect_match(source, "fpq-v4-scientific-visual", fixed = TRUE)
  expect_match(source, "fpq-v4-capabilities", fixed = TRUE)
  expect_match(source, "fpq-v4-jackknife-strip", fixed = TRUE)
})

test_that("Home presentation retains routing and contains no jackknife control", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_shell.R"),
    warn = FALSE), collapse = "\n")
  expect_match(source, 'session$ns("create_project")', fixed = TRUE)
  expect_match(source, 'session$ns("open_project")', fixed = TRUE)
  expect_false(grepl("include_jackknife", source, fixed = TRUE))
  expect_false(grepl("remove_jackknife", source, fixed = TRUE))
})

test_that("navigation visibility does not own or recreate project state", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_shell.R"),
    warn = FALSE), collapse = "\n")
  expect_equal(length(gregexpr("fpq_new_app_state\\(", source)[[1]]), 1L)
  expect_match(source, "conditionalPanel", fixed = TRUE)
  expect_match(source, "state$advanced_mode", fixed = TRUE)
})

test_that("authoritative logo copy is byte-identical", {
  source <- "C:/Users/Wesse/Downloads/MALDI PixelQuant Laser Spectrum Logo.png"
  copy <- file.path(fpq_repo_root(), "app", "www", "branding",
                    "MALDI PixelQuant Laser Spectrum Logo.png")
  web_copy <- file.path(fpq_repo_root(), "app", "www", "branding",
                        "MALDI-PixelQuant-Laser-Spectrum-Logo.png")
  skip_if_not(file.exists(source), "Authoritative local source logo unavailable")
  expect_true(file.exists(copy))
  expect_identical(unname(tools::md5sum(copy)), unname(tools::md5sum(source)))
  expect_true(file.exists(web_copy))
  expect_identical(unname(tools::md5sum(web_copy)), unname(tools::md5sum(source)))
})

test_that("V4 migrated pages retain the approved researcher-facing structure", {
  page_source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_pages.R"),
    warn = FALSE), collapse = "\n")
  for (label in c("Conditions and Treatments", "Biological Replicates",
                  "Comparisons", "Analysis Settings", "Primary intensity strategy",
                  "Include Jackknife Statistics", "Remove Jackknife Statistics",
                  "Technical validation details", "Final workbook", "Run History"))
    expect_match(page_source, label, fixed = TRUE)
  expect_match(page_source, "MALDIPixelQuant_Final_Results_Unavailable.xlsx",
               fixed = TRUE)
})

test_that("V4 workflow uses method-aware presentation labels", {
  expect_identical(fpq_v4_workflow_stage_name("normalizing", "legacy"),
                   "Normalizing intensities")
  expect_identical(fpq_v4_workflow_stage_name("final_workbook", "legacy"),
                   "Creating final Excel workbook")
  expect_identical(fpq_v4_workflow_stage_name("annotating", "Integrating annotations"),
                   "Integrating annotations")
})

test_that("V4 analysis settings keep normalization and jackknife deliberate", {
  html <- htmltools::renderTags(fpq_analysis_settings_ui("analysis_settings"))$html
  expect_match(html, "Extraction mass tolerance", fixed = TRUE)
  expect_match(html, "design-mass_tolerance_mode", fixed = TRUE)
  expect_match(html, "analysis_settings-content", fixed = TRUE)
  source <- paste(deparse(body(fpq_analysis_settings_server)), collapse = "\n")
  expect_match(source, "fpq_set_normalization_method", fixed = TRUE)
  expect_match(source, "fpq_set_jackknife_enabled", fixed = TRUE)
  expect_match(source, "input$include", fixed = TRUE)
  expect_match(source, "input$remove", fixed = TRUE)
  expect_false(fpq_jackknife_enabled(list()))
})

test_that("validated final exporters use clean V4 output wording", {
  root <- fpq_repo_root()
  paths <- c(
    file.path(root, "tools", "generic_export_adapter.py"),
    file.path(root, "tools", "quantify_only_export_adapter.py"),
    file.path(root, "tools", "multi_group_export_adapter.py"))
  source <- paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")
  expect_match(source, "Validated workflow output", fixed = TRUE)
  expect_match(source, "MALDIPixelQuant_Final_Results.xlsx", fixed = TRUE)
  expect_false(grepl("TECHNICAL_VALIDATION_CANDIDATE.xlsx", source, fixed = TRUE))
  expect_false(grepl("'Technical Validation Candidate'", source, fixed = TRUE))
})

test_that("V4 startup modules initialize inside valid reactive contexts", {
  state <- fpq_new_app_state(fpq_repo_root())
  shiny::testServer(fpq_home_server, args = list(state = state), {
    session$flushReact()
    html <- paste(as.character(output$home_content), collapse = "\n")
    expect_match(html, "Welcome to MALDIPixelQuant", fixed = TRUE)
    expect_false(grepl("fpq-v4-home-brand", html, fixed = TRUE))
    expect_match(html, "create_project", fixed = TRUE)
    expect_match(html, "open_project", fixed = TRUE)
  })
  shiny::testServer(fpq_analysis_settings_server,
                    args = list(state = state), {
    session$flushReact()
    html <- paste(as.character(output$content), collapse = "\n")
    expect_match(html, "Primary intensity strategy", fixed = TRUE)
    expect_match(html, "Jackknife robustness analysis", fixed = TRUE)
  })
})

test_that("V4 startup shell does not suppress Shiny disconnect failures", {
  css <- fpq_v4_css()
  expect_false(grepl("shiny-disconnected-overlay", css, fixed = TRUE))
  expect_false(grepl("modal-backdrop", css, fixed = TRUE))
  source <- paste(deparse(body(fpq_analysis_settings_server)), collapse = "\n")
  expect_match(source, "reactiveVal(FALSE)", fixed = TRUE)
  expect_match(source, "observeEvent(state$configuration()", fixed = TRUE)
})

test_that("visible runtime identity is MALDIPixelQuant V4", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_shell.R"),
    warn = FALSE), collapse = "\n")
  expect_match(source, 'tags$title("MALDIPixelQuant")', fixed = TRUE)
  expect_match(source, "v4.0.0-dev", fixed = TRUE)
  expect_false(grepl("MALDIPixelQuant 4.0.0-dev", source, fixed = TRUE))
  expect_identical(trimws(readLines(file.path(fpq_repo_root(), "VERSION"))),
                   "4.0.0-dev")
})
