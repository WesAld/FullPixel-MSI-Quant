fpq_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$section(class = "fpq-hero",
      htmltools::tags$h1("FullPixel MSI Quant V2"),
      htmltools::tags$p(class = "fpq-subtitle",
        "A Shiny application for spectrum-sum-normalized quantification of MZmine-aligned features from full-pixel MALDI-MSI data."),
      htmltools::tags$p("The aligned MZmine feature list supplies feature identifiers and target m/z values. Quantitative pixel-level signal is extracted from the original centroided full-pixel imzML/IBD data."),
      htmltools::tags$p("Putative annotation metadata are integrated only after quantitative calculations.")),
    shiny::uiOutput(ns("project_state")),
    htmltools::tags$h2("Workflow stages"),
    shiny::uiOutput(ns("stages")),
    htmltools::tags$h2("Primary actions"),
    shiny::actionButton(ns("new_project"), "Create New Project", class = "btn-primary"),
    shiny::actionButton(ns("open_project"), "Open Existing Project"),
    shiny::actionButton(ns("continue_setup"), "Continue Project Setup"),
    shiny::actionButton(ns("validate"), "Validate Project"),
    shiny::actionButton(ns("workflow"), "View Workflow"))
}
fpq_home_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$project_state <- shiny::renderUI({
      x <- fpq_app_summary(state)
      if (!x$project_open) return(fpq_empty_state("No project open",
        "Create or open a project. Browser session state is not authoritative."))
      htmltools::tags$div(class = "fpq-status-strip",
        fpq_badge(state$project()$manifest$project_status,
                  paste("Project", state$project()$manifest$project_status)),
        htmltools::tags$span(paste("Current project:", x$project_name)),
        htmltools::tags$span(paste("Profile:", x$profile)),
        fpq_badge(if (x$dirty) "WARNING" else "PASS",
                  if (x$dirty) "Unsaved changes" else "Saved"))
    })
    output$stages <- shiny::renderUI({
      config <- state$configuration()
      ids <- c("extract_pixel_intensities", "summarize_replicate_readouts",
               "integrate_mzmine_annotations", "create_combined_publication_export")
      names <- c("Extract Pixel Intensities", "Summarize Replicate Readouts",
                 "Integrate MZmine Annotations", "Create Combined Publication Export")
      htmltools::tags$div(class = "fpq-card-grid", Map(function(id, name) {
        x <- fpq_workflow_stage_status(id, config$ion_mode %||% "NEG",
                                       config$exposure_label %||% "Single")
        workflow <- if (is.null(state$project())) NULL else
          fpq_workflow_stage_summary(state$project())
        stage_status <- if (!is.null(workflow)) {
          mapping <- c(extract_pixel_intensities = "extracting_neg",
            summarize_replicate_readouts = "summarizing",
            integrate_mzmine_annotations = "annotating",
            create_combined_publication_export = "combined_export")
          found <- Filter(function(s) identical(s$stage_id, mapping[[id]]),
                          workflow$stages)
          if (length(found)) found[[1]]$status else NULL
        } else NULL
        display <- if (!is.null(stage_status) && stage_status != "waiting")
          tools::toTitleCase(stage_status) else if (
            id == "create_combined_publication_export")
              "Waiting for annotation integration" else if (x$executable)
                "Available after validation" else x$status
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3(name),
          fpq_badge(if (identical(stage_status, "completed")) "PASS" else
            if (identical(stage_status, "failed")) "FAIL" else "WARNING",
                    display),
          if (!is.null(x$reason)) htmltools::tags$p(x$reason))
      }, ids, names))
    })
  })
}

fpq_about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Settings and About"),
    shiny::uiOutput(ns("content")))
}
fpq_about_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$content <- shiny::renderUI({
      integrity <- fpq_verify_scientific_core(state$repository_root)
      htmltools::tags$div(class = "fpq-card-grid",
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3("FullPixel MSI Quant V2"),
          htmltools::tags$p("Application version 2.0.0-dev"),
          htmltools::tags$p(paste("Package version", as.character(
            utils::packageVersion("FullPixelMSIQuant")))),
          htmltools::tags$p("Scientific-core version 1.0.0"),
          htmltools::tags$p("Repository: FullPixel-MSI-Quant"),
          fpq_badge(integrity$status, paste("Scientific integrity", integrity$status))),
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3("Scientific purpose"),
          htmltools::tags$p("Representative spectrum-sum-normalized quantitative summaries across complete full-pixel MALDI-MSI datasets."),
          htmltools::tags$p("Putative annotations remain putative accurate-mass matches.")),
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3("Current limitations"),
          htmltools::tags$ul(
            htmltools::tags$li("Positive, Sequential, and combined workflows remain Technical Validation Candidates until independent scientific review."),
            htmltools::tags$li("Candidate status is disclosed in validation, execution provenance, results, and workbook naming."),
            htmltools::tags$li("Citation authorship and public license status remain unresolved."),
            htmltools::tags$li("This is not a publication release."))))
    })
  })
}

fpq_shiny_app <- function(repository_root = fpq_repo_root()) {
  css <- "
  body { background:#f4f6f8; color:#1d2a35; }
  .navbar { background:#263746 !important; }
  .navbar-brand,.navbar-nav>li>a { color:#fff !important; }
  .container-fluid { max-width:1800px; }
  .fpq-hero,.fpq-card,.fpq-review,.fpq-empty { background:#fff; border:1px solid #d9e0e5; border-radius:6px; padding:16px; margin:12px 0; }
  .fpq-subtitle { font-size:1.15rem; color:#40576a; }
  .fpq-card-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(260px,1fr)); gap:16px; }
  .fpq-badge { display:inline-block; font-weight:700; border-radius:999px; padding:4px 10px; margin:3px; border:1px solid currentColor; }
  .fpq-pass { color:#146c43; background:#e7f5ee; }
  .fpq-warning { color:#805500; background:#fff4cf; }
  .fpq-fail { color:#a52222; background:#fdeaea; }
  .fpq-neutral { color:#40576a; background:#eef2f5; }
  .fpq-warning-panel,.fpq-error { border-left:5px solid #b26a00; background:#fff5df; padding:14px; margin:12px 0; }
  .fpq-error { border-color:#a52222; background:#fdeaea; }
  .fpq-block-reason { color:#7a2525; font-weight:600; }
  .fpq-status-strip { display:flex; gap:12px; align-items:center; flex-wrap:wrap; background:#fff; padding:10px; border:1px solid #d9e0e5; }
  .btn { margin:4px; } table { background:#fff; }
  #shiny-notification-panel { left:12px; right:auto; bottom:12px; top:auto; }
  :focus { outline:3px solid #2b78c5 !important; outline-offset:2px; }
  @media(max-width:900px){.fpq-card-grid{grid-template-columns:1fr;}}
  "
  ui <- shiny::navbarPage(
    title = "FullPixel MSI Quant V2",
    id = "main_navigation", inverse = TRUE, collapsible = TRUE,
    header = htmltools::tagList(htmltools::tags$style(htmltools::HTML(css)),
      htmltools::tags$div(class = "fpq-status-strip",
        shiny::uiOutput("global_project"), shiny::uiOutput("global_validation"),
        shiny::uiOutput("global_run"),
        shiny::checkboxInput("advanced_mode", "Advanced Mode", FALSE),
        htmltools::tags$span("App 1.1.1 - Scientific core 1.0.0"))),
    shiny::tabPanel("Home", fpq_home_ui("home")),
    shiny::tabPanel("Project", fpq_project_ui("project")),
    shiny::tabPanel("MSI Samples", fpq_sample_import_ui("sample_import")),
    shiny::tabPanel("MZmine Feature List", fpq_mzmine_import_ui("mzmine_import")),
    shiny::tabPanel("Dataset Configuration",
      shiny::tabsetPanel(shiny::tabPanel("Conditions and Replicates", fpq_design_ui("design")),
                         shiny::tabPanel("Files and Columns", fpq_dataset_ui("dataset")))),
    shiny::tabPanel("Validation", fpq_validation_ui("validation")),
    shiny::tabPanel("Workflow", fpq_workflow_ui("workflow")),
    shiny::tabPanel("Runs", fpq_runs_ui("runs")),
    shiny::tabPanel("Results", fpq_results_ui("results")),
    shiny::tabPanel("Artifacts", fpq_artifacts_ui("artifacts")),
    shiny::tabPanel("Logs", fpq_logs_ui("logs")),
    shiny::tabPanel("Provenance", fpq_provenance_ui("provenance")),
    shiny::tabPanel("Advanced Settings", value = "advanced_settings",
      shiny::tabsetPanel(
        shiny::tabPanel("Scientific validation details",
          fpq_profile_ui("profile")))),
    shiny::tabPanel("About", fpq_about_ui("about")),
    footer = htmltools::tags$footer(class = "container-fluid",
      htmltools::tags$hr(),
      htmltools::tags$p("FullPixel MSI Quant - putative annotations remain putative.")))
  server <- function(input, output, session) {
    state <- fpq_new_app_state(repository_root)
    shiny::observeEvent(input$advanced_mode, {
      state$advanced_mode(isTRUE(input$advanced_mode))
      if (isTRUE(input$advanced_mode)) {
        shiny::showTab("main_navigation", "advanced_settings", session = session)
      } else {
        if (identical(input$main_navigation, "advanced_settings"))
          shiny::updateNavbarPage(session, "main_navigation", selected = "Home")
        shiny::hideTab("main_navigation", "advanced_settings", session = session)
      }
    }, ignoreInit = FALSE)
    fpq_home_server("home", state)
    fpq_project_server("project", state)
    fpq_sample_import_server("sample_import", state)
    fpq_mzmine_import_server("mzmine_import", state)
    fpq_design_server("design", state)
    fpq_dataset_server("dataset", state)
    fpq_profile_server("profile", state)
    fpq_validation_server("validation", state)
    fpq_workflow_server("workflow", state)
    fpq_runs_server("runs", state)
    fpq_results_server("results", state)
    fpq_artifacts_server("artifacts", state)
    fpq_logs_server("logs", state)
    fpq_provenance_server("provenance", state)
    fpq_about_server("about", state)
    output$global_project <- shiny::renderUI({
      p <- state$project()
      if (is.null(p)) fpq_badge("neutral", "No project open") else
        htmltools::tags$span(paste("Project:", p$manifest$project_name),
                            fpq_badge(p$manifest$project_status))
    })
    output$global_validation <- shiny::renderUI({
      v <- state$validations()
      if (!length(v)) fpq_badge("neutral", "Not validated") else
        fpq_badge(if (v$profile$classification == "profile_compatible") "PASS" else
          if (v$profile$classification == "profile_candidate_compatible")
            "WARNING" else "FAIL",
          if (v$profile$classification == "profile_candidate_compatible")
            "Ready with scientific warning" else v$profile$classification)
    })
    output$global_run <- shiny::renderUI({
      process <- state$active_worker()
      workflow <- if (is.null(state$project())) NULL else
        fpq_workflow_stage_summary(state$project())
      workflow_active <- !is.null(workflow) &&
        workflow$status %in% c("prepared", "starting", "running", "cancelling")
      legacy_active <- !is.null(process) && process$is_alive()
      label <- if (workflow_active || legacy_active) "Run active" else
        if (!is.null(workflow) && workflow$status == "completed")
          "Last run completed" else "No active run"
      fpq_badge(if (workflow_active || legacy_active) "WARNING" else
        if (label == "Last run completed") "PASS" else "neutral", label)
    })
  }
  shiny::shinyApp(ui, server)
}
