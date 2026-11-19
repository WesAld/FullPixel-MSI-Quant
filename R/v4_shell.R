# Fourth-generation visual shell implementation. Existing modules remain
# mounted with their original IDs and server implementations.

fpq_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("home_content"))
}

fpq_home_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$home_content <- shiny::renderUI({
      project <- state$project()
      logo <- htmltools::tags$img(
        class = "fpq-v4-home-logo",
        src = "branding/MALDI-PixelQuant-Laser-Spectrum-Logo.png",
        alt = "MALDIPixelQuant")
      if (is.null(project)) {
        capabilities <- list(
          list("chart-simple", "Normalization", "Raw, TIC, RMS and Median"),
          list("chart-line", "Statistical analysis",
               "Group comparison with optional jackknife"),
          list("diagram-project", "Integrated workflow",
               "From MSI data to publication-ready output"),
          list("file-excel", "Final Excel workbook",
               "All results in a single file"))
        steps <- list(
          list("Create or open a project",
               "Define project location and acquisition settings."),
          list("Register MSI samples", "Add imzML/IBD data."),
          list("Configure dataset",
               "Define groups, replicates and analysis parameters."),
          list("Run analysis", "Generate results and final workbook."))

        htmltools::tags$div(class = "fpq-v4-home",
          htmltools::tags$section(class = "fpq-v4-home-hero fpq-v4-surface",
            htmltools::tags$div(class = "fpq-v4-hero-copy",
              htmltools::tags$h1("Welcome to MALDIPixelQuant"),
              htmltools::tags$p(class = "fpq-v4-hero-lead",
                "Pixel-level quantitative analysis of MALDI mass spectrometry imaging data."),
              htmltools::tags$p(class = "fpq-v4-hero-detail",
                paste("Extract, normalize, compare and export feature-level",
                      "MALDI-MSI intensities using reproducible workflows.")),
              htmltools::tags$div(class = "fpq-v4-home-actions",
                htmltools::tags$div(class = "fpq-v4-action-block",
                  shiny::actionButton(session$ns("create_project"),
                    "+  Create New Project",
                    class = "btn btn-primary fpq-v4-primary"),
                  htmltools::tags$span(
                    "Set up a new MALDIPixelQuant project")),
                htmltools::tags$div(class = "fpq-v4-action-block",
                  shiny::actionButton(session$ns("open_project"),
                    "Open Existing Project", class = "btn fpq-v4-secondary"),
                  htmltools::tags$span(
                    "Open a previously created project")))),
            htmltools::tags$div(class = "fpq-v4-scientific-visual",
              role = "img",
              `aria-label` = paste("Abstract MALDI mass spectrum and pixel",
                                   "grid illustration"),
              htmltools::tags$div(class = "fpq-v4-pixel-field",
                lapply(seq_len(28L), function(i)
                  htmltools::tags$span(class = paste0("p", i)))),
              htmltools::tags$div(class = "fpq-v4-spectrum",
                lapply(c(20, 34, 52, 29, 74, 42, 88, 38, 67, 27, 56, 35),
                  function(height) htmltools::tags$span(
                    style = sprintf("height:%s%%", height))))),
            htmltools::tags$div(class = "fpq-v4-capabilities",
              lapply(capabilities, function(item)
                htmltools::tags$div(class = "fpq-v4-capability",
                  htmltools::tags$div(class = "fpq-v4-capability-icon",
                    shiny::icon(item[[1L]])),
                  htmltools::tags$div(
                    htmltools::tags$strong(item[[2L]]),
                    htmltools::tags$span(item[[3L]])))))),
          htmltools::tags$div(class = "fpq-v4-home-lower",
            htmltools::tags$section(class = "fpq-v4-status-panel fpq-v4-surface",
              htmltools::tags$h2("Application status"),
              htmltools::tags$div(class = "fpq-v4-status-grid",
                fpq_v4_status_item("folder", "Project", "No project open"),
                fpq_v4_status_item("shield", "Validation", htmltools::HTML("&mdash;")),
                fpq_v4_status_item("circle-play", "Active run", htmltools::HTML("&mdash;")),
                fpq_v4_status_item("server", "Version",
                  htmltools::tagList(htmltools::tags$span("App 2.0.1"),
                                     htmltools::tags$span(
                                       "Scientific core 1.0.0"))))),
            htmltools::tags$section(class = "fpq-v4-getting-started fpq-v4-surface",
              htmltools::tags$h2("Getting started"),
              htmltools::tags$ol(
                lapply(seq_along(steps), function(i)
                  htmltools::tags$li(
                    htmltools::tags$span(class = "fpq-v4-step-number", i),
                    htmltools::tags$div(
                      htmltools::tags$strong(steps[[i]][[1L]]),
                      htmltools::tags$span(steps[[i]][[2L]]))))))),
          htmltools::tags$section(class = "fpq-v4-jackknife-strip fpq-v4-surface",
            htmltools::tags$div(class = "fpq-v4-jackknife-icon",
              shiny::icon("circle-info")),
            htmltools::tags$div(
              htmltools::tags$strong("Jackknife robustness analysis"),
              htmltools::tags$span(paste("Optionally include jackknife statistics",
                "based on matched biological replicates. Results are integrated",
                "into the final Excel workbook.")))))
      } else {
      config <- state$configuration()
      included <- Filter(function(x)
        identical(x$inclusion_status %||% "included", "included"),
        config$samples %||% list())
      sample_ids <- unique(vapply(included, function(x)
        x$biological_sample %||% x$sample_id %||% "", character(1L)))
      sample_ids <- sample_ids[nzchar(sample_ids)]
      modes <- unique(toupper(vapply(included, function(x)
        x$ion_mode %||% "", character(1L))))
      modes <- modes[nzchar(modes)]
      validation <- state$validations()
      readiness <- if (!length(validation)) "Not validated" else
        switch(validation$readiness %||% "blocked",
          ready = "Ready", ready_with_warning = "Ready with warning",
          candidate_workflow_ready = "Ready", "Not ready")
      workflow <- tryCatch(fpq_workflow_stage_summary(project),
                           error = function(e) NULL)
      run_status <- if (is.null(workflow)) "No recorded run" else
        tools::toTitleCase(workflow$status %||% "Unknown")

      htmltools::tags$section(class = "fpq-v4-home fpq-v4-surface",
        htmltools::tags$div(class = "fpq-v4-dashboard-heading",
          htmltools::tags$div(
            htmltools::tags$div(class = "fpq-v4-eyebrow", "CURRENT PROJECT"),
            htmltools::tags$h1(project$manifest$project_name)),
          logo),
        htmltools::tags$p(class = "fpq-v4-lead",
          "Continue configuration, validate the project, or review the workflow."),
        htmltools::tags$div(class = "fpq-v4-dashboard-grid",
          fpq_v4_metric("Samples", length(sample_ids)),
          fpq_v4_metric("Ion modes", if (length(modes))
            paste(modes, collapse = " + ") else "Not configured"),
          fpq_v4_metric("Readiness", readiness),
          fpq_v4_metric("Primary intensity",
            fpq_normalization_labels()[[fpq_normalization_method(config)]]),
          fpq_v4_metric("Jackknife",
            if (fpq_jackknife_enabled(config)) "Enabled" else "Disabled"),
          fpq_v4_metric("Recent run", run_status)),
        htmltools::tags$div(class = "fpq-v4-home-actions",
          shiny::actionButton(session$ns("continue_setup"), "Continue Setup",
                              class = "btn fpq-v4-secondary"),
          shiny::actionButton(session$ns("validate_project"), "Validate Project",
                              class = "btn fpq-v4-secondary"),
          shiny::actionButton(session$ns("view_workflow"), "View Workflow",
                              class = "btn btn-primary fpq-v4-primary")))
      }
    })

    shiny::observeEvent(input$create_project,
      fpq_v4_navigate(session$rootScope(), "project_create"))
    shiny::observeEvent(input$open_project,
      fpq_v4_navigate(session$rootScope(), "project_open"))
    shiny::observeEvent(input$continue_setup,
      fpq_v4_navigate(session$rootScope(), "project_overview"))
    shiny::observeEvent(input$validate_project,
      fpq_v4_navigate(session$rootScope(), "validation"))
    shiny::observeEvent(input$view_workflow,
      fpq_v4_navigate(session$rootScope(), "workflow"))
  })
}

fpq_v4_metric <- function(label, value) {
  htmltools::tags$div(class = "fpq-v4-metric",
    htmltools::tags$span(label), htmltools::tags$strong(value))
}

fpq_v4_status_item <- function(icon, label, value) {
  htmltools::tags$div(class = "fpq-v4-status-item",
    htmltools::tags$div(class = "fpq-v4-status-icon", shiny::icon(icon)),
    htmltools::tags$div(
      htmltools::tags$strong(label),
      htmltools::tags$span(value)))
}

fpq_v4_route_panel <- function(route, ...) {
  shiny::conditionalPanel(
    condition = sprintf("input.v4_route === '%s'", route),
    htmltools::tags$main(class = "fpq-v4-page", ...))
}

fpq_v4_sidebar <- function() {
  logo_path <- "branding/MALDI-PixelQuant-Laser-Spectrum-Logo.png"
  htmltools::tags$aside(class = "fpq-v4-sidebar",
    htmltools::tags$div(class = "fpq-v4-brand",
      htmltools::tags$img(src = logo_path, alt = "MALDIPixelQuant")),
    htmltools::tags$nav(class = "fpq-v4-navigation",
      htmltools::tags$div(class = "fpq-v4-nav-heading", "HOME"),
      fpq_v4_nav_item(identity, "home", "Home", "house"),
      htmltools::tags$div(class = "fpq-v4-nav-heading", "PROJECT"),
      fpq_v4_nav_item(identity, "project", "Project", "folder"),
      fpq_v4_nav_item(identity, "msi_samples", "MSI Samples", "microscope"),
      fpq_v4_nav_item(identity, "mzmine", "MZmine Feature List", "list"),
      fpq_v4_nav_item(identity, "dataset_configuration", "Dataset Configuration", "sliders"),
      fpq_v4_nav_item(identity, "validation", "Validation", "shield"),
      htmltools::tags$div(class = "fpq-v4-nav-heading", "ANALYSIS"),
      fpq_v4_nav_item(identity, "workflow", "Workflow", "diagram-project"),
      fpq_v4_nav_item(identity, "runs", "Run History", "clock-rotate-left"),
      shiny::uiOutput("v4_technical_navigation"),
      htmltools::tags$div(class = "fpq-v4-nav-heading", "OTHER"),
      fpq_v4_nav_item(identity, "about", "About", "circle-info")),
    htmltools::tags$div(class = "fpq-v4-sidebar-footer",
      htmltools::tags$div(class = "fpq-v4-mode-control",
        htmltools::tags$span("Display mode"),
        shiny::checkboxInput("advanced_mode", "Advanced Mode", FALSE)),
      htmltools::tags$div(class = "fpq-v4-version",
        htmltools::tags$strong("MALDIPixelQuant"),
        htmltools::tags$span("V2 / 2.0.1"),
        htmltools::tags$span("Scientific core 1.0.0")))
  )
}

fpq_v4_css <- function() "
:root{--fpq-navy:#102a56;--fpq-blue:#0877e8;--fpq-bg:#f3f7fb;--fpq-border:#dbe5ef;--fpq-muted:#5e718c;--fpq-success:#16885a;--fpq-warning:#a66b00;--fpq-danger:#bd3038}
html,body{height:100%;margin:0;background:var(--fpq-bg);color:#172b4d;font-family:Inter,'Segoe UI',Arial,sans-serif;font-size:15px}
.fpq-v4-app{min-height:100vh}.fpq-v4-sidebar{position:fixed;inset:0 auto 0 0;width:272px;background:#fff;border-right:1px solid var(--fpq-border);display:flex;flex-direction:column;z-index:20;box-shadow:2px 0 10px rgba(23,43,77,.025)}
.fpq-v4-brand{height:138px;padding:6px 3px 4px;border-bottom:1px solid #edf2f7;display:flex;align-items:center;justify-content:center}.fpq-v4-brand img{width:266px;height:125px;max-width:100%;object-fit:contain;object-position:center 60%}
.fpq-v4-navigation{padding:12px 15px 16px;overflow-y:auto;flex:1}.fpq-v4-nav-heading{font-size:11px;font-weight:800;letter-spacing:.1em;color:#71839a;margin:20px 13px 7px}.fpq-v4-nav-heading:first-child{margin-top:8px}.fpq-v4-nav-item{border-radius:9px;margin:3px 0}.fpq-v4-nav-item a{display:flex;align-items:center;gap:13px;padding:10px 14px;color:#294365;text-decoration:none;font-size:14px;font-weight:650;line-height:1.25}.fpq-v4-nav-item a:hover{background:#edf6ff;color:var(--fpq-blue)}.fpq-v4-nav-item.active{background:#e4f1ff;box-shadow:inset 3px 0 0 var(--fpq-blue)}.fpq-v4-nav-item.active a{color:#066dd5;font-weight:750}.fpq-v4-nav-item .fa{width:19px;text-align:center;font-size:15px}
.fpq-v4-sidebar-footer{padding:16px 22px 20px;border-top:1px solid #edf2f7}.fpq-v4-mode-control>span{display:block;font-size:12px;color:var(--fpq-muted);margin-bottom:5px}.fpq-v4-mode-control .form-group{margin:0}.fpq-v4-version{display:flex;flex-direction:column;color:#71829a;font-size:12px;margin-top:14px;line-height:1.5}.fpq-v4-version strong{color:#425a78}
.fpq-v4-main{margin-left:272px;min-height:100vh}.fpq-v4-topbar{height:52px;background:rgba(255,255,255,.96);border-bottom:1px solid var(--fpq-border);display:flex;justify-content:flex-end;align-items:center;padding:0 30px;position:sticky;top:0;z-index:10}.fpq-v4-top-status{display:flex;gap:7px;align-items:center}.fpq-v4-top-status .fpq-badge{font-size:11px;font-weight:650;padding:3px 9px;border-color:#d8e1ea;background:#f6f8fa;color:#5d6f82}.fpq-v4-page{padding:28px 34px 44px;max-width:1720px;margin:0 auto}.fpq-v4-route-control{display:none}
.fpq-v4-surface,.fpq-card,.fpq-review,.fpq-empty{background:#fff;border:1px solid var(--fpq-border);border-radius:12px;box-shadow:0 3px 12px rgba(23,43,77,.045)}.fpq-card,.fpq-review,.fpq-empty{padding:18px;margin:12px 0}.fpq-card-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:16px}
.fpq-v4-home{max-width:1480px;margin:14px auto 0}.fpq-v4-home-hero{display:grid;grid-template-columns:minmax(0,1.35fr) minmax(330px,.65fr);padding:34px 38px 0;overflow:hidden}.fpq-v4-hero-copy{padding:25px 8px 30px 4px}.fpq-v4-home-logo{width:430px;height:82px;max-width:100%;object-fit:contain;object-position:left center}.fpq-v4-home h1{font-size:38px;line-height:1.16;color:var(--fpq-navy);margin:7px 0 10px;text-align:left;font-weight:760;letter-spacing:-.02em}.fpq-v4-hero-lead{margin:0 0 9px;font-size:19px;font-weight:550;color:#334f73;line-height:1.45}.fpq-v4-hero-detail{max-width:760px;margin:0;color:var(--fpq-muted);font-size:16px;line-height:1.5}.fpq-v4-home-actions{display:flex;justify-content:flex-start;gap:16px;flex-wrap:wrap;margin:26px 0 0}.fpq-v4-action-block{display:flex;flex-direction:column;align-items:center;gap:6px}.fpq-v4-action-block>span{font-size:12px;color:#6e8096}.fpq-v4-primary,.fpq-v4-secondary{min-width:240px;border-radius:7px;padding:11px 21px;font-size:15px;font-weight:750}.fpq-v4-primary{background:var(--fpq-blue);border-color:var(--fpq-blue);box-shadow:0 3px 8px rgba(8,119,232,.17)}.fpq-v4-primary:hover{background:#066ccd;border-color:#066ccd}.fpq-v4-secondary{background:#fff;border:1px solid #b5c5d6;color:var(--fpq-navy)}
.fpq-v4-scientific-visual{position:relative;min-height:290px;margin:-34px -38px 0 22px;background:linear-gradient(150deg,#fbfdff 0%,#f0f6fc 52%,#e9f2fb 100%);overflow:hidden;border-left:1px solid #edf2f7;opacity:.84}.fpq-v4-scientific-visual:after{content:'';position:absolute;inset:0;background:radial-gradient(circle at 70% 22%,rgba(70,139,207,.11),transparent 31%),linear-gradient(115deg,transparent 58%,rgba(255,255,255,.72) 59%,transparent 62%)}.fpq-v4-pixel-field{position:absolute;right:30px;top:27px;display:grid;grid-template-columns:repeat(7,26px);gap:5px;transform:rotate(-6deg)}.fpq-v4-pixel-field span{height:26px;border-radius:4px;background:#dfecf8}.fpq-v4-pixel-field .p3,.fpq-v4-pixel-field .p9,.fpq-v4-pixel-field .p17,.fpq-v4-pixel-field .p25{background:#a8caeb}.fpq-v4-pixel-field .p11,.fpq-v4-pixel-field .p18{background:#75abe0}.fpq-v4-pixel-field .p12{background:#4b94d7}.fpq-v4-spectrum{position:absolute;left:42px;right:38px;bottom:42px;height:118px;border-left:2px solid #6785a8;border-bottom:2px solid #6785a8;display:flex;align-items:flex-end;gap:12px;padding:0 14px}.fpq-v4-spectrum span{display:block;flex:1;max-width:9px;background:#4a94d8;border-radius:3px 3px 0 0;opacity:.72}
.fpq-v4-capabilities{grid-column:1/-1;display:grid;grid-template-columns:repeat(4,1fr);border-top:1px solid #e4ebf2;margin:0 -38px;padding:0 22px}.fpq-v4-capability{display:flex;align-items:center;justify-content:center;gap:12px;min-height:104px;padding:14px 18px;border-right:1px solid #e4ebf2}.fpq-v4-capability:last-child{border-right:0}.fpq-v4-capability-icon{flex:0 0 40px;height:40px;border-radius:10px;background:#e8f3ff;color:var(--fpq-blue);display:grid;place-items:center;font-size:19px}.fpq-v4-capability strong{display:block;color:var(--fpq-navy);font-size:15px;margin-bottom:3px}.fpq-v4-capability span{display:block;color:var(--fpq-muted);font-size:13px;line-height:1.4}
.fpq-v4-home-lower{display:grid;grid-template-columns:minmax(0,1.35fr) minmax(360px,.65fr);gap:18px;margin-top:18px}.fpq-v4-status-panel,.fpq-v4-getting-started{padding:20px 22px}.fpq-v4-status-panel h2,.fpq-v4-getting-started h2{font-size:20px;color:var(--fpq-navy);margin:0 0 16px}.fpq-v4-status-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:11px}.fpq-v4-status-item{min-height:86px;padding:15px 13px;border:1px solid #e1e9f1;border-radius:9px;background:#f9fbfd;display:flex;align-items:center;gap:11px}.fpq-v4-status-icon{flex:0 0 38px;height:38px;border-radius:9px;background:#e8f3ff;color:var(--fpq-blue);display:grid;place-items:center;font-size:16px}.fpq-v4-status-item strong{display:block;color:var(--fpq-navy);font-size:13px;margin-bottom:3px}.fpq-v4-status-item span{display:block;color:#61758b;font-size:13px;line-height:1.35}.fpq-v4-getting-started ol{list-style:none;padding:0;margin:0;display:grid;gap:10px}.fpq-v4-getting-started li{display:flex;align-items:flex-start;gap:11px}.fpq-v4-step-number{flex:0 0 25px;height:25px;border-radius:50%;display:grid!important;place-items:center;background:var(--fpq-blue);color:#fff!important;font-weight:800;font-size:12px!important}.fpq-v4-getting-started strong{display:block;color:var(--fpq-navy);font-size:13px}.fpq-v4-getting-started li div span{display:block;color:var(--fpq-muted);font-size:12px;margin-top:1px}.fpq-v4-jackknife-strip{display:flex;align-items:center;gap:12px;margin-top:18px;padding:13px 18px}.fpq-v4-jackknife-icon{flex:0 0 32px;height:32px;border-radius:50%;background:#e8f3ff;color:var(--fpq-blue);display:grid;place-items:center}.fpq-v4-jackknife-strip strong{display:block;color:var(--fpq-navy);font-size:13px}.fpq-v4-jackknife-strip span{display:block;color:var(--fpq-muted);font-size:12px;margin-top:2px}
.fpq-v4-lead{max-width:760px;margin:0 auto 28px;text-align:center;font-size:18px;color:var(--fpq-muted);line-height:1.55}.fpq-v4-eyebrow{font-size:12px;font-weight:800;letter-spacing:.08em;color:#6d7f96}.fpq-v4-dashboard-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-top:13px}.fpq-v4-metric{padding:14px;border:1px solid #e1e8f0;border-radius:8px;background:#f9fbfd;display:flex;flex-direction:column;gap:4px}.fpq-v4-metric span{font-size:12px;color:var(--fpq-muted)}.fpq-v4-dashboard-heading{display:flex;align-items:center;justify-content:space-between}.fpq-v4-dashboard-heading h1{text-align:left;margin:5px 0}.fpq-v4-dashboard-heading .fpq-v4-home-logo{width:360px;height:90px}.fpq-v4-dashboard-grid{grid-template-columns:repeat(3,1fr)}
.fpq-v4-page-heading{display:flex;align-items:flex-start;justify-content:space-between;gap:24px;margin:2px 0 22px}.fpq-v4-page-heading h1{margin:0;color:var(--fpq-navy);font-size:30px;line-height:1.2}.fpq-v4-page-heading p{margin:5px 0 0;color:var(--fpq-muted);font-size:15px}.fpq-v4-section,.fpq-v4-analysis-summary,.fpq-v4-run-card,.fpq-v4-about{background:#fff;border:1px solid var(--fpq-border);border-radius:11px;box-shadow:0 2px 8px rgba(23,43,77,.04);padding:20px;margin:14px 0}.fpq-v4-section-heading{display:flex;align-items:flex-start;gap:13px;margin-bottom:17px}.fpq-v4-section-heading h2,.fpq-v4-analysis-summary h2,.fpq-v4-workflow-output h2{font-size:20px;color:var(--fpq-navy);margin:0}.fpq-v4-section-heading p{color:var(--fpq-muted);margin:4px 0 0}.fpq-v4-step{display:inline-grid;place-items:center;flex:0 0 30px;height:30px;border-radius:50%;background:var(--fpq-blue);color:#fff;font-weight:800}.fpq-v4-import-grid,.fpq-v4-method-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:14px}.fpq-v4-import-card{background:#fff;border:1px solid var(--fpq-border);border-radius:11px;padding:22px;text-align:center}.fpq-v4-import-card h3{color:var(--fpq-navy);margin:10px 0 5px}.fpq-v4-import-card p{color:var(--fpq-muted);min-height:42px}.fpq-v4-import-icon{margin:auto;display:grid;place-items:center;width:46px;height:46px;border-radius:12px;background:#e7f2ff;color:var(--fpq-blue);font-size:20px}.fpq-v4-table-toolbar{display:flex;align-items:flex-end;justify-content:space-between;gap:14px;flex-wrap:wrap;margin-bottom:12px}.fpq-v4-table-toolbar>div{display:flex;align-items:flex-end;gap:7px;flex-wrap:wrap}.fpq-v4-field-action{padding-top:21px}.fpq-v4-form-footer{display:flex;justify-content:flex-end;align-items:center;gap:10px;border-top:1px solid #edf1f5;margin-top:18px;padding-top:16px}.fpq-v4-method-card{display:block;cursor:pointer;border:1px solid var(--fpq-border);border-radius:9px;padding:14px;background:#fff;min-height:90px}.fpq-v4-method-card.selected{border:2px solid var(--fpq-blue);background:#eff7ff}.fpq-v4-method-card input{margin-right:7px}.fpq-v4-method-card strong{color:var(--fpq-navy)}.fpq-v4-method-card span{display:block;color:var(--fpq-muted);font-size:12px;margin:7px 0 0 24px}.fpq-v4-jackknife{margin-top:18px;border-top:1px solid #e7edf4;padding-top:17px}.fpq-v4-notice{padding:11px 13px;border-radius:7px;margin:10px 0}.fpq-v4-notice.warning{background:#fff6dc;color:#754d00;border:1px solid #f1d58a}.fpq-v4-notice.success{background:#e9f7f0;color:#146c43;border:1px solid #b9e2cd}.fpq-v4-muted{color:var(--fpq-muted)}.fpq-v4-actions-menu{position:relative;display:inline-block;margin:4px}.fpq-v4-actions-menu>summary{cursor:pointer;list-style:none;border:1px solid #b9c8d8;border-radius:6px;padding:8px 12px;background:#fff;font-weight:700}.fpq-v4-actions-menu[open]{padding:8px;background:#fff;border:1px solid var(--fpq-border);border-radius:8px;box-shadow:0 8px 24px rgba(20,42,80,.12)}.fpq-v4-technical-details{margin-top:16px;background:#fff;border:1px solid var(--fpq-border);border-radius:10px;padding:14px}.fpq-v4-technical-details>summary{cursor:pointer;color:var(--fpq-navy);font-weight:800}.fpq-v4-summary-grid{display:grid;grid-template-columns:repeat(6,minmax(120px,1fr));gap:10px;margin-top:13px}.fpq-v4-workflow-output{min-height:220px}.fpq-v4-run-list{display:grid;gap:12px}.fpq-v4-run-card{display:grid;grid-template-columns:minmax(210px,1fr) auto minmax(360px,2fr);gap:18px;align-items:center}.fpq-v4-run-card>div:first-child{display:flex;flex-direction:column;gap:3px}.fpq-v4-run-card dl{display:grid;grid-template-columns:auto 1fr;gap:4px 12px;margin:0}.fpq-v4-run-card dt{color:var(--fpq-muted)}.fpq-v4-run-card dd{margin:0}.fpq-v4-about{max-width:880px;margin:22px auto;text-align:center;padding:42px}.fpq-v4-about-logo{width:100%;height:170px;object-fit:contain}.fpq-v4-about h1{color:var(--fpq-navy);font-size:34px;margin:18px 0 5px}.fpq-v4-version-list{display:grid;grid-template-columns:1fr 1fr;max-width:480px;margin:26px auto;text-align:left;border-top:1px solid var(--fpq-border)}.fpq-v4-version-list dt,.fpq-v4-version-list dd{padding:9px;border-bottom:1px solid var(--fpq-border);margin:0}
.fpq-v4-path-picker{display:grid;grid-template-columns:minmax(0,1fr) auto;gap:8px;align-items:end;max-width:900px}.fpq-v4-path-picker .form-group{margin-bottom:0}.fpq-v4-path-picker .btn{margin:0 0 1px}.fpq-v4-project-identity h2{margin:0;color:var(--fpq-navy)}.fpq-v4-project-path{margin:7px 0 0;color:var(--fpq-muted);overflow-wrap:anywhere}.fpq-v4-project-summary{padding:14px 18px}.fpq-v4-project-summary h3{margin:0 0 10px}.fpq-v4-project-summary-items{display:flex;align-items:center;gap:11px;flex-wrap:wrap}.fpq-v4-project-summary-items>span:not(:last-child)::after{content:'\\00b7';color:#9aabba;margin-left:11px}.fpq-v4-project-settings{max-width:none}.fpq-v4-settings-grid{display:grid;grid-template-columns:repeat(3,minmax(190px,1fr));gap:10px 16px}.fpq-v4-project-settings h4{margin:22px 0 6px;color:var(--fpq-navy)}
.fpq-badge{display:inline-block;font-weight:700;border-radius:999px;padding:4px 10px;border:1px solid currentColor}.fpq-pass{color:#146c43;background:#e7f5ee}.fpq-warning{color:#805500;background:#fff4cf}.fpq-fail{color:#a52222;background:#fdeaea}.fpq-neutral{color:#40576a;background:#eef2f5}.fpq-warning-panel,.fpq-error{border-left:4px solid var(--fpq-warning);background:#fff7e5;padding:14px;margin:12px 0}.fpq-error{border-color:var(--fpq-danger);background:#fdefef}.fpq-status-strip{display:flex;gap:8px;align-items:center}.btn{margin:4px}table{background:#fff}:focus{outline:3px solid #2b78c5!important;outline-offset:2px}
@media(max-width:1280px){.fpq-v4-home-hero{grid-template-columns:minmax(0,1.2fr) minmax(280px,.8fr)}.fpq-v4-capabilities{grid-template-columns:repeat(2,1fr)}.fpq-v4-capability:nth-child(2){border-right:0}.fpq-v4-capability:nth-child(-n+2){border-bottom:1px solid #e4ebf2}.fpq-v4-status-grid{grid-template-columns:repeat(2,1fr)}.fpq-v4-home-lower{grid-template-columns:1fr 370px}.fpq-v4-summary-grid{grid-template-columns:repeat(3,1fr)}.fpq-v4-run-card{grid-template-columns:1fr}}
@media(max-width:1020px){.fpq-v4-sidebar{width:232px}.fpq-v4-brand{height:112px}.fpq-v4-brand img{width:208px;height:98px}.fpq-v4-main{margin-left:232px}.fpq-v4-page{padding:22px}.fpq-v4-home-hero{grid-template-columns:1fr;padding:28px 30px 0}.fpq-v4-scientific-visual{display:none}.fpq-v4-capabilities{margin:0 -30px}.fpq-v4-home-lower{grid-template-columns:1fr}.fpq-v4-nav-item a{font-size:13px;padding:9px 11px}.fpq-v4-home h1{font-size:33px}.fpq-v4-status-grid,.fpq-v4-dashboard-grid,.fpq-v4-summary-grid{grid-template-columns:repeat(2,1fr)}.fpq-card-grid{grid-template-columns:1fr}.fpq-v4-page-heading{flex-direction:column}.fpq-v4-form-footer{justify-content:flex-start}}
@media(max-width:760px){.fpq-v4-capabilities,.fpq-v4-status-grid,.fpq-v4-dashboard-grid,.fpq-v4-summary-grid,.fpq-v4-settings-grid{grid-template-columns:1fr}.fpq-v4-path-picker{grid-template-columns:1fr}.fpq-v4-capability{border-right:0;border-bottom:1px solid #e4ebf2}.fpq-v4-capability:last-child{border-bottom:0}.fpq-v4-home-actions{flex-direction:column;align-items:stretch}.fpq-v4-action-block{align-items:stretch}.fpq-v4-primary,.fpq-v4-secondary{width:100%}}
"

fpq_shiny_app <- function(repository_root = fpq_repo_root()) {
  branding_root <- file.path(repository_root, "app", "www", "branding")
  if (!dir.exists(branding_root))
    branding_root <- file.path(fpq_repo_root(), "app", "www", "branding")
  registered <- shiny::resourcePaths()
  if ("branding" %in% names(registered)) shiny::removeResourcePath("branding")
  shiny::addResourcePath("branding", branding_root)
  route_panels <- htmltools::tagList(
    fpq_v4_route_panel("home", fpq_home_ui("home")),
    fpq_v4_route_panel("project", fpq_project_ui("project")),
    fpq_v4_route_panel("msi_samples", fpq_sample_import_ui("sample_import")),
    fpq_v4_route_panel("mzmine", fpq_mzmine_import_ui("mzmine_import")),
    fpq_v4_route_panel("dataset_configuration",
      shiny::tabsetPanel(id = "dataset_configuration_tabs",
        shiny::tabPanel("Experimental Design",
          fpq_design_ui("design"),
          fpq_analysis_settings_ui("analysis_settings")),
        shiny::tabPanel("Files and Columns", fpq_dataset_ui("dataset")))),
    fpq_v4_route_panel("validation", fpq_validation_ui("validation")),
    fpq_v4_route_panel("workflow", fpq_workflow_ui("workflow")),
    fpq_v4_route_panel("runs", fpq_runs_ui("runs")),
    fpq_v4_route_panel("results_internal", fpq_results_ui("results")),
    fpq_v4_route_panel("artifacts", fpq_artifacts_ui("artifacts")),
    fpq_v4_route_panel("logs", fpq_logs_ui("logs")),
    fpq_v4_route_panel("provenance", fpq_provenance_ui("provenance")),
    fpq_v4_route_panel("scientific_validation_details", fpq_profile_ui("profile")),
    fpq_v4_route_panel("about", fpq_about_ui("about")))

  ui <- shiny::fluidPage(class = "fpq-v4-app",
    htmltools::tags$head(
      htmltools::tags$title("MALDIPixelQuant"),
      htmltools::tags$style(htmltools::HTML(fpq_v4_css())),
      htmltools::tags$script(htmltools::HTML(
        "Shiny.addCustomMessageHandler('fpq-v4-route', function(x){
          document.querySelectorAll('.fpq-v4-nav-item').forEach(function(el){
            el.classList.toggle('active', el.dataset.route === x.route);
          });
        });"))),
    fpq_v4_sidebar(),
    htmltools::tags$div(class = "fpq-v4-main",
      htmltools::tags$header(class = "fpq-v4-topbar",
        htmltools::tags$div(class = "fpq-v4-top-status",
          shiny::uiOutput("global_project"),
          shiny::uiOutput("global_validation"),
          shiny::uiOutput("global_run"))),
      htmltools::tags$div(class = "fpq-v4-route-control",
        shiny::selectInput("v4_route", NULL, choices = fpq_v4_routes(),
                           selected = "home")),
      route_panels))

  server <- function(input, output, session) {
    state <- fpq_new_app_state(repository_root)
    shiny::observeEvent(input$advanced_mode,
      state$advanced_mode(isTRUE(input$advanced_mode)), ignoreInit = FALSE)
    output$v4_technical_navigation <- shiny::renderUI({
      if (!isTRUE(state$advanced_mode())) return(NULL)
      htmltools::tagList(
        htmltools::tags$div(class = "fpq-v4-nav-heading", "TECHNICAL RECORDS"),
        fpq_v4_nav_item(identity, "artifacts", "Artifacts", "box-archive"),
        fpq_v4_nav_item(identity, "logs", "Technical Logs", "file-lines"),
        fpq_v4_nav_item(identity, "provenance", "Provenance", "share-nodes"),
        fpq_v4_nav_item(identity, "scientific_validation_details",
                        "Scientific Validation Details", "flask"))
    })
    shiny::observeEvent(state$advanced_mode(), {
      if (!isTRUE(state$advanced_mode()) &&
          input$v4_route %in% fpq_v4_advanced_routes())
        fpq_v4_navigate(session, "home")
    })
    fpq_v4_bind_navigation(input, session, state)

    fpq_home_server("home", state)
    fpq_project_server("project", state)
    fpq_sample_import_server("sample_import", state)
    fpq_mzmine_import_server("mzmine_import", state)
    fpq_design_server("design", state)
    fpq_analysis_settings_server("analysis_settings", state)
    fpq_dataset_server("dataset", state)
    fpq_profile_server("profile", state)
    fpq_validation_server("validation", state)
    fpq_workflow_server("workflow", state)
    fpq_v4_workflow_companion_server("workflow", state)
    fpq_runs_server("runs", state)
    fpq_v4_runs_companion_server("runs", state)
    fpq_results_server("results", state)
    fpq_artifacts_server("artifacts", state)
    fpq_logs_server("logs", state)
    fpq_provenance_server("provenance", state)
    fpq_about_server("about", state)

    output$global_project <- shiny::renderUI({
      project <- state$project()
      if (is.null(project)) fpq_badge("neutral", "No project open") else
        fpq_badge("PASS", project$manifest$project_name)
    })
    output$global_validation <- shiny::renderUI({
      validation <- state$validations()
      if (!length(validation)) fpq_badge("neutral", "Not validated") else
        fpq_badge(if (validation$readiness == "blocked") "FAIL" else "PASS",
          switch(validation$readiness, ready = "Ready",
            ready_with_warning = "Ready with warning",
            candidate_workflow_ready = "Ready", "Not ready"))
    })
    output$global_run <- shiny::renderUI({
      workflow <- if (is.null(state$project())) NULL else
        tryCatch(fpq_workflow_stage_summary(state$project()),
                 error = function(e) NULL)
      active <- !is.null(workflow) && workflow$status %in%
        c("prepared", "starting", "running", "cancelling")
      fpq_badge(if (active) "WARNING" else "neutral",
        if (active) "Run active" else "No active run")
    })
  }
  shiny::shinyApp(ui, server)
}
