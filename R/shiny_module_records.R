fpq_artifacts_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Artifacts"),
    shiny::actionButton(ns("refresh"), "Refresh"),
    shiny::actionButton(ns("verify"), "Verify Selected Hash"),
    DT::DTOutput(ns("table")), shiny::uiOutput(ns("metadata")),
    shiny::uiOutput(ns("column_legend")),
    htmltools::tags$h3("Validated tabular preview"),
    DT::DTOutput(ns("preview")))
}
fpq_artifacts_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    tick <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$refresh, tick(tick()+1L))
    artifacts <- shiny::reactive({ tick(); fpq_list_artifacts(state$project()) })
    output$table <- DT::renderDT({
      x <- artifacts(); if (!length(x)) return(data.frame())
      rows <- do.call(rbind, lapply(x, function(a) data.frame(
        artifact_id = a$artifact_id, display_name = a$display_name,
        type = a$artifact_type, path = a$path, size = a$file_size,
        sha256 = a$sha256, validation = a$validation_status,
        status = a$active_status, run = a$producing_run_id %||% "",
        profile = if(is.list(a$scientific_profile))
          a$scientific_profile$profile_id %||% "" else a$scientific_profile %||% "",
        core = a$scientific_core_version)))
      DT::datatable(rows, rownames = FALSE, selection = "single",
                    options = list(scrollX = TRUE, pageLength = 10))
    })
    selected <- shiny::reactive({
      i <- input$table_rows_selected; x <- artifacts()
      if (!length(i) || i > length(x)) return(NULL)
      x[[i]]
    })
    shiny::observeEvent(input$verify, {
      x <- selected(); if (is.null(x)) return()
      fpq_verify_artifact(state$project(), x$artifact_id); tick(tick()+1L)
    })
    output$metadata <- shiny::renderUI({
      x <- selected(); if (is.null(x)) return(NULL)
      htmltools::tags$details(htmltools::tags$summary("View metadata"),
        htmltools::tags$p(htmltools::tags$strong("Exact path: "), x$path),
        htmltools::tags$p(htmltools::tags$strong("Path type: "), x$path_type),
        htmltools::tags$p(htmltools::tags$strong("Parent artifacts: "),
                          paste(unlist(x$parent_artifact_ids), collapse = ", ")),
        htmltools::tags$p(htmltools::tags$strong("Dimensions: "),
          paste(x$row_count %||% "not recorded", "rows x",
                x$column_count %||% "not recorded", "columns")),
        htmltools::tags$p(htmltools::tags$strong("Validation: "), x$validation_status),
        htmltools::tags$p(htmltools::tags$strong("Active/stale status: "), x$active_status),
        if (identical(x$active_status, "stale"))
          htmltools::tags$div(class = "fpq-block-reason",
            htmltools::tags$p(htmltools::tags$strong("Stale reason: "),
                              x$notes %||% "An upstream dependency is no longer eligible."),
            htmltools::tags$p(htmltools::tags$strong("Affected parent: "),
                              paste(unlist(x$parent_artifact_ids), collapse = ", ")),
            htmltools::tags$p(htmltools::tags$strong("Affected downstream stage: "),
                              "Any stage requiring this artifact remains blocked."),
            htmltools::tags$p(htmltools::tags$strong("Recommended remediation: "),
                              "Restore or re-register the exact parent, revalidate it, then create a new run. Do not reactivate this historical artifact."),
            htmltools::tags$p(htmltools::tags$strong("Historical availability: "),
                              "Preserved and inspectable; not eligible as an active downstream input.")),
        htmltools::tags$p(htmltools::tags$strong("Producing run: "),
                          x$producing_run_id %||% "external input"),
        htmltools::tags$p(htmltools::tags$strong("Scientific profile: "),
          if(is.list(x$scientific_profile))
            x$scientific_profile$profile_id %||% "" else x$scientific_profile %||% ""),
        htmltools::tags$p(htmltools::tags$strong("Scientific-core version: "),
                          x$scientific_core_version),
        htmltools::tags$p("Use the operating system to open the containing folder or copy this path. Scientific artifacts are read-only in the application."))
    })
    output$column_legend <- shiny::renderUI({
      x <- selected()
      if (is.null(x) || !identical(x$artifact_type,"annotated_three_readout_master"))
        return(NULL)
      headers <- fpq_preview_csv(x$path,1L)$columns
      annotation <- headers[grepl("^preferred_annotation:|^annotation_|^has_annotation$|^is_",headers)]
      quantitative <- setdiff(headers,annotation)
      htmltools::tags$section(class="fpq-review",
        htmltools::tags$h3("Accessible column-category legend"),
        htmltools::tags$p(htmltools::tags$strong("Annotation metadata columns: "),
          paste(annotation,collapse=", ")),
        htmltools::tags$p(htmltools::tags$strong("Quantitative and quantitative-status columns: "),
          paste(quantitative,collapse=", ")),
        htmltools::tags$p("Categories are stated textually and do not rely on color. The stored artifact is not modified."))
    })
    output$preview <- DT::renderDT({
      x <- selected()
      if (is.null(x) || !grepl("csv$", x$path, ignore.case = TRUE)) return(data.frame())
      preview <- fpq_preview_csv(x$path, 25L)
      DT::datatable(preview$data, rownames = FALSE,
                    caption = paste("Preview only - authoritative hash:", x$sha256),
                    options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

fpq_logs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Logs"),
    shiny::selectInput(ns("stream"), "Log stream",
      c("Structured run events" = "run_events",
        "Application diagnostics" = "application_diagnostic",
        "Audit records" = "audit",
        "Scientific stdout" = "stdout", "Scientific stderr" = "stderr")),
    shiny::selectInput(ns("run_filter"), "Run filter", choices = "All"),
    shiny::selectInput(ns("stage_filter"), "Stage filter", choices = "All"),
    shiny::selectInput(ns("severity"), "Severity",
      c("All", "DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL")),
    DT::DTOutput(ns("events")), shiny::verbatimTextOutput(ns("scientific")))
}
fpq_logs_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    all_events <- shiny::reactive({
      if (is.null(state$project())) return(list())
      fpq_read_jsonl(fpq_project_paths(state$project()$root)$logs)
    })
    shiny::observe({
      events <- all_events()
      runs <- unique(Filter(nzchar, vapply(events, function(x) x$run_id %||% "", "")))
      stages <- unique(Filter(nzchar, vapply(events, function(x) x$stage_id %||% "", "")))
      shiny::updateSelectInput(session, "run_filter",
        choices = c("All", sort(runs)), selected = shiny::isolate(input$run_filter) %||% "All")
      shiny::updateSelectInput(session, "stage_filter",
        choices = c("All", sort(stages)), selected = shiny::isolate(input$stage_filter) %||% "All")
    })
    output$events <- DT::renderDT({
      if (input$stream %in% c("stdout", "stderr") || is.null(state$project()))
        return(data.frame())
      events <- all_events()
      run_event <- function(x) grepl(
        "^(run\\.|worker\\.|recovery\\.|cancellation\\.|cancel\\.)",
        x$event_code %||% ""
      )
      events <- switch(input$stream,
        run_events = Filter(run_event, events),
        application_diagnostic = Filter(function(x)
          identical(x$stream %||% "", "application_diagnostic") && !run_event(x), events),
        audit = Filter(function(x)
          identical(x$stream %||% "", "audit") && !run_event(x), events),
        list())
      if (!identical(input$run_filter, "All"))
        events <- Filter(function(x) identical(x$run_id %||% "", input$run_filter), events)
      if (!identical(input$stage_filter, "All"))
        events <- Filter(function(x) identical(x$stage_id %||% "", input$stage_filter), events)
      if (input$severity != "All")
        events <- Filter(function(x) identical(x$level, input$severity), events)
      if (!length(events)) return(data.frame())
      rows <- do.call(rbind, lapply(events, function(x) data.frame(
        timestamp = x$timestamp, level = x$level, run_id = x$run_id %||% "",
        stage = x$stage_id %||% "", event_code = x$event_code, message = x$message)))
      DT::datatable(rows, rownames = FALSE, options = list(pageLength = 15))
    })
    output$scientific <- shiny::renderText({
      if (!input$stream %in% c("stdout", "stderr") || is.null(state$project())) return("")
      runs <- rev(fpq_list_runs(state$project()))
      if (!identical(input$run_filter, "All"))
        runs <- Filter(function(x) identical(x$run_id, input$run_filter), runs)
      if (!length(runs)) return("No persisted scientific streams.")
      refs <- runs[[1]]$logs
      path <- refs[[input$stream]] %||% NULL
      if (is.null(path) || !file.exists(path)) return("Selected stream is unavailable.")
      paste(utils::tail(readLines(path, warn = FALSE), 200), collapse = "\n")
    })
  })
}

fpq_provenance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Provenance"),
    shiny::selectInput(ns("artifact"), "Output artifact", choices = NULL),
    shiny::uiOutput(ns("lineage")), DT::DTOutput(ns("edges")))
}
fpq_provenance_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      artifacts <- fpq_list_artifacts(state$project())
      choices <- setNames(vapply(artifacts, `[[`, "", "artifact_id"),
                          vapply(artifacts, `[[`, "", "display_name"))
      active_master <- Filter(
        function(x) identical(x$artifact_type, "annotated_three_readout_master") &&
          identical(x$active_status, "active"),
        artifacts
      )
      selected <- if (length(active_master)) {
        active_master[[1]]$artifact_id
      } else {
        shiny::isolate(input$artifact) %||% if (length(choices)) unname(choices[[1]]) else character()
      }
      shiny::updateSelectInput(session, "artifact", choices = choices, selected = selected)
    })
    edges <- shiny::reactive({
      if (is.null(state$project()) || !nzchar(input$artifact %||% "")) return(list())
      fpq_trace_provenance(state$project(), input$artifact)
    })
    output$lineage <- shiny::renderUI({
      if (!length(edges())) return(fpq_empty_state("No lineage selected",
        "Select an output artifact to trace it to registered sources."))
      artifact <- tryCatch(fpq_inspect_artifact(state$project(),input$artifact),
                           error=function(e) NULL)
      parents <- if(is.null(artifact)) list() else lapply(
        artifact$parent_artifact_ids %||% list(),
        function(id) tryCatch(fpq_inspect_artifact(state$project(),id),
                              error=function(e) list(artifact_id=id,artifact_type="unavailable")))
      htmltools::tags$div(class = "fpq-lineage",
        htmltools::tags$p("Registered imzML/IBD inputs -> Script 01 run -> pixel-intensity output -> Script 03 run -> replicate-summary quantitative artifact."),
        if(identical(artifact$artifact_type,"annotated_three_readout_master"))
          htmltools::tags$div(
            htmltools::tags$p(htmltools::tags$strong("Separate Script 04 parents:")),
            htmltools::tags$ul(lapply(parents,function(x) htmltools::tags$li(
              paste(x$artifact_type, "--", x$artifact_id)))),
            htmltools::tags$p("The replicate summary supplies quantitative values. The annotated MZmine artifact supplies putative metadata only.")),
        htmltools::tags$p("Parents -> Script 04 annotation-integration run -> annotated quantitative master -> validation records."))
    })
    output$edges <- DT::renderDT({
      x <- edges(); if (!length(x)) return(data.frame())
      rows <- do.call(rbind, lapply(x, function(e) data.frame(
        source = e$subject_id, relationship = e$relationship, target = e$object_id,
        run_id = e$run_id %||% "", evidence_hash = e$evidence_hash %||% "",
        notes = e$notes %||% "")))
      DT::datatable(rows, rownames = FALSE, options = list(scrollX = TRUE))
    })
  })
}
