fpq_run_readiness <- function(project, request, config, demonstration = FALSE,
                              repository_root = fpq_repo_root()) {
  reasons <- character()
  if (demonstration) reasons <- c(reasons, "Synthetic demonstration projects cannot execute.")
  if (is.null(project)) reasons <- c(reasons, "No project is open.")
  if (!is.null(project) && !identical(request$project_id, project$manifest$project_id))
    reasons <- c(reasons, "Request project ID does not match the open project.")
  stage <- fpq_workflow_stage_status(request$stage_id %||% "",
    request$ion_mode %||% "", request$exposure_label %||% "")
  if (!stage$executable) reasons <- c(reasons, stage$reason)
  structural <- fpq_validate_structural(config)
  if (structural$classification != "structurally_valid")
    reasons <- c(reasons, structural$records[[1]]$message)
  compatibility <- fpq_validate_profile_compatibility(config,
    request$scientific_profile$profile_id %||% "")
  if (!compatibility$classification %in%
      c("profile_compatible", "profile_candidate_compatible"))
    reasons <- c(reasons, compatibility$records[[1]]$message)
  integrity <- fpq_verify_scientific_core(repository_root)
  if (integrity$status != "compatible") reasons <- c(reasons, "Frozen scientific-core integrity failed.")
  if (identical(request$stage_id, "summarize_replicate_readouts") &&
      !is.null(project)) {
    input_check <- tryCatch({
      fpq_validate_script03_input(project, request)
      NULL
    }, fpq_error = function(e) e$message)
    if (!is.null(input_check)) reasons <- c(reasons, input_check)
  }
  if (identical(request$stage_id,"integrate_mzmine_annotations") &&
      !is.null(project)) {
    checks <- tryCatch({
      fpq_validate_script04_parent(project,
        request$registered_inputs$replicate_summary_artifact,
        "replicate_readout_summary",request)
      fpq_validate_script04_parent(project,
        request$registered_inputs$annotation_artifact,
        "annotated_feature_list",request)
      NULL
    },error=function(e)conditionMessage(e))
    if (!is.null(checks)) reasons <- c(reasons,checks)
  }
  candidate <- identical(compatibility$classification, "profile_candidate_compatible") ||
    identical(stage$scientific_status %||% "", "technical_validation_candidate")
  list(ready = !length(reasons), ready_with_warning = !length(reasons) && candidate,
       candidate_acknowledgement_required = !length(reasons) && candidate,
       reasons = unique(reasons), structural = structural,
       compatibility = compatibility, stage = stage)
}

fpq_record_candidate_acknowledgement <- function(project, request) {
  record <- list(schema_version = "1.0.0",
    acknowledgement_id = fpq_id("ack"), project_id = project$manifest$project_id,
    run_id = request$run_id, request_id = request$request_id,
    scientific_status = "technical_validation_candidate",
    independent_review = "pending", acknowledged = TRUE,
    acknowledged_at = fpq_now(),
    statement = paste("This workflow has passed technical validation but is",
      "awaiting independent scientific review. Results will be labelled as a",
      "Technical Validation Candidate."))
  path <- file.path(fpq_project_paths(project$root)$reports,
                    paste0(record$run_id, "-candidate-acknowledgement.json"))
  fpq_atomic_write_json(record, path)
  edge <- list(schema_version = "1.0.0", provenance_edge_id = fpq_id("prov"),
    project_id = project$manifest$project_id, subject_type = "run",
    subject_id = request$run_id, relationship = "acknowledges_scientific_status",
    object_type = "scientific_status", object_id = "technical_validation_candidate",
    recorded_at = record$acknowledged_at, run_id = request$run_id,
    evidence_hash = fpq_hash_file(path),
    notes = "Independent scientific review pending; execution explicitly acknowledged.")
  fpq_store_provenance(project, list(edge))
  record$path <- path
  record
}

fpq_runs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Runs"),
    shiny::uiOutput(ns("advanced_request_controls")),
    shiny::uiOutput(ns("review")),
    shiny::uiOutput(ns("start_control")),
    htmltools::tags$p(class = "fpq-help",
      "A supported stage can start only after every blocking check passes."),
    shiny::uiOutput(ns("cancel_control")),
    shiny::uiOutput(ns("monitor")),
    htmltools::tags$h3("Full workflow history"),
    DT::DTOutput(ns("workflow_table")),
    htmltools::tags$h3("Persisted runs"),
    DT::DTOutput(ns("runs_table")),
    shiny::uiOutput(ns("run_details")))
}

fpq_list_runs <- function(project) {
  if (is.null(project)) return(list())
  files <- list.files(fpq_project_paths(project$root)$runs, pattern = "run.json$",
                      recursive = TRUE, full.names = TRUE)
  lapply(files, fpq_read_json)
}

fpq_runs_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    reviewed <- shiny::reactiveVal(NULL)
    start_enabled <- shiny::reactiveVal(FALSE)
    worker_info <- shiny::reactiveVal(NULL)
    output$advanced_request_controls <- shiny::renderUI({
      if (!isTRUE(state$advanced_mode()))
        return(htmltools::tags$p(class = "fpq-help",
          "Full workflows are started from the Workflow page. Advanced Mode exposes immutable stage-request inspection."))
      shiny::tagList(
        shiny::textInput(session$ns("request_path"),
          "Prepared execution-request JSON", updateOn = "blur"),
        shiny::actionButton(session$ns("inspect"), "Review Prepared Run"))
    })
    shiny::observeEvent(input$inspect, {
      tryCatch({
        request <- fpq_read_json(input$request_path)
        readiness <- fpq_run_readiness(state$project(), request,
          state$configuration(), state$demonstration(), state$repository_root)
        if (identical(request$stage_id, "integrate_mzmine_annotations")) {
          mapping_state <- state$annotation_mapping_validation()
          if (!identical(mapping_state$status, "PASS")) {
            readiness$ready <- FALSE
            readiness$reasons <- unique(c(
              readiness$reasons,
              mapping_state$message %||%
                "Inspect and correct the annotated-table mappings before starting Script 04."
            ))
          }
        }
        reviewed(list(request = request, readiness = readiness,
          request_hash = fpq_hash_file(input$request_path)))
        start_enabled(readiness$ready)
      }, error = function(e) {
        state$last_error(e); reviewed(NULL); start_enabled(FALSE)
      })
    })
    output$review <- shiny::renderUI({
      if (!isTRUE(state$advanced_mode())) return(NULL)
      x <- reviewed()
      if (is.null(x)) return(fpq_empty_state("No run prepared",
        "Select an immutable execution-request snapshot and review it."))
      r <- x$request
      htmltools::tags$div(class = "fpq-review",
        fpq_badge(if (x$readiness$ready_with_warning) "WARNING" else
            if (x$readiness$ready) "PASS" else "FAIL",
          if (x$readiness$ready_with_warning) "Ready with warning" else
            if (x$readiness$ready) "Ready" else "Blocked"),
        htmltools::tags$dl(
          htmltools::tags$dt("Project"), htmltools::tags$dd(r$project_id),
          htmltools::tags$dt("Dataset"), htmltools::tags$dd(r$dataset_id),
          htmltools::tags$dt("Stage"), htmltools::tags$dd(r$stage_id),
          htmltools::tags$dt("Ion mode / exposure"), htmltools::tags$dd(paste(r$ion_mode, r$exposure_label)),
          htmltools::tags$dt("Profile"), htmltools::tags$dd(r$scientific_profile$profile_id),
          htmltools::tags$dt("Scientific core"), htmltools::tags$dd(r$scientific_core_version),
          htmltools::tags$dt("Script hash"), htmltools::tags$dd(r$frozen_script$sha256),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dt("Script 03 quantitative parent"),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dd(paste(r$registered_inputs$replicate_summary_artifact$artifact_id,
              "run",r$registered_inputs$replicate_summary_artifact$producing_run_id,
              "hash",r$registered_inputs$replicate_summary_artifact$sha256)),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dt("Annotated MZmine metadata parent"),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dd(paste(r$registered_inputs$annotation_artifact$artifact_id,
              "hash",r$registered_inputs$annotation_artifact$sha256)),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dt("Exact annotation mappings"),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dd(paste(names(r$annotation_column_mappings),
               unlist(r$annotation_column_mappings),sep=" -> ",collapse="; ")),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dt("Expected output contract"),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dd(paste(length(r$expected_output_artifact_types),
              "declared output artifact roles; frozen contract requires 12 files")),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dt("Scientific gates"),
          if(identical(r$stage_id,"integrate_mzmine_annotations"))
            htmltools::tags$dd("Exact quantitative preservation; annotation equivalence; row retention; 129-column schema"),
          htmltools::tags$dt("Configuration hash"), htmltools::tags$dd(x$request_hash),
          htmltools::tags$dt("Selected samples"), htmltools::tags$dd(
            paste(vapply(r$samples, `[[`, "", "sample_id"), collapse = ", "))),
        if (length(x$readiness$reasons))
          htmltools::tags$ul(class = "fpq-block-reason",
            lapply(x$readiness$reasons, htmltools::tags$li)))
    })
    output$start_control <- shiny::renderUI({
      if (!isTRUE(state$advanced_mode())) return(NULL)
      label <- if (!is.null(reviewed()) &&
                   identical(reviewed()$request$stage_id,
                             "summarize_replicate_readouts"))
        "Start Replicate Summary" else if(!is.null(reviewed()) &&
          identical(reviewed()$request$stage_id,"integrate_mzmine_annotations"))
        "Start Annotation Integration" else "Start Approved Extraction"
      shiny::actionButton(session$ns("start"), label,
        class = "btn-primary", disabled = if (start_enabled()) NULL else "disabled",
        `aria-disabled` = if (start_enabled()) "false" else "true")
    })
    start_reviewed_run <- function() {
      x <- reviewed()
      if (is.null(x) || !isTRUE(x$readiness$ready))
        return(shiny::showNotification("Run remains blocked by validation.", type = "error"))
      if (!is.null(state$active_worker()) && state$active_worker()$is_alive())
        return(shiny::showNotification("A run is already active.", type = "warning"))
      stdout <- file.path(dirname(input$request_path), paste0(x$request$run_id, ".worker.out"))
      stderr <- file.path(dirname(input$request_path), paste0(x$request$run_id, ".worker.err"))
      worker_id <- fpq_id("worker")
      process <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
        c(file.path(state$repository_root, "tools", "fpq_dispatch_worker.R"),
          state$project()$root, normalizePath(input$request_path),
          state$repository_root, worker_id, x$request$run_id),
        wd = state$repository_root,
        stdout = stdout, stderr = stderr, cleanup = FALSE, supervise = FALSE)
      state$active_worker(process)
      worker_info(list(run_id = x$request$run_id, pid = process$get_pid(),
                       worker_id = worker_id, started_at = fpq_now(),
                       stdout = stdout, stderr = stderr))
    }
    shiny::observeEvent(input$start, {
      x <- reviewed()
      if (!is.null(x) && isTRUE(x$readiness$candidate_acknowledgement_required) &&
          is.null(state$candidate_acknowledgement())) {
        return(shiny::showModal(shiny::modalDialog(
          "This workflow has passed technical validation but is awaiting independent scientific review. Results will be labelled as a Technical Validation Candidate. Continue?",
          footer = shiny::tagList(shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("continue_candidate"), "Continue",
                                class = "btn-primary")))))
      }
      start_reviewed_run()
    })
    shiny::observeEvent(input$continue_candidate, {
      shiny::removeModal()
      x <- reviewed()
      if (is.null(x)) return()
      tryCatch({
        acknowledgement <- fpq_record_candidate_acknowledgement(
          state$project(), x$request)
        state$candidate_acknowledgement(acknowledgement)
        start_reviewed_run()
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification("Candidate acknowledgement could not be recorded.",
                                type = "error", duration = NULL)
      })
    })
    output$cancel_control <- shiny::renderUI({
      shiny::invalidateLater(1000, session)
      info <- worker_info()
      if (is.null(info) || is.null(state$project())) return(NULL)
      run <- tryCatch(fpq_load_run(state$project(), info$run_id),
                      error = function(e) NULL)
      if (is.null(run) || !run$status %in% c("starting", "running", "cancelling"))
        return(NULL)
      if (run$status == "cancelling")
        return(htmltools::tags$p(class = "fpq-help",
          "Cancellation requested. Waiting for confirmed process termination."))
      shiny::actionButton(session$ns("cancel"), "Cancel Run",
        class = "btn-warning",
        `aria-label` = paste("Cancel run", info$run_id))
    })
    shiny::observeEvent(input$cancel, {
      info <- worker_info()
      if (is.null(info)) return()
      shiny::showModal(shiny::modalDialog(
        title = paste("Cancel", info$run_id, "?"),
        htmltools::tags$p(
          "The exact registered worker will be asked to terminate. Partial outputs remain inactive."),
        easyClose = TRUE,
        footer = shiny::tagList(shiny::modalButton("Keep running"),
          shiny::actionButton(session$ns("confirm_cancel"),
                              "Confirm cancellation", class = "btn-danger"))))
    })
    shiny::observeEvent(input$confirm_cancel, {
      shiny::removeModal()
      info <- worker_info()
      if (is.null(info)) return()
      tryCatch({
        fpq_request_cancellation(state$project(), info$run_id)
        shiny::showNotification(
          "Cancellation requested; termination is not reported until confirmed.",
          type = "warning")
      }, error = function(e) state$last_error(e))
    })
    output$monitor <- shiny::renderUI({
      shiny::invalidateLater(1500, session)
      info <- worker_info(); process <- state$active_worker()
      if (is.null(info)) {
        recoveries <- if (is.null(state$project())) list() else
          tryCatch(fpq_recover_interrupted_runs(state$project()),
                   error = function(e) list())
        if (!length(recoveries)) return(fpq_empty_state("No active run",
          "Completed and failed runs remain available from project storage."))
        recovered_identity <- recoveries[[1]]$inspection$identity
        if (!is.list(recovered_identity)) recovered_identity <- list()
        info <- list(run_id = recoveries[[1]]$run_id,
          pid = recovered_identity$process_id %||% NA_integer_,
          started_at = recovered_identity$launch_timestamp %||% "",
          stdout = "", stderr = "")
      }
      alive <- !is.null(process) && process$is_alive()
      run <- tryCatch(fpq_load_run(state$project(), info$run_id), error = function(e) NULL)
      status <- if (!is.null(run)) run$status else if (alive) "starting" else "failed"
      htmltools::tags$div(class = "fpq-card",
        htmltools::tags$h3(paste("Run", info$run_id)),
        fpq_badge(if (status == "completed") "PASS" else
          if (status %in% c("failed", "validation_failed")) "FAIL" else "WARNING", status),
        htmltools::tags$p(paste("Worker PID:", info$pid)),
        htmltools::tags$p(paste("Started:", info$started_at)),
        htmltools::tags$p(paste("Subprocess:", if (alive) "active" else "exited")),
        htmltools::tags$p("Progress is reported only as genuine persisted backend states."),
        if (file.exists(info$stdout)) htmltools::tags$p("Worker stdout is available."),
        if (file.exists(info$stderr)) htmltools::tags$p("Worker stderr is available."))
    })
    output$runs_table <- DT::renderDT({
      shiny::invalidateLater(2000, session)
      runs <- fpq_list_runs(state$project())
      if (!length(runs)) return(data.frame())
      rows <- do.call(rbind, lapply(runs, function(r) data.frame(
        run_id = r$run_id, status = r$status, stage = r$stage_id,
        ion_mode = r$ion_mode, exposure = r$exposure,
        profile = r$scientific_profile$profile_id %||% "",
        created = r$created_at, updated = r$updated_at)))
      DT::datatable(rows, rownames = FALSE, selection = "single")
    })
    output$workflow_table <- DT::renderDT({
      shiny::invalidateLater(1500, session)
      records <- fpq_list_workflow_records(state$project())
      if (!length(records)) return(data.frame())
      rows <- do.call(rbind, lapply(records, function(x) data.frame(
        workflow_id = x$workflow_id, status = x$status,
        exposure = x$exposure,
        modes = paste(unlist(x$modes), collapse = ", "),
        scientific_status = x$scientific_status,
        current_stage = x$current_stage,
        created = x$created_at, updated = x$updated_at)))
      DT::datatable(rows, rownames = FALSE, selection = "single")
    })
    output$run_details <- shiny::renderUI({
      selection <- input$runs_table_rows_selected; runs <- fpq_list_runs(state$project())
      if (!length(selection) || selection > length(runs)) return(NULL)
      r <- runs[[selection]]
      htmltools::tags$details(open = TRUE, htmltools::tags$summary("Run details"),
        htmltools::tags$pre(jsonlite::toJSON(r, pretty = TRUE, auto_unbox = TRUE,
                                             null = "null")))
    })
  })
}
