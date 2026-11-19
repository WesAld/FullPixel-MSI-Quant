fpq_profile_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Scientific validation details"),
    htmltools::tags$p(
      "The application selects and verifies scientific compatibility automatically from the saved project design and validation evidence."),
    shiny::uiOutput(ns("details")))
}
fpq_profile_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$details <- shiny::renderUI({
      profile_id <- if (is.null(state$project()))
        "generic-two-group-v1" else
          state$project()$manifest$scientific_profile_id
      resolved <- fpq_resolve_profile(profile_id, state$repository_root)
      p <- resolved$profile
      locked <- if (!is.null(p$locked_scientific_settings)) {
        unlist(p$locked_scientific_settings, recursive = TRUE, use.names = TRUE)
      } else if (is.list(p$override_policy)) {
        p$override_policy$locked %||% p$override_policy$locked_fields %||% character()
      } else p$override_policy
      locked_labels <- if (length(locked)) {
        paste0(gsub("\\.", " ", names(locked)), ": ", as.character(locked))
      } else "No profile-specific locked settings were declared."
      htmltools::tags$div(class = "fpq-card-grid",
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3(p$profile_id),
          htmltools::tags$p(paste("Version", p$profile_version)),
          htmltools::tags$p(paste("Scientific core", p$scientific_core_version)),
          htmltools::tags$p(paste("Resolution:", resolved$relationship)),
          if (!identical(resolved$requested_id, resolved$resolved_id))
            htmltools::tags$p(paste(resolved$requested_id, "->", resolved$resolved_id))),
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3("Locked scientific settings"),
          htmltools::tags$ul(lapply(locked_labels, htmltools::tags$li)),
          htmltools::tags$p("Locked settings are inspectable but not editable.")),
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3("Scientific status"),
          htmltools::tags$p("Generic workflows are executable as Technical Validation Candidates after structural, numerical, and provenance checks pass."),
          htmltools::tags$p("Candidate results remain labelled while independent scientific review is pending."),
          htmltools::tags$p("Compare two groups requires two equal replicate groups. Quantify samples only accepts one or more independent samples and produces no group statistics.")))
    })
  })
}

fpq_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Validation Center"),
    shiny::checkboxGroupInput(ns("statuses"), "Show statuses",
      c("PASS", "WARNING", "FAIL", "NOT_EVALUABLE"),
      selected = c("PASS", "WARNING", "FAIL", "NOT_EVALUABLE"), inline = TRUE),
    shiny::actionButton(ns("validate"), "Validate Project", class = "btn-primary"),
    shiny::actionButton(ns("files"), "Revalidate Files"),
    shiny::actionButton(ns("environment"), "Check Environment"),
    shiny::uiOutput(ns("layers")),
    DT::DTOutput(ns("records")))
}
fpq_validation_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    run_validation <- function() {
      tryCatch(shiny::withProgress(message = "Validating project", value = 0, {
        config <- state$configuration()
        structural <- fpq_validate_structural(config)
        shiny::incProgress(0.2)
        registration <- fpq_dataset_registration_status(config)
        profile_id <- if (is.null(state$project())) "generic-two-group-v1" else
          state$project()$manifest$scientific_profile_id
        compatibility <- fpq_validate_profile_compatibility(config, profile_id)
        shiny::incProgress(0.3)
        environment <- fpq_check_environment(repository_root = fpq_repo_root())
        workflow_environment_status <- fpq_environment_workflow_status(environment)
        registration_record <- fpq_validation_record("dataset.registration",
          if (registration$blocking) "FAIL" else "PASS",
          if (registration$blocking) paste(registration$missing, collapse = " ") else
            paste(registration$summary$msi_datasets, "MSI datasets;",
              registration$summary$mzmine_feature_lists, "MZmine feature lists;",
              registration$summary$biological_samples, "biological samples;",
              registration$summary$comparisons, "comparison registered."),
          if (registration$blocking) "CRITICAL" else "INFO",
          registration$blocking)
        records <- c(structural$records, list(registration_record),
          compatibility$records, list(fpq_validation_record("environment.overall",
            if (workflow_environment_status == "incompatible") "FAIL" else
              if (workflow_environment_status %in% c("unresolved", "compatible_with_warning")) "WARNING" else "PASS",
            paste("Required environment status:", workflow_environment_status,
              "(technical status:", environment$status, ")"),
            if (workflow_environment_status == "incompatible") "CRITICAL" else
              if (workflow_environment_status == "compatible") "INFO" else "WARNING",
            workflow_environment_status == "incompatible", workflow_environment_status,
            "compatible")), fpq_environment_detail_records(environment))
        blocking <- registration$blocking ||
          structural$classification != "structurally_valid" ||
          compatibility$classification %in% c("profile_incompatible",
            "profile_unsupported", "profile_blocked") ||
          workflow_environment_status == "incompatible"
        result <- list(structural = structural, registration = registration,
          profile = compatibility, environment = environment, records = records,
          readiness = if (blocking) "blocked" else
            if (workflow_environment_status == "compatible_with_warning")
              "ready_with_warning" else "ready")
        state$validations(result)
        if (!is.null(state$project()))
          fpq_store_validation(state$project(), records, replace = TRUE)
        shiny::incProgress(0.5)
        invisible(result)
      }), error = function(e) {
        state$last_error(e)
        shiny::showNotification(
          "Validation could not be completed because an application resource could not be found. The project remains open.",
          type = "error", duration = NULL)
        invisible(NULL)
      })
    }
    shiny::observeEvent(input$validate, run_validation())
    shiny::observeEvent(input$environment, run_validation())
    shiny::observeEvent(input$files, run_validation())
    output$layers <- shiny::renderUI({
      x <- state$validations()
      if (!length(x)) return(fpq_empty_state("Not yet validated",
        "Structural validity and profile compatibility will be displayed separately."))
      htmltools::tags$div(class = "fpq-card-grid",
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3("1. Project structure"),
          fpq_badge(if (x$structural$classification == "structurally_valid") "PASS" else "FAIL",
                    if (x$structural$classification == "structurally_valid") "Passed" else "Action required")),
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3("2. Data registration"),
          {
            fpq_badge(if (!x$registration$blocking) "PASS" else "FAIL",
                      if (!x$registration$blocking) "Passed" else "Action required")
          },
          if (x$registration$blocking) htmltools::tagList(
            htmltools::tags$p(paste(x$registration$missing, collapse = " ")),
            shiny::actionButton(session$ns("go_msi"), "Go to MSI Samples"),
            shiny::actionButton(session$ns("go_mzmine"), "Go to MZmine Feature List"))),
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3("3. Analysis compatibility"),
          fpq_badge(if (x$profile$classification %in%
              c("profile_compatible", "profile_candidate_compatible")) "PASS" else "FAIL",
                    if (x$profile$classification %in%
                      c("profile_compatible", "profile_candidate_compatible"))
                      "Passed" else "Action required"),
          if (x$profile$classification %in% c("profile_incompatible", "profile_unsupported", "profile_blocked"))
            shiny::actionButton(session$ns("go_configuration"),
              "Review Dataset Configuration")),
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3("4. Software environment"),
          {
            environment_status <- fpq_environment_workflow_status(x$environment)
            fpq_badge(if (environment_status == "incompatible") "FAIL" else
                if (environment_status == "compatible") "PASS" else "WARNING",
                      if (environment_status == "incompatible") "Action required" else
                        if (environment_status == "compatible") "Passed" else
                          "Passed with warning")
          }),
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3("5. Ready to run"),
          fpq_badge(if (x$readiness == "blocked") "FAIL" else
              if (x$readiness == "ready_with_warning") "WARNING" else "PASS",
            switch(x$readiness, ready_with_warning = "Ready with warning",
              ready = "Ready", "Blocked"))))
    })
    shiny::observeEvent(input$go_msi,
      fpq_v4_navigate(session$rootScope(), "msi_samples"))
    shiny::observeEvent(input$go_mzmine,
      fpq_v4_navigate(session$rootScope(), "mzmine"))
    shiny::observeEvent(input$go_configuration,
      fpq_v4_navigate(session$rootScope(), "dataset_configuration"))
    output$records <- DT::renderDT({
      x <- state$validations(); if (!length(x)) return(data.frame())
      records <- Filter(function(r) r$status %in% input$statuses,
                        x$records)
      if (!length(records)) return(data.frame())
      fields <- c("status", "severity", "check_name", "observed_value", "expected_value",
                  "message", "scientific_consequence", "remediation", "blocking_status")
      rows <- lapply(records, function(r) as.data.frame(lapply(r[fields], function(v)
        paste(unlist(v), collapse = ", ")), stringsAsFactors = FALSE))
      DT::datatable(do.call(rbind, rows), rownames = FALSE, options = list(scrollX = TRUE))
    })
  })
}

fpq_workflow_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(htmltools::tags$h2("Workflow"),
    htmltools::tags$p("Run the complete validated workflow from persisted project inputs."),
    shiny::uiOutput(ns("primary_action")),
    shiny::uiOutput(ns("current_run")),
    shiny::uiOutput(ns("progress")),
    shiny::uiOutput(ns("stages")))
}
fpq_workflow_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    latest_workflow <- shiny::reactive({
      shiny::invalidateLater(1000, session)
      if (is.null(state$project())) return(NULL)
      workflows <- fpq_list_workflow_records(state$project())
      if (!length(workflows)) return(NULL)
      selected_id <- state$workflow_id()
      if (!is.null(selected_id) && nzchar(selected_id)) {
        selected <- Filter(function(x) identical(x$workflow_id, selected_id), workflows)
        if (length(selected)) return(selected[[1]])
      }
      workflows[[1]]
    })
    result_export <- fpq_v4_result_export_controller(input, output, session, state)
    validation_ready <- shiny::reactive({
      value <- state$validations()
      length(value) && value$readiness %in%
        c("ready", "ready_with_warning", "candidate_workflow_ready")
    })
    run_active <- shiny::reactive({
      workflow <- latest_workflow()
      process <- state$workflow_process()
      process_alive <- !is.null(process) &&
        tryCatch(process$is_alive(), error = function(e) FALSE)
      process_alive || (!is.null(workflow) && workflow$status %in%
        c("prepared", "starting", "running", "cancelling"))
    })
    output$primary_action <- shiny::renderUI({
      if (is.null(state$project()))
        return(fpq_empty_state("No project open",
          "Open and validate a project before starting analysis."))
      disabled <- !validation_ready() || run_active()
      reason <- if (!validation_ready())
        "Validate Project before starting." else if (run_active())
          "A full workflow is already active." else NULL
      htmltools::tags$section(class = "fpq-review",
        shiny::actionButton(session$ns("run_full"),
          "Run Full-Pixel Quantification", class = "btn-primary",
          disabled = if (disabled) "disabled" else NULL,
          `aria-disabled` = if (disabled) "true" else "false"),
        if (!is.null(reason)) htmltools::tags$p(class = "fpq-help", reason),
        if (!isTRUE(state$advanced_mode()))
          htmltools::tags$p(class = "fpq-help",
            "Execution requests and stage transitions are created automatically."))
    })
    prepare_and_start <- function() {
      tryCatch({
        prepared <- fpq_prepare_full_workflow(state$project(),
          state$configuration(), state$validations(), state$repository_root,
          hash_raw_inputs = FALSE)
        acknowledgement <- fpq_record_candidate_acknowledgement(
          state$project(), list(run_id = prepared$record$workflow_id,
            request_id = paste0("request-", prepared$record$workflow_id)))
        acknowledgement$workflow_id <- prepared$record$workflow_id
        prepared$record <- fpq_update_workflow_record(state$project(),
          prepared$record$workflow_id,
          list(candidate_acknowledgement = acknowledgement))
        state$candidate_acknowledgement(acknowledgement)
        state$workflow_id(prepared$record$workflow_id)
        shiny::showNotification("Run prepared successfully",
                                type = "message", duration = 5)
        stdout <- file.path(prepared$record$output_folder, "workflow-worker.out")
        stderr <- file.path(prepared$record$output_folder, "workflow-worker.err")
        process <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
          c(file.path(state$repository_root, "tools",
                      "fpq_full_workflow_worker.R"),
            state$project()$root, prepared$record$workflow_id,
            state$repository_root),
          wd = fpq_safe_subprocess_cwd(), stdout = stdout, stderr = stderr,
          cleanup = FALSE, supervise = FALSE)
        state$workflow_process(process)
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste(
          "Run preparation failed:", conditionMessage(e),
          "The project remains open."), type = "error", duration = NULL)
      })
    }
    shiny::observeEvent(input$run_full, {
      if (!validation_ready() || run_active()) return()
      scientific <- state$validations()$profile$scientific_status
      if (isTRUE(state$advanced_mode()) &&
          identical(scientific$overall, "technical_validation_candidate")) {
        shiny::showModal(shiny::modalDialog(
          if (identical(state$analysis_type(), "quantify_only"))
            "This per-sample workflow passed technical validation. It will export individual normalized-intensity and detection readouts without group statistics, and results will be labelled as a Technical Validation Candidate. Continue?"
          else
            "This generic two-group workflow passed technical validation but does not have a dataset-specific historical oracle. Results will be labelled as a Technical Validation Candidate. Continue?",
          easyClose = FALSE,
          footer = shiny::tagList(shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("continue_full"), "Continue",
                                class = "btn-primary"))))
      } else prepare_and_start()
    })
    shiny::observeEvent(input$continue_full, {
      shiny::removeModal()
      prepare_and_start()
    })
    shiny::observeEvent(input$cancel_full, {
      workflow <- latest_workflow()
      if (!is.null(workflow))
        tryCatch({
          fpq_request_workflow_cancellation(state$project(),
                                            workflow$workflow_id)
          shiny::showNotification("Cancellation requested", type = "warning")
        }, error = function(e) {
          state$last_error(e)
          shiny::showNotification("Cancellation could not be requested.",
                                  type = "error")
        })
    })
    shiny::observeEvent(input$retry_full, {
      workflow <- latest_workflow()
      if (is.null(workflow) || run_active()) return()
      resumable_stages <- c("summarizing", "annotating", "combined_export",
                            "final_workbook", "validating_workbook")
      resume_available <- identical(workflow$status, "failed") &&
        workflow$failure$stage %in% resumable_stages
      tryCatch({
        if (resume_available) {
          stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
          state$workflow_id(workflow$workflow_id)
          process <- processx::process$new(
            file.path(R.home("bin"), "Rscript.exe"),
            c(file.path(state$repository_root, "tools",
                        "fpq_full_workflow_worker.R"),
              state$project()$root, workflow$workflow_id,
              state$repository_root),
            wd = fpq_safe_subprocess_cwd(),
            stdout = file.path(workflow$output_folder,
                               paste0("workflow-resume-", stamp, ".out")),
            stderr = file.path(workflow$output_folder,
                               paste0("workflow-resume-", stamp, ".err")),
            cleanup = FALSE, supervise = FALSE)
          state$workflow_process(process)
          shiny::showNotification(
            "Resume started. Validated completed stages will be reused.",
            type = "message", duration = 6)
          return()
        }

        prepared <- fpq_retry_workflow(state$project(), workflow$workflow_id,
          state$configuration(), state$validations(), state$repository_root,
          hash_raw_inputs = FALSE)
        acknowledgement <- fpq_record_candidate_acknowledgement(
          state$project(), list(run_id = prepared$record$workflow_id,
            request_id = paste0("request-", prepared$record$workflow_id)))
        prepared$record <- fpq_update_workflow_record(state$project(),
          prepared$record$workflow_id,
          list(candidate_acknowledgement = acknowledgement))
        state$workflow_id(prepared$record$workflow_id)
        process <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
          c(file.path(state$repository_root, "tools",
                      "fpq_full_workflow_worker.R"),
            state$project()$root, prepared$record$workflow_id,
            state$repository_root),
          wd = fpq_safe_subprocess_cwd(),
          stdout = file.path(prepared$record$output_folder,
                             "workflow-worker.out"),
          stderr = file.path(prepared$record$output_folder,
                             "workflow-worker.err"),
          cleanup = FALSE, supervise = FALSE)
        state$workflow_process(process)
        shiny::showNotification("Retry prepared successfully", type = "message")
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste("Retry failed:", conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    })
    completion_notified <- shiny::reactiveVal(NULL)
    shiny::observe({
      workflow <- latest_workflow()
      if (is.null(workflow) ||
          identical(completion_notified(), workflow$workflow_id)) return()
      if (identical(workflow$status, "completed")) {
        completion_notified(workflow$workflow_id)
        shiny::showNotification("Final results workbook created",
                                type = "message", duration = 8)
      } else if (identical(workflow$status, "failed")) {
        completion_notified(workflow$workflow_id)
        shiny::showNotification(paste(
          "Workflow failed during", workflow$failure$stage %||% "an unknown stage",
          "-", workflow$failure$message %||% "Review the run details.",
          "The project remains open."), type = "error", duration = NULL)
      }
    })
    output$current_run <- shiny::renderUI({
      workflow <- latest_workflow()
      if (is.null(workflow)) return(NULL)
      htmltools::tags$section(class = "fpq-card",
        htmltools::tags$h3("Current run"),
        htmltools::tags$dl(
          htmltools::tags$dt("Run ID"),
          htmltools::tags$dd(workflow$workflow_id),
          htmltools::tags$dt("Started"),
          htmltools::tags$dd(workflow$started_at %||% "Not started"),
          htmltools::tags$dt("Current stage"),
          htmltools::tags$dd(fpq_v4_workflow_stage_name(
            workflow$current_stage, workflow$current_stage)),
          htmltools::tags$dt("Output folder"),
          htmltools::tags$dd(workflow$output_folder)),
        if (workflow$status %in% c("prepared", "starting", "running"))
          shiny::actionButton(session$ns("cancel_full"), "Cancel Run",
                              class = "btn-warning"),
        if (workflow$status %in% c("failed", "cancelled") &&
            validation_ready()) {
          resumable_stages <- c("summarizing", "annotating",
                                "combined_export", "final_workbook",
                                "validating_workbook")
          resume_available <- identical(workflow$status, "failed") &&
            workflow$failure$stage %in% resumable_stages
          shiny::actionButton(session$ns("retry_full"),
            if (resume_available) "Resume Failed Run" else "Retry Failed Run",
            class = "btn-primary")
        },
        if (identical(workflow$status, "completed") &&
            isTRUE(state$advanced_mode()))
          shiny::actionButton(session$ns("open_results"), "Open Results",
                              class = "btn-primary"),
        if (isTRUE(fpq_result_export_eligibility(
              state$project(), workflow$workflow_id)$eligible))
          shiny::actionButton(session$ns("freeze_result"),
            "Freeze / Export Result", class = "btn-primary"),
        shiny::actionButton(session$ns("view_details"), "View Run Details"))
    })
    shiny::observeEvent(input$freeze_result, {
      workflow <- latest_workflow()
      if (!is.null(workflow)) result_export$open(workflow$workflow_id)
    })
    shiny::observeEvent(input$open_results, {
      fpq_v4_navigate(session$rootScope(), "results_internal")
    })
    output$progress <- shiny::renderUI({
      workflow <- latest_workflow()
      if (is.null(workflow)) return(NULL)
      htmltools::tags$section(class = "fpq-card",
        htmltools::tags$h3("Workflow progress"),
        htmltools::tags$ol(lapply(workflow$stages, function(stage)
          htmltools::tags$li(
            fpq_badge(if (stage$status == "completed") "PASS" else
              if (stage$status == "failed") "FAIL" else
                if (stage$status %in% c("running", "cancelled")) "WARNING" else
                  "neutral", tools::toTitleCase(stage$status)),
            htmltools::tags$span(fpq_v4_workflow_stage_name(
              stage$stage_id %||% stage$id, stage$display_name)),
            if (!is.null(stage$message))
              htmltools::tags$span(class = "fpq-help", stage$message)))))
    })
    output$stages <- shiny::renderUI({
      config <- state$configuration()
      stages <- list(
        list(id = "extract_pixel_intensities", name = "Extract Pixel Intensities",
          purpose = "Extract target signals across complete full-pixel images."),
        list(id = "summarize_replicate_readouts", name = "Summarize Replicate Readouts",
          purpose = "Create official biological-replicate readouts."),
        list(id = "integrate_mzmine_annotations", name = "Integrate MZmine Annotations",
          purpose = "Add putative annotation metadata after quantification."),
        list(id = "create_combined_publication_export", name = "Create Combined Publication Export",
          purpose = "Create validated combined publication artifacts."))
      htmltools::tags$div(class = "fpq-card-grid", lapply(stages, function(stage) {
        status <- fpq_workflow_stage_status(stage$id, config$ion_mode %||% "NEG",
                                            config$exposure_label %||% "Single")
        executable <- status$executable
        display_status <- status$status
        display_reason <- status$reason
        if (!isTRUE(state$advanced_mode()) &&
            identical(display_status, "candidate_workflow_ready")) {
          display_status <- "ready"
          display_reason <- "Ready"
        }
        if (stage$id == "summarize_replicate_readouts" &&
            !is.null(state$project()) && executable) {
          artifacts <- fpq_list_artifacts(state$project())
          upstream <- Filter(function(x)
            identical(x$artifact_type, "pixel_intensity_table") &&
              identical(x$active_status, "active") &&
              identical(x$validation_status, "PASS"), artifacts)
          completed <- Filter(function(x)
            identical(x$artifact_type, "replicate_readout_summary") &&
              identical(x$active_status, "active") &&
              identical(x$validation_status, "PASS"), artifacts)
          if (length(completed)) {
            display_status <- "completed"
            display_reason <- "Validated replicate-summary artifact is active."
          } else if (!length(upstream)) {
            executable <- FALSE
            display_status <- "waiting_for_extraction"
            display_reason <- paste(
              "A validated active Extract Pixel Intensities artifact",
              "is required.")
          } else {
            display_status <- "ready"
            display_reason <- "Validated extraction input is available."
          }
        }
        if (stage$id == "integrate_mzmine_annotations" &&
            !is.null(state$project()) && executable) {
          artifacts <- fpq_list_artifacts(state$project())
          quantitative <- Filter(function(x) x$artifact_type=="replicate_readout_summary" &&
            x$active_status=="active" && x$validation_status=="PASS",artifacts)
          annotation <- Filter(function(x) x$artifact_type=="annotated_feature_list" &&
            x$active_status=="active" && x$validation_status=="PASS",artifacts)
          completed <- Filter(function(x) x$artifact_type=="annotated_three_readout_master" &&
            x$active_status=="active" && x$validation_status=="PASS",artifacts)
          if(length(completed)) {
            display_status <- "completed"
            display_reason <- "Validated annotated master is active; quantitative and annotation parents remain distinct."
          } else if(!length(quantitative)) {
            executable<-FALSE;display_status<-"waiting_for_replicate_summary"
            display_reason<-"An active validated Script 03 quantitative parent is required."
          } else if(!length(annotation)) {
            executable<-FALSE;display_status<-"annotation_input_missing"
            display_reason<-"A registered active annotated MZmine metadata artifact is required."
          } else {
            display_status<-"ready"
            display_reason<-"Both exact parent artifacts are active; review mappings and execution request."
          }
        }
        if (stage$id == "create_combined_publication_export" &&
            !is.null(state$project())) {
          artifacts <- fpq_list_artifacts(state$project())
          masters <- Filter(function(x)
            identical(x$artifact_type, "annotated_three_readout_master") &&
              identical(x$active_status, "active") &&
              identical(x$validation_status, "PASS"), artifacts)
          final <- Filter(function(x)
            identical(x$artifact_type, "final_results_workbook") &&
              identical(x$active_status, "active") &&
              identical(x$validation_status, "PASS"), artifacts)
          modes <- unique(vapply(masters, function(x) x$ion_mode %||% "",
                                 character(1)))
          required_modes <- fpq_active_project_modes(
            config, state$project()$manifest)
          generic_workflow <- identical(
            state$project()$manifest$scientific_profile_id,
            "generic-two-group-v1") ||
            !identical(config$exposure_label %||% "", "Single") ||
            !identical(required_modes, "NEG")
          masters_ready <- length(required_modes) > 0L &&
            all(required_modes %in% modes)
          if (length(final)) {
            display_status <- "completed"
            display_reason <- "Validated final workbook is available."
          } else if (!masters_ready) {
            executable <- FALSE
            display_status <- "waiting_for_annotation_integration"
            display_reason <- paste(
              "Validated annotated masters are required for:",
              paste(required_modes, collapse = ", "))
          } else {
            display_status <- "ready"
            display_reason <- if (generic_workflow)
              "All configured ion-mode masters are available."
            else "Both validated annotated masters are available."
          }
        }
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3(stage$name), htmltools::tags$p(stage$purpose),
          fpq_badge(if (display_status == "completed") "PASS" else
            if (executable) "READY" else
              if (display_status == "awaiting_validation") "WARNING" else
                "BLOCKED", display_status),
          if (!is.null(display_reason)) htmltools::tags$p(
            class = if (executable) "fpq-help" else "fpq-block-reason",
            display_reason))
      }))
    })
  })
}
