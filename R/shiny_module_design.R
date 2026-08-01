fpq_design_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$h2("Conditions, Treatments, Replicates and Comparisons"),
    shiny::uiOutput(ns("demo_warning")),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("condition"), "Selected condition", choices = NULL)),
      shiny::column(8,
        shiny::actionButton(ns("add_condition"), "Add Condition"),
        shiny::actionButton(ns("add_treatment"), "Add Treatment"),
        shiny::actionButton(ns("duplicate_condition"), "Duplicate Condition Configuration",
                            `aria-label` = "Duplicate selected condition configuration"),
        shiny::actionButton(ns("remove_condition"), "Remove Condition",
                            `aria-label` = "Remove selected condition"),
        shiny::actionButton(ns("move_up"), "Move Up",
                            `aria-label` = "Move selected condition up"),
        shiny::actionButton(ns("move_down"), "Move Down",
                            `aria-label` = "Move selected condition down"))),
    shiny::uiOutput(ns("condition_editor")),
    htmltools::tags$hr(),
    htmltools::tags$h3("Biological replicates"),
    shiny::actionButton(ns("add_replicate"), "Add Replicate"),
    shiny::actionButton(ns("duplicate_replicate"), "Duplicate Replicate Row",
                        `aria-label` = "Duplicate selected replicate row"),
    shiny::actionButton(ns("remove_replicate"), "Remove Replicate",
                        `aria-label` = "Remove selected replicate row"),
    shiny::fileInput(ns("import_metadata"), "Import Sample Metadata",
                     accept = c(".csv", "text/csv")),
    shiny::downloadButton(ns("export_template"), "Export Sample Metadata Template"),
    DT::DTOutput(ns("samples")),
    shiny::uiOutput(ns("sample_editor")),
    htmltools::tags$hr(),
    htmltools::tags$h3("Comparisons"),
    shiny::textInput(ns("comparison_id"), "Comparison ID"),
    shiny::textInput(ns("comparison_label"), "Comparison label"),
    shiny::selectInput(ns("comparison_reference"), "Reference condition", choices = NULL),
    shiny::selectInput(ns("comparison_target"), "Comparison condition", choices = NULL),
    shiny::actionButton(ns("add_comparison"), "Add Comparison"),
    shiny::actionButton(ns("remove_comparison"), "Remove Comparison",
                        `aria-label` = "Remove selected comparison"),
    shiny::actionButton(ns("reorder_comparison"), "Reorder Comparison",
                        `aria-label` = "Reorder selected comparison"),
    DT::DTOutput(ns("comparisons")),
    shiny::actionButton(ns("save"), "Save Configuration", class = "btn-primary"),
    shiny::actionButton(ns("load_demo"), "Load Synthetic Demonstration"))
}

fpq_design_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh_choices <- function() {
      config <- state$configuration()
      choices <- setNames(vapply(config$conditions %||% list(),
        function(x) x$condition_id, character(1)), vapply(config$conditions %||% list(),
          function(x) x$condition_label %||% x$condition_id, character(1)))
      shiny::updateSelectInput(session, "condition", choices = choices,
        selected = if (length(choices)) choices[[1]] else character())
      shiny::updateSelectInput(session, "comparison_reference", choices = choices)
      shiny::updateSelectInput(session, "comparison_target", choices = choices,
        selected = if (length(choices) > 1L) choices[[2]] else
          if (length(choices)) choices[[1]] else character())
    }
    shiny::observe(refresh_choices())
    add_condition <- function(treatment = FALSE) {
      config <- state$configuration(); n <- length(config$conditions) + 1L
      id <- paste0(if (treatment) "treatment-" else "condition-", n)
      config <- fpq_add_condition(config, list(condition_id = id,
        condition_label = if (treatment) paste("Treatment", n) else paste("Condition", n),
        analytical_role = if (!length(config$conditions)) "reference" else "comparison",
        is_reference = !length(config$conditions), display_order = n,
        treatment_label = if (treatment) paste("Treatment", n) else NULL,
        dose = NULL, notes = NULL))
      state$configuration(config); state$dirty(TRUE); refresh_choices()
    }
    shiny::observeEvent(input$add_condition, add_condition(FALSE))
    shiny::observeEvent(input$add_treatment, add_condition(TRUE))
    shiny::observeEvent(input$duplicate_condition, {
      config <- state$configuration(); selected <- input$condition
      source <- Filter(function(x) identical(x$condition_id, selected), config$conditions)
      shiny::req(length(source) == 1L)
      copy <- source[[1]]; copy$condition_id <- paste0(selected, "-copy-", length(config$conditions)+1L)
      copy$condition_label <- paste(copy$condition_label, "copy"); copy$is_reference <- FALSE
      copy$analytical_role <- "comparison"; copy$display_order <- length(config$conditions)+1L
      config <- fpq_add_condition(config, copy)
      state$configuration(config); state$dirty(TRUE); refresh_choices()
    })
    shiny::observeEvent(input$remove_condition, {
      tryCatch({
        config <- fpq_remove_condition(state$configuration(), input$condition, cascade = FALSE)
        state$configuration(config); state$dirty(TRUE); refresh_choices()
      }, fpq_error = function(e) shiny::showModal(shiny::modalDialog(
        title = "Condition is referenced", e$message,
        "Remove linked samples and comparisons explicitly before removing this condition.",
        easyClose = TRUE, footer = shiny::modalButton("Close"))))
    })
    move <- function(delta) {
      config <- state$configuration()
      ids <- vapply(config$conditions, `[[`, "", "condition_id")
      i <- match(input$condition, ids); j <- i + delta
      if (is.na(i) || j < 1L || j > length(ids)) return()
      ids[c(i,j)] <- ids[c(j,i)]
      state$configuration(fpq_reorder_conditions(config, ids)); state$dirty(TRUE)
    }
    shiny::observeEvent(input$move_up, move(-1L))
    shiny::observeEvent(input$move_down, move(1L))
    output$condition_editor <- shiny::renderUI({
      config <- state$configuration(); selected <- input$condition
      condition <- Filter(function(x) identical(x$condition_id, selected), config$conditions)
      if (!length(condition)) return(fpq_empty_state("No conditions configured",
        "Use Add Condition or Add Treatment. No application-level maximum is imposed."))
      x <- condition[[1]]
      shiny::wellPanel(
        shiny::textInput(session$ns("edit_id"), "Condition ID", x$condition_id),
        shiny::textInput(session$ns("edit_label"), "Condition display name", x$condition_label),
        shiny::selectInput(session$ns("edit_role"), "Analytical role",
          c("Reference" = "reference", "Comparison" = "comparison"), x$analytical_role),
        shiny::checkboxInput(session$ns("edit_reference"), "Reference condition", x$is_reference),
        shiny::textInput(session$ns("edit_treatment"), "Treatment name",
                         x$treatment_label %||% ""),
        shiny::textInput(session$ns("edit_dose"), "Optional dose value and unit",
                         x$dose %||% ""),
        shiny::textAreaInput(session$ns("edit_notes"), "Notes", x$notes %||% ""),
        shiny::actionButton(session$ns("apply_condition"), "Apply Condition Changes"))
    })
    shiny::observeEvent(input$apply_condition, {
      config <- state$configuration()
      updates <- list(condition_id = input$edit_id, condition_label = input$edit_label,
        analytical_role = input$edit_role, is_reference = input$edit_reference,
        treatment_label = if (nzchar(input$edit_treatment)) input$edit_treatment else NULL,
        dose = if (nzchar(input$edit_dose)) input$edit_dose else NULL,
        notes = if (nzchar(input$edit_notes)) input$edit_notes else NULL)
      tryCatch({
        config <- fpq_update_condition(config, input$condition, updates)
        state$configuration(config); state$dirty(TRUE); refresh_choices()
      }, fpq_error = function(e) state$last_error(e))
    })
    shiny::observeEvent(input$add_replicate, {
      shiny::req(nzchar(input$condition))
      config <- state$configuration()
      n <- sum(vapply(config$samples %||% list(), function(x)
        identical(x$condition_id, input$condition), logical(1))) + 1L
      config <- fpq_add_sample(config, list(sample_id = paste0(input$condition, "-", n),
        sample_label = paste(input$condition, n), condition_id = input$condition,
        replicate_id = paste0("R", n), inclusion_status = "included",
        imzml_path = "", ibd_path = "", notes = ""))
      state$configuration(config); state$dirty(TRUE)
      shiny::showNotification("Replicate added", type = "message")
    })
    shiny::observeEvent(input$duplicate_replicate, {
      config <- state$configuration()
      rows <- Filter(function(x) identical(x$condition_id, input$condition), config$samples)
      if (!length(rows)) return()
      selected <- input$samples_rows_selected %||% length(rows)
      selected <- min(selected, length(rows))
      copy <- rows[[selected]]; copy$sample_id <- paste0(copy$sample_id, "-copy")
      copy$replicate_id <- paste0(copy$replicate_id, "-copy")
      state$configuration(fpq_add_sample(config, copy)); state$dirty(TRUE)
    })
    shiny::observeEvent(input$remove_replicate, {
      config <- state$configuration()
      rows <- Filter(function(x) identical(x$condition_id, input$condition), config$samples)
      if (!length(rows)) return()
      selected <- input$samples_rows_selected %||% length(rows)
      selected <- min(selected, length(rows))
      state$configuration(fpq_remove_sample(config, rows[[selected]]$sample_id))
      state$dirty(TRUE)
    })
    output$samples <- DT::renderDT({
      rows <- state$configuration()$samples %||% list()
      rows <- Filter(function(x) identical(x$condition_id, input$condition), rows)
      if (!length(rows)) return(data.frame())
      fields <- c("sample_id", "sample_label", "condition_id", "replicate_id",
                  "inclusion_status", "imzml_path", "ibd_path", "notes")
      data <- do.call(rbind, lapply(rows, function(x) as.data.frame(
        setNames(lapply(fields, function(field) {
          value <- x[[field]]
          if (is.null(value) || !length(value)) "" else as.character(value[[1]])
        }), fields), stringsAsFactors = FALSE)))
      DT::datatable(data, rownames = FALSE,
                    selection = "single", options = list(pageLength = 8, scrollX = TRUE))
    })
    output$sample_editor <- shiny::renderUI({
      config <- state$configuration()
      rows <- Filter(function(x) identical(x$condition_id, input$condition),
                     config$samples %||% list())
      if (!length(rows)) return(NULL)
      selected <- input$samples_rows_selected %||% 1L
      selected <- min(selected, length(rows)); x <- rows[[selected]]
      shiny::wellPanel(
        shiny::textInput(session$ns("sample_id"), "Sample ID", x$sample_id),
        shiny::textInput(session$ns("sample_label"), "Sample display label",
                         x$sample_label %||% x$sample_id),
        shiny::textInput(session$ns("replicate_id"), "Replicate ID", x$replicate_id),
        shiny::selectInput(session$ns("inclusion_status"), "Inclusion status",
          c("Included" = "included", "Excluded" = "excluded"), x$inclusion_status),
        shiny::textInput(session$ns("imzml_path"), "imzML assignment", x$imzml_path %||% ""),
        shiny::textInput(session$ns("ibd_path"), "IBD assignment", x$ibd_path %||% ""),
        shiny::textAreaInput(session$ns("sample_notes"), "Sample notes", x$notes %||% ""),
        shiny::actionButton(session$ns("apply_sample"), "Apply Sample Changes"))
    })
    shiny::observeEvent(input$apply_sample, {
      config <- state$configuration()
      rows <- Filter(function(x) identical(x$condition_id, input$condition), config$samples)
      selected <- input$samples_rows_selected %||% 1L
      selected <- min(selected, length(rows)); old_id <- rows[[selected]]$sample_id
      tryCatch({
        state$configuration(fpq_update_sample(config, old_id, list(
          sample_id = input$sample_id, sample_label = input$sample_label,
          replicate_id = input$replicate_id, inclusion_status = input$inclusion_status,
          imzml_path = input$imzml_path, ibd_path = input$ibd_path,
          notes = input$sample_notes)))
        state$dirty(TRUE)
      }, fpq_error = function(e) state$last_error(e))
    })
    shiny::observeEvent(input$import_metadata, {
      tryCatch({
        incoming <- utils::read.csv(input$import_metadata$datapath,
                                    stringsAsFactors = FALSE, check.names = FALSE)
        required <- c("sample_id", "sample_label", "condition_id", "replicate_id",
                      "inclusion_status", "imzml_path", "ibd_path", "notes")
        if (length(setdiff(required, names(incoming))))
          fpq_stop("ui.sample_import_columns",
            paste("Sample metadata requires:", paste(required, collapse = ", ")))
        config <- state$configuration()
        for (i in seq_len(nrow(incoming))) {
          row <- as.list(incoming[i, required, drop = FALSE])
          existing <- vapply(config$samples, function(x) identical(x$sample_id,
            as.character(row$sample_id)), logical(1))
          if (any(existing)) config <- fpq_update_sample(config, as.character(row$sample_id), row)
          else config <- fpq_add_sample(config, row)
        }
        state$configuration(config); state$dirty(TRUE)
      }, error = function(e) state$last_error(e))
    })
    output$export_template <- shiny::downloadHandler(
      filename = function() "FullPixel_MSI_Quant_sample_metadata_template.csv",
      content = function(file) {
        rows <- state$configuration()$samples %||% list()
        fields <- c("sample_id", "sample_label", "condition_id", "replicate_id",
                    "inclusion_status", "imzml_path", "ibd_path", "notes")
        if (!length(rows)) data <- as.data.frame(setNames(replicate(length(fields),
          character(), simplify = FALSE), fields))
        else data <- do.call(rbind, lapply(rows, function(x) as.data.frame(
          lapply(fields, function(f) x[[f]] %||% ""), stringsAsFactors = FALSE)))
        names(data) <- fields
        utils::write.csv(data, file, row.names = FALSE, na = "")
      })
    shiny::observeEvent(input$add_comparison, {
      config <- state$configuration(); conditions <- config$conditions
      if (length(conditions) < 2L) return()
      reference_id <- input$comparison_reference
      comparison_id <- input$comparison_target
      if (!nzchar(reference_id) || !nzchar(comparison_id) ||
          identical(reference_id, comparison_id))
        return(shiny::showNotification("Select two distinct conditions.", type = "error"))
      n <- length(config$comparisons %||% list()) + 1L
      id <- if (nzchar(input$comparison_id)) input$comparison_id else paste0("cmp-", n)
      label <- if (nzchar(input$comparison_label)) input$comparison_label else
        paste(comparison_id, "versus", reference_id)
      state$configuration(fpq_add_comparison(config, list(comparison_id = id,
        comparison_label = label, reference_condition_id = reference_id,
        comparison_condition_id = comparison_id, enabled = TRUE, display_order = n)))
      state$dirty(TRUE)
    })
    shiny::observeEvent(input$remove_comparison, {
      config <- state$configuration()
      if (!length(config$comparisons)) return()
      state$configuration(fpq_remove_comparison(config,
        config$comparisons[[length(config$comparisons)]]$comparison_id)); state$dirty(TRUE)
    })
    shiny::observeEvent(input$reorder_comparison, {
      config <- state$configuration()
      if (length(config$comparisons) > 1L) {
        ids <- rev(vapply(config$comparisons, `[[`, "", "comparison_id"))
        state$configuration(fpq_reorder_comparisons(config, ids)); state$dirty(TRUE)
      }
    })
    output$comparisons <- DT::renderDT({
      rows <- state$configuration()$comparisons %||% list()
      if (!length(rows)) return(data.frame())
      DT::datatable(do.call(rbind, lapply(rows, as.data.frame)), rownames = FALSE)
    })
    shiny::observeEvent(input$save, {
      project <- state$project()
      if (is.null(project)) return(shiny::showNotification("Open a project first.", type = "error"))
      config <- state$configuration()
      config$schema_version <- config$schema_version %||% "1.0.0"
      config$dataset_id <- config$dataset_id %||% "dset-project-configuration"
      tryCatch({
        fpq_save_configuration(project, "datasets", config$dataset_id, config,
                               fpq_validate_dynamic_configuration)
        state$dirty(FALSE); shiny::showNotification("Configuration saved.", type = "message")
      }, error = function(e) state$last_error(e))
    })
    shiny::observeEvent(input$load_demo, {
      state$configuration(fpq_make_demo_configuration()); state$dirty(TRUE)
      state$demonstration(TRUE); refresh_choices()
    })
    output$demo_warning <- shiny::renderUI(if (state$demonstration())
      htmltools::tags$div(class = "fpq-warning-panel", role = "status",
        htmltools::tags$strong("Synthetic demonstration only"),
        htmltools::tags$p("Not scientifically validated data. Scientific execution is disabled.")))
  })
}
