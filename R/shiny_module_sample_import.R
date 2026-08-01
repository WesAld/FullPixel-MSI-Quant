fpq_sample_import_ui <- function(id) {
  ns <- shiny::NS(id)

  mode_card <- function(mode, heading, folder_label, folder_title,
                        files_label) {
    htmltools::tags$section(
      class = "fpq-card",
      htmltools::tags$h3(heading),
      shinyFiles::shinyDirButton(
        ns(paste0("folder_", mode)),
        folder_label,
        folder_title
      ),
      shiny::conditionalPanel(
        condition = "output.show_advanced_mode",
        ns = ns,
        shiny::actionButton(
          ns(paste0("files_", mode)),
          files_label
        )
      )
    )
  }

  shiny::tagList(
    htmltools::tags$h2("MSI Samples"),
    htmltools::tags$p(
      paste(
        "Select local imzML/IBD data. Files are registered in place",
        "and are not uploaded or copied."
      )
    ),
    htmltools::tags$div(
      class = "fpq-card-grid",
      shiny::conditionalPanel(
        condition = "output.show_pos_mode",
        ns = ns,
        mode_card(
          "POS",
          "Positive mode",
          "Select Positive MSI Folder",
          "Select Positive MSI folder",
          "Select Positive MSI Files"
        )
      ),
      shiny::conditionalPanel(
        condition = "output.show_neg_mode",
        ns = ns,
        mode_card(
          "NEG",
          "Negative mode",
          "Select Negative MSI Folder",
          "Select Negative MSI folder",
          "Select Negative MSI Files"
        )
      ),
      shiny::conditionalPanel(
        condition = "output.show_unassigned_mode",
        ns = ns,
        mode_card(
          "UNASSIGNED",
          "Ion mode to be confirmed",
          "Select Unassigned MSI Folder",
          "Select MSI folder for automatic ion-mode detection",
          "Select Unassigned MSI Files"
        )
      )
    ),
    shiny::uiOutput(ns("advanced_one_sample")),
    shiny::checkboxInput(ns("show_details"), "Show full path details", FALSE),
    DT::DTOutput(ns("review")),
    shiny::actionButton(ns("remove_selected"), "Remove Selected Sample"),
    shiny::actionButton(ns("clear_pos"), "Clear Positive Samples"),
    shiny::actionButton(ns("clear_neg"), "Clear Negative Samples"),
    shiny::actionButton(ns("clear_all"), "Clear All Samples"),
    shiny::uiOutput(ns("repair_identifiers")),
    shiny::actionButton(ns("confirm"), "Confirm Samples", class = "btn-primary"),
    shiny::uiOutput(ns("comparison_setup")),
    shiny::uiOutput(ns("summary"))
  )
}

fpq_sample_import_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    directory_roots <- shinyFiles::getVolumes()()
    roots_are_valid <- is.character(directory_roots) &&
      length(directory_roots) > 0L &&
      !is.null(names(directory_roots)) &&
      all(nzchar(names(directory_roots)))

    if (!roots_are_valid) {
      directory_roots <- c(
        Home = normalizePath(
          path.expand("~"),
          winslash = "/",
          mustWork = FALSE
        )
      )
    }

    active_modes <- shiny::reactive({
      project <- state$project()
      manifest <- if (is.null(project)) list() else project$manifest
      fpq_active_project_modes(
        config = state$configuration(),
        manifest = manifest,
        detected_rows = state$detected_samples(),
        fallback = state$selected_ion_mode()
      )
    })

    output$show_pos_mode <- shiny::reactive("POS" %in% active_modes())
    output$show_neg_mode <- shiny::reactive("NEG" %in% active_modes())
    output$show_unassigned_mode <- shiny::reactive(
      "UNASSIGNED" %in% active_modes()
    )
    output$show_advanced_mode <- shiny::reactive(
      isTRUE(state$advanced_mode())
    )

    for (output_id in c(
      "show_pos_mode",
      "show_neg_mode",
      "show_unassigned_mode",
      "show_advanced_mode"
    )) {
      shiny::outputOptions(
        output,
        output_id,
        suspendWhenHidden = FALSE
      )
    }

    one_imzml_path <- shiny::reactiveVal("")
    one_ibd_path <- shiny::reactiveVal("")
    output$advanced_one_sample <- shiny::renderUI({
      if (!isTRUE(state$advanced_mode())) return(NULL)
      htmltools::tags$details(
        htmltools::tags$summary("Add One Sample"),
        shiny::textInput(session$ns("one_name"), "Sample name"),
        shiny::textInput(session$ns("one_group"), "Optional sample group"),
        shiny::textInput(session$ns("one_replicate"), "Optional replicate"),
        shiny::selectInput(session$ns("one_mode"), "Ion mode",
                           c("Negative" = "NEG", "Positive" = "POS")),
        shiny::actionButton(session$ns("one_imzml"), "Browse for imzML"),
        shiny::actionButton(session$ns("one_ibd"), "Browse for IBD"),
        shiny::uiOutput(session$ns("one_paths")),
        shiny::actionButton(session$ns("add_one"), "Add Sample"))
    })
    choose_folder <- function(mode, selection) {
      # shinyDirButton also has Shiny's action-button class. A plain button
      # click can therefore briefly produce an integer click count before
      # shinyFiles replaces the input with the selected directory structure.
      # Ignore that click-count value and import only a completed selection.
      if (
        is.null(selection) ||
        !is.list(selection) ||
        is.null(selection$root) ||
        is.null(selection$path)
      ) {
        return(invisible(NULL))
      }

      tryCatch({
        path <- shinyFiles::parseDirPath(directory_roots, selection)
        if (!length(path) || !nzchar(path[[1]])) {
          return(invisible(NULL))
        }

        selected_path <- normalizePath(
          path[[1]],
          winslash = "/",
          mustWork = TRUE
        )
        result <- fpq_import_msi_folder(
          state$detected_samples(),
          selected_path,
          mode
        )
        state$detected_samples(result$rows)

        label <- switch(
          mode,
          POS = "Positive",
          NEG = "Negative",
          "Unassigned"
        )
        message <- if (result$added) {
          paste(result$added, label, "MSI sample pairs detected")
        } else if (result$duplicates) {
          "Sample folder already registered — duplicate pairs ignored"
        } else {
          "No valid imzML/IBD pairs were found in the selected folder"
        }

        shiny::showNotification(
          message,
          type = if (result$added) "message" else "warning"
        )
      }, error = function(e) {
        state$last_error(e)
        project <- state$project()
        if (!is.null(project)) {
          try(
            fpq_log_event(
              project,
              "msi.folder_selector.failed",
              details = list(
                mode = mode,
                message = conditionMessage(e)
              )
            ),
            silent = TRUE
          )
        }
        shiny::showNotification(
          "The selected folder could not be processed. The project remains unchanged.",
          type = "error",
          duration = NULL
        )
      })
    }
    choose_files <- function(mode) {
      paths <- utils::choose.files(caption = paste("Select", mode, "imzML and IBD files"),
        filters = matrix(c("MSI data", "*.imzML;*.imzml;*.IBD;*.ibd",
                           "All files", "*.*"), ncol = 2L, byrow = TRUE),
        multi = TRUE)
      if (!length(paths)) return()
      rows <- fpq_pair_msi_files(paths, mode, recursive = FALSE)
      result <- fpq_append_unique_msi_pairs(state$detected_samples(), rows)
      state$detected_samples(result$rows)
      message <- if (!result$added && result$duplicates)
        "Sample already added — duplicate ignored" else
        paste0(result$added, " new samples added; ", result$duplicates,
               " duplicates ignored")
      shiny::showNotification(message, type = if (result$added) "message" else "warning")
    }
    for (mode in c("POS", "NEG", "UNASSIGNED")) {
      local({
        current_mode <- mode
        shinyFiles::shinyDirChoose(input, paste0("folder_", current_mode),
          roots = directory_roots, session = session, allowDirCreate = FALSE)
        shiny::observeEvent(input[[paste0("folder_", current_mode)]],
          choose_folder(current_mode,
            input[[paste0("folder_", current_mode)]]), ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("files_", current_mode)]],
                            choose_files(current_mode))
      })
    }
    shiny::observeEvent(input$one_imzml, {
      path <- utils::choose.files(caption = "Select imzML file",
                                  filters = c("imzML", "*.imzML;*.imzml"), multi = FALSE)
      if (length(path)) one_imzml_path(normalizePath(path[[1]], mustWork = TRUE))
    })
    shiny::observeEvent(input$one_ibd, {
      path <- utils::choose.files(caption = "Select IBD file",
                                  filters = c("IBD", "*.IBD;*.ibd"), multi = FALSE)
      if (length(path)) one_ibd_path(normalizePath(path[[1]], mustWork = TRUE))
    })
    output$one_paths <- shiny::renderUI(htmltools::tags$ul(
      htmltools::tags$li(paste("imzML:", basename(one_imzml_path()))),
      htmltools::tags$li(paste("IBD:", basename(one_ibd_path())))))
    shiny::observeEvent(input$add_one, {
      if (!nzchar(input$one_name) || !nzchar(one_imzml_path()) ||
          !nzchar(one_ibd_path()))
        return(shiny::showNotification(
          "Enter a sample name and select both imzML and IBD files.", type = "error"))
      paired <- fpq_pair_msi_files(c(one_imzml_path(), one_ibd_path()),
                                   input$one_mode, recursive = FALSE)
      if (length(paired) != 1L || paired[[1]]$pair_status != "complete")
        return(shiny::showNotification(
          "The selected imzML and IBD files do not have matching basenames.",
          type = "error", duration = NULL))
      paired[[1]]$biological_sample <- input$one_name
      paired[[1]]$dataset_id <- fpq_mode_dataset_id(input$one_name, input$one_mode)
      paired[[1]]$dataset_name <- paired[[1]]$dataset_id
      paired[[1]]$sample_group <- input$one_group
      paired[[1]]$replicate <- input$one_replicate
      result <- fpq_append_unique_msi_pairs(state$detected_samples(), paired)
      state$detected_samples(result$rows)
      shiny::showNotification(if (result$added) "Sample added" else
        "Sample already added — duplicate ignored",
        type = if (result$added) "message" else "warning")
    })
    output$review <- DT::renderDT({
      data <- fpq_msi_review_data(state$detected_samples(), input$show_details)
      names(data)[match(c("include", "biological_sample", "dataset_id",
        "sample_group", "replicate", "ion_mode", "imzml_filename",
        "ibd_filename", "status"), names(data))] <- c("Include",
        "Biological sample", "Dataset ID", "Sample group", "Replicate",
        "Ion mode", "imzML filename", "IBD filename", "Status")
      DT::datatable(data, rownames = FALSE, editable = TRUE,
                    selection = list(mode = "multiple", target = "row"),
                    options = list(scrollX = TRUE, pageLength = 12))
    })
    shiny::observeEvent(input$review_cell_edit, {
      edit <- input$review_cell_edit
      rows <- state$detected_samples()
      displayed <- fpq_msi_review_data(rows, input$show_details)
      field <- names(displayed)[edit$col + 1L]
      displayed_names <- c("Include" = "include", "Biological sample" = "biological_sample",
        "Sample group" = "sample_group", "Replicate" = "replicate",
        "Ion mode" = "ion_mode")
      field <- if (field %in% names(displayed_names))
        unname(displayed_names[[field]]) else field
      editable <- unname(displayed_names)
      if (!field %in% editable) return()
      value <- DT::coerceValue(edit$value, displayed[edit$row, edit$col + 1L])
      rows[[edit$row]][[field]] <- value
      if (field %in% c("biological_sample", "ion_mode")) {
        rows[[edit$row]]$dataset_id <- fpq_mode_dataset_id(
          rows[[edit$row]]$biological_sample, rows[[edit$row]]$ion_mode)
        rows[[edit$row]]$dataset_name <- rows[[edit$row]]$dataset_id
      }
      state$detected_samples(rows)
    })
    persist_removed_rows <- function(remaining) {
      config <- state$configuration()
      confirmed <- length(config$samples %||% list()) > 0L
      if (confirmed) {
        config <- fpq_configuration_from_detected_rows(config, remaining)
        fpq_validate_dynamic_configuration(config)
        project <- state$project()
        if (!is.null(project)) {
          fpq_persist_msi_configuration(project, config)
          fpq_invalidate_project_validation(project)
        }
        state$configuration(config)
        state$validations(list())
        state$dirty(is.null(project))
      }
      state$detected_samples(remaining)
      invisible(TRUE)
    }
    remove_rows <- function(indices = integer(), mode = NULL, clear_all = FALSE,
                            notification) {
      result <- fpq_remove_msi_rows(state$detected_samples(), indices, mode, clear_all)
      if (!result$removed) return(shiny::showNotification(
        "No matching samples were removed.", type = "warning"))
      tryCatch({
        persist_removed_rows(result$rows)
        shiny::showNotification(notification(result$removed), type = "message")
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste("Samples were not removed:", conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    }
    shiny::observeEvent(input$remove_selected, {
      selected <- input$review_rows_selected %||% integer()
      if (!length(selected)) return(shiny::showNotification(
        "Select one or more sample rows first.", type = "warning"))
      remove_rows(indices = selected,
                  notification = function(n) if (n == 1L) "Sample removed" else
                    paste(n, "samples removed"))
    })
    confirm_clear <- function(mode, prompt, action_id) {
      shiny::showModal(shiny::modalDialog(prompt,
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns(action_id), "Clear", class = "btn-danger"))))
    }
    shiny::observeEvent(input$clear_pos,
      confirm_clear("POS", "Clear all Positive-mode samples?", "confirm_clear_pos"))
    shiny::observeEvent(input$clear_neg,
      confirm_clear("NEG", "Clear all Negative-mode samples?", "confirm_clear_neg"))
    shiny::observeEvent(input$clear_all,
      confirm_clear(NULL, "Clear all detected samples?", "confirm_clear_all"))
    shiny::observeEvent(input$confirm_clear_pos, {
      shiny::removeModal()
      remove_rows(mode = "POS",
        notification = function(n) paste(n, "Positive-mode samples cleared"))
    })
    shiny::observeEvent(input$confirm_clear_neg, {
      shiny::removeModal()
      remove_rows(mode = "NEG",
        notification = function(n) paste(n, "Negative-mode samples cleared"))
    })
    shiny::observeEvent(input$confirm_clear_all, {
      shiny::removeModal()
      remove_rows(clear_all = TRUE,
        notification = function(n) "All MSI samples cleared")
    })
    output$repair_identifiers <- shiny::renderUI({
      config <- state$configuration()
      modes <- unique(vapply(config$samples %||% list(),
        function(x) toupper(x$ion_mode %||% ""), character(1)))
      if (!all(c("POS", "NEG") %in% modes)) return(NULL)
      shiny::actionButton(session$ns("repair_combined"),
                          "Repair Combined-Mode Sample Identifiers")
    })
    shiny::observeEvent(input$repair_combined, {
      tryCatch({
        repaired <- fpq_repair_combined_sample_identifiers(state$configuration())
        if (!repaired$changed) return(shiny::showNotification(
          "Combined-mode identifiers already conform.", type = "message"))
        project <- state$project()
        if (!is.null(project)) {
          fpq_persist_msi_configuration(project, repaired$config)
          fpq_invalidate_project_validation(project)
        }
        state$configuration(repaired$config)
        state$detected_samples(fpq_detected_rows_from_configuration(repaired$config))
        state$validations(list()); state$dirty(is.null(project))
        shiny::showNotification("Combined-mode sample identifiers repaired",
                                type = "message")
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste("Identifier repair failed:", conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    })
    shiny::observeEvent(input$confirm, {
      rows <- Filter(function(x) isTRUE(x$include), state$detected_samples())
      invalid <- Filter(function(x) !identical(x$validation_status, "PASS"), rows)
      if (length(invalid))
        return(shiny::showNotification(
          "Resolve missing, ambiguous, or duplicate file pairs before confirming.",
          type = "error", duration = NULL))
      config <- fpq_configuration_from_detected_rows(state$configuration(), rows)
      saved <- tryCatch({
        fpq_validate_dynamic_configuration(config)
        project <- state$project()
        if (!is.null(project)) fpq_persist_msi_configuration(project, config)
        state$configuration(config); state$dirty(is.null(project))
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste("Sample configuration was not saved:", conditionMessage(e)),
          type = "error", duration = NULL)
        FALSE
      })
      if (isTRUE(saved))
        shiny::showNotification("Sample configuration saved", type = "message")
    })
    output$comparison_setup <- shiny::renderUI({
      analysis_type <- state$analysis_type()
      if (!analysis_type %in% c("compare_two_groups",
                                "compare_multiple_groups")) return(NULL)
      conditions <- state$configuration()$conditions %||% list()
      groups <- vapply(conditions, `[[`, "", "condition_id")
      labels <- vapply(conditions, function(x)
        x$condition_label %||% x$condition_id, character(1L))
      choices <- stats::setNames(groups, labels)
      minimum_groups <- if (identical(analysis_type,
                                      "compare_multiple_groups")) 3L else 2L
      if (length(groups) < minimum_groups) return(
        fpq_empty_state(
          "Not enough sample groups",
          paste("Confirm at least", minimum_groups,
                "groups before configuring this comparison.")))
      selected_reference <- input$reference_group %||% groups[[1L]]
      if (!selected_reference %in% groups)
        selected_reference <- groups[[1L]]
      comparison_choices <- choices[groups != selected_reference]
      existing <- state$configuration()$comparisons %||% list()
      existing_treatments <- vapply(existing, function(x)
        x$comparison_condition_id %||% "", character(1L))
      selected_treatments <- intersect(existing_treatments,
                                       unname(comparison_choices))
      if (!length(selected_treatments))
        selected_treatments <- unname(comparison_choices)
      htmltools::tags$section(class = "fpq-review",
        htmltools::tags$h3(if (identical(analysis_type,
          "compare_multiple_groups")) "Multiple-group comparisons"
          else "Comparison"),
        shiny::selectInput(session$ns("reference_group"), "Reference group",
                           choices = choices,
                           selected = selected_reference),
        if (identical(analysis_type, "compare_multiple_groups"))
          shiny::selectInput(session$ns("comparison_groups"),
            "Groups compared with the reference",
            choices = comparison_choices,
            selected = selected_treatments,
            multiple = TRUE)
        else
          shiny::selectInput(session$ns("comparison_group"),
            "Comparison group",
            choices = comparison_choices,
            selected = selected_treatments[[1L]]),
        if (identical(analysis_type, "compare_multiple_groups"))
          htmltools::tags$p(class = "fpq-review",
            "Each selected group will be compared descriptively with the same reference group.")
        else NULL,
        shiny::actionButton(session$ns("confirm_comparison"),
          if (identical(analysis_type, "compare_multiple_groups"))
            "Confirm Comparisons" else "Confirm Comparison",
          class = "btn-primary"))
    })
    shiny::observeEvent(input$confirm_comparison, {
      analysis_type <- state$analysis_type()
      reference <- input$reference_group %||% ""
      treatments <- if (identical(analysis_type,
        "compare_multiple_groups"))
          input$comparison_groups %||% character()
        else input$comparison_group %||% character()
      treatments <- unique(as.character(treatments))
      treatments <- treatments[nzchar(treatments) & treatments != reference]
      required <- if (identical(analysis_type,
                                "compare_multiple_groups")) 2L else 1L
      if (!nzchar(reference) || length(treatments) < required)
        return(shiny::showNotification(
          if (required == 2L)
            "Select one reference group and at least two comparison groups."
          else "Select two distinct sample groups.",
          type = "error", duration = NULL))
      config <- state$configuration()
      condition_ids <- vapply(config$conditions %||% list(),
        `[[`, "", "condition_id")
      if (!reference %in% condition_ids ||
          any(!treatments %in% condition_ids))
        return(shiny::showNotification(
          "One or more selected groups are no longer configured.",
          type = "error", duration = NULL))
      config$conditions <- lapply(config$conditions, function(x) {
        x$is_reference <- identical(x$condition_id, reference)
        x$analytical_role <- if (x$is_reference)
          "reference" else "comparison"
        x
      })
      config$comparisons <- lapply(seq_along(treatments), function(index) {
        treatment <- treatments[[index]]
        list(
          comparison_id = paste0("comparison-", index),
          comparison_label = paste(treatment, "versus", reference),
          reference_condition_id = reference,
          comparison_condition_id = treatment,
          enabled = TRUE,
          display_order = index
        )
      })
      saved <- tryCatch({
        fpq_validate_dynamic_configuration(config)
        project <- state$project()
        if (!is.null(project)) {
          fpq_persist_msi_configuration(project, config)
          fpq_invalidate_project_validation(project)
        }
        state$configuration(config)
        state$validations(list())
        state$dirty(is.null(project))
        TRUE
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(
          paste("Comparisons could not be saved:",
                conditionMessage(e)),
          type = "error", duration = NULL)
        FALSE
      })
      if (isTRUE(saved))
        shiny::showNotification(
          if (length(treatments) > 1L)
            paste(length(treatments), "comparisons confirmed")
          else "Comparison confirmed",
          type = "message")
    })
    output$summary <- shiny::renderUI({
      rows <- state$detected_samples()
      if (!length(rows)) return(fpq_empty_state("No MSI data selected",
        "Select an MSI folder or a set of imzML/IBD files."))
      valid <- sum(vapply(rows, function(x) identical(x$validation_status, "PASS"),
                          logical(1)))
      htmltools::tags$p(paste(valid, "of", length(rows), "file pairs are valid."))
    })
  })
}
