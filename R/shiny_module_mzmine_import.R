fpq_mzmine_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$h2("MZmine Feature List"),
    htmltools::tags$p("Select one aligned and annotated MZmine CSV per ion mode."),
    shiny::uiOutput(ns("selectors")),
    shiny::uiOutput(ns("review")),
    shiny::conditionalPanel("input.advanced_mode",
      shiny::checkboxInput(ns("separate_annotations"),
        "Use a separate annotation file", FALSE),
      htmltools::tags$p(class = "fpq-help",
        "This advanced option is reserved for laboratories with separate target and annotation files.")))
}

fpq_mzmine_import_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    choose_csv <- function(mode) {
      path <- utils::choose.files(caption = paste("Select", mode, "MZmine CSV"),
        filters = matrix(c("CSV", "*.csv", "All files", "*.*"), ncol = 2L,
                         byrow = TRUE), multi = FALSE)
      if (!length(path)) return()
      tryCatch({
        inspection <- fpq_inspect_mzmine_csv(path[[1]])
        files <- state$mzmine_files()
        files[[mode]] <- inspection
        state$mzmine_files(files)
        shiny::showNotification(paste(mode, "MZmine file selected"), type = "message")
      }, error = function(e) shiny::showNotification(
        paste("MZmine file could not be selected:", conditionMessage(e)),
        type = "error", duration = NULL))
    }
    output$selectors <- shiny::renderUI({
      project <- state$project()
      modes <- fpq_active_project_modes(state$configuration(),
        if (is.null(project)) list() else project$manifest,
        state$detected_samples(), state$selected_ion_mode())
      htmltools::tags$div(class = "fpq-card-grid", lapply(modes, function(mode)
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3(paste(mode, "mode")),
          shiny::actionButton(session$ns(paste0("select_", mode)),
                              paste("Select", mode, "MZmine CSV")))))
    })
    for (mode in c("POS", "NEG", "UNASSIGNED")) {
      local({
        current_mode <- mode
        shiny::observeEvent(input[[paste0("select_", current_mode)]],
                            choose_csv(current_mode))
      })
    }
    output$review <- shiny::renderUI({
      files <- state$mzmine_files()
      if (!length(files)) return(fpq_empty_state("No MZmine CSV selected",
        "Select the aligned and annotated feature list for each ion mode."))
      cards <- lapply(names(files), function(mode) {
        x <- files[[mode]]
        detected <- names(Filter(Negate(is.null), x$mappings))
        htmltools::tags$section(class = "fpq-card",
          htmltools::tags$h3(paste(mode, "-", x$filename)),
          fpq_badge(if (x$status == "PASS") "PASS" else "WARNING", x$status),
          htmltools::tags$p(paste(x$feature_count, "features")),
          htmltools::tags$p(if (length(detected))
            paste(paste(detected, collapse = ", "), "detected")
            else "No standard columns detected."),
          if (length(x$ambiguous)) htmltools::tags$p(class = "fpq-warning-panel",
            paste("Ambiguous:", paste(names(x$ambiguous), collapse = ", "))),
          if (length(x$missing_required)) htmltools::tags$p(class = "fpq-warning-panel",
            paste("Required mapping:", paste(unlist(x$missing_required),
                                             collapse = ", "))),
          if (x$status != "PASS") lapply(unique(c(names(x$ambiguous),
              unlist(x$missing_required))), function(field)
            shiny::selectInput(session$ns(paste0("map_", mode, "_", field)),
              paste("Select", field, "column"),
              choices = c("Choose a column" = "", unlist(x$columns)),
              selected = x$mappings[[field]] %||% "")),
          shiny::actionButton(session$ns(paste0("confirm_", mode)),
                              paste("Confirm", mode, "Column Mapping")))
      })
      htmltools::tags$div(class = "fpq-card-grid", cards)
    })
    for (mode in c("POS", "NEG", "UNASSIGNED")) {
      local({
        current_mode <- mode
        shiny::observeEvent(input[[paste0("confirm_", current_mode)]], {
          files <- state$mzmine_files()
          if (is.null(files[[current_mode]])) return()
          inspection <- files[[current_mode]]
          mapping <- inspection$mappings
          review_fields <- unique(c(names(inspection$ambiguous),
                                    unlist(inspection$missing_required)))
          for (field in review_fields) {
            value <- input[[paste0("map_", current_mode, "_", field)]]
            if (!is.null(value) && nzchar(value)) mapping[[field]] <- value
          }
          tryCatch({
            files[[current_mode]] <- fpq_confirm_mzmine_mapping(inspection, mapping)
            state$mzmine_files(files)
            config <- state$configuration()
            config$mzmine_registrations <- files
            project <- state$project()
            if (!is.null(project)) fpq_persist_msi_configuration(project, config)
            state$configuration(config)
            shiny::showNotification("Column mapping confirmed", type = "message")
          }, error = function(e) shiny::showNotification(conditionMessage(e),
            type = "error", duration = NULL))
        })
      })
    }
  })
}
