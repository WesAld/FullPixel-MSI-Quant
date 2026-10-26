fpq_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$h2("Project"),
    shiny::tabsetPanel(
      shiny::tabPanel("Create",
        shiny::textInput(ns("name"), "Project name"),
        shiny::textAreaInput(ns("description"), "Project description"),
        shiny::textInput(ns("location"), "Project folder", updateOn = "blur"),
        shiny::actionButton(ns("browse_location"), "Browse"),
        shiny::textInput(ns("investigator"), "Investigator or laboratory metadata"),
        shiny::textInput(ns("exposure"), "Exposure or acquisition label", value = "Single"),
        shiny::radioButtons(ns("pixel_geometry_policy"), "Pixel geometry policy",
          c("Strict predefined pixel count" = "strict_predefined",
            "Unequal / sparse pixel area" = "sparse_observed"),
          selected = "strict_predefined"),
        htmltools::tags$p(class = "help-block",
          "Unequal / sparse pixel area is a Technical Validation Candidate. It uses only stored coordinates and spectra; it does not impute, pad, or create zero-valued pixels."),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("pixel_size_um"), "Pixel size (um)", 40, min = 0.001)),
          shiny::column(3, shiny::conditionalPanel(
            "input.pixel_geometry_policy == 'strict_predefined'",
            shiny::numericInput(ns("coordinate_count"), "Expected pixels per sample", 1600, min = 1, step = 1),
            ns = ns)),
          shiny::column(3, shiny::textInput(ns("positive_matrix"), "Positive-mode matrix", "DHB")),
          shiny::column(3, shiny::textInput(ns("negative_matrix"), "Negative-mode matrix", "9AA"))
        ),
        shiny::conditionalPanel("input.advanced_mode",
          shiny::textInput(ns("project_id"), "Internal project identifier", value = "prj-")),
        shiny::actionButton(ns("check_location"), "Validate Project Location"),
        shiny::actionButton(ns("create"), "Create Project", class = "btn-primary"),
        shiny::uiOutput(ns("location_status")),
        shiny::uiOutput(ns("next_step"))),
      shiny::tabPanel("Open",
        shiny::textInput(ns("open_location"), "Existing project folder", updateOn = "blur"),
        shiny::actionButton(ns("open"), "Open Existing Project", class = "btn-primary"),
        shiny::uiOutput(ns("open_status"))),
      shiny::tabPanel("Overview", shiny::uiOutput(ns("overview")))
    ))
}

fpq_project_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    location_result <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$browse_location, {
      if (.Platform$OS.type != "windows")
        return(shiny::showNotification(
          "Folder browsing is currently available in the local Windows application.",
          type = "warning"))
      selected <- utils::choose.dir(caption = "Select the parent folder for the new project")
      if (!is.na(selected) && nzchar(selected)) {
        proposed <- file.path(selected, gsub("[^A-Za-z0-9._-]+", "-",
                                             trimws(input$name %||% "project")))
        shiny::updateTextInput(session, "location", value = proposed)
        location_result(NULL)
      }
    })
    shiny::observeEvent(input$check_location, {
      path <- normalizePath(input$location, mustWork = FALSE)
      parent <- dirname(path)
      location_result(list(path = path, exists = file.exists(path) || dir.exists(path),
        parent_exists = dir.exists(parent), writable = dir.exists(parent) &&
          file.access(parent, 2L) == 0L))
    })
    output$location_status <- shiny::renderUI({
      x <- location_result()
      if (is.null(x)) return(NULL)
      htmltools::tags$div(class = "fpq-review",
        htmltools::tags$p(htmltools::tags$strong("Resolved path: "), x$path),
        fpq_badge(if (!x$exists && x$writable) "PASS" else "FAIL",
          if (x$exists) "Folder already exists; creation would overwrite and is blocked"
          else if (!x$writable) "Parent folder is not writable" else "Location is available"))
    })
    shiny::observeEvent(input$create, {
      state$project(NULL); state$configuration(list(conditions = list(), samples = list(),
        comparisons = list(), ion_mode = "NEG", exposure_label = "Single"))
      state$dirty(FALSE); state$last_error(NULL)
      tryCatch({
        result <- location_result()
        requested <- normalizePath(input$location, mustWork = FALSE)
        if (is.null(result) || !identical(result$path, requested) ||
            result$exists || !result$writable)
          fpq_stop("ui.project_location", "Validate an available writable location first.")
        project_id <- if (isTRUE(state$advanced_mode()) &&
          nzchar(input$project_id %||% "")) input$project_id else
            fpq_project_id_from_name(input$name)
        profile <- "generic-two-group-v1"
        exposure <- trimws(input$exposure %||% "")
        if (!nzchar(exposure)) exposure <- "Unspecified"
        project <- fpq_create_project(input$location, project_id, input$name,
          profile, project_description = input$description,
          project_preferences = list(investigator_or_laboratory = input$investigator,
            default_exposure = exposure,
            pixel_size_um = as.numeric(input$pixel_size_um),
            expected_coordinate_count = as.integer(input$coordinate_count),
            pixel_geometry_policy = input$pixel_geometry_policy %||%
              "strict_predefined",
            primary_normalization_method = "spectrum_sum",
            positive_matrix = trimws(input$positive_matrix %||% "DHB"),
            negative_matrix = trimws(input$negative_matrix %||% "9AA"),
            full_pixel = TRUE,
            analysis_type = state$analysis_type(),
            initial_ion_registration = state$selected_ion_mode()))
        manifest_path <- fpq_project_paths(project$root)$manifest
        if (!file.exists(manifest_path))
          fpq_stop("ui.project_manifest_missing",
                   "Project creation did not persist project.json.", "infrastructure")
        project <- fpq_open_project(project$root)
        state$project(project); state$configuration(fpq_load_project_configuration(project))
        state$detected_samples(
          fpq_detected_rows_from_configuration(state$configuration()))
        state$mzmine_files(state$configuration()$mzmine_registrations %||% list())
        state$selected_ion_mode(fpq_modes_to_selection(fpq_active_project_modes(
          state$configuration(), project$manifest, state$detected_samples(), "NEG")))
        state$analysis_type(state$configuration()$analysis_type %||% "quantify_only")
        state$dirty(FALSE); state$last_error(NULL)
        shiny::showNotification("Project created successfully", type = "message")
      }, error = function(e) {
        friendly <- if (inherits(e, "fpq_error")) e else structure(
          list(message = conditionMessage(e), code = "ui.project_create", call = NULL),
          class = c("fpq_ui_error", "error", "condition"))
        state$last_error(friendly)
        shiny::showNotification(paste("Project could not be created:", friendly$message),
                                type = "error", duration = NULL)
      })
    })
    output$next_step <- shiny::renderUI({
      if (is.null(state$project())) return(NULL)
      htmltools::tags$section(class = "fpq-review",
        htmltools::tags$h3("What do you want to do?"),
        shiny::radioButtons(session$ns("analysis_type"), NULL,
          c("Quantify samples only" = "quantify_only",
            "Compare two groups" = "compare_two_groups",
            "Compare multiple groups" = "compare_multiple_groups"),
          selected = state$analysis_type()),
        htmltools::tags$h3("Which ion-mode data do you have?"),
        shiny::radioButtons(session$ns("selected_ion_mode"), NULL,
          c("Negative mode only" = "NEG", "Positive mode only" = "POS",
            "Positive and Negative modes" = "BOTH",
            "Detect automatically" = "AUTO"),
          selected = state$selected_ion_mode()),
        htmltools::tags$h3("Primary intensity strategy"),
        shiny::radioButtons(session$ns("primary_normalization_method"), NULL,
          choices = fpq_normalization_ui_choices(),
          selected = fpq_normalization_method(state$configuration())),
        htmltools::tags$dl(lapply(names(fpq_normalization_descriptions()),
          function(method) htmltools::tagList(
            htmltools::tags$dt(fpq_normalization_labels()[[method]]),
            htmltools::tags$dd(fpq_normalization_descriptions()[[method]])))),
        if (state$selected_ion_mode() %in% c("POS", "BOTH", "AUTO"))
          htmltools::tags$p(class = "fpq-warning-panel",
            "Positive and combined workflows with passing technical evidence are executable as Technical Validation Candidates; independent scientific review remains pending."),
        shiny::actionButton(session$ns("save_next_step"), "Continue",
                            class = "btn-primary"))
    })
    shiny::observeEvent(input$save_next_step, {
      tryCatch({
        project <- state$project()
        if (is.null(project)) fpq_stop("analysis_design.project_missing",
          "Open or create a project before saving the analysis setup.")
        config <- fpq_apply_analysis_design(state$configuration(),
          input$analysis_type, input$selected_ion_mode)
        config <- fpq_set_normalization_method(config,
          input$primary_normalization_method %||% "spectrum_sum")
        config$schema_version <- config$schema_version %||% "1.0.0"
        config$dataset_id <- config$dataset_id %||% "dset-project-configuration"
        fpq_with_project_lock(project$root, {
          fpq_persist_msi_configuration(project, config)
          preferences <- project$manifest$project_preferences %||% list()
          preferences$analysis_type <- input$analysis_type
          preferences$initial_ion_registration <- input$selected_ion_mode
          preferences$ion_mode <- input$selected_ion_mode
          preferences$enabled_modes <- config$enabled_ion_modes
          preferences$analysis_uses_both_ion_modes <-
            config$analysis_uses_both_ion_modes
          preferences$primary_normalization_method <-
            config$primary_normalization_method
          fpq_save_project(project, list(project_preferences = preferences))
        })
        state$project(fpq_open_project(project$root))
        state$analysis_type(input$analysis_type)
        state$selected_ion_mode(input$selected_ion_mode)
        state$configuration(config); state$dirty(FALSE)
        shiny::showNotification("Analysis setup saved", type = "message")
        shiny::updateNavbarPage(session$rootScope(), "main_navigation",
                                selected = "MSI Samples")
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste("Analysis setup could not be saved:",
          conditionMessage(e)), type = "error", duration = NULL)
      })
    })
    shiny::observeEvent(input$open, {
      tryCatch({
        project <- fpq_open_project(input$open_location)
        recovery <- fpq_recover_project(project, "inspect")
        if (isTRUE(recovery$lock_exists))
          fpq_stop("ui.project_locked", "The project has a lock. Review recovery before opening.")
        state$project(project); state$configuration(fpq_load_project_configuration(project))
        state$detected_samples(
          fpq_detected_rows_from_configuration(state$configuration()))
        state$mzmine_files(state$configuration()$mzmine_registrations %||% list())
        state$selected_ion_mode(fpq_modes_to_selection(fpq_active_project_modes(
          state$configuration(), project$manifest, state$detected_samples(), "NEG")))
        state$analysis_type(state$configuration()$analysis_type %||%
          project$manifest$project_preferences$analysis_type %||% "quantify_only")
        state$dirty(FALSE); state$last_error(NULL)
      }, error = function(e) state$last_error(e))
    })
    output$open_status <- shiny::renderUI({
      error <- state$last_error()
      if (!is.null(error)) return(fpq_error_ui(error))
      project <- state$project()
      if (is.null(project)) return(fpq_empty_state("No project open",
        "Enter an existing project folder and select Open Existing Project."))
      htmltools::tags$div(fpq_badge("PASS", "Project open"),
        htmltools::tags$p(project$root))
    })
    output$overview <- shiny::renderUI({
      summary <- fpq_app_summary(state)
      if (!summary$project_open) return(fpq_empty_state("No project open",
        "Create or open a project to view its persisted configuration."))
      htmltools::tags$div(class = "fpq-card-grid",
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3(summary$project_name),
          htmltools::tags$dl(
            htmltools::tags$dt("Project ID"), htmltools::tags$dd(summary$project_id),
            htmltools::tags$dt("Path"), htmltools::tags$dd(state$project()$root),
            htmltools::tags$dt("Profile"), htmltools::tags$dd(summary$profile))),
        htmltools::tags$section(class = "fpq-card", htmltools::tags$h3("Configuration"),
          htmltools::tags$p(paste(summary$conditions, "conditions")),
          htmltools::tags$p(paste(summary$samples, "samples")),
          htmltools::tags$p(paste(summary$comparisons, "comparisons")),
          fpq_badge(if (summary$dirty) "WARNING" else "PASS",
                    if (summary$dirty) "Unsaved changes" else "Saved")),
        {
          config <- state$configuration()
          preferences <- state$project()$manifest$project_preferences %||% list()
          htmltools::tags$section(class = "fpq-card",
            htmltools::tags$h3("Acquisition metadata"),
            shiny::textInput(session$ns("edit_exposure"),
              "Exposure or acquisition label",
              config$exposure_label %||% preferences$default_exposure %||%
                "Unspecified"),
            shiny::numericInput(session$ns("edit_pixel_size_um"),
              "Pixel size (um)",
              config$pixel_size_um %||% preferences$pixel_size_um %||% 40,
              min = 0.001),
            shiny::radioButtons(session$ns("edit_pixel_geometry_policy"),
              "Pixel geometry policy",
              c("Strict predefined pixel count" = "strict_predefined",
                "Unequal / sparse pixel area" = "sparse_observed"),
              selected = config$pixel_geometry_policy %||%
                preferences$pixel_geometry_policy %||% "strict_predefined"),
            htmltools::tags$p(class = "help-block",
              "Unequal / sparse pixel area is a Technical Validation Candidate. It uses only stored coordinates and spectra; it does not impute, pad, or create zero-valued pixels."),
            shiny::conditionalPanel(
              "input.edit_pixel_geometry_policy == 'strict_predefined'",
              shiny::numericInput(session$ns("edit_coordinate_count"),
                "Expected pixels per sample",
                config$expected_coordinate_count %||%
                  preferences$expected_coordinate_count %||% 1600,
                min = 1, step = 1), ns = session$ns),
            shiny::textInput(session$ns("edit_positive_matrix"),
              "Positive-mode matrix",
              config$positive_matrix %||% preferences$positive_matrix %||% "DHB"),
            shiny::textInput(session$ns("edit_negative_matrix"),
              "Negative-mode matrix",
              config$negative_matrix %||% preferences$negative_matrix %||% "9AA"),
            shiny::radioButtons(session$ns("edit_primary_normalization_method"),
              "Primary intensity strategy",
              choices = fpq_normalization_ui_choices(),
              selected = fpq_normalization_method(config)),
            htmltools::tags$p(class = "help-block",
              "Spectrum-sum is the validated V1 reference. RMS and Median are V2 technical-validation methods."),
            shiny::actionButton(session$ns("save_acquisition"),
              "Save Acquisition Metadata", class = "btn-primary"))
        })
    })
    shiny::observeEvent(input$save_acquisition, {
      project <- state$project()
      if (is.null(project)) return()
      tryCatch({
        exposure <- trimws(input$edit_exposure %||% "")
        positive_matrix <- trimws(input$edit_positive_matrix %||% "")
        negative_matrix <- trimws(input$edit_negative_matrix %||% "")
        pixel_size <- suppressWarnings(as.numeric(input$edit_pixel_size_um))
        coordinate_count <- suppressWarnings(as.integer(input$edit_coordinate_count))
        geometry_policy <- input$edit_pixel_geometry_policy %||%
          "strict_predefined"
        if (!nzchar(exposure))
          fpq_stop("project.exposure_blank",
            "Exposure or acquisition label must be nonblank.", "configuration")
        if (!is.finite(pixel_size) || pixel_size <= 0)
          fpq_stop("project.pixel_size",
            "Pixel size must be a positive number.", "configuration")
        if (identical(geometry_policy, "strict_predefined") &&
            (is.na(coordinate_count) || coordinate_count <= 0))
          fpq_stop("project.coordinate_count",
            "Expected pixels per sample must be a positive integer.",
            "configuration")
        if (!nzchar(positive_matrix) || !nzchar(negative_matrix))
          fpq_stop("project.matrix_blank",
            "Positive- and Negative-mode matrix labels must be nonblank.",
            "configuration")

        config <- state$configuration()
        config$exposure_label <- exposure
        config$pixel_size_um <- pixel_size
        config$expected_coordinate_count <- coordinate_count
        config$pixel_geometry_policy <- geometry_policy
        config$positive_matrix <- positive_matrix
        config$negative_matrix <- negative_matrix
        config <- fpq_set_normalization_method(config,
          input$edit_primary_normalization_method %||% "spectrum_sum")
        config$schema_version <- config$schema_version %||% "1.0.0"
        config$dataset_id <- config$dataset_id %||%
          "dset-project-configuration"

        preferences <- project$manifest$project_preferences %||% list()
        preferences$default_exposure <- exposure
        preferences$pixel_size_um <- pixel_size
        preferences$expected_coordinate_count <- coordinate_count
        preferences$pixel_geometry_policy <- geometry_policy
        preferences$positive_matrix <- positive_matrix
        preferences$negative_matrix <- negative_matrix
        preferences$primary_normalization_method <-
          config$primary_normalization_method
        preferences$full_pixel <- TRUE

        fpq_with_project_lock(project$root, {
          fpq_persist_msi_configuration(project, config)
          fpq_save_project(project, list(project_preferences = preferences))
          fpq_invalidate_project_validation(project)
        })
        state$project(fpq_open_project(project$root))
        state$configuration(config)
        state$validations(list())
        state$dirty(FALSE)
        shiny::showNotification("Acquisition metadata saved", type = "message")
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste("Acquisition metadata could not be saved:",
          conditionMessage(e)), type = "error", duration = NULL)
      })
    })
  })
}
