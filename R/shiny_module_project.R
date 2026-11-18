fpq_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$h2("Project"),
    shiny::tabsetPanel(id = ns("project_tabs"),
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

fpq_validate_new_project_inputs <- function(name, exposure, geometry_policy,
                                            pixel_size_um, coordinate_count) {
  errors <- character()
  if (!nzchar(trimws(name %||% "")))
    errors <- c(errors, "Enter a project name.")
  if (!nzchar(trimws(exposure %||% "")))
    errors <- c(errors, "Enter an experiment label.")
  if (!length(geometry_policy) ||
      !geometry_policy %in% c("strict_predefined", "sparse_observed"))
    errors <- c(errors, "Select a pixel geometry policy.")
  pixel_size <- suppressWarnings(as.numeric(pixel_size_um))
  if (!length(pixel_size) || !is.finite(pixel_size) || pixel_size <= 0)
    errors <- c(errors, "Enter a positive pixel size in micrometres.")
  coordinates <- suppressWarnings(as.numeric(coordinate_count))
  if (identical(geometry_policy, "strict_predefined") &&
      (!length(coordinates) || !is.finite(coordinates) || coordinates < 1 ||
       coordinates != as.integer(coordinates)))
    errors <- c(errors,
      "Enter a positive whole-number expected pixel count for strict geometry.")
  unique(errors)
}

fpq_project_open_status_ui <- function(status, advanced = FALSE) {
  if (is.null(status)) return(NULL)
  if (identical(status$type, "success"))
    return(htmltools::tags$div(fpq_badge("PASS", "Project open"),
                              htmltools::tags$p(status$path)))
  details <- if (isTRUE(advanced) && length(status$technical %||% list()))
    htmltools::tags$details(
      htmltools::tags$summary("Technical details"),
      htmltools::tags$dl(lapply(names(status$technical), function(name)
        htmltools::tagList(htmltools::tags$dt(name),
                           htmltools::tags$dd(status$technical[[name]])))))
  htmltools::tags$div(class = "fpq-v4-notice warning",
    htmltools::tags$p(status$message), details)
}

fpq_local_directory_roots <- function(volumes = NULL) {
  if (is.null(volumes))
    volumes <- tryCatch(shinyFiles::getVolumes()(), error = function(e) character())
  roots <- volumes
  if (.Platform$OS.type == "windows") {
    drive_paths <- paste0(LETTERS, ":/")
    drive_paths <- drive_paths[dir.exists(drive_paths)]
    missing <- !tolower(drive_paths) %in% tolower(unname(roots))
    if (any(missing)) {
      discovered <- drive_paths[missing]
      names(discovered) <- sub("/$", "", discovered)
      roots <- c(roots, discovered)
    }
  }
  roots <- roots[dir.exists(unname(roots))]
  if (!length(roots))
    roots <- c(Home = normalizePath(path.expand("~"), winslash = "/",
                                   mustWork = FALSE))
  if (is.null(names(roots)) || any(!nzchar(names(roots))))
    names(roots) <- unname(roots)
  roots[!duplicated(tolower(unname(roots)))]
}

fpq_project_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    location_result <- shiny::reactiveVal(NULL)
    create_result <- shiny::reactiveVal(NULL)
    open_result <- shiny::reactiveVal(NULL)
    jackknife_selection <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(state$configuration(), {
      jackknife_selection(fpq_jackknife_enabled(state$configuration()))
    }, ignoreInit = FALSE)
    directory_roots <- fpq_local_directory_roots()
    shinyFiles::shinyDirChoose(input, "browse_location", roots = directory_roots,
      session = session, allowDirCreate = TRUE)
    shinyFiles::shinyDirChoose(input, "browse_open_location",
      roots = directory_roots, session = session, allowDirCreate = FALSE)
    shiny::observeEvent(input$browse_location, {
      selected <- tryCatch(
        shinyFiles::parseDirPath(directory_roots, input$browse_location),
        error = function(e) character()
      )
      if (!length(selected) || !nzchar(selected[[1]])) return()
      folder_name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(input$name %||% ""))
      if (!nzchar(folder_name)) folder_name <- "project"
      proposed <- file.path(selected[[1]], folder_name)
      shiny::updateTextInput(session, "location", value = proposed)
      location_result(NULL)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$browse_open_location, {
      selected <- tryCatch(
        shinyFiles::parseDirPath(directory_roots, input$browse_open_location),
        error = function(e) character())
      if (!length(selected) || !nzchar(selected[[1]])) return()
      shiny::updateTextInput(session, "open_location", value = selected[[1]])
      open_result(NULL)
    }, ignoreInit = TRUE)
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
      create_result(NULL)
      tryCatch({
        # The V4 form owns these inputs. Keep the long-standing module API usable
        # by backend/server tests and older shells that do not render this form.
        is_v4_create_form <- !is.null(input$pixel_size_um)
        validation_errors <- if (is_v4_create_form)
          fpq_validate_new_project_inputs(
            input$name, input$exposure, input$pixel_geometry_policy,
            input$pixel_size_um, input$coordinate_count)
        else character()
        if (length(validation_errors)) {
          create_result(list(type = "error", messages = validation_errors))
          return()
        }
        result <- location_result()
        requested <- normalizePath(input$location, mustWork = FALSE)
        if (is.null(result) || !identical(result$path, requested) ||
            result$exists || !result$writable)
          fpq_stop("ui.project_location", "Validate an available writable location first.")
        project_id <- if (isTRUE(state$advanced_mode()) &&
          nzchar(input$project_id %||% "")) input$project_id else
            fpq_project_id_from_name(input$name)
        profile <- "generic-two-group-v1"
        exposure <- trimws(input$exposure %||% "Single")
        pixel_size <- if (is_v4_create_form) as.numeric(input$pixel_size_um) else NULL
        geometry_policy <- if (is_v4_create_form)
          input$pixel_geometry_policy else "strict_predefined"
        coordinates <- if (is_v4_create_form && identical(geometry_policy,
                                      "strict_predefined"))
          as.integer(input$coordinate_count) else NULL
        project <- fpq_create_project(input$location, project_id, input$name,
          profile, project_description = NULL,
          project_preferences = list(investigator_or_laboratory = input$investigator,
            default_exposure = exposure,
            pixel_size_um = pixel_size,
            expected_coordinate_count = coordinates,
            pixel_geometry_policy = geometry_policy,
            primary_normalization_method = "spectrum_sum",
            positive_matrix = trimws(input$positive_matrix %||% ""),
            negative_matrix = trimws(input$negative_matrix %||% ""),
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
        create_result(list(type = "success", messages = "Project created successfully."))
        shiny::showNotification("Project created successfully", type = "message")
        fpq_v4_navigate(session$rootScope(), "project_overview")
      }, error = function(e) {
        friendly <- if (inherits(e, "fpq_error")) e else structure(
          list(message = conditionMessage(e), code = "ui.project_create", call = NULL),
          class = c("fpq_ui_error", "error", "condition"))
        state$last_error(friendly)
        create_result(list(type = "error", messages = friendly$message))
        shiny::showNotification(paste("Project could not be created:", friendly$message),
                                type = "error", duration = NULL)
      })
    })
    output$create_status <- shiny::renderUI({
      result <- create_result()
      if (is.null(result)) return(NULL)
      htmltools::tags$div(
        class = paste("fpq-v4-notice", if (identical(result$type, "success"))
          "success" else "warning"),
        htmltools::tags$ul(lapply(result$messages, htmltools::tags$li)))
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
        shiny::uiOutput(session$ns("jackknife_setup")),
        shiny::actionButton(session$ns("save_next_step"), "Continue",
                            class = "btn-primary"))
    })
    output$jackknife_setup <- shiny::renderUI(
      fpq_jackknife_ui(session$ns, state$configuration(),
        jackknife_selection(), "setup_"))
    shiny::observeEvent(input$setup_include_jackknife, {
      design <- fpq_jackknife_design(state$configuration())
      if (isTRUE(design$eligible)) jackknife_selection(TRUE)
    })
    shiny::observeEvent(input$setup_remove_jackknife,
      jackknife_selection(FALSE))
    shiny::observeEvent(input$save_next_step, {
      tryCatch({
        project <- state$project()
        if (is.null(project)) fpq_stop("analysis_design.project_missing",
          "Open or create a project before saving the analysis setup.")
        config <- fpq_apply_analysis_design(state$configuration(),
          input$analysis_type, input$selected_ion_mode)
        config <- fpq_set_normalization_method(config,
          input$primary_normalization_method %||% "spectrum_sum")
        config <- fpq_set_jackknife_enabled(config, jackknife_selection())
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
          preferences$statistical_analyses <- config$statistical_analyses
          fpq_save_project(project, list(project_preferences = preferences))
        })
        state$project(fpq_open_project(project$root))
        state$analysis_type(input$analysis_type)
        state$selected_ion_mode(input$selected_ion_mode)
        state$configuration(config); state$dirty(FALSE)
        shiny::showNotification("Analysis setup saved", type = "message")
        fpq_v4_navigate(session$rootScope(), "msi_samples")
      }, error = function(e) {
        state$last_error(e)
        shiny::showNotification(paste("Analysis setup could not be saved:",
          conditionMessage(e)), type = "error", duration = NULL)
      })
    })
    shiny::observeEvent(input$open, {
      requested <- trimws(input$open_location %||% "")
      if (!nzchar(requested)) {
        open_result(list(type = "error",
          message = "Please select a MALDIPixelQuant project folder first."))
        return()
      }
      searched <- fpq_project_paths(requested)
      if (!file.exists(searched$manifest)) {
        open_result(list(type = "error", message = paste(
          "This folder does not contain a MALDIPixelQuant project.",
          "Select the project folder containing MALDIPixelQuant_ProjectFiles/",
          "or a legacy project folder containing project.json."),
          technical = list(
            `Searched path` = searched$manifest,
            `Error identifier` = "ui.project_not_found",
            Remediation = "Select a V4 project root or legacy project root.")))
        return()
      }
      tryCatch({
        project <- fpq_open_project(requested)
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
        open_result(list(type = "success", path = project$root))
        fpq_v4_navigate(session$rootScope(), "project_overview")
      }, error = function(e) {
        open_result(list(type = "error",
          message = "This project could not be opened. Check the folder and try again.",
          technical = list(
            `Error identifier` = e$code %||% class(e)[[1L]],
            `Searched path` = searched$manifest,
            `Backend message` = conditionMessage(e),
            Remediation = e$remediation %||%
              "Review the project metadata or open a known valid project folder.")))
      })
    })
    output$open_status <- shiny::renderUI({
      status <- open_result()
      if (!is.null(status))
        return(fpq_project_open_status_ui(status, state$advanced_mode()))
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
      config <- state$configuration()
      external_paths <- c(
        unlist(lapply(config$samples %||% list(), function(x)
          c(x$imzml_path %||% "", x$ibd_path %||% "")), use.names = FALSE),
        unlist(lapply(config$mzmine_registrations %||% list(), function(x)
          x$path %||% ""), use.names = FALSE))
      external_paths <- external_paths[nzchar(external_paths)]
      htmltools::tagList(
        htmltools::tags$section(class = "fpq-card fpq-v4-project-identity",
          htmltools::tags$h2(summary$project_name),
          htmltools::tags$p(class = "fpq-v4-project-path", state$project()$root),
          if (isTRUE(state$advanced_mode())) htmltools::tags$details(
            class = "fpq-v4-technical-details",
            htmltools::tags$summary("Technical project identifiers"),
            htmltools::tags$dl(
              htmltools::tags$dt("Project ID"), htmltools::tags$dd(summary$project_id),
              htmltools::tags$dt("Scientific profile ID"),
              htmltools::tags$dd(summary$profile)))),
        htmltools::tags$section(class = "fpq-card fpq-v4-project-summary",
          htmltools::tags$h3("Configuration"),
          htmltools::tags$div(class = "fpq-v4-project-summary-items",
            htmltools::tags$span(htmltools::tags$strong(summary$conditions),
              if (identical(summary$conditions, 1L)) " Condition" else " Conditions"),
            htmltools::tags$span(htmltools::tags$strong(summary$samples),
              if (identical(summary$samples, 1L)) " Sample" else " Samples"),
            htmltools::tags$span(htmltools::tags$strong(summary$comparisons),
              if (identical(summary$comparisons, 1L)) " Comparison" else " Comparisons"),
            fpq_badge(if (summary$dirty) "WARNING" else "PASS",
                      if (summary$dirty) "Unsaved changes" else "Saved"))),
        {
          config <- state$configuration()
          preferences <- state$project()$manifest$project_preferences %||% list()
          htmltools::tags$section(class = "fpq-card fpq-v4-project-settings",
            htmltools::tags$h3("Project settings"),
            htmltools::tags$div(class = "fpq-v4-settings-grid",
              shiny::textInput(session$ns("edit_exposure"),
                "Experiment label",
                config$exposure_label %||% preferences$default_exposure %||%
                  "Unspecified"),
              shiny::numericInput(session$ns("edit_pixel_size_um"),
                htmltools::HTML("Pixel size (&micro;m)"),
                config$pixel_size_um %||% preferences$pixel_size_um %||% 40,
                min = 0.001),
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
                config$negative_matrix %||% preferences$negative_matrix %||% "9AA")),
            shiny::radioButtons(session$ns("edit_pixel_geometry_policy"),
              "Pixel geometry policy",
              c("Strict predefined pixel count" = "strict_predefined",
                "Unequal / sparse pixel area" = "sparse_observed"),
              selected = config$pixel_geometry_policy %||%
                preferences$pixel_geometry_policy %||% "strict_predefined",
              inline = TRUE),
            shiny::radioButtons(session$ns("edit_primary_normalization_method"),
              "Primary intensity strategy",
              choices = fpq_normalization_ui_choices(),
              selected = fpq_normalization_method(config), inline = TRUE),
            htmltools::tags$h4("Statistical analyses"),
            shiny::uiOutput(session$ns("jackknife_overview")),
            shiny::actionButton(session$ns("save_acquisition"),
              "Save Project Settings", class = "btn-primary"))
        },
        if (length(external_paths)) htmltools::tags$section(
          class = "fpq-v4-notice warning",
          htmltools::tags$strong("External source paths remain absolute."),
          htmltools::tags$p(paste(
            "A copied or moved project still depends on its original MSI and MZmine files.",
            "MALDIPixelQuant preserves these paths for provenance and does not rewrite them automatically.")))
      )
    })
    output$jackknife_overview <- shiny::renderUI(
      fpq_jackknife_ui(session$ns, state$configuration(),
        jackknife_selection(), "overview_", show_section_heading = FALSE))
    shiny::observeEvent(input$overview_include_jackknife, {
      design <- fpq_jackknife_design(state$configuration())
      if (isTRUE(design$eligible)) jackknife_selection(TRUE)
    })
    shiny::observeEvent(input$overview_remove_jackknife,
      jackknife_selection(FALSE))
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
        config <- fpq_set_jackknife_enabled(config, jackknife_selection())
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
        preferences$statistical_analyses <- config$statistical_analyses
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
