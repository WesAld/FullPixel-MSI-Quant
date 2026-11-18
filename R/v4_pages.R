# V4 presentation layer. These UI overrides retain the established input/output
# IDs so the V3 server and scientific services remain authoritative.

fpq_v4_page_heading <- function(title, subtitle = NULL, actions = NULL) {
  htmltools::tags$div(class = "fpq-v4-page-heading",
    htmltools::tags$div(htmltools::tags$h1(title),
      if (!is.null(subtitle)) htmltools::tags$p(subtitle)), actions)
}

fpq_v4_section <- function(number = NULL, title, subtitle = NULL, ..., class = "") {
  htmltools::tags$section(class = paste("fpq-v4-section", class),
    htmltools::tags$div(class = "fpq-v4-section-heading",
      if (!is.null(number)) htmltools::tags$span(class = "fpq-v4-step", number),
      htmltools::tags$div(htmltools::tags$h2(title),
        if (!is.null(subtitle)) htmltools::tags$p(subtitle))), ...)
}

fpq_v4_workflow_stage_name <- function(stage_id, fallback = stage_id) {
  switch(stage_id %||% "",
    normalizing = "Normalizing intensities",
    final_workbook = "Creating final Excel workbook",
    fallback %||% stage_id)
}

fpq_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fpq_v4_page_heading("Project",
      "Create, open, and review a reproducible MALDIPixelQuant project."),
    shiny::tabsetPanel(id = ns("project_tabs"),
      shiny::tabPanel("Create",
        fpq_v4_section("1", "Project identity",
          "Choose a clear project name.",
          shiny::textInput(ns("name"), "Project name")),
        fpq_v4_section("2", "Project storage",
          paste("MALDIPixelQuant stores project configuration, workflow runs,",
                "outputs, and final results in this folder."),
          htmltools::tags$p(class = "fpq-v4-muted",
            paste("The final Excel workbook is placed directly in this folder.",
                  "Internal MALDIPixelQuant files are kept in a separate project-data folder.")),
          htmltools::tags$div(class = "fpq-v4-path-picker",
            shiny::textInput(ns("location"),
              "Project folder", updateOn = "blur"),
            shinyFiles::shinyDirButton(ns("browse_location"), "Browse",
              "Select the parent folder for the new project")),
          shiny::actionButton(ns("check_location"), "Check Folder",
                              class = "fpq-v4-secondary"),
          shiny::uiOutput(ns("location_status"))),
        fpq_v4_section("3", "Acquisition setup",
          "Record the acquisition context used to validate and interpret the project.",
          shiny::fluidRow(
            shiny::column(4, shiny::textInput(ns("exposure"),
              "Experiment label", value = "", placeholder = "e.g. Single exposure")),
            shiny::column(4, shiny::radioButtons(ns("pixel_geometry_policy"),
              "Pixel geometry policy", c("Strict predefined pixel count" = "strict_predefined",
                "Unequal / sparse pixel area" = "sparse_observed"),
              selected = character(0))),
            shiny::column(2, shiny::textInput(ns("pixel_size_um"),
              htmltools::HTML("Pixel size (&micro;m)"),
              value = "", placeholder = "e.g. 40")),
            shiny::column(2, shiny::conditionalPanel(
              "input.pixel_geometry_policy == 'strict_predefined'",
              shiny::textInput(ns("coordinate_count"),
                "Expected pixels per sample", value = "",
                placeholder = "e.g. 1600"), ns = ns))),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("positive_matrix"),
              "Positive-mode matrix", value = "", placeholder = "e.g. DHB")),
            shiny::column(6, shiny::textInput(ns("negative_matrix"),
              "Negative-mode matrix", value = "", placeholder = "e.g. 9AA")))),
        shiny::conditionalPanel("input.advanced_mode",
          shiny::textInput(ns("project_id"), "Internal project identifier",
                           value = "prj-")),
        htmltools::tags$div(class = "fpq-v4-form-footer",
          shiny::actionButton(ns("create"), "Create Project",
                              class = "btn-primary fpq-v4-primary")),
        shiny::uiOutput(ns("create_status")),
        shiny::uiOutput(ns("next_step"))),
      shiny::tabPanel("Open",
        fpq_v4_section(NULL, "Open an existing project",
          "Select the researcher-facing MALDIPixelQuant project folder.",
          htmltools::tags$div(class = "fpq-v4-path-picker",
            shiny::textInput(ns("open_location"),
              "Existing project folder", updateOn = "blur"),
            shinyFiles::shinyDirButton(ns("browse_open_location"), "Browse",
              "Select an existing MALDIPixelQuant project folder")),
          shiny::actionButton(ns("open"), "Open Existing Project",
                              class = "btn-primary fpq-v4-primary"),
          shiny::uiOutput(ns("open_status")))),
      shiny::tabPanel("Overview", shiny::uiOutput(ns("overview")))))
}

fpq_sample_import_ui <- function(id) {
  ns <- shiny::NS(id)
  mode_card <- function(mode, title) htmltools::tags$section(
    class = paste("fpq-v4-import-card", tolower(mode)),
    htmltools::tags$div(class = "fpq-v4-import-icon", shiny::icon("folder-open")),
    htmltools::tags$h3(paste(title, "mode")),
    htmltools::tags$p("Select a folder containing paired centroided imzML and IBD files."),
    shinyFiles::shinyDirButton(ns(paste0("folder_", mode)),
      paste("Select", title, "MSI Folder"), paste("Select", title, "MSI folder")),
    shiny::conditionalPanel("output.show_advanced_mode", ns = ns,
      shiny::actionButton(ns(paste0("files_", mode)),
        paste("Select", title, "MSI Files"))))
  shiny::tagList(
    fpq_v4_page_heading("MSI Samples",
      "Register original full-pixel imzML/IBD data in place; source files are not copied."),
    htmltools::tags$div(class = "fpq-v4-import-grid",
      shiny::conditionalPanel("output.show_pos_mode", ns = ns,
        mode_card("POS", "Positive")),
      shiny::conditionalPanel("output.show_neg_mode", ns = ns,
        mode_card("NEG", "Negative")),
      shiny::conditionalPanel("output.show_unassigned_mode", ns = ns,
        mode_card("UNASSIGNED", "Unassigned"))),
    shiny::uiOutput(ns("advanced_one_sample")),
    fpq_v4_section(NULL, "Registered samples",
      "Review sample identity, biological pairing, ion mode, and file status before confirmation.",
      htmltools::tags$div(class = "fpq-v4-table-toolbar",
        shiny::checkboxInput(ns("show_details"), "Show full path details", FALSE),
        htmltools::tags$div(
          shiny::actionButton(ns("remove_selected"), "Remove Selected"),
          htmltools::tags$details(class = "fpq-v4-actions-menu",
            htmltools::tags$summary("Clear samples"),
            shiny::actionButton(ns("clear_pos"), "Clear POS"),
            shiny::actionButton(ns("clear_neg"), "Clear NEG"),
            shiny::actionButton(ns("clear_all"), "Clear All")))),
      DT::DTOutput(ns("review")),
      shiny::uiOutput(ns("repair_identifiers")),
      shiny::uiOutput(ns("comparison_setup")),
      shiny::uiOutput(ns("summary")),
      htmltools::tags$div(class = "fpq-v4-form-footer",
        shiny::actionButton(ns("confirm"), "Confirm Samples",
                            class = "btn-primary fpq-v4-primary"))))
}

fpq_mzmine_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fpq_v4_page_heading("MZmine Feature List",
      "Register one aligned and annotated MZmine feature list for each active ion mode."),
    shiny::uiOutput(ns("selectors")),
    shiny::uiOutput(ns("review")),
    shiny::conditionalPanel("input.advanced_mode",
      fpq_v4_section(NULL, "Advanced annotation sources",
        "Use only when target and annotation metadata are stored separately.",
        shiny::checkboxInput(ns("separate_annotations"),
          "Use a separate annotation file", FALSE))))
}

fpq_design_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("demo_warning")),
    fpq_v4_section("1", "Conditions and Treatments",
      "Define experimental conditions and treatments in their intended display order.",
      htmltools::tags$div(class = "fpq-v4-table-toolbar",
        shiny::selectInput(ns("condition"), "Selected condition", choices = NULL),
        htmltools::tags$div(
          shiny::actionButton(ns("add_condition"), "+ Add Condition"),
          shiny::actionButton(ns("add_treatment"), "+ Add Treatment"),
          shiny::actionButton(ns("duplicate_condition"), "Duplicate Selected"),
          shiny::actionButton(ns("remove_condition"), "Remove Selected"),
          shiny::actionButton(ns("move_up"), "Move Up"),
          shiny::actionButton(ns("move_down"), "Move Down"))),
      shiny::uiOutput(ns("condition_editor"))),
    fpq_v4_section("2", "Biological Replicates",
      "Map biological replicate identities to samples without treating pixels as replicates.",
      htmltools::tags$div(class = "fpq-v4-table-toolbar",
        htmltools::tags$div(
          shiny::actionButton(ns("add_replicate"), "+ Add Replicate"),
          shiny::actionButton(ns("duplicate_replicate"), "Duplicate Replicate Row"),
          shiny::actionButton(ns("remove_replicate"), "Remove Replicate")),
        htmltools::tags$div(
          shiny::fileInput(ns("import_metadata"), "Import metadata", accept = ".csv"),
          shiny::downloadButton(ns("export_template"), "Export Metadata Template"))),
      DT::DTOutput(ns("samples")), shiny::uiOutput(ns("sample_editor"))),
    fpq_v4_section("3", "Comparisons",
      "Define the reference and comparison conditions used for descriptive effect sizes.",
      shiny::fluidRow(
        shiny::column(3, shiny::textInput(ns("comparison_id"), "Comparison ID")),
        shiny::column(3, shiny::textInput(ns("comparison_label"), "Comparison label")),
        shiny::column(2, shiny::selectInput(ns("comparison_reference"),
          "Reference condition", choices = NULL)),
        shiny::column(2, shiny::selectInput(ns("comparison_target"),
          "Comparison condition", choices = NULL)),
        shiny::column(2, htmltools::tags$div(class = "fpq-v4-field-action",
          shiny::actionButton(ns("add_comparison"), "Add Comparison",
                              class = "btn-primary")))),
      DT::DTOutput(ns("comparisons")),
      shiny::actionButton(ns("remove_comparison"), "Remove Comparison"),
      shiny::actionButton(ns("reorder_comparison"), "Reorder Comparison")),
    htmltools::tags$div(class = "fpq-v4-form-footer",
      shiny::uiOutput(ns("advanced_demo_control")),
      shiny::actionButton(ns("save"), "Save Experimental Design",
                          class = "btn-primary fpq-v4-primary")))
}

fpq_analysis_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  design_ns <- shiny::NS("design")
  fpq_v4_section("4", "Analysis Settings",
    "Configure extraction tolerance, primary intensity strategy, and optional robustness analysis.",
    htmltools::tags$h3("Extraction mass tolerance"),
    htmltools::tags$p(class = "fpq-v4-muted",
      "Use the validated default or explicitly configure a stricter combined window."),
    shiny::radioButtons(design_ns("mass_tolerance_mode"), NULL,
      choices = c("Validated default: wider of 10 ppm and 0.002 Da" = "validated_default",
        "Custom strict combined window" = "custom_strict_combined"),
      selected = "validated_default"),
    shiny::conditionalPanel("input.mass_tolerance_mode == 'custom_strict_combined'",
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(design_ns("custom_ppm_tolerance"),
          "PPM tolerance", 10, min = .1, max = 100, step = .1)),
        shiny::column(6, shiny::numericInput(design_ns("custom_absolute_da_tolerance"),
          "Absolute m/z tolerance (Da, half-window)", .002,
          min = .000001, max = 1, step = .0001))), ns = design_ns),
    shiny::actionButton(design_ns("apply_mass_tolerance"), "Apply Mass Tolerance",
                        class = "btn-primary"),
    shiny::uiOutput(design_ns("mass_tolerance_status")),
    shiny::uiOutput(ns("content")))
}

fpq_analysis_settings_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    selection <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(state$configuration(),
      selection(fpq_jackknife_enabled(state$configuration())), ignoreInit = FALSE)
    output$content <- shiny::renderUI({
      config <- state$configuration(); design <- fpq_jackknife_design(config)
      method <- fpq_normalization_method(config)
      labels <- fpq_normalization_labels()
      method_cards <- lapply(names(labels), function(value)
        htmltools::tags$label(class = paste("fpq-v4-method-card",
          if (identical(method, value)) "selected" else ""),
          htmltools::tags$input(type = "radio", name = session$ns("method"),
            value = value, checked = if (identical(method, value)) "checked" else NULL),
          htmltools::tags$strong(labels[[value]]),
          htmltools::tags$span(fpq_normalization_descriptions()[[value]])))
      htmltools::tagList(
        htmltools::tags$h3("Primary intensity strategy"),
        htmltools::tags$div(id = session$ns("method"),
          class = "shiny-input-radiogroup fpq-v4-method-grid",
          role = "radiogroup", method_cards),
        htmltools::tags$hr(),
        htmltools::tags$div(class = "fpq-v4-jackknife",
          htmltools::tags$div(class = "fpq-v4-jackknife-heading",
            htmltools::tags$div(htmltools::tags$h3("Jackknife robustness analysis"),
              htmltools::tags$p(
                "Evaluate how strongly log2 fold changes depend on matched biological replicates.")),
            if (selection()) fpq_badge("PASS", "INCLUDED")),
          fpq_badge(if (design$eligible) "PASS" else "WARNING",
            if (design$eligible) "Eligible" else "Unavailable"),
          htmltools::tags$p(design$reason),
          if (!selection() && design$eligible)
            shiny::actionButton(session$ns("include"),
              "+ Include Jackknife Statistics", class = "btn-primary"),
          if (selection()) htmltools::tagList(
            htmltools::tags$p(htmltools::tags$strong("Resampling unit: "),
              htmltools::HTML("matched biological replicate pairs &mdash; never MSI pixels")),
            htmltools::tags$p(paste(design$n_pairs,
              "leave-one-pair-out estimates will be calculated.")),
            htmltools::tags$ul(
              htmltools::tags$li("Full-data log2FC and leave-one-out estimates"),
              htmltools::tags$li("Jackknife mean and classical standard error"),
              htmltools::tags$li("Minimum, maximum, range, and direction stability")),
            shiny::actionButton(session$ns("remove"),
              "Remove Jackknife Statistics"))),
        htmltools::tags$div(class = "fpq-v4-form-footer",
          shiny::actionButton(session$ns("save"), "Save Analysis Settings",
                              class = "btn-primary fpq-v4-primary")))
    })
    shiny::observeEvent(input$include, selection(TRUE))
    shiny::observeEvent(input$remove, selection(FALSE))
    shiny::observeEvent(input$save, {
      project <- state$project(); if (is.null(project)) return()
      method <- input$method %||% fpq_normalization_method(state$configuration())
      config <- fpq_set_normalization_method(state$configuration(), method)
      design <- fpq_jackknife_design(config)
      enabled <- selection() && isTRUE(design$eligible)
      config <- fpq_set_jackknife_enabled(config, enabled)
      config$schema_version <- config$schema_version %||% "1.0.0"
      config$dataset_id <- config$dataset_id %||% "dset-project-configuration"
      fpq_with_project_lock(project$root,
        fpq_persist_msi_configuration(project, config))
      preferences <- project$manifest$project_preferences %||% list()
      preferences$primary_normalization_method <-
        config$primary_normalization_method
      preferences$statistical_analyses <- config$statistical_analyses
      project <- fpq_save_project(project,
        list(project_preferences = preferences))
      state$project(project); state$configuration(config)
      state$dirty(FALSE); state$validations(list())
      shiny::showNotification("Analysis settings saved", type = "message")
    })
  })
}

fpq_dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fpq_v4_page_heading("Files and Columns",
      "Inspect and explicitly map quantitative target and annotation source columns."),
    fpq_v4_section("1", "Dataset identity and MSI files",
      "Register the full-pixel acquisition context and explicit sample-to-file assignments.",
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(ns("dataset_id"), "Dataset ID", "dset-project-neg")),
        shiny::column(4, shiny::textInput(ns("dataset_name"), "Dataset name", "Negative full-pixel dataset")),
        shiny::column(4, shiny::selectInput(ns("ion_mode"), "Ion mode",
          c("Negative" = "NEG", "Positive" = "POS")))),
      shiny::fluidRow(
        shiny::column(3, shiny::textInput(ns("exposure"), "Exposure label", "Single")),
        shiny::column(3, shiny::textInput(ns("matrix"), "Matrix", "9AA")),
        shiny::column(3, shiny::textInput(ns("pixel_size"), "Pixel size (um)", "40")),
        shiny::column(3, shiny::textInput(ns("coordinates"), "Expected pixels", "1600"))),
      shiny::textAreaInput(ns("pairs"),
        "Sample assignments: sample_id | imzML path | IBD path", rows = 6)),
    fpq_v4_section("2", "Aligned feature target list",
      "The target list provides feature identifiers and target m/z values.",
      shiny::textInput(ns("target_path"), "Aligned target-list path"),
      shiny::actionButton(ns("inspect_target"), "Inspect Target Table"),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("feature_id_column"), "Feature-ID column", NULL)),
        shiny::column(6, shiny::selectInput(ns("target_mz_column"), "Target m/z column", NULL))),
      shiny::uiOutput(ns("target_status")),
      htmltools::tags$details(htmltools::tags$summary("Preview target table"),
        DT::DTOutput(ns("target_preview")))),
    fpq_v4_section("3", "Putative annotation mappings",
      "Map every required MZmine annotation field explicitly.",
      shiny::textInput(ns("annotation_path"), "Annotated feature-list path"),
      shiny::actionButton(ns("inspect_annotation"), "Inspect Annotated Table"),
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("ann_feature_id"), "Feature ID", NULL, selectize = FALSE)),
        shiny::column(3, shiny::selectInput(ns("ann_mz"), "Annotation m/z", NULL, selectize = FALSE)),
        shiny::column(3, shiny::selectInput(ns("ann_name"), "Annotation name", NULL, selectize = FALSE)),
        shiny::column(3, shiny::selectInput(ns("ann_formula"), "Formula", NULL, selectize = FALSE))),
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("ann_adduct"), "Adduct", NULL, selectize = FALSE)),
        shiny::column(3, shiny::selectInput(ns("ann_score"), "Annotation score", NULL, selectize = FALSE)),
        shiny::column(3, shiny::selectInput(ns("ann_compound_score"), "Compound score", NULL, selectize = FALSE)),
        shiny::column(3, shiny::selectInput(ns("ann_database"), "Database", NULL, selectize = FALSE))),
      shiny::uiOutput(ns("annotation_status")),
      htmltools::tags$details(htmltools::tags$summary("Preview annotation table"),
        DT::DTOutput(ns("annotation_preview"))),
      htmltools::tags$div(class = "fpq-v4-form-footer",
        shiny::actionButton(ns("register"), "Save Technical Mappings",
                            class = "btn-primary fpq-v4-primary")),
      shiny::uiOutput(ns("registration_status"))))
}

fpq_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fpq_v4_page_heading("Validation",
      "Check whether the project is correctly configured and ready to run.",
      htmltools::tags$div(
        shiny::actionButton(ns("validate"), "Validate Project",
                            class = "btn-primary fpq-v4-primary"),
        htmltools::tags$details(class = "fpq-v4-actions-menu",
          htmltools::tags$summary("Additional checks"),
          shiny::actionButton(ns("files"), "Revalidate Files"),
          shiny::actionButton(ns("environment"), "Check Environment")))),
    fpq_v4_section(NULL, "Project readiness",
      "Each validation layer must be resolved before analysis can begin.",
      shiny::uiOutput(ns("layers"))),
    htmltools::tags$details(class = "fpq-v4-technical-details",
      htmltools::tags$summary("Technical validation details"),
      shiny::checkboxGroupInput(ns("statuses"), "Show",
        c("PASS", "WARNING", "FAIL", "NOT_EVALUABLE"),
        selected = c("PASS", "WARNING", "FAIL", "NOT_EVALUABLE"), inline = TRUE),
      DT::DTOutput(ns("records"))))
}

fpq_workflow_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fpq_v4_page_heading("Workflow",
      "Run the complete validated quantitative workflow from persisted project inputs."),
    shiny::uiOutput(ns("v4_analysis_summary")),
    shiny::uiOutput(ns("primary_action")),
    shiny::fluidRow(
      shiny::column(7, shiny::uiOutput(ns("current_run")),
        shiny::uiOutput(ns("progress")), shiny::uiOutput(ns("stages"))),
      shiny::column(5, shiny::uiOutput(ns("v4_outputs")))))
}

fpq_v4_workflow_companion_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$v4_analysis_summary <- shiny::renderUI({
      project <- state$project(); if (is.null(project)) return(NULL)
      config <- state$configuration()
      samples <- Filter(function(x) identical(x$inclusion_status %||% "included", "included"),
                        config$samples %||% list())
      comparisons <- Filter(function(x) isTRUE(x$enabled %||% TRUE),
                            config$comparisons %||% list())
      comparison <- if (length(comparisons)) comparisons[[1]]$comparison_label %||%
        comparisons[[1]]$comparison_id else "Not configured"
      validation <- state$validations()
      readiness <- if (!length(validation)) "Not validated" else
        switch(validation$readiness %||% "blocked", ready = "Ready",
          ready_with_warning = "Ready with warning",
          candidate_workflow_ready = "Ready", "Not ready")
      htmltools::tags$section(class = "fpq-v4-analysis-summary",
        htmltools::tags$h2("Analysis summary"),
        htmltools::tags$div(class = "fpq-v4-summary-grid",
          fpq_v4_metric("Project", project$manifest$project_name),
          fpq_v4_metric("Comparison", comparison),
          fpq_v4_metric("Samples", length(samples)),
          fpq_v4_metric("Normalization",
            fpq_normalization_labels()[[fpq_normalization_method(config)]]),
          fpq_v4_metric("Jackknife",
            if (fpq_jackknife_enabled(config)) "Included" else "Not included"),
          fpq_v4_metric("Validation", readiness)))
    })
    final <- shiny::reactivePoll(2000, session,
      checkFunc = function() fpq_results_project_refresh_token(state$project()),
      valueFunc = function() fpq_current_final_results_workbook(
        state$repository_root, state$project()))
    output$v4_outputs <- shiny::renderUI({
      current <- final()
      htmltools::tags$section(class = "fpq-v4-section fpq-v4-workflow-output",
        htmltools::tags$h2("Final workbook"),
        if (!isTRUE(current$available))
          htmltools::tags$p(class = "fpq-v4-muted",
            "The validated final workbook will appear here after completion.")
        else htmltools::tagList(
          fpq_badge("PASS", "Validated and ready"),
          htmltools::tags$p(basename(current$path)),
          shiny::downloadButton(session$ns("v4_download_workbook"),
            "Download Workbook", class = "btn-primary"),
          shiny::actionButton(session$ns("v4_open_output"),
            "Open Project Folder"),
          if ("Jackknife_Log2FC" %in% (current$validation$sheets %||% character()))
            htmltools::tags$p(class = "fpq-v4-notice success",
              "Includes Jackknife_Log2FC robustness results.")))
    })
    output$v4_download_workbook <- shiny::downloadHandler(
      filename = function() {
        current <- final(); if (isTRUE(current$available)) basename(current$path)
        else "MALDIPixelQuant_Final_Results_Unavailable.xlsx"
      }, content = function(file) {
        current <- final()
        if (!isTRUE(current$available) || !file.copy(current$path, file, overwrite = TRUE))
          fpq_stop("results.download_unavailable", "Validated workbook unavailable.",
                   "infrastructure")
      })
    shiny::observeEvent(input$v4_open_output, {
      current <- final()
      if (isTRUE(current$available))
        try(shell.exec(fpq_project_paths(state$project()$root)$root), silent = TRUE)
    })
  })
}

fpq_runs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    fpq_v4_page_heading("Run History",
      "Review completed, active, and failed analyses and resume recoverable runs."),
    shiny::uiOutput(ns("v4_cards")),
    shiny::uiOutput(ns("review")), shiny::uiOutput(ns("start_control")),
    shiny::uiOutput(ns("cancel_control")), shiny::uiOutput(ns("monitor")),
    htmltools::tags$details(class = "fpq-v4-technical-details",
      htmltools::tags$summary("Technical run records"),
      shiny::uiOutput(ns("advanced_request_controls")),
      htmltools::tags$h3("Workflow records"), DT::DTOutput(ns("workflow_table")),
      htmltools::tags$h3("Persisted stage runs"), DT::DTOutput(ns("runs_table")),
      shiny::uiOutput(ns("run_details"))))
}

fpq_v4_runs_companion_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    action_button <- function(label, action, workflow_id, class = "btn-default") {
      payload <- jsonlite::toJSON(list(action = action, workflow_id = workflow_id),
        auto_unbox = TRUE)
      htmltools::tags$button(type = "button", class = paste("btn", class),
        onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'});",
          session$ns("run_action"), payload), label)
    }
    output$v4_cards <- shiny::renderUI({
      shiny::invalidateLater(2000, session)
      records <- fpq_list_workflow_records(state$project())
      if (!length(records)) return(fpq_empty_state("No workflow runs yet",
        "Validated analyses started from Workflow will appear here."))
      records <- rev(records)
      htmltools::tags$div(class = "fpq-v4-run-list", lapply(records, function(x) {
        method <- x$normalization$primary_normalization_method %||%
          x$display_metadata$normalization$primary_normalization_method %||% "spectrum_sum"
        key <- gsub("[^A-Za-z0-9_-]", "-", x$workflow_id)
        download_id <- paste0("download_", key)
        workbook <- x$final_workbook$path %||% ""
        if (identical(x$status, "completed") && nzchar(workbook) && file.exists(workbook)) {
          local({
            path <- workbook; id <- download_id
            output[[id]] <- shiny::downloadHandler(filename = function() basename(path),
              content = function(file) file.copy(path, file, overwrite = TRUE))
          })
        }
        resumable <- identical(x$status, "failed") &&
          (x$failure$stage %||% "") %in% c("summarizing", "annotating",
            "combined_export", "final_workbook", "validating_workbook")
        htmltools::tags$section(class = "fpq-v4-run-card",
          htmltools::tags$div(
            htmltools::tags$strong(format(as.POSIXct(x$created_at %||% x$updated_at,
              tz = "UTC"), "%d %b %Y %H:%M")),
            htmltools::tags$span(x$workflow_id)),
          fpq_badge(if (identical(x$status, "completed")) "PASS" else
            if (identical(x$status, "failed")) "FAIL" else "WARNING",
            tools::toTitleCase(x$status %||% "unknown")),
          htmltools::tags$dl(
            htmltools::tags$dt("Normalization"),
            htmltools::tags$dd(fpq_normalization_labels()[[method]] %||% method),
            htmltools::tags$dt("Jackknife"), htmltools::tags$dd(
              if (isTRUE(x$display_metadata$jackknife$enabled)) "Included" else "Not included"),
            htmltools::tags$dt("Output"), htmltools::tags$dd(
              if (identical(x$status, "completed")) "Final workbook available" else "Not available")),
          htmltools::tags$div(class = "fpq-v4-run-actions",
            if (identical(x$status, "completed") && nzchar(workbook) && file.exists(workbook))
              shiny::downloadButton(session$ns(download_id), "Download Workbook"),
            action_button(if (identical(x$status, "failed"))
              "View Failure Details" else "View Run Details", "view", x$workflow_id),
            if (nzchar(x$output_folder %||% "") && dir.exists(x$output_folder))
              action_button("Open Output Folder", "open", x$workflow_id),
            if (resumable) action_button("Resume Run", "resume", x$workflow_id,
              "btn-primary")))
      }))
    })
    shiny::observeEvent(input$run_action, {
      request <- input$run_action
      if (!is.list(request) || !nzchar(request$workflow_id %||% "")) return()
      records <- fpq_list_workflow_records(state$project())
      matches <- Filter(function(x) identical(x$workflow_id, request$workflow_id), records)
      if (!length(matches)) return()
      workflow <- matches[[1]]
      if (identical(request$action, "open")) {
        if (dir.exists(workflow$output_folder %||% ""))
          try(shell.exec(workflow$output_folder), silent = TRUE)
        return()
      }
      state$workflow_id(workflow$workflow_id)
      if (identical(request$action, "resume")) {
        stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
        process <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
          c(file.path(state$repository_root, "tools", "fpq_full_workflow_worker.R"),
            state$project()$root, workflow$workflow_id, state$repository_root),
          wd = state$repository_root,
          stdout = file.path(workflow$output_folder,
            paste0("workflow-resume-", stamp, ".out")),
          stderr = file.path(workflow$output_folder,
            paste0("workflow-resume-", stamp, ".err")),
          cleanup = FALSE, supervise = FALSE)
        state$workflow_process(process)
        shiny::showNotification("Resume started. Completed stages will be reused.",
          type = "message", duration = 6)
      }
      fpq_v4_navigate(session$rootScope(), "workflow")
    })
  })
}

fpq_about_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tags$section(class = "fpq-v4-about fpq-v4-surface",
    htmltools::tags$img(class = "fpq-v4-about-logo",
      src = "branding/MALDI-PixelQuant-Laser-Spectrum-Logo.png",
      alt = "MALDIPixelQuant"),
    htmltools::tags$h1("MALDIPixelQuant"),
    htmltools::tags$p(class = "fpq-v4-lead", "Quantitative MALDI-MSI analysis"),
    htmltools::tags$p(
      "MALDIPixelQuant is a graphical user interface for quantitative analysis of MALDI mass spectrometry imaging data, including pixel-level quantification, biological-replicate statistics, and integration of putative metabolite annotations."),
    htmltools::tags$dl(class = "fpq-v4-version-list",
      htmltools::tags$dt("Application version"), htmltools::tags$dd("4.0.0-dev"),
      htmltools::tags$dt("Package version"), htmltools::tags$dd(
        as.character(utils::packageVersion("FullPixelMSIQuant"))),
      htmltools::tags$dt("Scientific core"), htmltools::tags$dd("1.0.0")),
    htmltools::tags$div(class = "fpq-v4-home-actions",
      htmltools::tags$a(class = "btn btn-primary", target = "_blank",
        href = "https://github.com/WesAld/FullPixel-MSI-Quant", "GitHub Repository"),
      htmltools::tags$button(class = "btn fpq-v4-secondary", disabled = "disabled",
        "View Documentation"),
      htmltools::tags$button(class = "btn fpq-v4-secondary", disabled = "disabled",
        "Release Notes")))
}
