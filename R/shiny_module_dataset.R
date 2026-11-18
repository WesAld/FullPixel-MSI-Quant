fpq_dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$h2("Dataset Registration"),
    htmltools::tags$p("The aligned MZmine list defines feature identifiers and target m/z values. Original centroided full-pixel imzML/IBD data provide quantitative pixel-level signal."),
    shiny::textInput(ns("dataset_id"), "Dataset ID", "dset-project-neg"),
    shiny::textInput(ns("dataset_name"), "Dataset name", "Negative full-pixel dataset"),
    shiny::selectInput(ns("ion_mode"), "Ion mode", c("Negative" = "NEG",
      "Positive - registration only" = "POS")),
    shiny::textInput(ns("exposure"), "Exposure label", "Single"),
    shiny::textInput(ns("matrix"), "Matrix metadata", "9AA"),
    shiny::textInput(ns("pixel_size"), "Pixel size (um)", "40"),
    shiny::textInput(ns("coordinates"), "Expected coordinate count", "1600"),
    htmltools::tags$h3("Original full-pixel MALDI-MSI files"),
    htmltools::tags$p("One line per sample: sample_id | imzML absolute path | IBD absolute path"),
    shiny::textAreaInput(ns("pairs"), "Explicit sample-to-file assignments", rows = 8),
    htmltools::tags$h3("Aligned MZmine feature target list"),
    shiny::textInput(ns("target_path"), "Aligned target-list path"),
    shiny::actionButton(ns("inspect_target"), "Inspect Target Table"),
    shiny::selectInput(ns("feature_id_column"), "Feature-ID column", choices = NULL),
    shiny::selectInput(ns("target_mz_column"), "Target m/z column", choices = NULL),
    shiny::uiOutput(ns("target_status")),
    DT::DTOutput(ns("target_preview")),
    htmltools::tags$h3("Annotated MZmine feature list - optional for extraction"),
    shiny::textInput(ns("annotation_path"), "Annotated feature-list path"),
    shiny::actionButton(ns("inspect_annotation"), "Inspect Annotated Table"),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("ann_feature_id"), "Annotation feature-ID column (required)", choices = NULL, selectize = FALSE)),
      shiny::column(3, shiny::selectInput(ns("ann_mz"), "Annotation m/z column (required)", choices = NULL, selectize = FALSE)),
      shiny::column(3, shiny::selectInput(ns("ann_name"), "Annotation-name column (required)", choices = NULL, selectize = FALSE)),
      shiny::column(3, shiny::selectInput(ns("ann_formula"), "Molecular-formula column (required)", choices = NULL, selectize = FALSE))),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("ann_adduct"), "Adduct column (required)", choices = NULL, selectize = FALSE)),
      shiny::column(3, shiny::selectInput(ns("ann_score"), "Annotation-score column (required)", choices = NULL, selectize = FALSE)),
      shiny::column(3, shiny::selectInput(ns("ann_compound_score"), "Compound-score column (required)", choices = NULL, selectize = FALSE)),
      shiny::column(3, shiny::selectInput(ns("ann_database"), "Database column (required)", choices = NULL, selectize = FALSE))),
    shiny::uiOutput(ns("annotation_status")),
    DT::DTOutput(ns("annotation_preview")),
    shiny::actionButton(ns("register"), "Register Dataset", class = "btn-primary"),
    shiny::uiOutput(ns("registration_status")))
}

fpq_parse_pair_assignments <- function(text) {
  lines <- trimws(strsplit(text %||% "", "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  lapply(lines, function(line) {
    fields <- trimws(strsplit(line, "|", fixed = TRUE)[[1]])
    if (length(fields) != 3L)
      fpq_stop("ui.pair_format", "Every file-assignment line requires sample ID, imzML and IBD.")
    list(sample_id = fields[[1]], imzml_path = fields[[2]], ibd_path = fields[[3]])
  })
}

fpq_inspect_target_table <- function(path, feature_id_column = NULL, mz_column = NULL) {
  preview <- fpq_preview_csv(path, 25L)
  result <- list(columns = preview$columns, preview = preview$data, records = list())
  if (!is.null(feature_id_column) && nzchar(feature_id_column)) {
    values <- preview$data[[feature_id_column]]
    if (any(is.na(values) | !nzchar(as.character(values))))
      result$records <- c(result$records, list(fpq_validation_record(
        "target.feature_id.empty", "FAIL", "Feature-ID preview contains empty values.")))
    if (anyDuplicated(values))
      result$records <- c(result$records, list(fpq_validation_record(
        "target.feature_id.duplicate", "FAIL", "Feature-ID preview contains duplicates.")))
  }
  if (!is.null(mz_column) && nzchar(mz_column)) {
    values <- suppressWarnings(as.numeric(preview$data[[mz_column]]))
    if (any(!is.finite(values)))
      result$records <- c(result$records, list(fpq_validation_record(
        "target.mz.numeric", "FAIL", "Target m/z values must be finite numeric values.")))
  }
  result
}

fpq_inspect_annotation_table <- function(path, mappings = list()) {
  if (!file.exists(path)) fpq_stop("annotation.file_missing", "Annotated MZmine table is unavailable.")
  data <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                          na.strings = character(), encoding = "UTF-8")
  roles <- c("feature_id", "mz", "annotation_name", "formula", "adduct",
             "score", "compound_score", "database")
  records <- list()
  failure <- function(id, message, observed = NULL, expected = NULL,
                      consequence = "Script 04 cannot safely integrate annotation metadata.",
                      remediation = "Correct the identified source column or mapping, then inspect the table again.") {
    fpq_validation_record(id, "FAIL", message, observed = observed,
      expected = expected, consequence = consequence, remediation = remediation)
  }
  selected <- unlist(mappings[roles], use.names = FALSE)
  if (length(selected) != length(roles) || any(!nzchar(selected)))
    records <- c(records, list(failure("annotation.mapping.complete",
      "Every required annotation role must be mapped explicitly.",
      remediation = "Select a source column for every control labelled required.")))
  if (length(selected) && anyDuplicated(selected))
    records <- c(records, list(failure("annotation.mapping.ambiguous",
      "A source column cannot be assigned to multiple incompatible annotation roles.",
      remediation = "Assign a distinct scientifically appropriate source column to each required role.")))
  missing <- setdiff(selected[nzchar(selected)], names(data))
  if (length(missing))
    records <- c(records, list(failure("annotation.mapping.missing",
      paste("Mapped columns are absent:", paste(missing, collapse = ", ")),
      observed = paste(missing, collapse = ", "), expected = "Columns present in the inspected file",
      remediation = "Restore the expected file or explicitly remap using columns present in this file.")))
  if (!length(missing) && length(selected) == length(roles) && all(nzchar(selected))) {
    ids <- as.character(data[[mappings$feature_id]])
    empty_ids <- is.na(ids) | !nzchar(trimws(ids))
    duplicate_ids <- duplicated(ids) & !empty_ids
    if (any(empty_ids) || any(duplicate_ids))
      records <- c(records, list(failure("annotation.feature_id.unique",
        "Feature identifiers must be nonempty and unique.",
        observed = paste("empty", sum(empty_ids), "duplicates",
                         sum(duplicate_ids)),
        expected = "0 empty and 0 duplicate identifiers")))
    for (role in c("mz", "score", "compound_score")) {
      source <- data[[mappings[[role]]]]
      raw <- as.character(source)
      nonblank <- !is.na(source) & nzchar(trimws(raw))
      numeric <- suppressWarnings(as.numeric(raw[nonblank]))
      if (any(!is.finite(numeric)))
        records <- c(records, list(failure(
          paste0("annotation.", role, ".numeric"),
          paste(role, "must contain finite numeric values when nonblank."),
          observed = sum(!is.finite(numeric)), expected = 0)))
    }
    score_raw <- as.character(data[[mappings$score]])
    compound_raw <- as.character(data[[mappings$compound_score]])
    paired <- !is.na(score_raw) & !is.na(compound_raw) &
      nzchar(trimws(score_raw)) & nzchar(trimws(compound_raw))
    score_values <- suppressWarnings(as.numeric(score_raw[paired]))
    compound_values <- suppressWarnings(as.numeric(compound_raw[paired]))
    disagreement <- sum(is.finite(score_values) & is.finite(compound_values) &
                          abs(score_values - compound_values) > 1e-6)
    if (disagreement)
      records <- c(records, list(failure("annotation.score.consistency",
        "Annotation score and compound score disagree beyond the frozen tolerance.",
        observed = disagreement, expected = 0,
        remediation = "Use a source export whose paired score columns agree within 1e-6.")))
    adducts <- trimws(as.character(data[[mappings$adduct]]))
    unexpected_adducts <- unique(adducts[!is.na(adducts) & nzchar(adducts) &
                                          adducts != "[M-H]-"])
    if (length(unexpected_adducts))
      records <- c(records, list(failure("annotation.adduct",
        "Unsupported or malformed nonblank adduct values were detected.",
        observed = paste(unexpected_adducts, collapse = ", "), expected = "[M-H]-",
        remediation = "Correct malformed adduct text or use the validated negative-mode [M-H]- export.")))
  }
  list(columns = names(data), preview = utils::head(data, 10L),
    rows = nrow(data), column_count = ncol(data), bytes = as.integer(file.info(path)$size),
    sha256 = fpq_hash_file(path), records = records)
}

fpq_v4_dataset_sync_snapshot <- function(config, mode = "NEG") {
  mode <- toupper(mode %||% "NEG")
  samples <- Filter(function(x) identical(toupper(x$ion_mode %||% ""), mode),
    config$samples %||% list())
  mzmine <- (config$mzmine_registrations %||% list())[[mode]] %||% list()
  list(mode = mode, samples = samples,
    pair_lines = vapply(samples, function(x) paste(
      x$sample_id %||% x$sample_label %||% "sample",
      x$imzml_path %||% "", x$ibd_path %||% "", sep = " | "), character(1)),
    mzmine = mzmine, mzmine_path = mzmine$path %||% "",
    feature_id = mzmine$mappings$feature_id %||% "id",
    target_mz = mzmine$mappings$mz %||% "mz",
    matrix = if (identical(mode, "POS")) config$positive_matrix %||% "DHB"
      else config$negative_matrix %||% "9AA")
}

fpq_dataset_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    inspection <- shiny::reactiveVal(NULL)
    annotation_inspection <- shiny::reactiveVal(NULL)
    registration <- shiny::reactiveVal(NULL)
    sync_from_authoritative <- function(mode = input$ion_mode %||% "NEG") {
      config <- state$configuration()
      snapshot <- fpq_v4_dataset_sync_snapshot(config, mode)
      mode <- snapshot$mode; pair_lines <- snapshot$pair_lines
      mzmine_path <- snapshot$mzmine_path
      feature_id <- snapshot$feature_id; target_mz <- snapshot$target_mz

      shiny::updateTextInput(session, "dataset_id", value = paste0(
        config$dataset_id %||% "dset-project-configuration", "-", tolower(mode)))
      shiny::updateTextInput(session, "dataset_name",
        value = paste(tools::toTitleCase(tolower(mode)), "registered full-pixel dataset"))
      shiny::updateTextInput(session, "exposure", value = config$exposure_label %||% "Single")
      shiny::updateTextInput(session, "matrix", value = snapshot$matrix)
      shiny::updateTextInput(session, "pixel_size",
        value = as.character(config$pixel_size_um %||% 40))
      shiny::updateTextInput(session, "coordinates",
        value = as.character(config$expected_coordinate_count %||% 1600))
      shiny::updateTextAreaInput(session, "pairs", value = paste(pair_lines, collapse = "\n"))
      shiny::updateTextInput(session, "target_path", value = mzmine_path)
      shiny::updateTextInput(session, "annotation_path", value = mzmine_path)

      if (nzchar(mzmine_path) && file.exists(mzmine_path)) {
        target <- fpq_inspect_target_table(mzmine_path, feature_id, target_mz)
        inspection(target)
        shiny::updateSelectInput(session, "feature_id_column",
          choices = target$columns, selected = feature_id)
        shiny::updateSelectInput(session, "target_mz_column",
          choices = target$columns, selected = target_mz)
        annotation_defaults <- list(feature_id = feature_id, mz = target_mz,
          annotation_name = "preferred_annotation:compound_name",
          formula = "preferred_annotation:mol_formula",
          adduct = "preferred_annotation:adduct", score = "preferred_annotation:score",
          compound_score = "preferred_annotation:compound_annotation_score",
          database = "preferred_annotation:database_name")
        annotation_inspection(fpq_inspect_annotation_table(mzmine_path,
          annotation_defaults))
        ids <- c(feature_id="ann_feature_id", mz="ann_mz",
          annotation_name="ann_name", formula="ann_formula", adduct="ann_adduct",
          score="ann_score", compound_score="ann_compound_score",
          database="ann_database")
        for (role in names(ids)) shiny::updateSelectInput(session, ids[[role]],
          choices = c("Select explicitly" = "", target$columns),
          selected = annotation_defaults[[role]])
      } else {
        inspection(NULL); annotation_inspection(NULL)
      }
      invisible(NULL)
    }
    shiny::observeEvent(state$configuration(), {
      config <- state$configuration()
      modes <- config$enabled_ion_modes %||% unique(vapply(
        config$samples %||% list(), function(x) x$ion_mode %||% "", character(1)))
      modes <- intersect(c("NEG", "POS"), toupper(modes[nzchar(modes)]))
      if (!length(modes)) modes <- toupper(config$ion_mode %||% "NEG")
      selected <- if ((input$ion_mode %||% "") %in% modes) input$ion_mode else modes[[1]]
      shiny::updateSelectInput(session, "ion_mode",
        choices = stats::setNames(modes,
          paste(tools::toTitleCase(tolower(modes)), "mode")), selected = selected)
      sync_from_authoritative(selected)
    }, ignoreInit = FALSE)
    shiny::observeEvent(input$ion_mode, sync_from_authoritative(input$ion_mode),
      ignoreInit = TRUE)
    shiny::observeEvent(input$inspect_target, {
      tryCatch({
        x <- fpq_inspect_target_table(input$target_path)
        inspection(x)
        shiny::updateSelectInput(session, "feature_id_column", choices = x$columns)
        shiny::updateSelectInput(session, "target_mz_column", choices = x$columns)
      }, error = function(e) state$last_error(e))
    })
    shiny::observeEvent(list(input$feature_id_column, input$target_mz_column), {
      if (file.exists(input$target_path))
        inspection(fpq_inspect_target_table(input$target_path,
          input$feature_id_column, input$target_mz_column))
    }, ignoreInit = TRUE)
    annotation_mappings <- function() list(feature_id=input$ann_feature_id %||% "",
      mz=input$ann_mz %||% "", annotation_name=input$ann_name %||% "",
      formula=input$ann_formula %||% "", adduct=input$ann_adduct %||% "",
      score=input$ann_score %||% "", compound_score=input$ann_compound_score %||% "",
      database=input$ann_database %||% "")
    inspect_annotation <- function() {
      x <- fpq_inspect_annotation_table(input$annotation_path, annotation_mappings())
      annotation_inspection(x)
      defaults <- c(feature_id="id", mz="mz",
        annotation_name="preferred_annotation:compound_name",
        formula="preferred_annotation:mol_formula",
        adduct="preferred_annotation:adduct", score="preferred_annotation:score",
        compound_score="preferred_annotation:compound_annotation_score",
        database="preferred_annotation:database_name")
      ids <- c(feature_id="ann_feature_id",mz="ann_mz",annotation_name="ann_name",
        formula="ann_formula",adduct="ann_adduct",score="ann_score",
        compound_score="ann_compound_score",database="ann_database")
      current <- annotation_mappings()
      for (role in names(ids)) {
        current_value <- current[[role]] %||% ""
        choices <- c("Select explicitly" = "", x$columns)
        if (nzchar(current_value) && !current_value %in% x$columns)
          choices <- c(choices, stats::setNames(current_value,
            paste0(current_value, " (missing from inspected file)")))
        selected <- if (defaults[[role]] %in% x$columns) defaults[[role]]
          else current_value
        shiny::updateSelectInput(session, ids[[role]], choices = choices,
                                 selected = selected)
      }
    }
    shiny::observeEvent(input$inspect_annotation, {
      tryCatch(inspect_annotation(), error=function(e) state$last_error(e))
    })
    shiny::observeEvent(list(input$ann_feature_id,input$ann_mz,input$ann_name,
      input$ann_formula,input$ann_adduct,input$ann_score,input$ann_compound_score,
      input$ann_database), {
        if (file.exists(input$annotation_path))
          annotation_inspection(fpq_inspect_annotation_table(
            input$annotation_path, annotation_mappings()))
      }, ignoreInit=TRUE)
    shiny::observe({
      x <- annotation_inspection()
      if (is.null(x)) return()
      failures <- Filter(function(r) identical(r$status, "FAIL"), x$records)
      state$annotation_mapping_validation(list(
        status = if (length(failures)) "FAIL" else "PASS",
        message = if (length(failures))
          "Annotation mapping or source validation is blocking Script 04."
        else "Annotation mappings and inspected source values pass.",
        records = x$records,
        source_hash = x$sha256,
        mappings = annotation_mappings()
      ))
    })
    shiny::observe({
      project <- state$project()
      if (is.null(project) || nzchar(input$annotation_path %||% "")) return()
      candidates <- Filter(function(x) identical(x$artifact_type,"annotated_feature_list") &&
        identical(x$active_status,"active"), fpq_list_artifacts(project))
      if (length(candidates)) shiny::updateTextInput(session,"annotation_path",
        value=candidates[[1]]$path)
    })
    output$target_preview <- DT::renderDT({
      x <- inspection(); if (is.null(x)) return(data.frame())
      DT::datatable(x$preview, rownames = FALSE, options = list(pageLength = 10,
                                                                scrollX = TRUE))
    })
    output$target_status <- shiny::renderUI({
      x <- inspection()
      if (is.null(x)) return(fpq_empty_state("Target table not inspected",
        "Select a CSV and inspect its header before mapping columns."))
      failures <- Filter(function(r) identical(r$status, "FAIL"), x$records)
      htmltools::tags$div(fpq_badge(if (length(failures)) "FAIL" else "PASS",
        if (length(failures)) "Column mapping requires correction" else "Preview mapping valid"),
        lapply(x$records, function(r) htmltools::tags$p(r$message)))
    })
    output$annotation_preview <- DT::renderDT({
      x <- annotation_inspection(); if (is.null(x)) return(data.frame())
      DT::datatable(x$preview, rownames=FALSE,
        options=list(pageLength=5,scrollX=TRUE,lengthChange=FALSE))
    })
    output$annotation_status <- shiny::renderUI({
      x <- annotation_inspection()
      if (is.null(x)) return(fpq_empty_state("Annotated table not inspected",
        "Inspect the registered table before configuring annotation integration."))
      failures <- Filter(function(r) identical(r$status,"FAIL"),x$records)
      htmltools::tags$div(
        fpq_badge(if(length(failures))"FAIL" else "PASS",
          if(length(failures))"Annotation mapping blocked" else "Annotation mapping valid"),
        htmltools::tags$dl(
          htmltools::tags$dt("Exact path"),htmltools::tags$dd(input$annotation_path),
          htmltools::tags$dt("Filename"),htmltools::tags$dd(basename(input$annotation_path)),
          htmltools::tags$dt("File size"),htmltools::tags$dd(x$bytes),
          htmltools::tags$dt("SHA-256"),htmltools::tags$dd(x$sha256),
          htmltools::tags$dt("Dimensions"),htmltools::tags$dd(paste(x$rows,"rows x",x$column_count,"columns")),
          htmltools::tags$dt("Registration status"),htmltools::tags$dd("Registered external input")),
        lapply(x$records,function(r) htmltools::tags$section(
          class = "fpq-validation-error", role = "alert",
          `data-check-id` = r$check_id,
          htmltools::tags$h4(r$check_id),
          htmltools::tags$p(htmltools::tags$strong("Message: "), r$message),
          htmltools::tags$p(htmltools::tags$strong("Blocking: "),
                            if (isTRUE(r$blocking_status)) "Yes" else "No"),
          htmltools::tags$p(htmltools::tags$strong("Remediation: "),
                            r$remediation %||% "Correct the input and inspect again."))))
    })
    shiny::observeEvent(input$register, {
      tryCatch({
        project <- state$project()
        if (is.null(project)) fpq_stop("ui.project_required", "Open a project first.")
        mapping_state <- state$annotation_mapping_validation()
        if (nzchar(input$annotation_path %||% "") &&
            !identical(mapping_state$status, "PASS"))
          fpq_stop("annotation.mapping_blocked",
            "Annotated-list registration is blocked until every mapping and source validation passes.",
            "input")
        config <- state$configuration()
        mode <- toupper(input$ion_mode %||% "NEG")
        authoritative <- (config$mzmine_registrations %||% list())[[mode]]
        if (is.null(authoritative) || !identical(
            normalizePath(authoritative$path, winslash = "/", mustWork = FALSE),
            normalizePath(input$target_path, winslash = "/", mustWork = FALSE)))
          fpq_stop("dataset.mapping_source_mismatch",
            "Technical mappings must use the MZmine file registered for the selected ion mode.",
            "input")
        mappings <- config$technical_column_mappings %||% list()
        mappings[[mode]] <- list(feature_id = input$feature_id_column,
          target_mz = input$target_mz_column, annotation = annotation_mappings(),
          source_path = authoritative$path,
          source_sha256 = authoritative$sha256 %||% NULL)
        config$technical_column_mappings <- mappings
        fpq_persist_msi_configuration(project, config)
        state$configuration(config); state$dirty(FALSE)
        registration(list(status = "PASS", message = paste(mode,
          "technical mappings saved; MSI and MZmine registrations remain authoritative.")))
      }, error = function(e) {
        state$last_error(e); registration(list(status = "FAIL", message = conditionMessage(e)))
      })
    })
    output$registration_status <- shiny::renderUI({
      x <- registration(); if (is.null(x)) return(NULL)
      htmltools::tags$div(fpq_badge(x$status), htmltools::tags$p(x$message))
    })
  })
}
