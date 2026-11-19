fpq_result_export_method_code <- function(workflow) {
  method <- workflow$normalization$primary_normalization_method %||%
    workflow$display_metadata$normalization$primary_normalization_method %||%
    workflow$primary_normalization_method %||% ""
  labels <- c(raw = "RAW", spectrum_sum = "TIC", rms = "RMS", median = "MEDIAN")
  label <- unname(labels[[method]])
  if (is.null(label))
    fpq_stop("result_export.method_unsupported",
      paste("The completed run has an unsupported normalization method:", method),
      "validation")
  list(internal = method, filename = label)
}

fpq_result_export_filename <- function(project, workflow, alternate_filename = NULL) {
  if (!is.null(alternate_filename) && nzchar(trimws(alternate_filename))) {
    stem <- tools::file_path_sans_ext(basename(trimws(alternate_filename)))
    return(paste0(fpq_sanitize_project_filename(stem), ".xlsx"))
  }
  method <- fpq_result_export_method_code(workflow)$filename
  paste0(fpq_sanitize_project_filename(project$manifest$project_name), "_",
         method, "_MALDIPixelQuant_Results.xlsx")
}

fpq_result_export_eligibility <- function(project, workflow_id) {
  fail <- function(code, message) list(eligible = FALSE, code = code, reason = message)
  if (is.null(project) || !nzchar(workflow_id %||% ""))
    return(fail("result_export.run_required", "Select a completed workflow run."))
  workflows <- fpq_list_workflow_records(project)
  matches <- Filter(function(x) identical(x$workflow_id, workflow_id), workflows)
  if (length(matches) != 1L)
    return(fail("result_export.run_not_found", "The selected workflow run was not found."))
  workflow <- matches[[1L]]
  if (!identical(workflow$status, "completed"))
    return(fail("result_export.run_incomplete", "Only completed workflow runs can be frozen."))
  workbook <- workflow$final_workbook %||% list()
  artifact_id <- workbook$artifact_id %||% ""
  if (!nzchar(artifact_id))
    return(fail("result_export.artifact_missing", "The run has no final-workbook artifact."))
  artifact <- tryCatch(fpq_inspect_artifact(project, artifact_id), error = function(e) NULL)
  if (is.null(artifact) ||
      !identical(artifact$artifact_type, "final_results_workbook") ||
      !identical(artifact$validation_status, "PASS"))
    return(fail("result_export.artifact_not_validated",
      "The final-workbook artifact is not validated."))
  if (!identical(artifact$producing_run_id %||% "", workflow_id))
    return(fail("result_export.run_artifact_mismatch",
      "The final-workbook artifact does not belong to the selected run."))
  source <- artifact$path %||% workbook$path %||% ""
  if (!nzchar(source) || !file.exists(source))
    return(fail("result_export.source_missing", "The validated workbook is unavailable."))
  expected <- toupper(artifact$sha256 %||% "")
  recorded <- toupper(workbook$sha256 %||% "")
  actual <- tryCatch(toupper(fpq_hash_file(source)), error = function(e) "")
  if (!nzchar(expected) || !identical(actual, expected) ||
      (nzchar(recorded) && !identical(recorded, expected)))
    return(fail("result_export.hash_mismatch",
      "The final workbook does not match its validated SHA-256 record."))
  method <- tryCatch(fpq_result_export_method_code(workflow), error = function(e) NULL)
  if (is.null(method))
    return(fail("result_export.method_unsupported",
      "The selected run has an unsupported normalization method."))
  list(eligible = TRUE, code = "result_export.ready", reason = "Ready to freeze",
       workflow = workflow, artifact = artifact, source = source,
       sha256 = expected, method = method)
}

fpq_repository_commit <- function(repository_root) {
  if (is.null(repository_root) || !dir.exists(repository_root)) return(NA_character_)
  root <- normalizePath(repository_root, winslash = "/")
  git_marker <- file.path(root, ".git")
  if (!file.exists(git_marker) && !dir.exists(git_marker)) return(NA_character_)
  value <- tryCatch(suppressWarnings(system2("git",
    c("-c", shQuote(paste0("safe.directory=", root)),
      "-C", shQuote(root), "rev-parse", "HEAD"),
    stdout = TRUE, stderr = FALSE)), error = function(e) character())
  if (length(value) == 1L && grepl("^[0-9a-fA-F]{40}$", value)) value else NA_character_
}

fpq_export_validated_result <- function(project, workflow_id,
                                        destination_directory,
                                        alternate_filename = NULL,
                                        repository_root = NULL,
                                        copy_file = file.copy,
                                        hash_file = fpq_hash_file,
                                        directory_writable = function(path)
                                          file.access(path, 2L) == 0L) {
  if (is.null(destination_directory) ||
      !nzchar(trimws(destination_directory %||% "")))
    return(list(status = "cancelled", exported = FALSE))
  eligibility <- fpq_result_export_eligibility(project, workflow_id)
  if (!isTRUE(eligibility$eligible))
    fpq_stop(eligibility$code, eligibility$reason, "validation")
  destination_directory <- normalizePath(destination_directory, winslash = "/",
                                          mustWork = FALSE)
  if (!dir.exists(destination_directory))
    fpq_stop("result_export.destination_missing",
      "The selected destination folder does not exist.", "configuration")
  if (!isTRUE(directory_writable(destination_directory)))
    fpq_stop("result_export.destination_not_writable",
      "The selected destination folder is not writable.", "configuration")
  filename <- fpq_result_export_filename(project, eligibility$workflow,
                                          alternate_filename)
  destination <- file.path(destination_directory, filename)
  if (file.exists(destination))
    fpq_stop("result_export.destination_exists",
      paste("A file named", filename,
            "already exists. Choose another folder or enter an alternate filename."),
      "configuration")

  token <- gsub("[^A-Za-z0-9]", "", fpq_id("freeze"))
  temporary <- file.path(destination_directory,
    paste0(".", filename, ".", token, ".tmp"))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  copied <- tryCatch(copy_file(eligibility$source, temporary, overwrite = FALSE,
                               copy.date = TRUE), error = function(e) FALSE)
  if (!isTRUE(copied))
    fpq_stop("result_export.copy_failed", "The validated workbook could not be copied.",
             "infrastructure")
  temporary_hash <- toupper(hash_file(temporary))
  if (!identical(temporary_hash, eligibility$sha256))
    fpq_stop("result_export.copy_hash_mismatch",
      "The copied workbook failed SHA-256 verification.", "infrastructure")
  if (file.exists(destination) || !file.rename(temporary, destination))
    fpq_stop("result_export.publish_failed",
      "The export could not be published without overwriting an existing file.",
      "infrastructure")
  destination_hash <- toupper(hash_file(destination))
  if (!identical(destination_hash, eligibility$sha256)) {
    unlink(destination, force = TRUE)
    fpq_stop("result_export.destination_hash_mismatch",
      "The exported workbook failed final SHA-256 verification.", "infrastructure")
  }

  workflow <- eligibility$workflow
  jackknife <- isTRUE(workflow$display_metadata$jackknife$enabled %||%
                        workflow$display_metadata$jackknife$requested)
  app_version <- workflow$display_metadata$normalization$application_version %||%
    tryCatch(as.character(utils::packageVersion("FullPixelMSIQuant")),
      error = function(e) project$manifest$application_version %||% NA_character_)
  edge <- list(
    schema_version = "1.0.0", provenance_edge_id = fpq_id("prov"),
    project_id = project$manifest$project_id, subject_type = "artifact",
    subject_id = eligibility$artifact$artifact_id,
    relationship = "frozen_as_researcher_export", object_type = "file",
    object_id = normalizePath(destination, winslash = "/"),
    recorded_at = fpq_now(), run_id = workflow_id,
    evidence_hash = destination_hash,
    notes = "Byte-identical researcher export of a validated final workbook.",
    source_artifact_id = eligibility$artifact$artifact_id,
    source_path = normalizePath(eligibility$source, winslash = "/"),
    source_sha256 = eligibility$sha256,
    destination_path = normalizePath(destination, winslash = "/"),
    destination_sha256 = destination_hash,
    normalization_method = eligibility$method$internal,
    jackknife_enabled = jackknife, application_version = app_version,
    software_commit = fpq_repository_commit(repository_root))
  tryCatch(fpq_store_provenance(project, list(edge)), error = function(e) {
    unlink(destination, force = TRUE)
    stop(e)
  })
  fpq_log_event(project, "result_export.frozen", run_id = workflow_id,
    details = list(source_artifact_id = eligibility$artifact$artifact_id,
      destination = normalizePath(destination, winslash = "/"),
      sha256 = destination_hash,
      normalization_method = eligibility$method$internal,
      jackknife_enabled = jackknife))
  list(status = "exported", exported = TRUE, path = normalizePath(destination,
    winslash = "/"), filename = filename, sha256 = destination_hash,
    workflow_id = workflow_id, method = eligibility$method$internal,
    jackknife_enabled = jackknife, provenance_edge_id = edge$provenance_edge_id)
}

fpq_v4_result_export_controller <- function(input, output, session, state) {
  selected_workflow_id <- shiny::reactiveVal(NULL)
  directory_roots <- fpq_local_directory_roots()
  shinyFiles::shinyDirChoose(input, "freeze_destination_browse",
    roots = directory_roots, session = session, allowDirCreate = TRUE)
  shiny::observeEvent(input$freeze_destination_browse, {
    selected <- tryCatch(shinyFiles::parseDirPath(
      directory_roots, input$freeze_destination_browse),
      error = function(e) character())
    if (length(selected) && nzchar(selected[[1]]))
      shiny::updateTextInput(session, "freeze_destination", value = selected[[1]])
  }, ignoreInit = TRUE)
  open <- function(workflow_id) {
    eligibility <- fpq_result_export_eligibility(state$project(), workflow_id)
    if (!isTRUE(eligibility$eligible)) {
      shiny::showNotification(eligibility$reason, type = "error", duration = 8)
      return(invisible(FALSE))
    }
    selected_workflow_id(workflow_id)
    filename <- fpq_result_export_filename(state$project(), eligibility$workflow)
    shiny::showModal(shiny::modalDialog(
      title = "Freeze / Export Result", easyClose = TRUE,
      htmltools::tags$p(
        "Create a byte-identical, hash-verified copy of this validated result."),
      shiny::textInput(session$ns("freeze_destination"), "Destination folder", ""),
      shinyFiles::shinyDirButton(session$ns("freeze_destination_browse"),
        "Browse", "Select an export destination folder"),
      shiny::textInput(session$ns("freeze_filename"), "Filename", filename),
      htmltools::tags$p(class = "fpq-help",
        "Existing files are never overwritten. Jackknife results remain inside the workbook."),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("freeze_confirm"), "Export Result",
          class = "btn-primary"))))
    invisible(TRUE)
  }
  shiny::observeEvent(input$freeze_confirm, {
    workflow_id <- selected_workflow_id()
    if (!nzchar(workflow_id %||% "")) return()
    tryCatch({
      result <- fpq_export_validated_result(state$project(), workflow_id,
        input$freeze_destination, input$freeze_filename,
        repository_root = state$repository_root)
      if (isTRUE(result$exported)) {
        shiny::removeModal()
        shiny::showNotification(paste("Export created:", result$path,
          "SHA-256:", result$sha256), type = "message", duration = NULL)
      }
    }, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })
  list(open = open)
}
