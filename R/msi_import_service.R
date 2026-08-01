fpq_msi_extension <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("imzml", "ibd")) extension else NA_character_
}

fpq_normalize_msi_path <- function(path) {
  if (is.null(path) || !length(path) || !nzchar(path)) return("")
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  tolower(normalized)
}

fpq_msi_pair_key <- function(row) paste(
  toupper(row$ion_mode %||% "UNASSIGNED"),
  fpq_normalize_msi_path(row$imzml_path %||% ""),
  fpq_normalize_msi_path(row$ibd_path %||% ""), sep = "|")

fpq_active_project_modes <- function(config = list(), manifest = list(),
                                     detected_rows = list(), fallback = "NEG") {
  normalize_modes <- function(values) {
    values <- unique(toupper(unlist(values, use.names = FALSE)))
    values <- values[values %in% c("POS", "NEG")]
    c("POS", "NEG")[c("POS", "NEG") %in% values]
  }
  declared_modes <- function(value) {
    value <- toupper(value %||% "")
    if (identical(value, "BOTH")) c("POS", "NEG") else
      if (value %in% c("POS", "NEG")) value else character()
  }
  design <- config$analysis_design %||% list()
  design_declared <- declared_modes(design$ion_mode %||%
    design$selected_ion_mode %||% NULL)
  if (length(design_declared)) return(design_declared)
  manifest_declared <- declared_modes(
    manifest$project_preferences$ion_mode %||%
      manifest$project_preferences$initial_ion_registration %||% NULL)
  if (length(manifest_declared)) return(manifest_declared)
  enabled <- normalize_modes(design$enabled_ion_modes %||%
    config$enabled_ion_modes %||%
    manifest$project_preferences$enabled_modes %||% list())
  if (length(enabled)) return(enabled)
  sample_modes <- normalize_modes(lapply(config$samples %||% list(),
    function(x) x$ion_mode %||% ""))
  if (length(sample_modes)) return(sample_modes)
  detected_modes <- normalize_modes(lapply(detected_rows %||% list(),
    function(x) x$ion_mode %||% ""))
  if (length(detected_modes)) return(detected_modes)
  declared <- toupper(config$ion_mode %||% fallback)
  if (declared == "BOTH") return(c("POS", "NEG"))
  if (declared %in% c("POS", "NEG")) return(declared)
  if (declared == "AUTO") return("UNASSIGNED")
  toupper(fallback)
}

fpq_apply_analysis_design <- function(config, analysis_type, ion_mode) {
  ion_mode <- toupper(ion_mode %||% "")
  if (!ion_mode %in% c("POS", "NEG", "BOTH", "AUTO"))
    fpq_stop("analysis_design.ion_mode", "Select a supported ion-mode design.")
  enabled <- switch(ion_mode, BOTH = c("POS", "NEG"), POS = "POS",
                    NEG = "NEG", AUTO = character())
  config$analysis_type <- analysis_type
  config$ion_mode <- ion_mode
  config$enabled_ion_modes <- as.list(enabled)
  config$analysis_uses_both_ion_modes <- identical(ion_mode, "BOTH")
  config$analysis_design <- list(analysis_type = analysis_type,
    ion_mode = ion_mode, enabled_ion_modes = as.list(enabled),
    analysis_uses_both_ion_modes = identical(ion_mode, "BOTH"))
  config
}

fpq_import_msi_folder <- function(existing, path, mode) {
  if (!dir.exists(path))
    fpq_stop("msi.folder_missing", paste("MSI folder does not exist:", path),
             "input")
  rows <- fpq_pair_msi_files(path, mode, recursive = TRUE)
  valid <- Filter(function(x)
    identical(x$validation_status, "PASS"), rows)
  fpq_append_unique_msi_pairs(existing, valid)
}

fpq_modes_to_selection <- function(modes) {
  modes <- unique(toupper(modes))
  if (all(c("POS", "NEG") %in% modes)) "BOTH" else
    if (length(modes) == 1L) modes[[1]] else "AUTO"
}

fpq_infer_biological_sample <- function(value) {
  matched <- regexec("(?i)(CTRL|TRT)[_-]?R([0-9]+)", value, perl = TRUE)
  parts <- regmatches(value, matched)[[1]]
  if (length(parts) == 3L) paste0(toupper(parts[[2]]), "_R", parts[[3]]) else value
}

fpq_mode_dataset_id <- function(biological_sample, ion_mode) {
  biological <- toupper(gsub("[^A-Za-z0-9]+", "_", biological_sample))
  biological <- gsub("(^_+|_+$)", "", biological)
  suffix <- toupper(ion_mode)
  if (!suffix %in% c("POS", "NEG")) suffix <- "UNASSIGNED"
  sub("_(POS|NEG)$", "", biological, ignore.case = TRUE) |>
    paste0("_", suffix)
}

fpq_msi_sample_suggestion <- function(imzml_path = "", ibd_path = "") {
  path <- if (nzchar(imzml_path)) imzml_path else ibd_path
  stem <- tools::file_path_sans_ext(basename(path))
  generic <- tolower(stem) %in% c("scan", "image", "msi", "data", "dataset")
  sample_name <- if (generic) basename(dirname(path)) else stem
  pattern <- "^(.*?)[_-](R[0-9]+)$"
  match <- regexec(pattern, sample_name, ignore.case = TRUE)
  parts <- regmatches(sample_name, match)[[1]]
  list(sample_name = sample_name,
       sample_group = if (length(parts) == 3L) parts[[2]] else "",
       replicate = if (length(parts) == 3L) toupper(parts[[3]]) else "")
}

fpq_discover_msi_files <- function(paths, recursive = TRUE) {
  paths <- unique(normalizePath(paths, mustWork = TRUE))
  files <- unlist(lapply(paths, function(path) {
    if (dir.exists(path)) list.files(path, recursive = recursive, full.names = TRUE,
      all.files = FALSE, include.dirs = FALSE) else path
  }), use.names = FALSE)
  files <- unique(normalizePath(files[file.exists(files)], mustWork = TRUE))
  files[!is.na(vapply(files, fpq_msi_extension, character(1)))]
}

fpq_pair_msi_files <- function(paths, ion_mode = "UNASSIGNED", recursive = TRUE) {
  files <- fpq_discover_msi_files(paths, recursive)
  if (!length(files)) return(list())
  extension <- vapply(files, fpq_msi_extension, character(1))
  key <- tolower(file.path(dirname(files), tools::file_path_sans_ext(basename(files))))
  groups <- split(seq_along(files), key)
  rows <- lapply(groups, function(index) {
    imzml <- files[index[extension[index] == "imzml"]]
    ibd <- files[index[extension[index] == "ibd"]]
    status <- if (length(imzml) == 1L && length(ibd) == 1L) "complete" else
      if (!length(imzml)) "missing_imzml" else if (!length(ibd)) "missing_ibd" else
        "ambiguous"
    suggestion <- fpq_msi_sample_suggestion(
      if (length(imzml)) imzml[[1]] else "",
      if (length(ibd)) ibd[[1]] else "")
    biological <- fpq_infer_biological_sample(suggestion$sample_name)
    parsed_group <- if (grepl("^(CTRL|TRT)_R[0-9]+$", biological))
      sub("_R[0-9]+$", "", biological) else suggestion$sample_group
    parsed_replicate <- if (grepl("^(CTRL|TRT)_R[0-9]+$", biological))
      sub("^(CTRL|TRT)_", "", biological) else suggestion$replicate
    list(include = identical(status, "complete"),
      biological_sample = biological,
      dataset_id = fpq_mode_dataset_id(biological, ion_mode),
      dataset_name = fpq_mode_dataset_id(biological, ion_mode),
      sample_group = parsed_group,
      replicate = parsed_replicate,
      ion_mode = toupper(ion_mode),
      imzml_path = if (length(imzml) == 1L) imzml[[1]] else "",
      ibd_path = if (length(ibd) == 1L) ibd[[1]] else "",
      imzml_filename = if (length(imzml) == 1L) basename(imzml) else "",
      ibd_filename = if (length(ibd) == 1L) basename(ibd) else "",
      pair_status = status,
      validation_status = if (identical(status, "complete")) "PASS" else "FAIL")
  })
  used <- unlist(lapply(rows, function(x) c(x$imzml_path, x$ibd_path)), use.names = FALSE)
  duplicates <- unique(used[nzchar(used) & duplicated(used)])
  if (length(duplicates))
    for (i in seq_along(rows))
      if (any(c(rows[[i]]$imzml_path, rows[[i]]$ibd_path) %in% duplicates)) {
        rows[[i]]$pair_status <- "duplicate_assignment"
        rows[[i]]$validation_status <- "FAIL"
        rows[[i]]$include <- FALSE
      }
  unname(rows)
}

fpq_append_unique_msi_pairs <- function(existing, incoming) {
  existing <- existing %||% list()
  incoming <- incoming %||% list()
  keys <- vapply(existing, fpq_msi_pair_key, character(1))
  added <- list(); duplicate_count <- 0L
  for (row in incoming) {
    key <- fpq_msi_pair_key(row)
    if (key %in% keys) {
      duplicate_count <- duplicate_count + 1L
    } else {
      added <- c(added, list(row)); keys <- c(keys, key)
    }
  }
  list(rows = c(existing, added), added = length(added),
       duplicates = duplicate_count)
}

fpq_remove_msi_rows <- function(rows, indices = integer(), ion_mode = NULL,
                                clear_all = FALSE) {
  rows <- rows %||% list()
  if (isTRUE(clear_all)) return(list(rows = list(), removed = length(rows)))
  remove <- rep(FALSE, length(rows))
  if (length(indices)) remove[indices[indices >= 1L & indices <= length(rows)]] <- TRUE
  if (!is.null(ion_mode))
    remove <- remove | vapply(rows, function(x)
      identical(toupper(x$ion_mode %||% ""), toupper(ion_mode)), logical(1))
  list(rows = rows[!remove], removed = sum(remove))
}

fpq_detected_rows_from_configuration <- function(config) {
  lapply(config$samples %||% list(), function(sample) {
    biological <- sample$biological_sample %||% sample$sample_label %||% sample$sample_id
    mode <- toupper(sample$ion_mode %||% "UNASSIGNED")
    list(include = !identical(sample$inclusion_status %||% "included", "excluded"),
      biological_sample = biological,
      dataset_id = sample$sample_id,
      dataset_name = sample$sample_id,
      sample_group = sample$condition_id %||% "",
      replicate = sample$replicate_id %||% "",
      ion_mode = mode, imzml_path = sample$imzml_path %||% "",
      ibd_path = sample$ibd_path %||% "",
      imzml_filename = basename(sample$imzml_path %||% ""),
      ibd_filename = basename(sample$ibd_path %||% ""),
      pair_status = "complete", validation_status = "PASS")
  })
}

fpq_repair_combined_sample_identifiers <- function(config) {
  samples <- config$samples %||% list()
  modes <- unique(vapply(samples, function(x) toupper(x$ion_mode %||% ""), character(1)))
  if (!all(c("POS", "NEG") %in% modes))
    return(list(config = config, changed = FALSE, repaired = 0L))
  changed <- 0L
  for (i in seq_along(samples)) {
    biological <- samples[[i]]$biological_sample %||%
      samples[[i]]$sample_label %||% samples[[i]]$sample_id
    biological <- fpq_infer_biological_sample(biological)
    dataset_id <- fpq_mode_dataset_id(biological, samples[[i]]$ion_mode)
    if (!identical(samples[[i]]$sample_id, dataset_id) ||
        !identical(samples[[i]]$biological_sample %||% NULL, biological))
      changed <- changed + 1L
    samples[[i]]$biological_sample <- biological
    samples[[i]]$sample_label <- biological
    samples[[i]]$sample_id <- dataset_id
  }
  config$samples <- samples
  fpq_validate_dynamic_configuration(config)
  list(config = config, changed = changed > 0L, repaired = changed)
}

fpq_configuration_from_detected_rows <- function(config, rows) {
  included <- Filter(function(x) isTRUE(x$include), rows %||% list())
  config$samples <- lapply(included, function(x) {
    group <- if (nzchar(x$sample_group %||% "")) x$sample_group else "ungrouped"
    biological <- x$biological_sample
    dataset_id <- fpq_mode_dataset_id(biological, x$ion_mode)
    list(sample_id = dataset_id, sample_label = biological,
      biological_sample = biological, condition_id = group,
      replicate_id = x$replicate, inclusion_status = "included",
      ion_mode = toupper(x$ion_mode), imzml_path = x$imzml_path,
      ibd_path = x$ibd_path, notes = "")
  })
  groups <- unique(vapply(config$samples, `[[`, "", "condition_id"))
  old_conditions <- config$conditions %||% list()
  config$conditions <- lapply(seq_along(groups), function(i) {
    old <- Filter(function(x) identical(x$condition_id, groups[[i]]), old_conditions)
    if (length(old)) return(old[[1]])
    list(condition_id = groups[[i]], condition_label = groups[[i]],
      analytical_role = if (i == 1L) "reference" else "comparison",
      is_reference = i == 1L, display_order = i, treatment_label = NULL,
      dose = NULL, notes = NULL)
  })
  config
}

fpq_persist_msi_configuration <- function(project, config) {
  config$schema_version <- config$schema_version %||% "1.0.0"
  config$dataset_id <- config$dataset_id %||% "dset-project-configuration"
  fpq_save_configuration(project, "datasets", config$dataset_id, config,
                         fpq_validate_dynamic_configuration)
}

fpq_invalidate_project_validation <- function(project) {
  fpq_write_collection(list(), fpq_project_paths(project$root)$validations)
  fpq_log_event(project, "validation.invalidated.samples_changed",
    details = list(message = "Sample assignments changed; validation must be rerun."))
  invisible(TRUE)
}

fpq_msi_review_data <- function(rows, details = FALSE) {
  fields <- c("include", "biological_sample", "dataset_id", "sample_group",
    "replicate", "ion_mode", "imzml_filename", "ibd_filename", "status")
  if (isTRUE(details)) fields <- c(fields, "imzml_path", "ibd_path")
  if (!length(rows)) return(as.data.frame(setNames(
    replicate(length(fields), character(), simplify = FALSE), fields)))
  do.call(rbind, lapply(rows, function(row) as.data.frame(setNames(
    lapply(fields, function(field) {
      value <- if (identical(field, "status"))
        paste(row$pair_status %||% "", row$validation_status %||% "", sep = " / ") else
        row[[field]]
      if (is.null(value) || !length(value)) "" else value[[1]]
    }), fields), stringsAsFactors = FALSE)))
}
