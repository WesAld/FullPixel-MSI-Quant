###############################################################################
# Direct pixel-level MALDI-MSI target extraction from centroided imzML files
###############################################################################

###############################################################################
# USER CONFIGURATION PANEL
###############################################################################

dataset_id <- "PVL_Single_Negative"

raw_data_root <-
  "C:/MALDI-MSI/Single Exposure/Negative Mode"

mzmine_csv <-
  "C:/MALDI-MSI/Single Exposure/Negative Mode/MZMINE output/PVL_Single_Negative_40um_Aligned_Feature_List.csv"

analysis_root <-
  "C:/MALDI-MSI/PVL_MSI_Analysis"

expected_exposure <- "Single"
expected_ion_mode <- "Negative"
expected_matrix <- "9AA"
expected_pixel_size_um <- 40
expected_full_pixel <- TRUE
expected_coordinate_pixels <- 1600L
mzmine_expected_row_range <- c(1000L, 1500L)

expected_sample_ids <- c(
  "CTRL_R1",
  "CTRL_R2",
  "CTRL_R3",
  "TRT_R1",
  "TRT_R2",
  "TRT_R3"
)

absolute_tolerance <- 0.002
ppm_tolerance <- 10

inclusive_detection_threshold <- 0
conservative_detection_threshold <- 1000

normalization_scale <- 1000000

save_pixel_level_output <- FALSE

primary_detection_threshold <- 1000
primary_minimum_detected_pixels <- 10L

###############################################################################

###############################################################################
# PACKAGE CHECKING AND RUN STATE
###############################################################################

required_packages <- c(
  "Cardinal", "data.table", "stringr", "matrixStats"
)

state <- new.env(parent = emptyenv())
state$warnings <- character()
state$errors <- character()
state$start_time <- Sys.time()

log_message <- function(...) {
  msg <- paste(...)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg, "\n")
}

add_warning <- function(...) {
  msg <- paste(...)
  state$warnings <- c(state$warnings, msg)
  warning(msg, call. = FALSE)
}

stop_critical <- function(...) {
  msg <- paste(...)
  state$errors <- c(state$errors, msg)
  stop(msg, call. = FALSE)
}

check_and_load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (pkg == "Cardinal") {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages("BiocManager", repos = "https://cloud.r-project.org")
        }
        BiocManager::install(pkg, ask = FALSE, update = FALSE)
      } else {
        install.packages(pkg, repos = "https://cloud.r-project.org")
      }
    }
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop_critical("Required R package is unavailable after install attempt:", pkg)
    }
  }
  suppressPackageStartupMessages({
    library(Cardinal)
    library(data.table)
    library(stringr)
    library(matrixStats)
  })
  versions <- data.table::data.table(
    package = pkgs,
    version = vapply(pkgs, function(x) as.character(utils::packageVersion(x)), "")
  )
  versions
}

validate_user_configuration <- function() {
  scalar_text <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  scalar_number <- function(x, minimum = 0, strictly_positive = FALSE) {
    valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
    if (!valid) return(FALSE)
    if (strictly_positive) x > minimum else x >= minimum
  }

  if (!scalar_text(dataset_id)) {
    stop_critical("Configuration error: dataset_id must be a nonblank string.")
  }
  if (!scalar_text(raw_data_root) || !dir.exists(raw_data_root)) {
    stop_critical("Configuration error: raw_data_root does not exist:", raw_data_root)
  }
  if (!scalar_text(mzmine_csv) || !file.exists(mzmine_csv)) {
    stop_critical("Configuration error: mzmine_csv does not exist:", mzmine_csv)
  }
  if (!scalar_text(analysis_root)) {
    stop_critical("Configuration error: analysis_root must be a nonblank path.")
  }
  if (!scalar_text(expected_exposure) ||
      !expected_exposure %in% c("Single", "Sequential")) {
    stop_critical("Configuration error: expected_exposure must be Single or Sequential.")
  }
  if (!scalar_text(expected_ion_mode) ||
      !expected_ion_mode %in% c("Positive", "Negative")) {
    stop_critical("Configuration error: expected_ion_mode must be Positive or Negative.")
  }
  if (!scalar_text(expected_matrix)) {
    stop_critical("Configuration error: expected_matrix must be a nonblank string.")
  }
  if (!scalar_number(expected_pixel_size_um, strictly_positive = TRUE)) {
    stop_critical("Configuration error: expected_pixel_size_um must be positive.")
  }
  if (!is.logical(expected_full_pixel) || length(expected_full_pixel) != 1L ||
      is.na(expected_full_pixel)) {
    stop_critical("Configuration error: expected_full_pixel must be TRUE or FALSE.")
  }
  if (!scalar_number(expected_coordinate_pixels, strictly_positive = TRUE) ||
      expected_coordinate_pixels != as.integer(expected_coordinate_pixels)) {
    stop_critical("Configuration error: expected_coordinate_pixels must be a positive integer.")
  }
  if (!is.numeric(mzmine_expected_row_range) ||
      length(mzmine_expected_row_range) != 2L ||
      anyNA(mzmine_expected_row_range) ||
      any(!is.finite(mzmine_expected_row_range)) ||
      any(mzmine_expected_row_range < 0) ||
      mzmine_expected_row_range[1] > mzmine_expected_row_range[2]) {
    stop_critical("Configuration error: mzmine_expected_row_range must contain a valid minimum and maximum.")
  }
  if (!is.character(expected_sample_ids) || length(expected_sample_ids) != 6L ||
      anyNA(expected_sample_ids) || any(!nzchar(trimws(expected_sample_ids)))) {
    stop_critical("Configuration error: expected_sample_ids must contain exactly six nonblank entries.")
  }
  if (anyDuplicated(expected_sample_ids)) {
    stop_critical("Configuration error: expected_sample_ids contains duplicates.")
  }
  if (!scalar_number(absolute_tolerance) ||
      !scalar_number(ppm_tolerance) ||
      !scalar_number(inclusive_detection_threshold) ||
      !scalar_number(conservative_detection_threshold) ||
      !scalar_number(primary_detection_threshold)) {
    stop_critical("Configuration error: tolerances and detection thresholds must be finite and nonnegative.")
  }
  if (absolute_tolerance == 0 && ppm_tolerance == 0) {
    stop_critical("Configuration error: at least one mass tolerance must be positive.")
  }
  if (inclusive_detection_threshold > conservative_detection_threshold) {
    stop_critical("Configuration error: inclusive_detection_threshold cannot exceed conservative_detection_threshold.")
  }
  if (primary_detection_threshold != conservative_detection_threshold) {
    stop_critical(
      paste(
        "Configuration error: primary_detection_threshold must equal",
        "conservative_detection_threshold because primary detection",
        "is based on conservative detected pixels."
      )
    )
  }
  if (!scalar_number(normalization_scale, strictly_positive = TRUE)) {
    stop_critical("Configuration error: normalization_scale must be positive.")
  }
  if (!is.logical(save_pixel_level_output) ||
      length(save_pixel_level_output) != 1L || is.na(save_pixel_level_output)) {
    stop_critical("Configuration error: save_pixel_level_output must be TRUE or FALSE.")
  }
  if (!scalar_number(primary_minimum_detected_pixels) ||
      primary_minimum_detected_pixels != as.integer(primary_minimum_detected_pixels)) {
    stop_critical("Configuration error: primary_minimum_detected_pixels must be a nonnegative integer.")
  }
  invisible(TRUE)
}

create_analysis_directories <- function() {
  dirs <- list(
    targets = file.path(analysis_root, "01_MZmine_Target_Lists", dataset_id),
    quant = file.path(analysis_root, "02_Pixel_Quantification", dataset_id),
    logs = file.path(analysis_root, "06_Logs_and_QC", dataset_id)
  )
  lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_dir <- file.path(dirs$quant, paste0("SixSample_", timestamp))
  if (dir.exists(run_dir)) {
    stop_critical("Output folder already exists:", run_dir)
  }
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  dirs$run <- run_dir
  dirs
}

safe_file_size <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  as.numeric(file.info(path)$size)
}

infer_condition <- function(text) {
  u <- toupper(text)
  has_ctrl <- grepl("CTRL|CONTROL", u)
  # "PVL" is also used as a project prefix in raw filenames, so treatment is
  # inferred only from explicit TRT/treated tokens.
  has_trt <- grepl("TRT|TREATED", u)
  if (has_ctrl && !has_trt) return("CTRL")
  if (has_trt && !has_ctrl) return("TRT")
  if (has_ctrl && has_trt) return("CTRL_TRT")
  NA_character_
}

infer_replicate <- function(text) {
  u <- toupper(text)
  r <- stringr::str_match(u, "(^|[^A-Z0-9])R([123])([^0-9]|$)")[, 3]
  if (!is.na(r)) return(paste0("R", r))
  r2 <- stringr::str_match(u, "REPLICATE[ _-]*([123])")[, 2]
  if (!is.na(r2)) return(paste0("R", r2))
  NA_character_
}

infer_pixel_size <- function(text) {
  u <- toupper(text)
  m <- stringr::str_match(u, "([0-9]+(?:\\.[0-9]+)?)\\s*UM")[, 2]
  if (is.na(m)) return(NA_real_)
  as.numeric(m[1])
}

infer_exposure <- function(text) {
  u <- toupper(text)
  has_single <- grepl("(^|[^A-Z])SINGLE([^A-Z]|$)", u)
  has_sequential <- grepl("(^|[^A-Z])SEQUENTIAL([^A-Z]|$)", u)
  if (has_single && !has_sequential) return("Single")
  if (has_sequential && !has_single) return("Sequential")
  NA_character_
}

infer_ion_mode <- function(text) {
  u <- toupper(text)
  has_positive <- grepl("POSITIV|POSITIVE|(^|[^A-Z])POS([^A-Z]|$)", u)
  has_negative <- grepl("NEGATIV|NEGATIVE|(^|[^A-Z])NEG([^A-Z]|$)", u)
  if (has_positive && !has_negative) return("Positive")
  if (has_negative && !has_positive) return("Negative")
  NA_character_
}

infer_matrix <- function(text) {
  if (grepl(toupper(expected_matrix), toupper(text), fixed = TRUE)) {
    expected_matrix
  } else {
    NA_character_
  }
}

is_excluded_input_path <- function(path) {
  normalized_analysis_root <- normalizePath(
    analysis_root, winslash = "/", mustWork = FALSE
  )
  in_analysis_output <- startsWith(
    tolower(path),
    paste0(tolower(normalized_analysis_root), "/")
  )
  excluded_name <- grepl(
    "MSI\\.EAGLE|(^|[/_. -])(TEMP|TMP|CONVERTED|OUTPUT)([/_. -]|$)",
    toupper(path)
  )
  in_analysis_output || excluded_name
}

###############################################################################
# INPUT DISCOVERY
###############################################################################

discover_imzml_pairs <- function(output_dir) {
  log_message("Discovering imzML/ibd pairs under", raw_data_root)
  all_imzml <- list.files(raw_data_root, pattern = "\\.imzml$", recursive = TRUE,
                          full.names = TRUE, ignore.case = TRUE)
  if (!length(all_imzml)) stop_critical("No imzML files found under:", raw_data_root)
  rows <- lapply(all_imzml, function(imzml) {
    norm <- normalizePath(imzml, winslash = "/", mustWork = TRUE)
    text <- paste(norm, basename(dirname(norm)), basename(dirname(dirname(norm))))
    base <- tools::file_path_sans_ext(basename(norm))
    parent <- dirname(norm)
    ibd_candidates <- file.path(parent, paste0(base, c(".ibd", ".IBD")))
    ibd_candidates <- ibd_candidates[file.exists(ibd_candidates)]
    ibd <- if (length(ibd_candidates)) normalizePath(ibd_candidates[1], winslash = "/", mustWork = TRUE) else NA_character_
    condition <- infer_condition(text)
    replicate <- infer_replicate(text)
    exposure <- infer_exposure(text)
    mode <- infer_ion_mode(text)
    matrix <- infer_matrix(text)
    pixel_size <- infer_pixel_size(text)
    full_pixel <- grepl("FULLPIXEL|FULL-PIXEL|FULL_PIXEL", toupper(text))
    sample_id <- if (!is.na(condition) && !is.na(replicate)) {
      paste(condition, replicate, sep = "_")
    } else {
      NA_character_
    }
    exclusion <- character()
    if (is_excluded_input_path(norm)) {
      exclusion <- c(exclusion, "excluded analysis/output/temporary/converted path")
    }
    if (is.na(ibd)) exclusion <- c(exclusion, "missing matching ibd/IBD")
    if (!condition %in% c("CTRL", "TRT")) {
      exclusion <- c(exclusion, paste("condition is", condition))
    }
    if (!replicate %in% c("R1", "R2", "R3")) {
      exclusion <- c(exclusion, paste("replicate is", replicate))
    }
    if (is.na(sample_id) || !sample_id %in% expected_sample_ids) {
      exclusion <- c(exclusion, paste("sample ID is", sample_id))
    }
    if (!identical(exposure, expected_exposure)) {
      exclusion <- c(exclusion, paste("exposure is", exposure))
    }
    if (!identical(mode, expected_ion_mode)) {
      exclusion <- c(exclusion, paste("ion mode is", mode))
    }
    if (!identical(matrix, expected_matrix)) {
      exclusion <- c(exclusion, paste("matrix is", matrix))
    }
    if (is.na(pixel_size) || pixel_size != expected_pixel_size_um) {
      exclusion <- c(exclusion, paste("pixel size is", pixel_size))
    }
    if (isTRUE(expected_full_pixel) && !isTRUE(full_pixel)) {
      exclusion <- c(exclusion, "not full-pixel")
    }
    selected <- length(exclusion) == 0
    data.table::data.table(
      selected = selected,
      sample_id = sample_id,
      condition = condition,
      replicate = replicate,
      exposure = exposure,
      mode = mode,
      matrix = matrix,
      pixel_size_um = pixel_size,
      full_pixel = full_pixel,
      imzml_path = norm,
      ibd_path = ibd,
      matching_basename = !is.na(ibd) && identical(tolower(tools::file_path_sans_ext(basename(norm))),
                                                   tolower(tools::file_path_sans_ext(basename(ibd)))),
      selection_evidence = paste(c(condition, replicate, exposure, mode, matrix, paste0(pixel_size, "um"), if (full_pixel) "Fullpixel"), collapse = "; "),
      exclusion_reason = if (length(exclusion)) paste(exclusion, collapse = "; ") else NA_character_
    )
  })
  discovered <- data.table::rbindlist(rows, fill = TRUE)
  data.table::fwrite(discovered, file.path(output_dir, "discovered_input_files.csv"))
  print(discovered[, .(selected, sample_id, pixel_size_um, full_pixel, imzml_path, exclusion_reason)])
  selected <- discovered[selected == TRUE]
  if (nrow(selected) != length(expected_sample_ids)) {
    data.table::fwrite(
      selected,
      file.path(output_dir, "selected_input_files_incomplete.csv")
    )
    stop_critical(
      "Input discovery requires exactly the six configured samples; found",
      nrow(selected),
      "of",
      length(expected_sample_ids),
      ". See discovered_input_files.csv."
    )
  }
  duplicate_sample_ids <- selected[, .N, by = sample_id][N > 1]
  if (nrow(duplicate_sample_ids)) {
    data.table::fwrite(
      selected,
      file.path(output_dir, "duplicate_sample_ids.csv")
    )
    stop_critical(
      "Duplicate sample IDs remain after filtering; refusing ambiguous selection."
    )
  }
  missing_sample_ids <- setdiff(expected_sample_ids, selected$sample_id)
  unexpected_sample_ids <- setdiff(selected$sample_id, expected_sample_ids)
  if (length(missing_sample_ids) || length(unexpected_sample_ids)) {
    stop_critical(
      "Selected sample IDs do not exactly match expected_sample_ids. Missing:",
      paste(missing_sample_ids, collapse = ", "),
      "Unexpected:",
      paste(unexpected_sample_ids, collapse = ", ")
    )
  }
  selected
}

###############################################################################
# MZMINE-TARGET VALIDATION
###############################################################################

validate_mzmine_csv <- function(output_dir) {
  log_message("Reading MZmine CSV:", mzmine_csv)
  if (!file.exists(mzmine_csv)) stop_critical("MZmine CSV missing:", mzmine_csv)
  dt <- data.table::fread(mzmine_csv, showProgress = FALSE)
  if (nrow(dt) < mzmine_expected_row_range[1] ||
      nrow(dt) > mzmine_expected_row_range[2]) {
    add_warning("MZmine row count is outside expected approximate range:", nrow(dt))
  }
  cn <- names(dt)
  mz_candidates <- cn[tolower(cn) %in% c("mz", "row_mz", "average_mz", "aligned_mz")]
  if (!length(mz_candidates)) {
    mz_candidates <- cn[grepl("(^|_)mz($|_)|average.*mz|aligned.*mz", tolower(cn))]
  }
  if (!length(mz_candidates)) stop_critical("Could not identify aligned m/z column.")
  mz_col <- mz_candidates[1]
  dt[, target_mz := suppressWarnings(as.numeric(get(mz_col)))]
  if (anyNA(dt$target_mz)) stop_critical("Aligned m/z column contains non-numeric or missing values:", mz_col)
  id_col <- if ("id" %in% cn) "id" else if ("row_id" %in% cn) "row_id" else NA_character_
  dt[, mzmine_row_id := if (!is.na(id_col)) as.character(get(id_col)) else sprintf("MZ_%06d", .I)]
  if (anyDuplicated(dt$mzmine_row_id)) {
    stop_critical("The aligned MZmine feature list contains duplicated row IDs.")
  }
  raw_sample_names <- unique(stringr::str_match(cn, "^datafile:(.*?):")[, 2])
  raw_sample_names <- raw_sample_names[!is.na(raw_sample_names)]
  if (length(raw_sample_names) < length(expected_sample_ids)) {
    add_warning(
      "Fewer than the configured number of sample-specific datafile column groups detected:",
      length(raw_sample_names)
    )
  }
  height_cols <- cn[grepl("^datafile:.*:height$", cn)]
  state_cols <- cn[grepl("^datafile:.*:feature_state$", cn)]
  list(
    data = dt,
    mz_col = mz_col,
    id_col = id_col,
    sample_names = raw_sample_names,
    height_cols = height_cols,
    state_cols = state_cols
  )
}

select_all_targets <- function(mz_info, output_dir) {
  dt <- copy(mz_info$data)
  targets <- dt[, .(
    target_id = sprintf("MZ_%06d", seq_len(.N)),
    mzmine_row_id,
    target_mz,
    selection_category = "all_aligned_features",
    selection_reason = "Extraction target from aligned MZmine m/z row",
    number_of_samples_detected = NA_integer_,
    minimum_sample_height = NA_real_,
    median_sample_height = NA_real_,
    maximum_sample_height = NA_real_
  )]
  if (length(mz_info$height_cols)) {
    hcols <- mz_info$height_cols
    for (cc in hcols) set(dt, j = cc, value = suppressWarnings(as.numeric(dt[[cc]])))
    height_data <- dt[, hcols, with = FALSE]
    targets[, minimum_sample_height := do.call(pmin, c(height_data, na.rm = TRUE))]
    targets[, maximum_sample_height := do.call(pmax, c(height_data, na.rm = TRUE))]
    targets[, median_sample_height := matrixStats::rowMedians(as.matrix(height_data), na.rm = TRUE)]
    targets[is.infinite(minimum_sample_height), minimum_sample_height := NA_real_]
    targets[is.infinite(maximum_sample_height), maximum_sample_height := NA_real_]
  }
  if (length(mz_info$state_cols)) {
    scols <- mz_info$state_cols
    state_data <- dt[, scols, with = FALSE]
    targets[, number_of_samples_detected := rowSums(as.data.frame(state_data) == "DETECTED", na.rm = TRUE)]
  }
  data.table::fwrite(targets, file.path(output_dir, "all_aligned_targets.csv"))
  targets
}

###############################################################################
# IMZML IMPORT AND CENTROID-ARRAY VALIDATION
###############################################################################

import_imzml_data <- function(selected) {
  log_message("Importing selected imzML:", selected$imzml_path)
  obj <- Cardinal::readImzML(selected$imzml_path, memory = FALSE, verbose = TRUE)
  obj
}

inspect_imzml_object <- function(obj) {
  coords <- as.data.frame(Cardinal::coord(obj))
  if (!nrow(coords)) stop_critical("Coordinates are missing.")
  dup <- duplicated(coords)
  if (any(dup)) stop_critical("Duplicated coordinates detected; stopping.")
  mz_list <- Cardinal::mz(obj)
  int_list <- Cardinal::intensity(obj)
  n <- length(int_list)
  if (length(mz_list) != n) stop_critical("m/z and intensity spectrum counts differ.")
  if (n != nrow(coords)) {
    stop_critical(
      paste(
        "The number of spectra does not equal the number of coordinates.",
        "Spectra:", n,
        "Coordinates:", nrow(coords)
      )
    )
  }
  lens <- integer(n)
  sorted <- logical(n)
  invalid <- logical(n)
  spectrum_sum <- numeric(n)
  min_mz <- rep(Inf, n)
  max_mz <- rep(-Inf, n)
  for (i in seq_len(n)) {
    mzv <- as.numeric(mz_list[[i]])
    inv <- as.numeric(int_list[[i]])
    if (length(mzv) != length(inv) || any(!is.finite(mzv)) || any(!is.finite(inv))) invalid[i] <- TRUE
    lens[i] <- length(mzv)
    sorted[i] <- length(mzv) <= 1 || !is.unsorted(mzv, strictly = FALSE)
    if (length(mzv)) {
      min_mz[i] <- min(mzv)
      max_mz[i] <- max(mzv)
    }
    spectrum_sum[i] <- if (any(!is.finite(inv))) NA_real_ else sum(inv)
  }
  if (any(!sorted, na.rm = TRUE)) {
    stop_critical(
      "At least one spectrum contains an unsorted m/z array. Target extraction was stopped."
    )
  }
  if (!any(!invalid)) stop_critical("No valid spectra are available.")
  list(
    coords = coords,
    mz_list = mz_list,
    int_list = int_list,
    n_spectra = n,
    n_coords = nrow(coords),
    n_unique_coords = nrow(unique(coords)),
    duplicated_coordinates = sum(dup),
    missing_coordinates = sum(!stats::complete.cases(coords)),
    x_range = range(coords$x, na.rm = TRUE),
    y_range = range(coords$y, na.rm = TRUE),
    z_values = if ("z" %in% names(coords)) unique(coords$z) else NA,
    spectrum_lengths = lens,
    sorted = sorted,
    invalid = invalid,
    spectrum_sum = spectrum_sum,
    min_mz = min(min_mz[is.finite(min_mz)]),
    max_mz = max(max_mz[is.finite(max_mz)]),
    original_arrays_confirmed = inherits(mz_list, "matter_list") &&
      inherits(int_list, "matter_list") && is.null(dim(mz_list)) && is.null(dim(int_list)),
    object_class = paste(class(obj), collapse = ", "),
    on_disk = inherits(mz_list, "matter_list") || inherits(int_list, "matter_list"),
    centroided = tryCatch({
      if ("centroided" %in% slotNames(obj)) slot(obj, "centroided") else Cardinal::centroided(obj)
    }, error = function(e) NA),
    continuous = tryCatch({
      if ("continuous" %in% slotNames(obj)) slot(obj, "continuous") else Cardinal::continuous(obj)
    }, error = function(e) NA)
  )
}

###############################################################################
# TARGET EXTRACTION AND SPECTRUM-SUM NORMALIZATION
###############################################################################

extract_targets_from_spectrum <- function(mzv, inv, targets) {
  n <- nrow(targets)
  raw <- numeric(n)
  count <- integer(n)
  selected_mz <- rep(NA_real_, n)
  selected_error_da <- rep(NA_real_, n)
  selected_error_ppm <- rep(NA_real_, n)
  summed <- numeric(n)
  if (!length(mzv)) {
    return(list(
      raw = raw, count = count, selected_mz = selected_mz,
      selected_error_da = selected_error_da,
      selected_error_ppm = selected_error_ppm, summed = summed
    ))
  }
  for (j in seq_len(n)) {
    lo <- targets$target_mz[j] - targets$extraction_half_window[j]
    hi <- targets$target_mz[j] + targets$extraction_half_window[j]
    left <- findInterval(lo, mzv, left.open = TRUE) + 1L
    right <- findInterval(hi, mzv)
    if (right >= left) {
      idx <- left:right
      count[j] <- length(idx)
      summed[j] <- sum(inv[idx])
      mass_errors <- abs(mzv[idx] - targets$target_mz[j])
      candidates <- which(mass_errors == min(mass_errors))
      if (length(candidates) > 1) {
        candidates <- candidates[which.max(inv[idx][candidates])]
      }
      best_idx <- idx[candidates[1]]
      selected_mz[j] <- mzv[best_idx]
      selected_error_da[j] <- selected_mz[j] - targets$target_mz[j]
      selected_error_ppm[j] <- (selected_error_da[j] / targets$target_mz[j]) * 1000000
      raw[j] <- inv[best_idx]
    }
  }
  list(
    raw = raw,
    count = count,
    selected_mz = selected_mz,
    selected_error_da = selected_error_da,
    selected_error_ppm = selected_error_ppm,
    summed = summed
  )
}

extract_all_pixels <- function(inspect, targets, selected, output_dir,
                               write_pixel_file = save_pixel_level_output,
                               pixel_filename = "target_pixel_values.csv") {
  targets <- copy(targets)
  targets[, extraction_half_window := pmax(absolute_tolerance, target_mz * ppm_tolerance / 1000000)]
  coords <- inspect$coords
  n_pixels <- nrow(coords)
  n_targets <- nrow(targets)
  expected <- n_pixels * n_targets
  chunks <- vector("list", n_pixels)
  for (i in seq_len(n_pixels)) {
    if (i %% 100 == 0) log_message("Extracting pixel", i, "of", n_pixels)
    spectrum_valid <- !inspect$invalid[i]
    mzv <- if (spectrum_valid) as.numeric(inspect$mz_list[[i]]) else numeric()
    inv <- if (spectrum_valid) as.numeric(inspect$int_list[[i]]) else numeric()
    extracted <- if (spectrum_valid) {
      extract_targets_from_spectrum(mzv, inv, targets)
    } else {
      list(
        raw = rep(NA_real_, n_targets),
        count = rep(NA_integer_, n_targets),
        selected_mz = rep(NA_real_, n_targets),
        selected_error_da = rep(NA_real_, n_targets),
        selected_error_ppm = rep(NA_real_, n_targets),
        summed = rep(NA_real_, n_targets)
      )
    }
    spectrum_sum <- inspect$spectrum_sum[i]
    valid_sum <- is.finite(spectrum_sum) && spectrum_sum > 0
    norm <- if (valid_sum) (extracted$raw / spectrum_sum) * normalization_scale else rep(NA_real_, n_targets)
    chunks[[i]] <- data.table::data.table(
      dataset_id = dataset_id,
      sample_id = selected$sample_id,
      condition = selected$condition,
      replicate = selected$replicate,
      pixel_index = i,
      x = coords$x[i],
      y = coords$y[i],
      z = if ("z" %in% names(coords)) coords$z[i] else NA,
      target_id = targets$target_id,
      mzmine_row_id = targets$mzmine_row_id,
      target_mz = targets$target_mz,
      extraction_half_window = targets$extraction_half_window,
      number_of_peaks_in_window = extracted$count,
      selected_peak_mz = extracted$selected_mz,
      selected_peak_mass_error_da = extracted$selected_error_da,
      selected_peak_mass_error_ppm = extracted$selected_error_ppm,
      summed_intensity_in_window = extracted$summed,
      raw_target_intensity = extracted$raw,
      spectrum_sum = spectrum_sum,
      spectrum_sum_normalized_per_million = norm,
      inclusive_detected = extracted$raw > inclusive_detection_threshold,
      conservative_detected = extracted$raw >= conservative_detection_threshold,
      valid_spectrum_sum = valid_sum,
      spectrum_valid = spectrum_valid
    )
  }
  pixel_values <- data.table::rbindlist(chunks)
  if (nrow(pixel_values) != expected) {
    stop_critical("Pixel output row count mismatch. Expected", expected, "actual", nrow(pixel_values))
  }
  if (isTRUE(write_pixel_file)) {
    data.table::fwrite(pixel_values, file.path(output_dir, pixel_filename))
  }
  pixel_values
}

q95 <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, 0.95, names = FALSE, type = 7))
}

mean_or_na <- function(x) if (length(x <- x[!is.na(x)])) mean(x) else NA_real_
median_or_na <- function(x) if (length(x <- x[!is.na(x)])) stats::median(x) else NA_real_
max_or_na <- function(x) if (length(x <- x[!is.na(x)])) max(x) else NA_real_

###############################################################################
# PIXEL DETECTION AND FEATURE SUMMARIZATION
###############################################################################

summarize_target_pixels <- function(pixel_values, output_dir,
                                    summary_filename = "target_summary.csv") {
  summary <- pixel_values[, {
    valid <- spectrum_valid == TRUE
    valid_sum <- valid_spectrum_sum == TRUE
    inc <- inclusive_detected == TRUE & valid_sum
    cons <- conservative_detected == TRUE & valid_sum
    raw_valid <- raw_target_intensity[valid]
    norm_valid <- spectrum_sum_normalized_per_million[valid_sum]
    .(
      dataset_id = dataset_id[1],
      sample_id = sample_id[1],
      condition = condition[1],
      replicate = replicate[1],
      target_mz = target_mz[1],
      total_coordinate_pixels = .N,
      valid_spectrum_pixels = sum(valid),
      invalid_spectrum_pixels = sum(!valid),
      zero_spectrum_sum_pixels = sum(valid & spectrum_sum == 0, na.rm = TRUE),
      raw_sum_all_valid_pixels = sum(raw_valid, na.rm = FALSE),
      raw_mean_all_valid_pixels = mean_or_na(raw_valid),
      raw_median_all_valid_pixels = median_or_na(raw_valid),
      raw_p95_all_valid_pixels = q95(raw_valid),
      raw_maximum_pixel = max_or_na(raw_valid),
      inclusive_detected_pixel_count = sum(inc),
      inclusive_detection_fraction = sum(inc) / sum(valid),
      raw_mean_inclusive_detected_pixels = mean_or_na(raw_target_intensity[inc]),
      raw_median_inclusive_detected_pixels = median_or_na(raw_target_intensity[inc]),
      raw_p95_inclusive_detected_pixels = q95(raw_target_intensity[inc]),
      normalized_mean_inclusive_detected_pixels = mean_or_na(spectrum_sum_normalized_per_million[inc]),
      normalized_median_inclusive_detected_pixels = median_or_na(spectrum_sum_normalized_per_million[inc]),
      normalized_p95_inclusive_detected_pixels = q95(spectrum_sum_normalized_per_million[inc]),
      normalized_maximum_inclusive_detected_pixels = max_or_na(spectrum_sum_normalized_per_million[inc]),
      conservative_detected_pixel_count = sum(cons),
      conservative_detection_fraction = sum(cons) / sum(valid),
      raw_mean_conservative_detected_pixels = mean_or_na(raw_target_intensity[cons]),
      raw_median_conservative_detected_pixels = median_or_na(raw_target_intensity[cons]),
      raw_p95_conservative_detected_pixels = q95(raw_target_intensity[cons]),
      normalized_mean_conservative_detected_pixels = mean_or_na(spectrum_sum_normalized_per_million[cons]),
      normalized_median_conservative_detected_pixels = median_or_na(spectrum_sum_normalized_per_million[cons]),
      normalized_p95_conservative_detected_pixels = q95(spectrum_sum_normalized_per_million[cons]),
      normalized_maximum_conservative_detected_pixels = max_or_na(spectrum_sum_normalized_per_million[cons]),
      normalized_mean_all_valid_pixels = mean_or_na(norm_valid),
      normalized_median_all_valid_pixels = median_or_na(norm_valid),
      normalized_p95_all_valid_pixels = q95(norm_valid),
      normalized_maximum_all_valid_pixels = max_or_na(norm_valid),
      raw_pixels_above_zero_below_conservative_threshold = sum(raw_target_intensity > 0 & raw_target_intensity < conservative_detection_threshold, na.rm = TRUE),
      pixels_with_multiple_peaks_in_window = sum(number_of_peaks_in_window > 1, na.rm = TRUE),
      maximum_peaks_in_window = max(number_of_peaks_in_window, na.rm = TRUE)
    )
  }, by = .(target_id, mzmine_row_id)]
  summary[, primary_detection_threshold := primary_detection_threshold]
  summary[, primary_minimum_detected_pixels := primary_minimum_detected_pixels]
  summary[, primary_detected_pixel_count := conservative_detected_pixel_count]
  summary[, primary_detection_fraction := conservative_detection_fraction]
  summary[, primary_quantification_eligible := primary_detected_pixel_count >= primary_minimum_detected_pixels]
  summary[, primary_raw_mean_detected_pixels := fifelse(
    primary_quantification_eligible,
    raw_mean_conservative_detected_pixels,
    NA_real_
  )]
  summary[, primary_normalized_mean_detected_pixels := fifelse(
    primary_quantification_eligible,
    normalized_mean_conservative_detected_pixels,
    NA_real_
  )]
  summary[, primary_qc_status := fifelse(
    primary_quantification_eligible,
    "primary_quantification_eligible",
    "sparse_detection_manual_review"
  )]
  data.table::setcolorder(summary, c(
    "dataset_id", "sample_id", "condition", "replicate", "target_id", "mzmine_row_id"
  ))
  data.table::fwrite(summary, file.path(output_dir, summary_filename))
  summary
}

run_one_sample <- function(selected, targets, output_dir,
                           write_pixel_file = FALSE,
                           pixel_filename = NULL,
                           summary_filename = NULL) {
  obj <- import_imzml_data(selected)
  inspect <- inspect_imzml_object(obj)
  if (inspect$n_coords != expected_coordinate_pixels) {
    stop_critical(
      "Unexpected coordinate count for",
      selected$sample_id,
      "- expected",
      expected_coordinate_pixels,
      "but found",
      inspect$n_coords
    )
  }
  if (!isTRUE(inspect$original_arrays_confirmed)) {
    writeLines(c("Cardinal did not expose original variable-length centroid arrays.",
                 paste("Object class:", inspect$object_class)),
               file.path(output_dir, paste0(selected$sample_id, "_imzml_extraction_diagnostic_report.txt")))
    stop_critical("Original centroid arrays were not accessible; stopping extraction for", selected$sample_id)
  }
  if (is.null(pixel_filename)) {
    pixel_filename <- paste0(selected$sample_id, "_target_pixel_values.csv")
  }
  if (is.null(summary_filename)) {
    summary_filename <- paste0(selected$sample_id, "_target_summary.csv")
  }
  pixel_values <- extract_all_pixels(
    inspect, targets, selected, output_dir,
    write_pixel_file = write_pixel_file,
    pixel_filename = pixel_filename
  )
  summary <- summarize_target_pixels(
    pixel_values, output_dir,
    summary_filename = summary_filename
  )
  list(obj = obj, inspect = inspect, pixel_values = pixel_values, summary = summary)
}

###############################################################################
# PROVENANCE OUTPUT
###############################################################################

get_running_script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_args <- sub(
    "^--file=",
    "",
    grep("^--file=", command_args, value = TRUE)
  )
  frame_files <- unlist(
    lapply(
      sys.frames(),
      function(frame) {
        if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
      }
    ),
    use.names = FALSE
  )
  candidates <- unique(c(file_args, frame_files))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) return(NA_character_)
  normalizePath(tail(candidates, 1L), winslash = "/", mustWork = TRUE)
}

write_analysis_configuration <- function(output_dir, package_versions, start_time) {
  configuration <- c(
    dataset_id = dataset_id,
    raw_data_root = raw_data_root,
    mzmine_csv = mzmine_csv,
    analysis_root = analysis_root,
    expected_exposure = expected_exposure,
    expected_ion_mode = expected_ion_mode,
    expected_matrix = expected_matrix,
    expected_pixel_size_um = as.character(expected_pixel_size_um),
    expected_full_pixel = as.character(expected_full_pixel),
    expected_coordinate_pixels = as.character(expected_coordinate_pixels),
    mzmine_expected_row_range = paste(mzmine_expected_row_range, collapse = ","),
    expected_sample_ids = paste(expected_sample_ids, collapse = ","),
    absolute_tolerance = as.character(absolute_tolerance),
    ppm_tolerance = as.character(ppm_tolerance),
    inclusive_detection_threshold = as.character(inclusive_detection_threshold),
    conservative_detection_threshold = as.character(conservative_detection_threshold),
    normalization_scale = as.character(normalization_scale),
    save_pixel_level_output = as.character(save_pixel_level_output),
    primary_detection_threshold = as.character(primary_detection_threshold),
    primary_minimum_detected_pixels = as.character(primary_minimum_detected_pixels),
    r_version = R.version.string,
    run_date_time = format(start_time, "%Y-%m-%d %H:%M:%S %Z")
  )
  configuration_table <- data.table::data.table(
    setting = names(configuration),
    value = unname(configuration)
  )
  package_table <- data.table::data.table(
    setting = paste0("package_version_", package_versions$package),
    value = package_versions$version
  )
  data.table::fwrite(
    data.table::rbindlist(list(configuration_table, package_table)),
    file.path(output_dir, "analysis_configuration_used.csv")
  )
}

write_full_extraction_diagnostic_report <- function(
    output_dir, package_versions, selected_samples, targets,
    combined_summary, sample_qc_dt, start_time) {
  finish_time <- Sys.time()
  expected_combined_rows <- nrow(targets) * length(expected_sample_ids)
  target_sample_counts <- combined_summary[
    , .(sample_rows = .N, samples = data.table::uniqueN(sample_id)),
    by = target_id
  ]
  every_target_complete <- all(
    target_sample_counts$sample_rows == length(expected_sample_ids) &
      target_sample_counts$samples == length(expected_sample_ids)
  )
  pixel_csvs <- grep(
    "pixel_values\\.csv$",
    list.files(output_dir),
    value = TRUE
  )
  report <- c(
    "Full extraction diagnostic report",
    paste("Start time:", format(start_time)),
    paste("Finish time:", format(finish_time)),
    paste(
      "Runtime seconds:",
      round(as.numeric(difftime(finish_time, start_time, units = "secs")), 2)
    ),
    paste("Dataset ID:", dataset_id),
    paste("R version:", R.version.string),
    paste("Cardinal version:", as.character(utils::packageVersion("Cardinal"))),
    paste(
      "Supporting package versions:",
      paste(
        package_versions$package,
        package_versions$version,
        sep = "=",
        collapse = "; "
      )
    ),
    paste("Output directory:", output_dir),
    paste("MZmine CSV:", mzmine_csv),
    paste("Samples quantified:", paste(selected_samples$sample_id, collapse = ", ")),
    paste("Targets quantified:", nrow(targets)),
    paste("Expected combined summary rows:", expected_combined_rows),
    paste("Actual combined summary rows:", nrow(combined_summary)),
    paste(
      "Every target has exactly one row for each configured sample:",
      every_target_complete
    ),
    paste(
      "All samples have the configured number of coordinates:",
      all(sample_qc_dt$number_of_coordinates == expected_coordinate_pixels)
    ),
    paste(
      "All samples have the configured number of unique coordinates:",
      all(sample_qc_dt$number_of_unique_coordinates == expected_coordinate_pixels)
    ),
    paste("Total invalid spectra:", sum(sample_qc_dt$invalid_spectra)),
    paste(
      "Total duplicate coordinates:",
      sum(sample_qc_dt$duplicate_coordinates)
    ),
    paste(
      "Pixel-level CSVs written:",
      if (length(pixel_csvs)) paste(pixel_csvs, collapse = ", ") else "none"
    ),
    paste(
      "Pixel-level output enabled by configuration:",
      save_pixel_level_output
    ),
    "Peak extraction rule: select the nearest centroid peak inside the configured tolerance window; ties use the greater intensity; summed_intensity_in_window is QC only.",
    "Spectrum-sum normalization uses the sum of all stored centroid intensities in each pixel and is not vendor TIC metadata.",
    "Inclusive detection uses raw_target_intensity greater than inclusive_detection_threshold.",
    "Conservative detection uses raw_target_intensity greater than or equal to conservative_detection_threshold.",
    "Primary quantification eligibility uses the conservative detected-pixel count and the configured minimum detected pixels.",
    "Sparse targets were retained and labelled sparse_detection_manual_review.",
    "Fold changes, statistical tests, filtering, imputation, and pseudocounts were not applied.",
    "",
    "Per-sample QC:",
    paste(utils::capture.output(print(sample_qc_dt)), collapse = "\n"),
    "",
    paste(
      "Warnings:",
      if (length(state$warnings)) {
        paste(state$warnings, collapse = " || ")
      } else {
        "none"
      }
    ),
    paste(
      "Errors:",
      if (length(state$errors)) paste(state$errors, collapse = " || ") else "none"
    ),
    paste(
      "Approximate memory use:",
      paste(utils::capture.output(gc()), collapse = " | ")
    ),
    "Conclusion: Complete configured six-sample extraction finished without changing the analytical calculations."
  )
  writeLines(
    report,
    file.path(output_dir, "full_extraction_diagnostic_report.txt")
  )
}

###############################################################################
# SIX-SAMPLE COMBINATION AND QC OUTPUT
###############################################################################

run_six_sample_extraction <- function(dirs, package_versions, start_time) {
  selected_samples <- discover_imzml_pairs(dirs$run)
  selected_samples <- selected_samples[match(expected_sample_ids, sample_id)]
  if (!identical(selected_samples$sample_id, expected_sample_ids)) {
    data.table::fwrite(
      selected_samples,
      file.path(dirs$run, "unexpected_selected_samples.csv")
    )
    stop_critical("Selected samples do not exactly match expected_sample_ids.")
  }
  mz_info <- validate_mzmine_csv(dirs$run)
  targets <- select_all_targets(mz_info, dirs$run)
  combined <- list()
  sample_qc <- list()
  for (i in seq_len(nrow(selected_samples))) {
    selected <- selected_samples[i]
    log_message(
      "Extracting sample",
      i,
      "of",
      nrow(selected_samples),
      selected$sample_id
    )
    result <- run_one_sample(
      selected, targets, dirs$run,
      write_pixel_file = save_pixel_level_output,
      pixel_filename = paste0(selected$sample_id, "_target_pixel_values.csv"),
      summary_filename = paste0(selected$sample_id, "_feature_summary.csv")
    )
    combined[[selected$sample_id]] <- result$summary
    inspect <- result$inspect
    summary <- result$summary
    sample_qc[[selected$sample_id]] <- data.table::data.table(
      sample_id = selected$sample_id,
      condition = selected$condition,
      replicate = selected$replicate,
      imzml_path = selected$imzml_path,
      ibd_path = selected$ibd_path,
      imzml_size_bytes = safe_file_size(selected$imzml_path),
      ibd_size_bytes = safe_file_size(selected$ibd_path),
      number_of_spectra = inspect$n_spectra,
      number_of_coordinates = inspect$n_coords,
      number_of_unique_coordinates = inspect$n_unique_coords,
      coordinate_dimensions = paste(length(unique(inspect$coords$x)), "x", length(unique(inspect$coords$y))),
      invalid_spectra = sum(inspect$invalid),
      duplicate_coordinates = inspect$duplicated_coordinates,
      zero_spectrum_sums = sum(inspect$spectrum_sum == 0, na.rm = TRUE),
      number_of_targets = nrow(targets),
      generated_summary_rows = nrow(summary),
      primary_eligible_features = sum(summary$primary_quantification_eligible, na.rm = TRUE),
      sparse_detection_features = sum(summary$primary_qc_status == "sparse_detection_manual_review", na.rm = TRUE),
      targets_with_multiple_peak_windows = sum(summary$pixels_with_multiple_peaks_in_window > 0, na.rm = TRUE),
      total_multiple_peak_window_pixels = sum(summary$pixels_with_multiple_peaks_in_window, na.rm = TRUE),
      min_spectrum_length = min(inspect$spectrum_lengths),
      median_spectrum_length = stats::median(inspect$spectrum_lengths),
      max_spectrum_length = max(inspect$spectrum_lengths),
      min_mz = inspect$min_mz,
      max_mz = inspect$max_mz,
      sorted_mz_fraction = mean(inspect$sorted),
      original_arrays_confirmed = inspect$original_arrays_confirmed,
      centroided = inspect$centroided,
      continuous = inspect$continuous
    )
    rm(result)
    gc()
  }
  sample_qc_dt <- data.table::rbindlist(sample_qc, fill = TRUE)
  data.table::fwrite(
    sample_qc_dt,
    file.path(dirs$run, "sample_extraction_qc.csv")
  )
  combined_summary <- data.table::rbindlist(
    combined,
    fill = TRUE,
    idcol = "combined_sample_id"
  )
  expected_combined_rows <- nrow(targets) * length(expected_sample_ids)
  if (nrow(combined_summary) != expected_combined_rows) {
    stop_critical(
      "Combined summary row count mismatch. Expected",
      expected_combined_rows,
      "actual",
      nrow(combined_summary)
    )
  }
  pair_counts <- combined_summary[, .N, by = .(target_id, sample_id)]
  target_sample_counts <- combined_summary[
    , .(number_of_samples = data.table::uniqueN(sample_id)),
    by = target_id
  ]
  if (any(pair_counts$N != 1L) ||
      nrow(pair_counts) != expected_combined_rows ||
      any(target_sample_counts$number_of_samples != length(expected_sample_ids))) {
    stop_critical(
      "Combined summary failed the one-row-per-sample-by-feature verification."
    )
  }
  data.table::fwrite(
    combined_summary,
    file.path(dirs$run, "six_sample_combined_feature_summary.csv")
  )
  write_full_extraction_diagnostic_report(
    dirs$run,
    package_versions,
    selected_samples,
    targets,
    combined_summary,
    sample_qc_dt,
    start_time
  )
  list(
    selected = selected_samples,
    targets = targets,
    combined_summary = combined_summary,
    sample_qc = sample_qc_dt
  )
}

main <- function() {
  start_time <- Sys.time()
  state$start_time <- start_time
  validate_user_configuration()
  package_versions <- check_and_load_packages(required_packages)
  dirs <- create_analysis_directories()
  write_analysis_configuration(dirs$run, package_versions, start_time)
  running_script <- get_running_script_path()
  if (is.na(running_script)) {
    add_warning(
      "Could not determine the running script path; the script-used copy was not written."
    )
  } else {
    copied <- file.copy(
      running_script,
      file.path(dirs$run, "01_extract_pixel_intensities_used.R"),
      overwrite = TRUE
    )
    if (!isTRUE(copied)) {
      add_warning(
        "Could not copy the running script into the output directory:",
        running_script
      )
    }
  }
  run_result <- run_six_sample_extraction(
    dirs,
    package_versions,
    start_time
  )
  utils::capture.output(
    utils::sessionInfo(),
    file = file.path(dirs$run, "sessionInfo.txt")
  )
  log_message("Run complete. Output directory:", dirs$run)
  invisible(run_result)
}

main()
