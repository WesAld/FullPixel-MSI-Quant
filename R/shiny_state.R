fpq_new_app_state <- function(repository_root) {
  list(repository_root = repository_root,
    advanced_mode = shiny::reactiveVal(FALSE),
    analysis_type = shiny::reactiveVal("quantify_only"),
    selected_ion_mode = shiny::reactiveVal("NEG"),
    detected_samples = shiny::reactiveVal(list()),
    mzmine_files = shiny::reactiveVal(list()),
    project = shiny::reactiveVal(NULL),
    configuration = shiny::reactiveVal(list(conditions = list(), samples = list(),
                                             comparisons = list(), ion_mode = "NEG",
                                             exposure_label = "Single",
                                             primary_normalization_method = "spectrum_sum",
                                             statistical_analyses = list(
                                               jackknife = list(enabled = FALSE)),
                                             mass_tolerance = fpq_default_mass_tolerance())),
    dirty = shiny::reactiveVal(FALSE),
    validations = shiny::reactiveVal(list()),
    annotation_mapping_validation = shiny::reactiveVal(list(
      status = "NOT_EVALUABLE",
      message = "Annotated table mappings have not been inspected in this session."
    )),
    selected_artifact = shiny::reactiveVal(NULL),
    candidate_acknowledgement = shiny::reactiveVal(NULL),
    workflow_process = shiny::reactiveVal(NULL),
    workflow_id = shiny::reactiveVal(NULL),
    active_worker = shiny::reactiveVal(NULL),
    last_error = shiny::reactiveVal(NULL),
    demonstration = shiny::reactiveVal(FALSE))
}

fpq_project_id_from_name <- function(name) {
  slug <- tolower(trimws(name %||% ""))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("(^-+|-+$)", "", slug)
  if (!nzchar(slug)) slug <- "project"
  slug <- substr(slug, 1L, 48L)
  paste0("prj-", slug, "-", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))
}

fpq_load_project_configuration <- function(project) {
  files <- list.files(fpq_project_paths(project$root)$datasets, pattern = "\\.json$",
                      full.names = TRUE)
  if (!length(files)) {
    preferences <- project$manifest$project_preferences %||% list()
    return(list(conditions = list(), samples = list(), comparisons = list(),
      ion_mode = preferences$ion_mode %||%
        preferences$initial_ion_registration %||% "NEG",
      exposure_label = preferences$default_exposure %||% "Unspecified",
      pixel_size_um = preferences$pixel_size_um %||% 40,
      expected_coordinate_count =
        preferences$expected_coordinate_count %||% 1600,
      pixel_geometry_policy =
        preferences$pixel_geometry_policy %||% "strict_predefined",
      primary_normalization_method =
        preferences$primary_normalization_method %||% "spectrum_sum",
      statistical_analyses = preferences$statistical_analyses %||%
        list(jackknife = list(enabled = FALSE)),
      positive_matrix = preferences$positive_matrix %||% "DHB",
      negative_matrix = preferences$negative_matrix %||% "9AA",
      mass_tolerance = fpq_default_mass_tolerance()))
  }
  configurations <- lapply(files, fpq_read_json)
  sample_counts <- vapply(configurations,
    function(x) length(x$samples %||% list()), integer(1))
  candidates <- which(sample_counts == max(sample_counts))
  if (length(candidates) > 1L) {
    modified <- file.info(files[candidates])$mtime
    candidates <- candidates[[which.max(modified)]]
  }
  resolved <- configurations[[candidates[[1]]]]
  preferences <- project$manifest$project_preferences %||% list()
  resolved$exposure_label <- resolved$exposure_label %||%
    preferences$default_exposure %||% "Unspecified"
  resolved$pixel_size_um <- resolved$pixel_size_um %||%
    preferences$pixel_size_um %||% 40
  resolved$expected_coordinate_count <-
    resolved$expected_coordinate_count %||%
    preferences$expected_coordinate_count %||% 1600
  resolved$pixel_geometry_policy <-
    resolved$pixel_geometry_policy %||%
    preferences$pixel_geometry_policy %||% "strict_predefined"
  resolved$primary_normalization_method <- fpq_normalization_method(list(
    primary_normalization_method = resolved$primary_normalization_method %||%
      preferences$primary_normalization_method %||% "spectrum_sum"))
  resolved$positive_matrix <- resolved$positive_matrix %||%
    preferences$positive_matrix %||% "DHB"
  resolved$negative_matrix <- resolved$negative_matrix %||%
    preferences$negative_matrix %||% "9AA"
  resolved$mass_tolerance <- resolved$mass_tolerance %||%
    fpq_default_mass_tolerance()
  resolved$statistical_analyses <- resolved$statistical_analyses %||%
    preferences$statistical_analyses %||% list(jackknife = list(enabled = FALSE))
  resolved
}

fpq_app_summary <- function(state) {
  project <- state$project(); config <- state$configuration()
  list(project_open = !is.null(project),
    project_id = if (is.null(project)) NULL else project$manifest$project_id,
    project_name = if (is.null(project)) NULL else project$manifest$project_name,
    profile = if (is.null(project)) NULL else project$manifest$scientific_profile_id,
    conditions = length(config$conditions %||% list()),
    samples = length(config$samples %||% list()),
    comparisons = length(config$comparisons %||% list()),
    dirty = state$dirty())
}
