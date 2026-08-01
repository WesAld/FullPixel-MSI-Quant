fpq_normalize_column_name <- function(x)
  gsub("[^a-z0-9]+", "", tolower(trimws(x)))

fpq_mzmine_aliases <- function() list(
  feature_id = c("id", "rowid", "featureid", "peakid"),
  mz = c("mz", "rowmz", "averagemz", "meanmz", "masscharge", "masstocharge"),
  annotation = c("annotation", "compoundname", "name", "identity",
                 "preferredannotationname"),
  formula = c("formula", "molecularformula", "sumformula"),
  adduct = c("adduct", "iontype", "adducttype"),
  retention_time = c("rt", "rowretentiontime", "retentiontime", "averagert"),
  annotation_score = c("annotationscore", "score", "similarity", "confidence",
                       "compounddatabasescore"))

fpq_detect_mzmine_columns <- function(column_names) {
  normalized <- fpq_normalize_column_name(column_names)
  aliases <- fpq_mzmine_aliases()
  mappings <- list()
  ambiguous <- list()
  for (field in names(aliases)) {
    index <- which(normalized %in% aliases[[field]])
    if (length(index) == 1L) mappings[[field]] <- column_names[[index]]
    else if (length(index) > 1L) ambiguous[[field]] <- as.list(column_names[index])
    else mappings[[field]] <- NULL
  }
  missing_required <- setdiff(c("feature_id", "mz"),
                              names(Filter(Negate(is.null), mappings)))
  status <- if (length(ambiguous) || length(missing_required)) "NEEDS_REVIEW" else "PASS"
  list(status = status, mappings = mappings, ambiguous = ambiguous,
       missing_required = as.list(missing_required), columns = as.list(column_names))
}

fpq_inspect_mzmine_csv <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  header <- tryCatch(utils::read.csv(path, nrows = 0L, check.names = FALSE,
                                     stringsAsFactors = FALSE),
    error = function(e) fpq_stop("mzmine.csv_unreadable",
      paste("MZmine CSV could not be read:", conditionMessage(e)), "input"))
  detection <- fpq_detect_mzmine_columns(names(header))
  rows <- tryCatch(nrow(utils::read.csv(path, check.names = FALSE,
                                        stringsAsFactors = FALSE)),
    error = function(e) fpq_stop("mzmine.csv_unreadable",
      paste("MZmine CSV could not be read:", conditionMessage(e)), "input"))
  c(list(path = path, filename = basename(path), readable = TRUE,
         feature_count = rows), detection)
}

fpq_confirm_mzmine_mapping <- function(inspection, mapping = inspection$mappings) {
  required <- c("feature_id", "mz")
  missing <- required[!vapply(required, function(field)
    !is.null(mapping[[field]]) && length(mapping[[field]]) == 1L &&
      nzchar(mapping[[field]]), logical(1))]
  if (length(missing))
    fpq_stop("mzmine.mapping_required",
      paste("Select columns for:", paste(missing, collapse = ", ")), "configuration")
  if (anyDuplicated(unlist(mapping, use.names = FALSE)))
    fpq_stop("mzmine.mapping_duplicate",
             "One CSV column cannot be assigned to multiple fields.", "configuration")
  inspection$mappings <- mapping
  inspection$status <- "PASS"
  inspection$ambiguous <- list()
  inspection$missing_required <- list()
  inspection
}
