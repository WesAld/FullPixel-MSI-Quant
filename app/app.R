file_argument <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
configured_root <- Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT", unset = "")
root_candidates <- c(configured_root, getwd(), file.path(getwd(), ".."),
  if (length(file_argument)) file.path(dirname(file_argument[[1]]), "..") else "")
root_candidates <- unique(root_candidates[nzchar(root_candidates)])
valid_root <- vapply(root_candidates, function(path)
  file.exists(file.path(path, "DESCRIPTION")) &&
    file.exists(file.path(path, "config", "scientific_core_manifest.yml")),
  logical(1))
if (!any(valid_root))
  stop("FullPixel MSI Quant repository root could not be resolved.", call. = FALSE)
repository_root <- normalizePath(root_candidates[which(valid_root)[[1]]], mustWork = TRUE)
Sys.setenv(FULLPIXEL_MSI_QUANT_ROOT = repository_root)
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(repository_root, quiet = TRUE)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(repository_root, quiet = TRUE)
} else {
  stop("Install pkgload or devtools to run the application from repository source.",
       call. = FALSE)
}
fpq_shiny_app(repository_root)
