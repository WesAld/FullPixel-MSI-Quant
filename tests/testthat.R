library(testthat)
library(FullPixelMSIQuant)
root_candidates <- c(
  Sys.getenv("FULLPIXEL_MSI_QUANT_ROOT", unset = ""),
  getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."),
  file.path(getwd(), "..", "00_pkg_src", "FullPixelMSIQuant"),
  file.path(getwd(), "..", "..", "00_pkg_src", "FullPixelMSIQuant"))
root_candidates <- unique(root_candidates[nzchar(root_candidates)])
valid <- vapply(root_candidates, function(path)
  file.exists(file.path(path, "DESCRIPTION")) &&
    file.exists(file.path(path, "config", "scientific_core_manifest.yml")),
  logical(1))
if (any(valid))
  Sys.setenv(FULLPIXEL_MSI_QUANT_ROOT =
    normalizePath(root_candidates[which(valid)[[1]]], mustWork = TRUE))
test_check("FullPixelMSIQuant")
