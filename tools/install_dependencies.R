#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

cran_packages <- c(
  "DT",
  "digest",
  "htmltools",
  "jsonlite",
  "later",
  "openxlsx",
  "processx",
  "shiny",
  "shinyFiles",
  "yaml",
  "data.table",
  "stringr",
  "matrixStats",
  "pkgload",
  "testthat"
)

missing_cran <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cran)) {
  message("Installing CRAN packages: ", paste(missing_cran, collapse = ", "))
  install.packages(missing_cran)
}

if (!requireNamespace("Cardinal", quietly = TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  message("Installing Bioconductor package: Cardinal")
  BiocManager::install("Cardinal", ask = FALSE, update = FALSE)
}

required <- c(cran_packages, "Cardinal")
available <- vapply(required, requireNamespace, logical(1), quietly = TRUE)

if (!all(available)) {
  stop(
    "Dependency installation incomplete: ",
    paste(names(available)[!available], collapse = ", "),
    call. = FALSE
  )
}

message("R dependencies are available.")
