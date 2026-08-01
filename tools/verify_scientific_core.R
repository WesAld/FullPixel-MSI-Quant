#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
script_path <- if (length(file_arg)) normalizePath(file_arg[[1]], mustWork = TRUE) else NA_character_
if (is.na(script_path)) {
  stop("Run this utility with Rscript so the repository root can be determined.", call. = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
manifest_path <- file.path(repo_root, "config", "scientific_core_manifest.yml")

for (pkg in c("yaml", "digest")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required verification package is unavailable: ", pkg, call. = FALSE)
  }
}
if (!file.exists(manifest_path)) stop("Manifest not found: ", manifest_path, call. = FALSE)
manifest <- yaml::read_yaml(manifest_path)
if (is.null(manifest$scripts) || !length(manifest$scripts)) {
  stop("Manifest contains no registered scientific scripts.", call. = FALSE)
}

failed <- FALSE
for (entry in manifest$scripts) {
  rel <- entry$repository_relative_path
  expected_hash <- toupper(entry$sha256)
  expected_size <- as.numeric(entry$file_size_bytes)
  path <- file.path(repo_root, rel)
  if (!file.exists(path)) {
    cat("FAIL missing:", rel, "\n")
    failed <- TRUE
    next
  }
  observed_hash <- toupper(digest::digest(file = path, algo = "sha256", serialize = FALSE))
  observed_size <- file.info(path)$size
  hash_ok <- identical(observed_hash, expected_hash)
  size_ok <- identical(as.numeric(observed_size), expected_size)
  cat(if (hash_ok && size_ok) "PASS" else "FAIL", rel,
      "sha256=", observed_hash, "size=", observed_size, "\n")
  if (!hash_ok || !size_ok) failed <- TRUE
}
if (failed) quit(save = "no", status = 1L)
cat("Scientific-core integrity verification PASSED.\n")
quit(save = "no", status = 0L)
