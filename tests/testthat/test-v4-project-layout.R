make_layout_project <- function(name = "PVL Single Exposure") {
  root <- tempfile("fpq-v4-layout-")
  fpq_create_project(root, "prj-v4-layout-test", name)
}

test_that("new V4 projects encapsulate internal state", {
  project <- make_layout_project()
  paths <- fpq_project_paths(project$root)
  expect_identical(paths$layout, "v4")
  expect_identical(basename(paths$internal_root),
                   "MALDIPixelQuant_ProjectFiles")
  expect_true(file.exists(paths$manifest))
  expect_true(all(dir.exists(c(paths$datasets, paths$runs, paths$results,
                               paths$reports, paths$temporary))))
  expect_setequal(list.files(project$root), "MALDIPixelQuant_ProjectFiles")
  expect_identical(fpq_open_project(project$root)$manifest$project_id,
                   project$manifest$project_id)
  expect_identical(normalizePath(fpq_open_project(paths$internal_root)$root,
                                 winslash = "/"),
                   normalizePath(project$root, winslash = "/"))
})

test_that("project result filenames are safe and human readable", {
  expect_identical(fpq_sanitize_project_filename("PVL Single Exposure"),
                   "PVL_Single_Exposure")
  expect_identical(fpq_sanitize_project_filename("LinO-Phe Organoids"),
                   "LinO-Phe_Organoids")
  expect_identical(fpq_sanitize_project_filename('A:B/C\\D*E?'), "A_B_C_D_E")
  project <- make_layout_project("PVL Single Exposure")
  expect_identical(basename(fpq_project_final_workbook_path(project)),
    "PVL_Single_Exposure_MALDIPixelQuant_Results.xlsx")
})

test_that("validated workbook promotion is hash-identical and replaceable", {
  project <- make_layout_project()
  paths <- fpq_project_paths(project$root)
  source1 <- file.path(paths$results, "run-1.xlsx")
  source2 <- file.path(paths$results, "run-2.xlsx")
  writeBin(charToRaw("validated-workbook-one"), source1)
  writeBin(charToRaw("validated-workbook-two"), source2)
  local_mocked_bindings(
    fpq_validate_published_final_results_workbook = function(path)
      list(status = "PASS", sheets = "fixture"),
    .package = "FullPixelMSIQuant"
  )
  artifact <- function(id, path) list(artifact_id = id,
    artifact_type = "final_results_workbook", validation_status = "PASS",
    path = path, sha256 = fpq_hash_file(path), producing_run_id = id)
  first <- fpq_promote_validated_workbook(project, artifact("run-1", source1))
  expect_true(first$promoted)
  expect_identical(fpq_hash_file(first$path), fpq_hash_file(source1))
  second <- fpq_promote_validated_workbook(project, artifact("run-2", source2))
  expect_identical(second$path, first$path)
  expect_identical(fpq_hash_file(second$path), fpq_hash_file(source2))
  expect_setequal(list.files(project$root), c(
    "MALDIPixelQuant_ProjectFiles",
    "PVL_Single_Exposure_MALDIPixelQuant_Results.xlsx"))
  expect_true(file.exists(source1))
  expect_true(file.exists(source2))
  edges <- fpq_read_collection(paths$provenance)
  expect_true(any(vapply(edges, function(x)
    identical(x$relationship, "promoted_as_researcher_facing_copy"), logical(1))))
})

test_that("failed or unvalidated output cannot replace a successful workbook", {
  project <- make_layout_project()
  destination <- fpq_project_final_workbook_path(project)
  writeBin(charToRaw("existing-success"), destination)
  before <- fpq_hash_file(destination)
  failed <- list(artifact_type = "final_results_workbook",
    validation_status = "FAIL", path = tempfile(fileext = ".xlsx"))
  expect_false(fpq_promote_validated_workbook(project, failed)$promoted)
  expect_identical(fpq_hash_file(destination), before)
})

test_that("legacy project roots remain discoverable without migration", {
  modern <- make_layout_project("Legacy Fixture")
  internal <- fpq_project_paths(modern$root)$internal_root
  legacy_root <- tempfile("fpq-legacy-layout-")
  dir.create(legacy_root)
  children <- list.files(internal, full.names = TRUE, all.files = TRUE,
                         no.. = TRUE)
  expect_true(all(file.rename(children, file.path(legacy_root,
                                                  basename(children)))))
  legacy <- fpq_open_project(legacy_root)
  expect_identical(fpq_project_paths(legacy$root)$layout, "legacy")
  expect_true(file.exists(file.path(legacy_root, "project.json")))
  expect_false(dir.exists(file.path(legacy_root,
                                    "MALDIPixelQuant_ProjectFiles")))
})

test_that("V4 distinguishes the project folder from historical run folders", {
  source <- paste(readLines(file.path(fpq_repo_root(), "R", "v4_pages.R"),
                            warn = FALSE), collapse = "\n")
  expect_match(source, '"Open Project Folder"', fixed = TRUE)
  expect_match(source, '"Open Output Folder"', fixed = TRUE)
  expect_match(source, "fpq_project_paths(state$project()$root)$root",
               fixed = TRUE)
})
