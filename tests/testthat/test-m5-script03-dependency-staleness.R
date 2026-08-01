test_that("Script 03 input requires same project, dataset, active state and hash", {
  root <- tempfile("fpq-m5-script03-")
  project <- fpq_create_project(root, "prj-m5-script03", "M5 Script 03")
  source <- tempfile(fileext = ".csv")
  writeLines("target_id,value\nA,1", source)
  upstream <- fpq_register_artifact(project, list(
    artifact_id = "art-m5-upstream", artifact_type = "pixel_intensity_table",
    path = source, project_id = project$manifest$project_id,
    dataset_id = "dset-m5", sha256 = fpq_hash_file(source),
    file_size = file.info(source)$size, validation_status = "PASS",
    scientific_profile_id = "pvl-fullpixel-reference-v1",
    scientific_core_version = "1.0.0", producing_run_id = "run-m5-upstream",
    producing_stage_id = "extract_pixel_intensities", ion_mode = "NEG",
    exposure = "Single"), activate = TRUE)
  request <- list(dataset_id = "dset-m5",
    registered_inputs = list(pixel_intensity_artifact = list(
      artifact_id = upstream$artifact_id, project_id = upstream$project_id,
      dataset_id = upstream$dataset_id, sha256 = upstream$sha256)))
  expect_identical(fpq_validate_script03_input(project, request)$artifact_id,
                   upstream$artifact_id)
  wrong <- request
  wrong$registered_inputs$pixel_intensity_artifact$project_id <- "prj-other"
  expect_error(fpq_validate_script03_input(project, wrong),
               class = "fpq_scientific_error")
  wrong <- request
  wrong$registered_inputs$pixel_intensity_artifact$dataset_id <- "dset-other"
  expect_error(fpq_validate_script03_input(project, wrong),
               class = "fpq_scientific_error")
  wrong <- request
  wrong$registered_inputs$pixel_intensity_artifact$sha256 <- paste(rep("0", 64),
                                                                   collapse = "")
  expect_error(fpq_validate_script03_input(project, wrong),
               class = "fpq_scientific_error")
})

test_that("changed or superseded upstream artifacts make Script 03 outputs stale", {
  root <- tempfile("fpq-m5-stale-")
  project <- fpq_create_project(root, "prj-m5-stale", "M5 stale")
  upstream_path <- tempfile(fileext = ".csv")
  downstream_path <- tempfile(fileext = ".csv")
  writeLines("id,value\nA,1", upstream_path)
  writeLines("id,summary\nA,1", downstream_path)
  upstream <- fpq_register_artifact(project, list(
    artifact_id = "art-stale-upstream", artifact_type = "pixel_intensity_table",
    path = upstream_path, project_id = project$manifest$project_id,
    dataset_id = "dset-stale", sha256 = fpq_hash_file(upstream_path),
    file_size = file.info(upstream_path)$size, validation_status = "PASS",
    scientific_profile_id = "pvl-fullpixel-reference-v1",
    scientific_core_version = "1.0.0", ion_mode = "NEG",
    exposure = "Single"), activate = TRUE)
  downstream <- fpq_register_artifact(project, list(
    artifact_id = "art-stale-downstream",
    artifact_type = "replicate_readout_summary", path = downstream_path,
    project_id = project$manifest$project_id, dataset_id = "dset-stale",
    sha256 = fpq_hash_file(downstream_path),
    file_size = file.info(downstream_path)$size, validation_status = "PASS",
    scientific_profile_id = "pvl-fullpixel-reference-v1",
    scientific_core_version = "1.0.0", ion_mode = "NEG",
    exposure = "Single", parent_artifact_ids = list(upstream$artifact_id)),
    activate = TRUE)
  expect_identical(downstream$active_status, "active")
  writeLines("id,value\nA,2", upstream_path)
  fpq_verify_artifact(project, upstream$artifact_id)
  expect_identical(fpq_inspect_artifact(project,
    downstream$artifact_id)$active_status, "stale")
})

test_that("two-stage provenance traverses artifact-run-artifact lineage", {
  root <- tempfile("fpq-m5-lineage-")
  project <- fpq_create_project(root, "prj-m5-lineage", "M5 lineage")
  edges <- list(
    list(schema_version = "1.0.0", provenance_edge_id = "prov-m5-a",
      project_id = project$manifest$project_id, subject_type = "artifact",
      subject_id = "art-summary", relationship = "produced_by",
      object_type = "run", object_id = "run-script03",
      recorded_at = fpq_now(), run_id = "run-script03",
      evidence_hash = paste(rep("A", 64), collapse = ""), notes = ""),
    list(schema_version = "1.0.0", provenance_edge_id = "prov-m5-b",
      project_id = project$manifest$project_id, subject_type = "artifact",
      subject_id = "art-pixels", relationship = "used_by",
      object_type = "run", object_id = "run-script03",
      recorded_at = fpq_now(), run_id = "run-script03",
      evidence_hash = paste(rep("B", 64), collapse = ""), notes = ""))
  fpq_store_provenance(project, edges)
  traced <- fpq_trace_provenance(project, "art-summary")
  expect_length(traced, 2)
  expect_true("art-pixels" %in% vapply(traced, `[[`, "", "subject_id"))
})
