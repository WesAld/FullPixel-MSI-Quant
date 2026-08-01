make_full_workflow_fixture <- function() {
  root <- tempfile("fpq-full-workflow-")
  project <- fpq_create_project(root, "prj-full-workflow-test",
                                "Full workflow test")
  conditions <- list(make_condition("CTRL", "CTRL", TRUE, 1L),
                     make_condition("TRT", "TRT", FALSE, 2L))
  samples <- list()
  for (mode in c("POS", "NEG"))
    for (condition in c("CTRL", "TRT"))
      for (replicate in paste0("R", 1:3)) {
        id <- paste(condition, replicate, mode, sep = "_")
        imzml <- file.path(root, paste0(id, ".imzML"))
        ibd <- file.path(root, paste0(id, ".IBD"))
        writeLines(paste("imzML", id), imzml)
        writeLines(paste("IBD", id), ibd)
        samples[[length(samples) + 1L]] <- list(
          sample_id = id, sample_label = paste(condition, replicate, sep = "_"),
          biological_sample = paste(condition, replicate, sep = "_"),
          condition_id = condition, replicate_id = replicate,
          inclusion_status = "included", ion_mode = mode,
          imzml_path = imzml, ibd_path = ibd)
      }
  mzmine <- list()
  for (mode in c("POS", "NEG")) {
    path <- file.path(root, paste0("mzmine-", mode, ".csv"))
    writeLines(c("id,mz,rt", "1,100.1,0"), path)
    mzmine[[mode]] <- list(path = path, status = "PASS",
      mappings = list(feature_id = "id", mz = "mz", retention_time = "rt"))
  }
  config <- list(schema_version = "1.0.0",
    dataset_id = "dset-full-workflow", analysis_type = "compare_two_groups",
    ion_mode = "BOTH", exposure_label = "Sequential",
    conditions = conditions, samples = samples,
    comparisons = list(make_comparison("CTRL_vs_TRT", "CTRL", "TRT")),
    mzmine_registrations = mzmine)
  validations <- list(readiness = "ready_with_warning",
    profile = list(scientific_status = list(
      overall = "technical_validation_candidate")))
  list(project = project, config = config, validations = validations)
}

test_that("automatic workflow requests contain all persisted scientific inputs", {
  x <- make_full_workflow_fixture()
  prepared <- fpq_prepare_full_workflow(x$project, x$config, x$validations,
    hash_raw_inputs = FALSE)
  expect_identical(unlist(prepared$record$modes), c("POS", "NEG"))
  expect_length(prepared$requests, 2L)
  expect_equal(sum(vapply(prepared$requests, function(request)
    length(request$samples), integer(1))), 12L)
  expect_true(all(vapply(prepared$requests, function(request)
    file.exists(request$registered_inputs$aligned_target_table$path),
    logical(1))))
  expect_true(all(vapply(prepared$requests, function(request)
    identical(request$comparison$comparison_id, "CTRL_vs_TRT"), logical(1))))
  expect_true(all(vapply(prepared$requests, function(request)
    identical(request$scientific_profile$profile_id,
      x$project$manifest$scientific_profile_id), logical(1))))
  expect_true(all(file.exists(unlist(prepared$record$request_paths))))
  expect_identical(prepared$record$scientific_status,
                   "technical_validation_candidate")
})

test_that("automatic requests become immutable after raw identities are finalized", {
  x <- make_full_workflow_fixture()
  prepared <- fpq_prepare_full_workflow(x$project, x$config, x$validations,
    hash_raw_inputs = FALSE)
  fpq_finalize_workflow_requests(x$project, prepared$record$workflow_id)
  paths <- unlist(prepared$record$request_paths)
  first <- vapply(paths, fpq_hash_file, character(1))
  fpq_finalize_workflow_requests(x$project, prepared$record$workflow_id)
  second <- vapply(paths, fpq_hash_file, character(1))
  expect_identical(first, second)
  requests <- lapply(paths, fpq_read_json)
  expect_true(all(vapply(requests, function(request)
    request$registered_inputs$raw_file_hash_registry$path != "pending",
    logical(1))))
})

test_that("workflow stages begin waiting and enforce combined-export parents", {
  x <- make_full_workflow_fixture()
  prepared <- fpq_prepare_full_workflow(x$project, x$config, x$validations,
    hash_raw_inputs = FALSE)
  stages <- prepared$record$stages
  expect_true(all(vapply(stages, function(stage)
    stage$status == "waiting", logical(1))))
  state <- fpq_new_app_state(fpq_repo_root())
  state$project(x$project)
  state$configuration(x$config)
  state$validations(x$validations)
  shiny::testServer(fpq_workflow_server, args = list(state = state), {
    session$flushReact()
    cards <- paste(as.character(output$stages), collapse = "")
    expect_match(cards, "waiting_for_extraction", fixed = TRUE)
    expect_match(cards, "waiting_for_replicate_summary", fixed = TRUE)
    expect_match(cards, "waiting_for_annotation_integration", fixed = TRUE)
  })
})

test_that("validated Simple Mode exposes one primary full-workflow action", {
  x <- make_full_workflow_fixture()
  state <- fpq_new_app_state(fpq_repo_root())
  state$project(x$project)
  state$configuration(x$config)
  state$validations(x$validations)
  state$advanced_mode(FALSE)
  shiny::testServer(fpq_workflow_server, args = list(state = state), {
    session$flushReact()
    action <- paste(as.character(output$primary_action), collapse = "")
    expect_match(action, "Run Full-Pixel Quantification", fixed = TRUE)
    expect_false(grepl("Prepared execution-request JSON", action, fixed = TRUE))
  })
})

test_that("unvalidated and active workflows disable duplicate execution", {
  x <- make_full_workflow_fixture()
  state <- fpq_new_app_state(fpq_repo_root())
  state$project(x$project)
  state$configuration(x$config)
  shiny::testServer(fpq_workflow_server, args = list(state = state), {
    session$flushReact()
    expect_match(paste(as.character(output$primary_action), collapse = ""),
                 "aria-disabled=\"true\"", fixed = TRUE)
  })
  state$validations(x$validations)
  prepared <- fpq_prepare_full_workflow(x$project, x$config, x$validations,
    hash_raw_inputs = FALSE)
  expect_identical(prepared$record$status, "prepared")
  shiny::testServer(fpq_workflow_server, args = list(state = state), {
    session$flushReact()
    expect_match(paste(as.character(output$primary_action), collapse = ""),
                 "A full workflow is already active.", fixed = TRUE)
  })
})

test_that("cancellation and retry preserve history with distinct identifiers", {
  x <- make_full_workflow_fixture()
  prepared <- fpq_prepare_full_workflow(x$project, x$config, x$validations,
    hash_raw_inputs = FALSE)
  cancelled <- fpq_update_workflow_record(x$project,
    prepared$record$workflow_id,
    list(status = "cancelled", finished_at = fpq_now()))
  retry <- fpq_retry_workflow(x$project, cancelled$workflow_id,
    x$config, x$validations, hash_raw_inputs = FALSE)
  expect_false(identical(retry$record$workflow_id, cancelled$workflow_id))
  expect_identical(retry$record$retry_of, cancelled$workflow_id)
  expect_length(fpq_list_workflow_records(x$project), 2L)
})

test_that("candidate acknowledgement is linked to workflow provenance", {
  x <- make_full_workflow_fixture()
  prepared <- fpq_prepare_full_workflow(x$project, x$config, x$validations,
    hash_raw_inputs = FALSE)
  acknowledgement <- fpq_record_candidate_acknowledgement(x$project,
    list(run_id = prepared$record$workflow_id,
         request_id = paste0("request-", prepared$record$workflow_id)))
  record <- fpq_update_workflow_record(x$project,
    prepared$record$workflow_id,
    list(candidate_acknowledgement = acknowledgement))
  expect_true(record$candidate_acknowledgement$acknowledged)
  provenance <- fpq_read_collection(
    fpq_project_paths(x$project$root)$provenance)
  expect_true(any(vapply(provenance, function(edge)
    identical(edge$subject_id, prepared$record$workflow_id) &&
      identical(edge$relationship, "acknowledges_scientific_status"),
    logical(1))))
})
