test_that("target-table inspection supports custom mappings and detects invalid m/z", {
  valid <- tempfile(fileext = ".csv")
  writeLines(c("custom_id,custom_mass,label", "a,100.1,A", "b,200.2,B"), valid)
  x <- fpq_inspect_target_table(valid, "custom_id", "custom_mass")
  expect_length(x$records, 0)
  invalid <- tempfile(fileext = ".csv")
  writeLines(c("custom_id,custom_mass", "a,bad", "a,200.2"), invalid)
  y <- fpq_inspect_target_table(invalid, "custom_id", "custom_mass")
  expect_true(all(c("target.feature_id.duplicate", "target.mz.numeric") %in%
                    vapply(y$records, `[[`, "", "check_id")))
})

test_that("validation separates structure, profile and scope blockers", {
  generic <- make_design(c("Control", "A", "B", "C"), rep(3, 4))
  expect_identical(fpq_validate_structural(generic)$classification, "structurally_valid")
  expect_identical(fpq_validate_profile_compatibility(generic,
    "pvl-fullpixel-reference-v1")$classification, "profile_incompatible")
  expect_true(fpq_workflow_stage_status("extract_pixel_intensities", "POS")$executable)
  expect_true(fpq_workflow_stage_status("extract_pixel_intensities", "NEG",
                                        "Sequential")$executable)
  expect_true(fpq_workflow_stage_status("summarize_replicate_readouts")$executable)
})

test_that("run readiness disables blocked requests and accepts approved shape", {
  root <- tempfile("fpq-run-ui-")
  project <- fpq_create_project(root, "prj-run-ui-test", "Run UI")
  request <- fpq_read_json(file.path(fpq_repo_root(), "config", "examples",
    "pvl_single_negative_extraction.execution_request.json"))
  request$project_id <- project$manifest$project_id
  config <- list(
    analysis_type = "compare_two_groups",
    ion_mode = "NEG",
    exposure_label = "Single",
    conditions = list(
      list(condition_id = "CTRL", analytical_role = "reference",
           is_reference = TRUE),
      list(condition_id = "TRT", analytical_role = "comparison",
           is_reference = FALSE)),
    samples = lapply(request$samples, function(x)
      list(sample_id = x$sample_id, condition_id = x$condition_id,
           replicate_id = x$replicate_id, ion_mode = "NEG",
           inclusion_status = "included")),
    comparisons = list(list(
      comparison_id = "CTRL_vs_TRT",
      reference_condition_id = "CTRL",
      comparison_condition_id = "TRT",
      enabled = TRUE)))
  ready <- fpq_run_readiness(project, request, config)
  expect_true(ready$ready)

  request$ion_mode <- "POS"
  candidate <- fpq_run_readiness(project, request, config)
  expect_true(candidate$ready)
  expect_true(candidate$ready_with_warning)
  expect_true(candidate$candidate_acknowledgement_required)
  expect_false(fpq_run_readiness(project, request, config, demonstration = TRUE)$ready)
})

test_that("run readiness blocks two-group samples without explicit ion mode", {
  root <- tempfile("fpq-run-ui-missing-mode-")
  project <- fpq_create_project(root, "prj-run-ui-missing-mode", "Missing mode")
  request <- fpq_read_json(file.path(fpq_repo_root(), "config", "examples",
    "pvl_single_negative_extraction.execution_request.json"))
  request$project_id <- project$manifest$project_id
  config <- list(
    analysis_type = "compare_two_groups",
    ion_mode = "NEG",
    exposure_label = "Single",
    conditions = list(
      list(condition_id = "CTRL", analytical_role = "reference"),
      list(condition_id = "TRT", analytical_role = "comparison")),
    samples = lapply(request$samples, function(x)
      list(sample_id = x$sample_id, condition_id = x$condition_id,
           replicate_id = x$replicate_id,
           inclusion_status = "included")),
    comparisons = list(list(
      comparison_id = "CTRL_vs_TRT",
      reference_condition_id = "CTRL",
      comparison_condition_id = "TRT",
      enabled = TRUE)))
  readiness <- fpq_run_readiness(project, request, config)
  expect_false(readiness$ready)
  expect_identical(readiness$compatibility$classification,
                   "profile_incompatible")
})

test_that("artifact preview is limited and lineage remains reconstructable", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("id,value", paste0(1:100, ",", 1:100)), path)
  preview <- fpq_preview_csv(path, 25)
  expect_equal(nrow(preview$data), 25)
  expect_equal(preview$columns, c("id", "value"))
})

test_that("Shiny Start control is disabled for blocked execution", {
  root <- tempfile("fpq-blocked-ui-")
  project <- fpq_create_project(root, "prj-blocked-ui", "Blocked UI")
  state <- fpq_new_app_state(fpq_repo_root())
  state$advanced_mode(TRUE)
  state$project(project)
  state$configuration(make_design(c("Control", "A", "B"), c(3,3,3)))
  request <- fpq_read_json(file.path(fpq_repo_root(), "config", "examples",
    "pvl_single_negative_extraction.execution_request.json"))
  request$project_id <- project$manifest$project_id
  request_path <- file.path(root, "blocked-request.json")
  fpq_atomic_write_json(request, request_path)
  shiny::testServer(fpq_runs_server, args = list(state = state), {
    session$setInputs(request_path = request_path, inspect = 1)
    session$flushReact()
    expect_false(start_enabled())
    rendered <- as.character(output$start_control)
    expect_match(rendered[[1]], "aria-disabled=\"true\"", fixed = TRUE)
  })
})

test_that("accessibility-critical controls have text labels and status text", {
  html <- paste(as.character(fpq_design_ui("a")),
                as.character(fpq_validation_ui("b")),
                as.character(fpq_runs_ui("c")))
  for (text in c("Add Treatment", "Add Replicate", "Remove Condition",
                 "Reference condition", "Show statuses",
                 "Full workflow history"))
    expect_match(html, text, fixed = TRUE)
  badge <- as.character(fpq_badge("FAIL", "Blocked because profile is incompatible"))
  expect_match(badge, "aria-label", fixed = TRUE)
  expect_match(badge, "Blocked because profile is incompatible", fixed = TRUE)
})
