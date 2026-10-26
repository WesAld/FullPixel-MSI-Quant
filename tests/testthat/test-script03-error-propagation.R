test_that("Script 03 dispatcher surfaces blocking adapter validation", {
  report <- list(validation_records = list(list(
    check_id = "environment.r", status = "FAIL", blocking = TRUE,
    message = "Configured Rscript is unavailable.")))
  expect_identical(fpq_script03_adapter_failure_message(report),
                   "Configured Rscript is unavailable.")
})

test_that("Script 03 dispatcher retains regression fallback when unspecific", {
  expect_identical(fpq_script03_adapter_failure_message(NULL),
    "Script 03 adapter failed without a passing regression report.")
  expect_identical(fpq_script03_adapter_failure_message(list(
    validation_records = list())),
    "Script 03 adapter failed without a passing regression report.")
})

test_that("Script 03 dispatcher prefers explicit adapter error", {
  expect_identical(fpq_script03_adapter_failure_message(list(
    error = "Frozen Script 03 failed for comparison.")),
    "Frozen Script 03 failed for comparison.")
})
