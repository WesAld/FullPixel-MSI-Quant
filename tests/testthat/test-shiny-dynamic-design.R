test_that("dynamic UI adds, duplicates, reorders and removes conditions", {
  state <- fpq_new_app_state(normalizePath("."))
  shiny::testServer(fpq_design_server, args = list(state = state), {
    session$setInputs(add_condition = 1); session$flushReact()
    session$setInputs(add_treatment = 1); session$flushReact()
    session$setInputs(add_treatment = 2); session$flushReact()
    session$setInputs(add_treatment = 3); session$flushReact()
    expect_length(state$configuration()$conditions, 4)
    session$setInputs(condition = state$configuration()$conditions[[2]]$condition_id,
                      duplicate_condition = 1)
    session$flushReact()
    expect_length(state$configuration()$conditions, 5)
    session$setInputs(move_down = 1); session$flushReact()
    removable <- state$configuration()$conditions[[5]]$condition_id
    session$setInputs(condition = removable, remove_condition = 1); session$flushReact()
    expect_length(state$configuration()$conditions, 4)
  })
})

test_that("dynamic UI supports six and unequal replicate counts", {
  state <- fpq_new_app_state(normalizePath("."))
  state$configuration(make_design(c("Control", "Treatment-A", "Treatment-B"),
                                  c(0, 0, 0)))
  shiny::testServer(fpq_design_server, args = list(state = state), {
    session$setInputs(condition = "Control")
    for (i in 1:6) { session$setInputs(add_replicate = i); session$flushReact() }
    session$setInputs(condition = "Treatment-A")
    for (i in 1:4) { session$setInputs(add_replicate = i); session$flushReact() }
    session$setInputs(condition = "Treatment-B")
    for (i in 1:3) { session$setInputs(add_replicate = i); session$flushReact() }
    counts <- table(vapply(state$configuration()$samples, `[[`, "", "condition_id"))
    expect_equal(as.integer(counts[c("Control", "Treatment-A", "Treatment-B")]), c(6,4,3))
    session$setInputs(samples_rows_selected = 1, duplicate_replicate = 1)
    session$flushReact()
    expect_equal(sum(vapply(state$configuration()$samples,
      function(x) x$condition_id == "Treatment-B", logical(1))), 4)
    session$setInputs(remove_replicate = 1); session$flushReact()
    expect_equal(sum(vapply(state$configuration()$samples,
      function(x) x$condition_id == "Treatment-B", logical(1))), 3)
  })
})

test_that("comparison editor supports one reference against multiple treatments", {
  state <- fpq_new_app_state(normalizePath("."))
  design <- make_design(c("Control", "A", "B", "C"), c(1,1,1,1))
  design$comparisons <- list()
  state$configuration(design)
  shiny::testServer(fpq_design_server, args = list(state = state), {
    targets <- c("A", "B", "C")
    for (i in seq_along(targets)) {
      session$setInputs(comparison_id = paste0("cmp-", i),
        comparison_label = paste(targets[[i]], "versus Control"),
        comparison_reference = "Control", comparison_target = targets[[i]],
        add_comparison = i)
      session$flushReact()
    }
    expect_length(state$configuration()$comparisons, 3)
    expect_true(all(vapply(state$configuration()$comparisons,
      function(x) x$reference_condition_id == "Control", logical(1))))
    session$setInputs(remove_comparison = 1); session$flushReact()
    expect_length(state$configuration()$comparisons, 2)
  })
})
