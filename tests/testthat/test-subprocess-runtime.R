test_that("subprocess cwd is independent of project and repository depth", {
  root <- fpq_safe_subprocess_cwd()
  expect_true(dir.exists(root))
  expect_false(grepl("MALDIPixelQuant_ProjectFiles", root, fixed = TRUE))
  expect_false(grepl("FullPixel-MSI-Quant-GitHub-Release", root, fixed = TRUE))
  expect_lt(nchar(root), 260L)
})

test_that("runtime cwd supports an ordinary external process", {
  result <- processx::run(
    file.path(R.home("bin"), "Rscript.exe"),
    c("-e", "cat(normalizePath(getwd(), winslash='/'))"),
    wd = fpq_safe_subprocess_cwd(), error_on_status = FALSE
  )
  expect_identical(result$status, 0L)
  expect_match(result$stdout, "MALDIPixelQuant/runtime", fixed = TRUE)
})
