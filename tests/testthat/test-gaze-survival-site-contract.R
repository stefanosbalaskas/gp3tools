test_that("Gazepoint survival site contract remains discoverable", {
  root <- gp3tools_require_source_contract_root()
  pkgdown <- file.path(root, "_pkgdown.yml")

  expect_true(file.exists(pkgdown))
  text <- paste(readLines(pkgdown, warn = FALSE), collapse = "\n")

  expect_match(text, "articles/gaze-survival-adapter", fixed = TRUE)
  expect_match(text, "articles/gaze-survival-verification-example", fixed = TRUE)
  expect_match(text, "articles/gaze-survival-reproducibility-checklist", fixed = TRUE)
  expect_match(text, "prepare_gazepoint_survival_data", fixed = TRUE)
  expect_match(text, "run_gazepoint_latency_analysis", fixed = TRUE)
})

test_that("Gazepoint survival adapter examples remain in the source tree", {
  root <- gp3tools_require_source_contract_root()
  required <- c(
    "R/gaze-survival-adapter.R",
    "man/gaze-survival-adapter.Rd",
    "inst/examples/worked_gazepoint_survival_analysis.R",
    "inst/examples/worked_gazepoint_verification_survival.R",
    "vignettes/articles/gaze-survival-adapter.Rmd",
    "vignettes/articles/gaze-survival-verification-example.Rmd",
    "vignettes/articles/gaze-survival-reproducibility-checklist.Rmd"
  )
  expect_true(all(file.exists(file.path(root, required))))
})
