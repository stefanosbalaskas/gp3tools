test_that("Gazepoint Data Quality website contract remains discoverable", {
  root <- gp3tools_require_source_contract_root()
  article <- file.path(
    root, "vignettes", "articles", "gazepoint-data-quality-workflow.Rmd"
  )
  pkgdown <- file.path(root, "_pkgdown.yml")

  expect_true(file.exists(article))
  expect_true(file.exists(pkgdown))

  article_text <- paste(readLines(article, warn = FALSE), collapse = "\n")
  pkgdown_text <- paste(readLines(pkgdown, warn = FALSE), collapse = "\n")

  expect_match(pkgdown_text, "articles/gazepoint-data-quality-workflow", fixed = TRUE)
  expect_match(article_text, "## Focused plot gallery", fixed = TRUE)

  for (symbol in c(
    "plot_gaze_accuracy",
    "plot_gaze_precision",
    "plot_bcea",
    "plot_sampling_intervals",
    "plot_gazepoint_quality_dashboard",
    "report_gazepoint_quality"
  )) {
    expect_match(article_text, symbol, fixed = TRUE)
  }

  exports <- getNamespaceExports("gp3tools")
  expect_true(all(c(
    "summarise_gazepoint_spatial_quality",
    "create_gazepoint_quality_report",
    "plot_gazepoint_quality_dashboard",
    "report_gazepoint_quality"
  ) %in% exports))
})
