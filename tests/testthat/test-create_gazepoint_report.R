testthat::test_that("create_gazepoint_report creates an HTML report", {
  output_dir <- tempfile()
  dir.create(output_dir)

  results <- list(
    all_gaze = tibble::tibble(x = 1:3),
    all_fix = tibble::tibble(x = 1:2),
    sampling = tibble::tibble(
      USER_FILE = c("User 0", "User 1"),
      MEDIA_ID = c(0, 0),
      estimated_hz = c(60, 48),
      duration_sec = c(10, 10)
    ),
    quality = tibble::tibble(
      USER_FILE = c("User 0", "User 1"),
      MEDIA_ID = c(0, 0),
      FPOGV_valid_pct = c(95, 55),
      RPV_valid_pct = c(95, 50)
    ),
    flagged_quality = tibble::tibble(
      USER_FILE = c("User 0", "User 1"),
      MEDIA_ID = c(0, 0),
      FPOGV_valid_pct = c(95, 55),
      RPV_valid_pct = c(95, 50),
      review_required = c(FALSE, TRUE)
    ),
    aoi_table = tibble::tibble(
      USER_ID = c(0, 1),
      MEDIA_ID = c(0, 0),
      AOI = c("AOI 1", "AOI 2"),
      fixation_count = c(3, 4)
    )
  )

  report_file <- file.path(output_dir, "report.html")

  written <- create_gazepoint_report(
    results = results,
    output_file = report_file,
    title = "Test report"
  )

  testthat::expect_s3_class(written, "tbl_df")
  testthat::expect_true(file.exists(report_file))
  testthat::expect_true(dir.exists(file.path(output_dir, "report_files")))
  testthat::expect_equal(written$n_flagged, 1)

  html <- paste(readLines(report_file), collapse = "\n")

  testthat::expect_true(grepl("Test report", html))
  testthat::expect_true(grepl("Recordings requiring review", html))
})

testthat::test_that("create_gazepoint_report requires workflow-like results", {
  output_dir <- tempfile()
  dir.create(output_dir)

  testthat::expect_error(
    create_gazepoint_report(
      results = list(sampling = tibble::tibble()),
      output_file = file.path(output_dir, "report.html")
    ),
    "`results` is missing required elements"
  )
})
