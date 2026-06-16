testthat::test_that("save_gazepoint_plots saves standard diagnostic plots", {
  output_dir <- tempfile()
  dir.create(output_dir)

  flagged_quality <- tibble::tibble(
    USER_FILE = c("User 0", "User 1"),
    MEDIA_ID = c(0, 0),
    FPOGV_valid_pct = c(95, 55),
    RPV_valid_pct = c(95, 50),
    review_required = c(FALSE, TRUE)
  )

  sampling <- tibble::tibble(
    USER_FILE = c("User 0", "User 1"),
    MEDIA_ID = c(0, 0),
    estimated_hz = c(60, 48)
  )

  written <- save_gazepoint_plots(
    flagged_quality = flagged_quality,
    sampling = sampling,
    output_dir = output_dir,
    prefix = "test"
  )

  testthat::expect_s3_class(written, "tbl_df")
  testthat::expect_equal(nrow(written), 2)

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_tracking_quality_plot.png"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_sampling_rate_plot.png"))
  )
})

testthat::test_that("save_gazepoint_plots requires at least one plot input", {
  output_dir <- tempfile()
  dir.create(output_dir)

  testthat::expect_error(
    save_gazepoint_plots(output_dir = output_dir),
    "At least one plot input must be provided"
  )
})
