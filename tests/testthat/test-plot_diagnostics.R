testthat::test_that("plot_tracking_quality returns a ggplot object", {
  flagged_quality <- tibble::tibble(
    USER_FILE = c("User 0", "User 1"),
    MEDIA_ID = c(0, 0),
    FPOGV_valid_pct = c(95, 55),
    LPV_valid_pct = c(95, 90),
    RPV_valid_pct = c(95, 50),
    review_required = c(FALSE, TRUE)
  )

  p <- plot_tracking_quality(flagged_quality)

  testthat::expect_true(inherits(p, "ggplot"))
})

testthat::test_that("plot_sampling_rate returns a ggplot object", {
  sampling <- tibble::tibble(
    USER_FILE = c("User 0", "User 1"),
    MEDIA_ID = c(0, 0),
    estimated_hz = c(60, 48)
  )

  p <- plot_sampling_rate(sampling)

  testthat::expect_true(inherits(p, "ggplot"))
})

testthat::test_that("plot_tracking_quality requires validity columns", {
  data <- tibble::tibble(
    USER_FILE = "User 0",
    MEDIA_ID = 0
  )

  testthat::expect_error(
    plot_tracking_quality(data),
    "No validity-percentage columns were found"
  )
})

testthat::test_that("plot_sampling_rate requires estimated_hz", {
  sampling <- tibble::tibble(
    USER_FILE = "User 0",
    MEDIA_ID = 0
  )

  testthat::expect_error(
    plot_sampling_rate(sampling),
    "Missing required columns"
  )
})
