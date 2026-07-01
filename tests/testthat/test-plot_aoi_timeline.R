
test_that("plot_gazepoint_aoi_timeline returns a ggplot", {
  skip_if_not_installed("ggplot2")

  dat <- data.frame(
    subject = rep(c("S01", "S02"), each = 4),
    trial = "T01",
    time = rep(1:4, 2),
    AOI = c("A", "A", "B", "C", "A", "B", "B", "C"),
    stringsAsFactors = FALSE
  )

  p <- plot_gazepoint_aoi_timeline(
    dat,
    aoi_col = "AOI",
    time_col = "time",
    subject_col = "subject",
    trial_col = "trial"
  )

  expect_s3_class(p, "ggplot")
})
