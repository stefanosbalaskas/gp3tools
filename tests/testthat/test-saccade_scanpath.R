test_that("compute_gazepoint_saccade_metrics computes Euclidean distances", {
  d <- data.frame(
    trial = c(1, 1, 1),
    time = c(0, 100, 200),
    x = c(0, 3, 3),
    y = c(0, 4, 8)
  )
  out <- compute_gazepoint_saccade_metrics(
    d, x_col = "x", y_col = "y", group_cols = "trial", time_col = "time"
  )
  expect_equal(nrow(out), 2)
  expect_equal(out$saccade_amplitude, c(5, 4))
  expect_equal(out$time_delta, c(100, 100))
  expect_equal(out$saccade_speed, c(0.05, 0.04))
})

test_that("compute_gazepoint_saccade_metrics handles start and end times", {
  d <- data.frame(
    start = c(0, 150, 300),
    end = c(100, 250, 400),
    x = c(0, 0, 6),
    y = c(0, 8, 8)
  )
  out <- compute_gazepoint_saccade_metrics(
    d, x_col = "x", y_col = "y", start_time_col = "start", end_time_col = "end"
  )
  expect_equal(out$saccade_amplitude, c(8, 6))
  expect_equal(out$time_delta, c(50, 50))
})

test_that("compute_gazepoint_saccade_metrics returns no rows for single-fixation groups", {
  d <- data.frame(
    trial = c(1, 2, 2),
    time = c(0, 0, 100),
    x = c(0, 0, 1),
    y = c(0, 0, 1)
  )
  out <- compute_gazepoint_saccade_metrics(
    d, x_col = "x", y_col = "y", group_cols = "trial", time_col = "time"
  )
  expect_equal(nrow(out), 1)
  expect_equal(unique(out$trial), 2)
})

test_that("plot_gazepoint_scanpath returns a ggplot", {
  d <- data.frame(
    trial = c(1, 1, 1),
    time = c(0, 100, 200),
    x = c(0, 3, 3),
    y = c(0, 4, 8)
  )
  p <- plot_gazepoint_scanpath(
    d, x_col = "x", y_col = "y", group_cols = "trial", time_col = "time"
  )
  expect_s3_class(p, "ggplot")
})
