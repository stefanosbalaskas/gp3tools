test_that("plot_gazepoint_time_series returns a ggplot object", {
  x <- simulate_gazepoint_pupil_data(
    n_subjects = 2,
    n_trials = 2,
    n_time_bins = 5,
    seed = 1
  )

  p <- plot_gazepoint_time_series(
    x,
    time_col = "time_bin",
    value_col = "pupil",
    group_cols = c("subject", "trial"),
    colour_col = "condition"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_time_series validates columns and plotting parameters", {
  x <- data.frame(time = 1:3, value = c(1, 2, 3))

  expect_error(
    plot_gazepoint_time_series(x, "missing", "value"),
    "missing required column"
  )

  expect_error(
    plot_gazepoint_time_series(x, "time", "value", alpha = 2),
    "alpha"
  )

  expect_error(
    plot_gazepoint_time_series(x, "time", "value", linewidth = 0),
    "linewidth"
  )
})


test_that("plot_gazepoint_scanpaths returns a ggplot object", {
  x <- simulate_gazepoint_pupil_data(
    n_subjects = 2,
    n_trials = 2,
    n_time_bins = 5,
    seed = 1
  )

  p <- plot_gazepoint_scanpaths(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    order_col = "time_bin",
    group_cols = c("subject", "trial"),
    colour_col = "condition",
    screen_width = 1920,
    screen_height = 1080
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_scanpaths supports faceting and no points", {
  x <- simulate_gazepoint_pupil_data(
    n_subjects = 2,
    n_trials = 2,
    n_time_bins = 5,
    seed = 1
  )

  p <- plot_gazepoint_scanpaths(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    order_col = "time_bin",
    group_cols = c("subject", "trial"),
    facet_col = "condition",
    show_points = FALSE
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_scanpaths validates inputs", {
  x <- data.frame(x = 1:3, y = 1:3, time = 1:3)

  expect_error(
    plot_gazepoint_scanpaths(x, "missing", "y"),
    "missing required column"
  )

  expect_error(
    plot_gazepoint_scanpaths(x, "x", "y", alpha = -0.1),
    "alpha"
  )

  expect_error(
    plot_gazepoint_scanpaths(x, "x", "y", screen_width = 0),
    "screen_width"
  )
})
