test_that("combine_gazepoint_eyes averages available left and right values", {
  x <- data.frame(
    left = c(3, NA, 5, 100),
    right = c(5, 4, NA, 6)
  )

  out <- combine_gazepoint_eyes(
    x,
    left_col = "left",
    right_col = "right",
    output_col = "pupil",
    method = "mean",
    valid_max = 10
  )

  expect_equal(out$pupil, c(4, 4, 5, 6))
})


test_that("combine_gazepoint_eyes supports preference and best-eye rules", {
  x <- data.frame(
    left = c(NA, NA, 3),
    right = c(4, 5, 6)
  )

  prefer_left <- combine_gazepoint_eyes(
    x,
    left_col = "left",
    right_col = "right",
    output_col = "pupil",
    method = "prefer_left"
  )

  best <- combine_gazepoint_eyes(
    x,
    left_col = "left",
    right_col = "right",
    output_col = "pupil",
    method = "best"
  )

  expect_equal(prefer_left$pupil, c(4, 5, 3))
  expect_equal(best$pupil, c(4, 5, 6))
})


test_that("clean_gazepoint_by_trackloss flags high-trackloss groups", {
  x <- data.frame(
    participant = rep(c("P1", "P2"), each = 4),
    trial = 1,
    valid = c(1, 1, 1, 0, 1, 0, 0, 0)
  )

  out <- clean_gazepoint_by_trackloss(
    x,
    group_cols = c("participant", "trial"),
    tracking_col = "valid",
    max_trackloss = 0.50,
    action = "flag"
  )

  expect_true(".gp3_trackloss_rate" %in% names(out))
  expect_true(".gp3_trackloss_exclude" %in% names(out))
  expect_false(any(out$.gp3_trackloss_exclude[out$participant == "P1"]))
  expect_true(all(out$.gp3_trackloss_exclude[out$participant == "P2"]))

  summary <- attr(out, "gp3_trackloss_summary")
  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 2)
})


test_that("clean_gazepoint_by_trackloss can filter high-trackloss groups", {
  x <- data.frame(
    participant = rep(c("P1", "P2"), each = 4),
    trial = 1,
    valid = c(1, 1, 1, 0, 1, 0, 0, 0)
  )

  out <- clean_gazepoint_by_trackloss(
    x,
    group_cols = c("participant", "trial"),
    tracking_col = "valid",
    max_trackloss = 0.50,
    action = "filter"
  )

  expect_equal(unique(out$participant), "P1")
  expect_equal(nrow(out), 4)
})


test_that("clean_gazepoint_by_trackloss can infer loss from gaze coordinates", {
  x <- data.frame(
    trial = rep(1:2, each = 3),
    x = c(100, 110, 120, 0, 0, 150),
    y = c(200, 210, 220, 0, 0, 250)
  )

  out <- clean_gazepoint_by_trackloss(
    x,
    group_cols = "trial",
    x_col = "x",
    y_col = "y",
    max_trackloss = 0.50
  )

  expect_false(any(out$.gp3_trackloss_exclude[out$trial == 1]))
  expect_true(all(out$.gp3_trackloss_exclude[out$trial == 2]))
})


test_that("simulate_gazepoint_pupil_data returns reproducible balanced synthetic data", {
  x1 <- simulate_gazepoint_pupil_data(
    n_subjects = 3,
    n_trials = 4,
    n_time_bins = 5,
    conditions = c("control", "treatment"),
    seed = 123
  )

  x2 <- simulate_gazepoint_pupil_data(
    n_subjects = 3,
    n_trials = 4,
    n_time_bins = 5,
    conditions = c("control", "treatment"),
    seed = 123
  )

  expect_equal(x1, x2)
  expect_equal(nrow(x1), 3 * 4 * 5)
  expect_true(all(c(
    "subject",
    "trial",
    "condition",
    "time_bin",
    "timestamp_ms",
    "gaze_x",
    "gaze_y",
    "pupil_left",
    "pupil_right",
    "pupil",
    "blink",
    "trackloss"
  ) %in% names(x1)))
  expect_equal(sort(unique(x1$condition)), c("control", "treatment"))
})


test_that("simulate_gazepoint_pupil_data validates basic arguments", {
  expect_error(
    simulate_gazepoint_pupil_data(n_subjects = 0),
    "positive integer"
  )

  expect_error(
    simulate_gazepoint_pupil_data(conditions = character()),
    "conditions"
  )
})
