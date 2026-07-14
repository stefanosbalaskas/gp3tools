test_that("velocity detector returns event and sample outputs", {
  gaze <- data.frame(
    USER_ID = "P01",
    TIME = seq(0, 0.19, by = 0.01),
    FPOGX = c(rep(0.25, 10), rep(0.75, 10)),
    FPOGY = rep(0.5, 20)
  )

  result <- detect_gazepoint_fixations_velocity(
    gaze,
    vmax = 5,
    min_duration = 40,
    return = "both"
  )

  expect_s3_class(result, "gp3_velocity_fixation_result")
  expect_s3_class(result$events, "gp3_velocity_fixations")
  expect_true(nrow(result$events) >= 1)
  expect_true(all(c(
    "fixation_id", "start_time", "end_time", "duration_ms",
    "mean_x", "mean_y", "median_velocity"
  ) %in% names(result$events)))
  expect_true(any(result$samples$velocity_fixation))
})


test_that("blink detection and interpolation work together", {
  pupil <- data.frame(
    USER_ID = "P01",
    TIME = seq(0, 0.19, by = 0.01),
    mean_pupil = c(
      rep(3.2, 7),
      NA, NA, NA,
      rep(3.3, 10)
    )
  )

  blinks <- detect_gazepoint_blinks(
    pupil,
    min_duration = 20,
    merge_gap_ms = 0
  )

  expect_s3_class(blinks, "gp3_blink_events")
  expect_equal(nrow(blinks), 1)
  expect_gte(blinks$duration_ms[1], 20)

  interpolated <- interpolate_gazepoint_blinks(
    pupil,
    blinks,
    pupil_cols = "mean_pupil",
    method = "linear"
  )

  expect_true("mean_pupil_blink_interp" %in% names(interpolated))
  expect_true(all(is.finite(
    interpolated$mean_pupil_blink_interp[8:10]
  )))
  expect_true(all(interpolated$blink_interpolated[8:10]))
})


test_that("coordinate smoothing adds bounded rolling outputs", {
  gaze <- data.frame(
    USER_ID = "P01",
    FPOGX = c(0.10, 0.11, 0.80, 0.12, 0.13),
    FPOGY = c(0.20, 0.21, 0.90, 0.22, 0.23)
  )

  smoothed <- smooth_gazepoint_coordinate(
    gaze,
    method = "median",
    window = 3
  )

  expect_true(all(c(
    "FPOGX_smooth", "FPOGY_smooth"
  ) %in% names(smoothed)))
  expect_lt(smoothed$FPOGX_smooth[3], gaze$FPOGX[3])
})


test_that("binocular pupil helpers return stable columns", {
  pupil <- data.frame(
    USER_ID = rep("P01", 20),
    LPupil = seq(3, 4, length.out = 20),
    RPupil = seq(3.1, 4.1, length.out = 20) +
      rep(c(-0.02, 0.02), 10)
  )

  mean_data <- mean_gazepoint_pupil(pupil)
  expect_equal(
    mean_data$mean_pupil[1],
    mean(c(pupil$LPupil[1], pupil$RPupil[1]))
  )

  regressed <- regress_gazepoint_pupils(
    pupil,
    min_complete = 5
  )
  expect_true(all(c(
    "pupil_regressed",
    "pupil_regression_residual",
    "pupil_regression_n"
  ) %in% names(regressed)))
  expect_true(all(is.finite(regressed$pupil_regressed)))
})


test_that("pupil downsampling aggregates within participant", {
  pupil <- data.frame(
    USER_ID = rep(c("P01", "P02"), each = 5),
    TIME = rep(seq(0, 0.04, by = 0.01), 2),
    mean_pupil = 1:10
  )

  downsampled <- downsample_gazepoint_pupil(
    pupil,
    factor = 2,
    pupil_cols = "mean_pupil"
  )

  expect_equal(nrow(downsampled), 6)
  expect_equal(downsampled$n_samples_aggregated, c(2, 2, 1, 2, 2, 1))
  expect_equal(downsampled$mean_pupil[1], 1.5)
})


test_that("window analysis returns requested summaries", {
  pupil <- data.frame(
    USER_ID = "P01",
    TIME = seq(0, 0.99, by = 0.01),
    mean_pupil = seq(3, 4, length.out = 100)
  )

  windows <- analyze_gazepoint_window(
    pupil,
    window_size = 100,
    step = 50,
    summary_stats = c("mean", "sd", "valid_prop"),
    value_cols = "mean_pupil"
  )

  expect_s3_class(windows, "gp3_window_summary")
  expect_true(nrow(windows) > 1)
  expect_true(all(c(
    "mean_pupil_mean",
    "mean_pupil_sd",
    "mean_pupil_valid_prop"
  ) %in% names(windows)))
})


test_that("AOI helper creates logical and label outputs", {
  gaze <- data.frame(
    FPOGX = c(0.2, 0.5, 0.8, NA),
    FPOGY = c(0.2, 0.5, 0.8, 0.2)
  )
  defs <- data.frame(
    name = c("top_left", "bottom_right"),
    L = c(0, 0.6),
    R = c(0.4, 1),
    T = c(0, 0.6),
    B = c(0.4, 1)
  )

  labelled <- add_gazepoint_aoi(
    gaze,
    defs,
    output = "both"
  )

  expect_true(all(c(
    "aoi_top_left",
    "aoi_bottom_right",
    "aoi_current",
    "aoi_overlap_count"
  ) %in% names(labelled)))
  expect_equal(labelled$aoi_current[1], "top_left")
  expect_equal(labelled$aoi_current[3], "bottom_right")
  expect_true(is.na(labelled$aoi_current[4]))
})


test_that("fixation simulation is reproducible and Gazepoint-like", {
  sim1 <- simulate_gazepoint_fixations(
    n_subjects = 2,
    n_fix = 5,
    seed = 42
  )
  sim2 <- simulate_gazepoint_fixations(
    n_subjects = 2,
    n_fix = 5,
    seed = 42
  )

  expect_s3_class(sim1, "gp3_simulated_fixations")
  expect_equal(nrow(sim1), 10)
  expect_equal(sim1, sim2)
  expect_true(all(c(
    "USER_ID", "FPOGID", "FPOGS", "FPOGD",
    "FPOGX", "FPOGY", "duration_ms"
  ) %in% names(sim1)))
})
