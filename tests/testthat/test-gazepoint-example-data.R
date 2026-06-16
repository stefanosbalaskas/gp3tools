test_that("example datasets are available and have expected columns", {
  data("gazepoint_example_master", package = "gp3tools")
  data("gazepoint_example_fixations", package = "gp3tools")
  data("gazepoint_example_aoi_geometry", package = "gp3tools")
  data("gazepoint_example_aoi_windows", package = "gp3tools")
  data("gazepoint_example_pupil_windows", package = "gp3tools")

  expect_s3_class(gazepoint_example_master, "data.frame")
  expect_s3_class(gazepoint_example_fixations, "data.frame")
  expect_s3_class(gazepoint_example_aoi_geometry, "data.frame")
  expect_s3_class(gazepoint_example_aoi_windows, "data.frame")
  expect_s3_class(gazepoint_example_pupil_windows, "data.frame")

  expect_gt(nrow(gazepoint_example_master), 0)
  expect_gt(nrow(gazepoint_example_fixations), 0)
  expect_gt(nrow(gazepoint_example_aoi_geometry), 0)
  expect_gt(nrow(gazepoint_example_aoi_windows), 0)
  expect_gt(nrow(gazepoint_example_pupil_windows), 0)

  expect_true(all(
    c(
      "subject",
      "MEDIA_ID",
      "trial_global",
      "condition",
      "time",
      "x",
      "y",
      "pupil",
      "valid",
      "artifact",
      "aoi_current",
      "event_label"
    ) %in% names(gazepoint_example_master)
  ))

  expect_true(all(
    c("USER_FILE", "MEDIA_ID", "FPOGS", "FPOGD", "FPOGX", "FPOGY", "FPOGV", "AOI") %in%
      names(gazepoint_example_fixations)
  ))

  expect_true(all(
    c("media_id", "aoi", "x_min", "y_min", "x_max", "y_max") %in%
      names(gazepoint_example_aoi_geometry)
  ))
})

test_that("example master works with core quality and pupil helpers", {
  data("gazepoint_example_master", package = "gp3tools")

  pupil_summary <- summarise_gazepoint_pupil(
    gazepoint_example_master,
    pupil_col = "pupil"
  )

  flagged <- flag_gazepoint_pupil(
    gazepoint_example_master,
    pupil_col = "pupil"
  )

  validation <- validate_gazepoint_master(gazepoint_example_master)

  expect_true(is.list(pupil_summary))
  expect_s3_class(flagged, "data.frame")
  expect_true(is.list(validation))
  expect_true("summary" %in% names(validation) || "checks" %in% names(validation))
})

test_that("example AOI windows support GLMM preparation", {
  data("gazepoint_example_aoi_windows", package = "gp3tools")

  aoi_glmm_data <- prepare_gazepoint_aoi_glmm_data(
    gazepoint_example_aoi_windows,
    success_col = "n_target_samples",
    denominator = "valid",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    min_denominator_samples = 1,
    outcome_label = "target"
  )

  expect_s3_class(aoi_glmm_data, "data.frame")
  expect_gt(nrow(aoi_glmm_data), 0)
  expect_true("aoi_glmm_status" %in% names(aoi_glmm_data))
})

test_that("example pupil windows support pupil-window model preparation", {
  data("gazepoint_example_pupil_windows", package = "gp3tools")

  pupil_model_data <- prepare_gazepoint_pupil_window_model_data(
    gazepoint_example_pupil_windows,
    outcome_col = "mean_pupil",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    trial_col = "trial_global",
    valid_samples_col = "n_valid_samples",
    total_samples_col = "n_samples",
    min_valid_samples = 1
  )

  expect_s3_class(pupil_model_data, "data.frame")
  expect_gt(nrow(pupil_model_data), 0)
  expect_true("pupil_model_status" %in% names(pupil_model_data))
})

test_that("example AOI geometry supports geometry audit", {
  data("gazepoint_example_aoi_geometry", package = "gp3tools")

  geometry_audit <- audit_gazepoint_aoi_geometry(
    gazepoint_example_aoi_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1)
  )

  expect_true(is.list(geometry_audit))
  expect_true("overview" %in% names(geometry_audit))
})
