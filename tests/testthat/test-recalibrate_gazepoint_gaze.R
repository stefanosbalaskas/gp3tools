make_test_recalibration_data <- function() {
  tidyr::expand_grid(
    subject = c("S1", "S2"),
    trial = "T1",
    sample = 1:8
  ) |>
    dplyr::mutate(
      time = .data$sample * 100,
      target_x = dplyr::if_else(.data$subject == "S1", 500, 700),
      target_y = dplyr::if_else(.data$subject == "S1", 400, 300),
      gaze_x = .data$target_x + dplyr::if_else(.data$subject == "S1", 20, -15),
      gaze_y = .data$target_y + dplyr::if_else(.data$subject == "S1", -10, 12),
      is_check_target = .data$sample <= 4
    )
}

test_that("recalibrate_gazepoint_gaze applies grouped median drift correction", {
  toy_data <- make_test_recalibration_data()

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    calibration_col = "is_check_target",
    calibration_value = TRUE,
    method = "median_shift",
    min_valid_points = 3,
    name = "toy_gaze_recalibration"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_s3_class(out, "tbl_df")

  expect_true(all(c(
    "gaze_x_recalibrated",
    "gaze_y_recalibrated",
    "gaze_recalibration_dx",
    "gaze_recalibration_dy",
    "gaze_recalibration_shift",
    "gaze_error_before_recalibration",
    "gaze_error_after_recalibration",
    "gaze_recalibration_status"
  ) %in% names(out)))

  expect_true(all(out$gaze_recalibration_status == "complete"))

  expect_equal(
    unique(out$gaze_recalibration_dx[out$subject == "S1"]),
    -20
  )
  expect_equal(
    unique(out$gaze_recalibration_dy[out$subject == "S1"]),
    10
  )
  expect_equal(
    unique(out$gaze_recalibration_dx[out$subject == "S2"]),
    15
  )
  expect_equal(
    unique(out$gaze_recalibration_dy[out$subject == "S2"]),
    -12
  )

  expect_equal(out$gaze_x_recalibrated, out$target_x)
  expect_equal(out$gaze_y_recalibrated, out$target_y)
  expect_true(all(out$gaze_error_after_recalibration < out$gaze_error_before_recalibration))
  expect_equal(out$gaze_error_after_recalibration, rep(0, nrow(out)))

  overview <- attr(out, "gp3_gaze_recalibration_overview")
  group_summary <- attr(out, "gp3_gaze_recalibration_group_summary")
  status_summary <- attr(out, "gp3_gaze_recalibration_status_summary")
  settings <- attr(out, "gp3_gaze_recalibration_settings")

  expect_s3_class(overview, "tbl_df")
  expect_s3_class(group_summary, "tbl_df")
  expect_s3_class(status_summary, "tbl_df")
  expect_s3_class(settings, "tbl_df")

  expect_equal(overview$object_name, "toy_gaze_recalibration")
  expect_equal(overview$recalibration_method, "median_shift")
  expect_equal(overview$x_col, "gaze_x")
  expect_equal(overview$y_col, "gaze_y")
  expect_equal(overview$target_x_col, "target_x")
  expect_equal(overview$target_y_col, "target_y")
  expect_equal(overview$time_col, "time")
  expect_equal(overview$grouping_cols, "subject, trial")
  expect_equal(overview$calibration_col, "is_check_target")
  expect_equal(overview$calibration_value, "TRUE")
  expect_equal(overview$n_input_rows, nrow(toy_data))
  expect_equal(overview$n_groups, 2)
  expect_equal(overview$n_complete_groups, 2)
  expect_equal(overview$n_problem_groups, 0)
  expect_equal(overview$n_recalibrated_rows, nrow(toy_data))
  expect_equal(overview$n_problem_rows, 0)
  expect_equal(overview$min_valid_points, 3)

  expect_equal(group_summary$group_status, c("complete", "complete"))
  expect_equal(group_summary$n_calibration_rows, c(4, 4))
  expect_equal(group_summary$n_valid_calibration_rows, c(4, 4))
  expect_true(all(group_summary$shift_applied))

  expect_equal(status_summary$status, "complete")
  expect_equal(status_summary$n, nrow(toy_data))

  expect_equal(settings$value[settings$setting == "method"], "median_shift")
  expect_equal(settings$value[settings$setting == "name"], "toy_gaze_recalibration")
})

test_that("recalibrate_gazepoint_gaze supports mean-shift correction", {
  toy_data <- tibble::tibble(
    subject = "S1",
    sample = 1:5,
    time = sample * 100,
    target_x = 100,
    target_y = 200,
    gaze_x = c(110, 112, 108, 110, 110),
    gaze_y = c(195, 194, 196, 195, 195),
    check = TRUE
  )

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    time_col = "time",
    calibration_col = "check",
    method = "mean_shift",
    min_valid_points = 3,
    name = "mean_shift_recalibration"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_equal(unique(out$gaze_recalibration_dx), mean(toy_data$target_x - toy_data$gaze_x))
  expect_equal(unique(out$gaze_recalibration_dy), mean(toy_data$target_y - toy_data$gaze_y))
  expect_true(all(out$gaze_recalibration_status == "complete"))

  overview <- attr(out, "gp3_gaze_recalibration_overview")
  expect_equal(overview$recalibration_method, "mean_shift")
  expect_equal(overview$n_complete_groups, 1)
})

test_that("recalibrate_gazepoint_gaze supports calibration values", {
  toy_data <- tibble::tibble(
    subject = "S1",
    sample = 1:6,
    target_x = 500,
    target_y = 400,
    gaze_x = 520,
    gaze_y = 390,
    row_type = c("check", "check", "check", "task", "task", "task")
  )

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    grouping_cols = "subject",
    calibration_col = "row_type",
    calibration_value = "check",
    min_valid_points = 3,
    name = "character_calibration_value"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_true(all(out$gaze_recalibration_status == "complete"))
  expect_equal(unique(out$gaze_recalibration_dx), -20)
  expect_equal(unique(out$gaze_recalibration_dy), 10)

  group_summary <- attr(out, "gp3_gaze_recalibration_group_summary")
  expect_equal(group_summary$n_calibration_rows, 3)
  expect_equal(group_summary$n_valid_calibration_rows, 3)

  settings <- attr(out, "gp3_gaze_recalibration_settings")
  expect_equal(settings$value[settings$setting == "calibration_value"], "check")
})

test_that("recalibrate_gazepoint_gaze records insufficient calibration targets", {
  toy_data <- tibble::tibble(
    subject = "S1",
    sample = 1:5,
    target_x = 500,
    target_y = 400,
    gaze_x = 520,
    gaze_y = 390,
    check = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    grouping_cols = "subject",
    calibration_col = "check",
    min_valid_points = 3,
    name = "insufficient_targets"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_true(all(out$gaze_recalibration_status == "insufficient_valid_targets"))
  expect_true(all(is.na(out$gaze_recalibration_dx)))
  expect_true(all(is.na(out$gaze_recalibration_dy)))
  expect_equal(out$gaze_x_recalibrated, out$gaze_x)
  expect_equal(out$gaze_y_recalibrated, out$gaze_y)

  overview <- attr(out, "gp3_gaze_recalibration_overview")
  group_summary <- attr(out, "gp3_gaze_recalibration_group_summary")
  status_summary <- attr(out, "gp3_gaze_recalibration_status_summary")

  expect_equal(overview$n_complete_groups, 0)
  expect_equal(overview$n_problem_groups, 1)
  expect_equal(overview$n_recalibrated_rows, 0)
  expect_equal(overview$n_problem_rows, nrow(toy_data))
  expect_equal(group_summary$group_status, "insufficient_valid_targets")
  expect_equal(status_summary$status, "insufficient_valid_targets")
})

test_that("recalibrate_gazepoint_gaze can block excessive shifts", {
  toy_data <- tibble::tibble(
    subject = "S1",
    sample = 1:5,
    target_x = 500,
    target_y = 400,
    gaze_x = 600,
    gaze_y = 500,
    check = TRUE
  )

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    grouping_cols = "subject",
    calibration_col = "check",
    min_valid_points = 3,
    max_shift = 50,
    name = "blocked_large_shift"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_true(all(out$gaze_recalibration_status == "shift_exceeds_max"))
  expect_equal(unique(out$gaze_recalibration_dx), -100)
  expect_equal(unique(out$gaze_recalibration_dy), -100)
  expect_equal(out$gaze_x_recalibrated, out$gaze_x)
  expect_equal(out$gaze_y_recalibrated, out$gaze_y)
  expect_equal(out$gaze_error_after_recalibration, out$gaze_error_before_recalibration)

  overview <- attr(out, "gp3_gaze_recalibration_overview")
  group_summary <- attr(out, "gp3_gaze_recalibration_group_summary")

  expect_equal(overview$n_complete_groups, 0)
  expect_equal(overview$n_problem_groups, 1)
  expect_equal(overview$max_shift, 50)
  expect_false(group_summary$shift_applied)
})

test_that("recalibrate_gazepoint_gaze records missing gaze rows", {
  toy_data <- tibble::tibble(
    subject = "S1",
    sample = 1:5,
    target_x = 500,
    target_y = 400,
    gaze_x = c(520, 520, 520, NA, 520),
    gaze_y = c(390, 390, 390, 390, NA),
    check = TRUE
  )

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    grouping_cols = "subject",
    calibration_col = "check",
    min_valid_points = 3,
    name = "missing_gaze"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_equal(out$gaze_recalibration_status[1:3], rep("complete", 3))
  expect_equal(out$gaze_recalibration_status[4:5], rep("missing_or_nonfinite_gaze", 2))
  expect_true(is.na(out$gaze_x_recalibrated[4]))
  expect_true(is.na(out$gaze_y_recalibrated[5]))

  overview <- attr(out, "gp3_gaze_recalibration_overview")
  status_summary <- attr(out, "gp3_gaze_recalibration_status_summary")

  expect_equal(overview$n_recalibrated_rows, 3)
  expect_equal(overview$n_problem_rows, 2)
  expect_true("complete" %in% status_summary$status)
  expect_true("missing_or_nonfinite_gaze" %in% status_summary$status)
})

test_that("recalibrate_gazepoint_gaze supports custom output columns and overwrite", {
  toy_data <- tibble::tibble(
    subject = rep("S1", 3),
    target_x = rep(500, 3),
    target_y = rep(400, 3),
    gaze_x = rep(520, 3),
    gaze_y = rep(390, 3),
    check = rep(TRUE, 3),
    gx_new = rep(999, 3)
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      output_x_col = "gx_new"
    ),
    "Output column(s) already exist in `data`",
    fixed = TRUE
  )

  out <- recalibrate_gazepoint_gaze(
    toy_data,
    x_col = "gaze_x",
    y_col = "gaze_y",
    target_x_col = "target_x",
    target_y_col = "target_y",
    calibration_col = "check",
    output_x_col = "gx_new",
    output_y_col = "gy_new",
    dx_col = "dx_new",
    dy_col = "dy_new",
    shift_col = "shift_new",
    error_before_col = "error_before_new",
    error_after_col = "error_after_new",
    status_col = "status_new",
    overwrite = TRUE,
    min_valid_points = 3,
    name = "custom_recalibration"
  )

  expect_s3_class(out, "gp3_gaze_recalibrated_data")
  expect_true(all(c(
    "gx_new",
    "gy_new",
    "dx_new",
    "dy_new",
    "shift_new",
    "error_before_new",
    "error_after_new",
    "status_new"
  ) %in% names(out)))

  expect_false(any(out$gx_new == 999))
  expect_true(all(out$status_new == "complete"))

  settings <- attr(out, "gp3_gaze_recalibration_settings")
  expect_equal(settings$value[settings$setting == "output_x_col"], "gx_new")
  expect_equal(settings$value[settings$setting == "status_col"], "status_new")
})

test_that("recalibrate_gazepoint_gaze checks invalid inputs", {
  toy_data <- make_test_recalibration_data()

  expect_error(
    recalibrate_gazepoint_gaze(
      data = list(),
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data[0, ],
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "bad_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y"
    ),
    "`x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "bad_y",
      target_x_col = "target_x",
      target_y_col = "target_y"
    ),
    "`y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "bad_target_x",
      target_y_col = "target_y"
    ),
    "`target_x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "bad_target_y"
    ),
    "`target_y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      grouping_cols = "bad_group"
    ),
    "`grouping_cols` contains column(s) not present in `data`",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      calibration_col = "bad_check"
    ),
    "`calibration_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      calibration_col = "is_check_target",
      calibration_value = NA
    ),
    "`calibration_value` must be NULL or a non-missing scalar",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      min_valid_points = 0
    ),
    "`min_valid_points` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      max_shift = 0
    ),
    "`max_shift` must be NULL or a positive finite number",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      output_x_col = ""
    ),
    "Each output column must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      output_x_col = "duplicate",
      output_y_col = "duplicate"
    ),
    "Output column names must be unique",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      overwrite = NA
    ),
    "`overwrite` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    recalibrate_gazepoint_gaze(
      data = toy_data,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )

  bad_time <- toy_data
  bad_time$time <- "not_numeric"

  expect_error(
    recalibrate_gazepoint_gaze(
      data = bad_time,
      x_col = "gaze_x",
      y_col = "gaze_y",
      target_x_col = "target_x",
      target_y_col = "target_y",
      time_col = "time"
    ),
    "`time_col` must be numeric or coercible to finite numeric values",
    fixed = TRUE
  )
})
