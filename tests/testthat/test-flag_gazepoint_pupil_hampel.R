make_test_hampel_data <- function() {
  tibble::tibble(
    subject = rep("S1", 15),
    trial = rep("T1", 15),
    time = seq(0, 1400, by = 100),
    pupil = c(
      3.0, 3.1, 3.0, 3.05, 3.1,
      8.0,
      3.05, 3.0, 2.95, 3.0,
      3.1, 3.0, 3.05, 3.1, 3.0
    )
  )
}

test_that("flag_gazepoint_pupil_hampel flags a local pupil artifact", {
  toy_data <- make_test_hampel_data()

  out <- flag_gazepoint_pupil_hampel(
    toy_data,
    pupil_col = "pupil",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    window_size_samples = 7,
    k = 3,
    min_valid_samples = 3,
    corrected_col = "pupil_hampel_corrected",
    name = "toy_hampel"
  )

  expect_s3_class(out, "gp3_pupil_hampel_flags")
  expect_s3_class(out, "tbl_df")

  expect_true(all(c(
    "pupil_hampel_median",
    "pupil_hampel_mad",
    "pupil_hampel_threshold",
    "pupil_hampel_outlier",
    "pupil_hampel_status",
    "pupil_hampel_corrected"
  ) %in% names(out)))

  expect_true(out$pupil_hampel_outlier[6])
  expect_equal(out$pupil[6], 8)
  expect_equal(out$pupil_hampel_median[6], 3.05)
  expect_equal(out$pupil_hampel_corrected[6], 3.05)
  expect_false(any(out$pupil_hampel_outlier[-6]))
  expect_true(all(out$pupil_hampel_status == "complete"))

  overview <- attr(out, "gp3_hampel_overview")
  status_summary <- attr(out, "gp3_hampel_status_summary")
  settings <- attr(out, "gp3_hampel_settings")

  expect_s3_class(overview, "tbl_df")
  expect_s3_class(status_summary, "tbl_df")
  expect_s3_class(settings, "tbl_df")

  expect_equal(overview$object_name, "toy_hampel")
  expect_equal(overview$filter, "hampel")
  expect_equal(overview$pupil_col, "pupil")
  expect_equal(overview$time_col, "time")
  expect_equal(overview$grouping_cols, "subject, trial")
  expect_equal(overview$n_input_rows, nrow(toy_data))
  expect_equal(overview$n_groups, 1)
  expect_equal(overview$window_size_samples, 7)
  expect_equal(overview$k, 3)
  expect_equal(overview$min_valid_samples, 3)
  expect_equal(overview$n_flagged, 1)
  expect_equal(overview$flagged_proportion, 1 / nrow(toy_data))
  expect_equal(overview$n_complete, nrow(toy_data))
  expect_equal(overview$n_problem_rows, 0)

  expect_equal(status_summary$status, "complete")
  expect_equal(status_summary$n, nrow(toy_data))

  expect_equal(settings$value[settings$setting == "corrected_col"], "pupil_hampel_corrected")
  expect_equal(settings$value[settings$setting == "name"], "toy_hampel")
})

test_that("flag_gazepoint_pupil_hampel works without time or grouping columns", {
  toy_data <- tibble::tibble(
    pupil = c(3, 3, 3, 9, 3, 3, 3)
  )

  out <- flag_gazepoint_pupil_hampel(
    toy_data,
    pupil_col = "pupil",
    window_size_samples = 5,
    k = 3,
    min_valid_samples = 3,
    name = "ungrouped_hampel"
  )

  expect_s3_class(out, "gp3_pupil_hampel_flags")
  expect_true(out$pupil_hampel_outlier[4])
  expect_equal(attr(out, "gp3_hampel_overview")$time_col, NA_character_)
  expect_equal(attr(out, "gp3_hampel_overview")$grouping_cols, NA_character_)
  expect_equal(attr(out, "gp3_hampel_overview")$n_groups, 1)
})

test_that("flag_gazepoint_pupil_hampel respects grouping boundaries", {
  toy_data <- tibble::tibble(
    subject = rep(c("S1", "S2"), each = 7),
    trial = rep("T1", 14),
    time = rep(seq(0, 600, by = 100), 2),
    pupil = c(
      3, 3, 3, 9, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4
    )
  )

  out <- flag_gazepoint_pupil_hampel(
    toy_data,
    pupil_col = "pupil",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    window_size_samples = 5,
    k = 3,
    min_valid_samples = 3,
    name = "grouped_hampel"
  )

  expect_s3_class(out, "gp3_pupil_hampel_flags")
  expect_true(out$pupil_hampel_outlier[4])
  expect_false(any(out$pupil_hampel_outlier[toy_data$subject == "S2"]))
  expect_equal(attr(out, "gp3_hampel_overview")$n_groups, 2)
  expect_equal(attr(out, "gp3_hampel_overview")$n_flagged, 1)
})

test_that("flag_gazepoint_pupil_hampel records missing and insufficient-window statuses", {
  toy_data <- tibble::tibble(
    subject = rep("S1", 5),
    time = seq(0, 400, by = 100),
    pupil = c(NA, 3, NA, 3.1, NA)
  )

  out <- flag_gazepoint_pupil_hampel(
    toy_data,
    pupil_col = "pupil",
    time_col = "time",
    grouping_cols = "subject",
    window_size_samples = 3,
    k = 3,
    min_valid_samples = 2,
    name = "missing_hampel"
  )

  expect_s3_class(out, "gp3_pupil_hampel_flags")
  expect_equal(out$pupil_hampel_status[1], "missing_or_nonfinite_pupil")
  expect_equal(out$pupil_hampel_status[2], "insufficient_valid_window")
  expect_equal(out$pupil_hampel_status[3], "missing_or_nonfinite_pupil")
  expect_equal(out$pupil_hampel_status[4], "insufficient_valid_window")
  expect_equal(out$pupil_hampel_status[5], "missing_or_nonfinite_pupil")
  expect_false(any(out$pupil_hampel_outlier))

  overview <- attr(out, "gp3_hampel_overview")
  status_summary <- attr(out, "gp3_hampel_status_summary")

  expect_equal(overview$n_flagged, 0)
  expect_equal(overview$n_complete, 0)
  expect_equal(overview$n_problem_rows, 5)
  expect_true("missing_or_nonfinite_pupil" %in% status_summary$status)
  expect_true("insufficient_valid_window" %in% status_summary$status)
})

test_that("flag_gazepoint_pupil_hampel handles zero-MAD windows", {
  toy_data <- tibble::tibble(
    pupil = c(3, 3, 3, 9, 3, 3, 3)
  )

  out <- flag_gazepoint_pupil_hampel(
    toy_data,
    pupil_col = "pupil",
    window_size_samples = 5,
    k = 3,
    min_valid_samples = 3,
    name = "zero_mad_hampel"
  )

  expect_s3_class(out, "gp3_pupil_hampel_flags")
  expect_true("complete_zero_mad" %in% out$pupil_hampel_status)
  expect_true(out$pupil_hampel_outlier[4])
  expect_equal(out$pupil_hampel_threshold[4], 0)
  expect_equal(attr(out, "gp3_hampel_overview")$n_flagged, 1)
})

test_that("flag_gazepoint_pupil_hampel supports custom output columns and overwrite", {
  toy_data <- tibble::tibble(
    pupil = c(3, 3, 3, 9, 3, 3, 3),
    flag = FALSE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      toy_data,
      pupil_col = "pupil",
      window_size_samples = 5,
      flag_col = "flag"
    ),
    "Output column(s) already exist in `data`",
    fixed = TRUE
  )

  out <- flag_gazepoint_pupil_hampel(
    toy_data,
    pupil_col = "pupil",
    window_size_samples = 5,
    flag_col = "flag",
    median_col = "local_median",
    mad_col = "local_mad",
    threshold_col = "local_threshold",
    corrected_col = "pupil_corrected",
    status_col = "hampel_status",
    overwrite = TRUE,
    name = "custom_hampel"
  )

  expect_s3_class(out, "gp3_pupil_hampel_flags")
  expect_true(all(c(
    "flag",
    "local_median",
    "local_mad",
    "local_threshold",
    "pupil_corrected",
    "hampel_status"
  ) %in% names(out)))

  expect_true(out$flag[4])
  expect_equal(out$pupil_corrected[4], out$local_median[4])
  expect_equal(attr(out, "gp3_hampel_settings")$value[attr(out, "gp3_hampel_settings")$setting == "flag_col"], "flag")
})

test_that("flag_gazepoint_pupil_hampel checks invalid inputs", {
  toy_data <- make_test_hampel_data()

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = list(),
      pupil_col = "pupil"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data[0, ],
      pupil_col = "pupil"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "bad_pupil"
    ),
    "`pupil_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      grouping_cols = "bad_group"
    ),
    "`grouping_cols` contains column(s) not present in `data`",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      window_size_samples = 4
    ),
    "`window_size_samples` must be an odd positive integer",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      window_size_samples = 5,
      min_valid_samples = 6
    ),
    "`min_valid_samples` must be less than or equal to `window_size_samples`",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      k = 0
    ),
    "`k` must be a positive finite number",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      scale_mad = 0
    ),
    "`scale_mad` must be a positive finite number",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      flag_col = ""
    ),
    "Each output column must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      flag_col = "duplicate",
      median_col = "duplicate"
    ),
    "Output column names must be unique",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      overwrite = NA
    ),
    "`overwrite` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = toy_data,
      pupil_col = "pupil",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )

  bad_time <- toy_data
  bad_time$time <- "not_numeric"

  expect_error(
    flag_gazepoint_pupil_hampel(
      data = bad_time,
      pupil_col = "pupil",
      time_col = "time"
    ),
    "`time_col` must be numeric or coercible to finite numeric values",
    fixed = TRUE
  )
})
