make_test_pchip_data <- function() {
  tibble::tibble(
    subject = rep("S1", 9),
    trial = rep(1, 9),
    time = seq(0, 800, by = 100),
    pupil_clean = c(3.1, 3.2, NA, NA, 3.6, 3.8, NA, 4.0, 4.1)
  )
}

test_that("interpolate_gazepoint_pupil_pchip fills short internal gaps", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- make_test_pchip_data()

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 250
  )

  expect_s3_class(out, "gp3_pupil_pchip_interpolation")
  expect_s3_class(out, "tbl_df")

  expect_true("pupil_interpolated_pchip" %in% names(out))
  expect_true("interpolated_pupil_pchip" %in% names(out))
  expect_true("pchip_interpolation_status" %in% names(out))
  expect_true("pchip_gap_id" %in% names(out))
  expect_true("pchip_gap_n_samples" %in% names(out))
  expect_true("pchip_gap_duration_ms" %in% names(out))
  expect_true("pchip_gap_within_limit" %in% names(out))

  expect_equal(sum(out$interpolated_pupil_pchip), 3)
  expect_true(all(out$interpolated_pupil_pchip[c(3, 4, 7)]))
  expect_false(any(out$interpolated_pupil_pchip[c(1, 2, 5, 6, 8, 9)]))

  expect_false(any(is.na(out$pupil_interpolated_pchip[c(3, 4, 7)])))
  expect_equal(out$pupil_interpolated_pchip[!out$interpolated_pupil_pchip], toy_data$pupil_clean[!out$interpolated_pupil_pchip])

  status_table <- table(out$pchip_interpolation_status)

  expect_equal(
    as.integer(status_table),
    c(3L, 6L)
  )

  expect_equal(
    names(status_table),
    c("interpolated_pchip", "observed")
  )

  expect_equal(out$pchip_gap_n_samples[c(3, 4)], c(2L, 2L))
  expect_equal(out$pchip_gap_n_samples[7], 1L)
  expect_equal(out$pchip_gap_duration_ms[c(3, 4)], c(200, 200))
  expect_equal(out$pchip_gap_duration_ms[7], 100)
  expect_true(all(out$pchip_gap_within_limit[c(3, 4, 7)]))
})

test_that("interpolate_gazepoint_pupil_pchip records settings attributes", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- make_test_pchip_data()

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 250,
    output_col = "pupil_pchip",
    flag_col = "was_pchip",
    status_col = "pchip_status"
  )

  settings <- attr(out, "gp3_pchip_settings")

  expect_s3_class(settings, "tbl_df")
  expect_equal(attr(out, "gp3_pchip_input_col"), "pupil_clean")
  expect_equal(attr(out, "gp3_pchip_time_col"), "time")
  expect_equal(attr(out, "gp3_pchip_output_col"), "pupil_pchip")

  expect_true("pupil_pchip" %in% names(out))
  expect_true("was_pchip" %in% names(out))
  expect_true("pchip_status" %in% names(out))

  expect_equal(settings$value[settings$setting == "pupil_col"], "pupil_clean")
  expect_equal(settings$value[settings$setting == "time_col"], "time")
  expect_equal(settings$value[settings$setting == "grouping_cols"], "subject, trial")
  expect_equal(settings$value[settings$setting == "max_gap_ms"], "250")
  expect_equal(settings$value[settings$setting == "output_col"], "pupil_pchip")
  expect_equal(settings$value[settings$setting == "flag_col"], "was_pchip")
  expect_equal(settings$value[settings$setting == "status_col"], "pchip_status")
})

test_that("interpolate_gazepoint_pupil_pchip auto-detects common columns", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- make_test_pchip_data()

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    max_gap_ms = 250
  )

  expect_s3_class(out, "gp3_pupil_pchip_interpolation")
  expect_equal(attr(out, "gp3_pchip_input_col"), "pupil_clean")
  expect_equal(attr(out, "gp3_pchip_time_col"), "time")
  expect_equal(sum(out$interpolated_pupil_pchip), 3)
})

test_that("interpolate_gazepoint_pupil_pchip respects max_gap_ms", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- tibble::tibble(
    subject = rep("S1", 7),
    trial = rep(1, 7),
    time = seq(0, 600, by = 100),
    pupil_clean = c(3.1, 3.2, NA, NA, NA, 3.9, 4.0)
  )

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 250
  )

  expect_equal(sum(out$interpolated_pupil_pchip), 0)
  expect_true(all(is.na(out$pupil_interpolated_pchip[3:5])))
  expect_true(all(out$pchip_interpolation_status[3:5] == "missing_long_gap"))
  expect_false(any(out$pchip_gap_within_limit[3:5]))
})


test_that("interpolate_gazepoint_pupil_pchip respects max_gap_samples", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- make_test_pchip_data()

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 500,
    max_gap_samples = 1
  )

  expect_false(any(out$interpolated_pupil_pchip[c(3, 4)]))
  expect_true(out$interpolated_pupil_pchip[7])

  expect_true(all(out$pchip_interpolation_status[c(3, 4)] == "missing_long_gap"))
  expect_equal(out$pchip_interpolation_status[7], "interpolated_pchip")
})

test_that("interpolate_gazepoint_pupil_pchip preserves leading and trailing gaps", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- tibble::tibble(
    subject = rep("S1", 7),
    trial = rep(1, 7),
    time = seq(0, 600, by = 100),
    pupil_clean = c(NA, 3.2, 3.3, NA, 3.6, 3.8, NA)
  )

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 250
  )

  expect_true(is.na(out$pupil_interpolated_pchip[1]))
  expect_true(is.na(out$pupil_interpolated_pchip[7]))
  expect_false(out$interpolated_pupil_pchip[1])
  expect_false(out$interpolated_pupil_pchip[7])
  expect_equal(out$pchip_interpolation_status[1], "missing_leading_or_trailing_gap")
  expect_equal(out$pchip_interpolation_status[7], "missing_leading_or_trailing_gap")

  expect_true(out$interpolated_pupil_pchip[4])
  expect_equal(out$pchip_interpolation_status[4], "interpolated_pchip")
})

test_that("interpolate_gazepoint_pupil_pchip handles insufficient valid points", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- tibble::tibble(
    subject = rep("S1", 5),
    trial = rep(1, 5),
    time = seq(0, 400, by = 100),
    pupil_clean = c(3.1, NA, NA, NA, 3.5)
  )

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 500,
    min_valid_points = 3
  )

  expect_equal(sum(out$interpolated_pupil_pchip), 0)
  expect_true(all(out$pchip_interpolation_status[2:4] == "missing_insufficient_valid_points"))
})

test_that("interpolate_gazepoint_pupil_pchip supports grouping", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- tibble::tibble(
    subject = rep(c("S1", "S2"), each = 5),
    trial = rep(1, 10),
    time = rep(seq(0, 400, by = 100), 2),
    pupil_clean = c(
      3.1, 3.2, NA, 3.5, 3.6,
      4.1, NA, NA, 4.7, 4.9
    )
  )

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = c("subject", "trial"),
    max_gap_ms = 250
  )

  expect_true(out$interpolated_pupil_pchip[3])
  expect_true(all(out$interpolated_pupil_pchip[7:8]))
  expect_equal(sum(out$interpolated_pupil_pchip), 3)
})

test_that("interpolate_gazepoint_pupil_pchip supports one global sequence", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- tibble::tibble(
    time = seq(0, 500, by = 100),
    pupil_clean = c(3.1, 3.2, NA, 3.5, 3.6, 3.7)
  )

  out <- interpolate_gazepoint_pupil_pchip(
    toy_data,
    pupil_col = "pupil_clean",
    time_col = "time",
    grouping_cols = character(0),
    max_gap_ms = 250
  )

  expect_equal(sum(out$interpolated_pupil_pchip), 1)
  expect_true(out$interpolated_pupil_pchip[3])
})

test_that("interpolate_gazepoint_pupil_pchip checks invalid inputs", {
  testthat::skip_if_not_installed("pracma")

  toy_data <- make_test_pchip_data()

  expect_error(
    interpolate_gazepoint_pupil_pchip(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      pupil_col = "bad_pupil"
    ),
    "`pupil_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      grouping_cols = "bad_group"
    ),
    "All `grouping_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      max_gap_ms = 0
    ),
    "`max_gap_ms` must be a finite positive number",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      max_gap_samples = 0
    ),
    "`max_gap_samples` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      min_valid_points = 2.5
    ),
    "`min_valid_points` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      output_col = ""
    ),
    "`output_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      flag_col = ""
    ),
    "`flag_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    interpolate_gazepoint_pupil_pchip(
      toy_data,
      status_col = ""
    ),
    "`status_col` must be a non-missing character scalar",
    fixed = TRUE
  )
})
