make_baseline_pupil_data <- function() {
  tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    time = c(-200, -100, 0, 100, 200, 300),
    pupil_interpolated = c(10, 12, 14, 16, 18, NA)
  )
}

test_that("baseline_correct_gazepoint_pupil adds baseline-correction columns", {
  data <- make_baseline_pupil_data()

  result <- baseline_correct_gazepoint_pupil(data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(data))

  expected_cols <- c(
    "pupil_baseline_value",
    "pupil_baseline_sd",
    "pupil_baseline_n",
    "pupil_baseline_available",
    "pupil_baseline_time_min",
    "pupil_baseline_time_max",
    "pupil_baseline_used",
    "pupil_baseline_corrected",
    "pupil_baseline_percent_change",
    "pupil_baseline_ratio",
    "pupil_baseline_z",
    "pupil_baseline_status",
    "pupil_baseline_pupil_column",
    "pupil_baseline_time_column",
    "pupil_baseline_flag_column",
    "pupil_baseline_window_start",
    "pupil_baseline_window_end",
    "pupil_baseline_method",
    "pupil_baseline_min_samples"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$pupil_baseline_pupil_column[1], "pupil_interpolated")
  expect_equal(result$pupil_baseline_time_column[1], "time")
  expect_true(is.na(result$pupil_baseline_flag_column[1]))
  expect_equal(result$pupil_baseline_window_start[1], -200)
  expect_equal(result$pupil_baseline_window_end[1], 0)
  expect_equal(result$pupil_baseline_method[1], "mean")
})

test_that("baseline_correct_gazepoint_pupil computes mean window baseline", {
  data <- make_baseline_pupil_data()

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(-200, 0)
  )

  expect_equal(result$pupil_baseline_n, rep(3, 6))
  expect_equal(result$pupil_baseline_value, rep(12, 6))
  expect_equal(result$pupil_baseline_sd, rep(2, 6))
  expect_equal(result$pupil_baseline_time_min, rep(-200, 6))
  expect_equal(result$pupil_baseline_time_max, rep(0, 6))

  expect_equal(
    result$pupil_baseline_used,
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_equal(
    result$pupil_baseline_corrected,
    c(-2, 0, 2, 4, 6, NA)
  )

  expect_equal(
    result$pupil_baseline_ratio,
    c(10 / 12, 1, 14 / 12, 16 / 12, 18 / 12, NA)
  )

  expect_equal(
    result$pupil_baseline_z,
    c(-1, 0, 1, 2, 3, NA)
  )

  expect_equal(
    result$pupil_baseline_status,
    c(
      "corrected",
      "corrected",
      "corrected",
      "corrected",
      "corrected",
      "missing_pupil"
    )
  )
})

test_that("baseline_correct_gazepoint_pupil supports early post-onset windows", {
  data <- make_baseline_pupil_data()

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(0, 200)
  )

  expect_equal(result$pupil_baseline_n, rep(3, 6))
  expect_equal(result$pupil_baseline_value, rep(16, 6))
  expect_equal(result$pupil_baseline_time_min, rep(0, 6))
  expect_equal(result$pupil_baseline_time_max, rep(200, 6))

  expect_equal(
    result$pupil_baseline_used,
    c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )

  expect_equal(
    result$pupil_baseline_corrected,
    c(-6, -4, -2, 0, 2, NA)
  )
})

test_that("baseline_correct_gazepoint_pupil supports user-defined baseline flags", {
  data <- make_baseline_pupil_data()
  data$pre_stimulus_period <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_flag_col = "pre_stimulus_period",
    baseline_window = NULL
  )

  expect_equal(result$pupil_baseline_n, rep(2, 6))
  expect_equal(result$pupil_baseline_value, rep(13, 6))
  expect_equal(result$pupil_baseline_flag_column[1], "pre_stimulus_period")
  expect_true(is.na(result$pupil_baseline_window_start[1]))
  expect_true(is.na(result$pupil_baseline_window_end[1]))

  expect_equal(
    result$pupil_baseline_used,
    c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  expect_equal(
    result$pupil_baseline_corrected,
    c(-3, -1, 1, 3, 5, NA)
  )
})

test_that("baseline_correct_gazepoint_pupil supports median baselines", {
  data <- tibble::tibble(
    subject = rep("P1", 4),
    MEDIA_ID = rep("M1", 4),
    time = c(-200, -100, 0, 100),
    pupil_interpolated = c(10, 12, 100, 20)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(-200, 0),
    baseline_method = "median"
  )

  expect_equal(result$pupil_baseline_value, rep(12, 4))
  expect_equal(result$pupil_baseline_method[1], "median")
  expect_equal(result$pupil_baseline_corrected[4], 8)
})

test_that("baseline_correct_gazepoint_pupil respects min_baseline_samples", {
  data <- make_baseline_pupil_data()

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(-200, 0),
    min_baseline_samples = 4
  )

  expect_false(any(result$pupil_baseline_available))
  expect_true(all(is.na(result$pupil_baseline_corrected)))

  expect_equal(
    result$pupil_baseline_status,
    c(
      "no_baseline",
      "no_baseline",
      "no_baseline",
      "no_baseline",
      "no_baseline",
      "missing_pupil"
    )
  )
})

test_that("baseline_correct_gazepoint_pupil keeps baselines within groups", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    MEDIA_ID = rep("M1", 6),
    time = c(0, 10, 20, 0, 10, 20),
    pupil_interpolated = c(10, 12, 14, 100, 102, 104)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(0, 0)
  )

  expect_equal(result$pupil_baseline_value, c(10, 10, 10, 100, 100, 100))
  expect_equal(result$pupil_baseline_corrected, c(0, 2, 4, 0, 2, 4))
})

test_that("baseline_correct_gazepoint_pupil can compute one global baseline", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    MEDIA_ID = rep("M1", 6),
    time = c(0, 10, 20, 0, 10, 20),
    pupil_interpolated = c(10, 12, 14, 100, 102, 104)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(0, 0),
    group_cols = character(0)
  )

  expect_equal(result$pupil_baseline_value, rep(55, 6))
  expect_equal(result$pupil_baseline_corrected, c(-45, -43, -41, 45, 47, 49))
})

test_that("baseline_correct_gazepoint_pupil auto-detects relative baseline time columns", {
  data <- tibble::tibble(
    subject = rep("P1", 4),
    MEDIA_ID = rep("M1", 4),
    time_ms = c(1000, 1100, 1200, 1300),
    time_relative_ms = c(-200, -100, 0, 100),
    pupil_interpolated = c(10, 12, 14, 16)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(-200, 0)
  )

  expect_equal(result$pupil_baseline_value, rep(12, 4))
  expect_equal(result$pupil_baseline_time_column[1], "time_relative_ms")
  expect_equal(result$pupil_baseline_corrected, c(-2, 0, 2, 4))
})

test_that("baseline_correct_gazepoint_pupil supports explicit pupil, time, and baseline-time columns", {
  data <- tibble::tibble(
    participant = rep("P1", 4),
    media_id = rep("M1", 4),
    custom_time = c(1000, 1100, 1200, 1300),
    custom_baseline_time = c(-200, -100, 0, 100),
    custom_pupil = c(10, 12, 14, 16)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    pupil_col = "custom_pupil",
    time_col = "custom_time",
    baseline_time_col = "custom_baseline_time",
    baseline_window = c(-200, 0)
  )

  expect_equal(result$pupil_baseline_value, rep(12, 4))
  expect_equal(result$pupil_baseline_pupil_column[1], "custom_pupil")
  expect_equal(result$pupil_baseline_time_column[1], "custom_baseline_time")
  expect_equal(result$pupil_baseline_corrected, c(-2, 0, 2, 4))
})

test_that("baseline_correct_gazepoint_pupil supports custom grouping columns", {
  data <- tibble::tibble(
    subject = rep("P1", 4),
    MEDIA_ID = rep("M1", 4),
    condition = c("A", "A", "B", "B"),
    time = c(0, 10, 0, 10),
    pupil_interpolated = c(10, 12, 100, 102)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(0, 0),
    group_cols = "condition"
  )

  expect_equal(result$pupil_baseline_value, c(10, 10, 100, 100))
  expect_equal(result$pupil_baseline_corrected, c(0, 2, 0, 2))
})

test_that("baseline_correct_gazepoint_pupil preserves row order and original columns", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1"),
    MEDIA_ID = c("M1", "M1", "M1"),
    time = c(20, 0, 10),
    pupil_interpolated = c(14, 10, 12)
  )

  result <- baseline_correct_gazepoint_pupil(
    data,
    baseline_window = c(0, 0)
  )

  expect_equal(result$subject, data$subject)
  expect_equal(result$MEDIA_ID, data$MEDIA_ID)
  expect_equal(result$time, data$time)
  expect_equal(result$pupil_interpolated, data$pupil_interpolated)
})

test_that("baseline_correct_gazepoint_pupil replaces pre-existing output columns", {
  data <- make_baseline_pupil_data()
  data$pupil_baseline_status <- "old"
  data$pupil_baseline_corrected <- -999

  result <- baseline_correct_gazepoint_pupil(data)

  expect_equal(sum(names(result) == "pupil_baseline_status"), 1)
  expect_equal(sum(names(result) == "pupil_baseline_corrected"), 1)
  expect_false(any(result$pupil_baseline_status == "old"))
  expect_false(any(result$pupil_baseline_corrected == -999, na.rm = TRUE))
})

test_that("baseline_correct_gazepoint_pupil validates arguments", {
  data <- make_baseline_pupil_data()

  expect_error(
    baseline_correct_gazepoint_pupil("not a data frame"),
    "`data` must be a data frame"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, time_col = c("a", "b")),
    "`time_col` must be `NULL` or a single character string"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_time_col = c("a", "b")),
    "`baseline_time_col` must be `NULL` or a single character string"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_flag_col = c("a", "b")),
    "`baseline_flag_col` must be `NULL` or a single character string"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, min_baseline_samples = c(1, 2)),
    "`min_baseline_samples` must be a single numeric value"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, min_baseline_samples = 0),
    "`min_baseline_samples` must be greater than or equal to 1"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_window = c(-200, 0, 100)),
    "`baseline_window` must be a numeric vector of length 2"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_window = c(-200, NA)),
    "`baseline_window` must not contain missing values"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_window = c(0, -200)),
    "`baseline_window\\[2\\]` must be greater than or equal"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_method = "mode"),
    "'arg' should be one of"
  )
})

test_that("baseline_correct_gazepoint_pupil errors when required columns are missing", {
  data <- make_baseline_pupil_data()

  no_pupil <- data
  no_pupil$pupil_interpolated <- NULL

  expect_error(
    baseline_correct_gazepoint_pupil(no_pupil),
    "No pupil column was found"
  )

  no_time <- data
  no_time$time <- NULL

  expect_error(
    baseline_correct_gazepoint_pupil(no_time),
    "No time column was found"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_time_col = "not_here"),
    "No baseline-time column was found"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, baseline_flag_col = "not_here"),
    "`baseline_flag_col` was not found in `data`"
  )

  no_subject <- data
  no_subject$subject <- NULL

  expect_error(
    baseline_correct_gazepoint_pupil(no_subject),
    "requested but not found"
  )

  expect_error(
    baseline_correct_gazepoint_pupil(data, group_cols = "condition"),
    "requested but not found"
  )
})
