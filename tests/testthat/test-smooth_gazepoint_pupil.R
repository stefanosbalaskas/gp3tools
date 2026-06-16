make_smooth_pupil_data <- function() {
  tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    pupil_interpolated = c(1, 2, 3, 4, 5)
  )
}

test_that("smooth_gazepoint_pupil adds smoothing columns", {
  data <- make_smooth_pupil_data()

  result <- smooth_gazepoint_pupil(data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(data))

  expected_cols <- c(
    "pupil_smoothed",
    "pupil_smoothing_status",
    "pupil_smoothing_window_n",
    "pupil_smoothing_input_column",
    "pupil_smoothing_time_column",
    "pupil_smoothing_method",
    "pupil_smoothing_align",
    "pupil_smoothing_window_samples",
    "pupil_smoothing_min_points",
    "pupil_smoothing_preserve_missing"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$pupil_smoothing_input_column[1], "pupil_interpolated")
  expect_equal(result$pupil_smoothing_time_column[1], "time")
  expect_equal(result$pupil_smoothing_method[1], "mean")
  expect_equal(result$pupil_smoothing_align[1], "center")
  expect_equal(result$pupil_smoothing_window_samples[1], 5)
})

test_that("smooth_gazepoint_pupil computes centered rolling means", {
  data <- make_smooth_pupil_data()

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "mean",
    align = "center"
  )

  expect_equal(
    result$pupil_smoothed,
    c(
      mean(c(1, 2)),
      mean(c(1, 2, 3)),
      mean(c(2, 3, 4)),
      mean(c(3, 4, 5)),
      mean(c(4, 5))
    )
  )

  expect_true(all(result$pupil_smoothing_status == "smoothed"))
  expect_equal(result$pupil_smoothing_window_n, c(2L, 3L, 3L, 3L, 2L))
})

test_that("smooth_gazepoint_pupil computes rolling medians", {
  data <- make_smooth_pupil_data()

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "median",
    align = "center"
  )

  expect_equal(
    result$pupil_smoothed,
    c(
      stats::median(c(1, 2)),
      stats::median(c(1, 2, 3)),
      stats::median(c(2, 3, 4)),
      stats::median(c(3, 4, 5)),
      stats::median(c(4, 5))
    )
  )

  expect_equal(result$pupil_smoothing_method[1], "median")
})

test_that("smooth_gazepoint_pupil supports right-aligned windows", {
  data <- make_smooth_pupil_data()

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "mean",
    align = "right"
  )

  expect_equal(
    result$pupil_smoothed,
    c(
      mean(c(1)),
      mean(c(1, 2)),
      mean(c(1, 2, 3)),
      mean(c(2, 3, 4)),
      mean(c(3, 4, 5))
    )
  )

  expect_equal(result$pupil_smoothing_align[1], "right")
})

test_that("smooth_gazepoint_pupil supports left-aligned windows", {
  data <- make_smooth_pupil_data()

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "mean",
    align = "left"
  )

  expect_equal(
    result$pupil_smoothed,
    c(
      mean(c(1, 2, 3)),
      mean(c(2, 3, 4)),
      mean(c(3, 4, 5)),
      mean(c(4, 5)),
      mean(c(5))
    )
  )

  expect_equal(result$pupil_smoothing_align[1], "left")
})

test_that("smooth_gazepoint_pupil preserves missing input by default", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    pupil_interpolated = c(1, 2, NA, 4, 5)
  )

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "mean",
    align = "center"
  )

  expect_true(is.na(result$pupil_smoothed[3]))
  expect_equal(result$pupil_smoothing_status[3], "missing_input")
  expect_false(is.na(result$pupil_smoothed[2]))
  expect_false(is.na(result$pupil_smoothed[4]))
})

test_that("smooth_gazepoint_pupil can smooth missing input when preserve_missing is FALSE", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, 10, 20),
    pupil_interpolated = c(1, NA, 3)
  )

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "mean",
    align = "center",
    preserve_missing = FALSE
  )

  expect_equal(result$pupil_smoothed[2], 2)
  expect_equal(result$pupil_smoothing_status[2], "smoothed")
  expect_false(is.na(result$pupil_smoothed[2]))
})

test_that("smooth_gazepoint_pupil respects min_points", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    pupil_interpolated = c(1, NA, 3, NA, 5)
  )

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    method = "mean",
    align = "center",
    min_points = 2
  )

  expect_equal(result$pupil_smoothing_status[1], "insufficient_window")
  expect_equal(result$pupil_smoothing_status[3], "insufficient_window")
  expect_equal(result$pupil_smoothing_status[5], "insufficient_window")
  expect_true(all(is.na(result$pupil_smoothed[c(1, 3, 5)])))
})

test_that("smooth_gazepoint_pupil keeps smoothing within groups", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    MEDIA_ID = rep("M1", 6),
    time = c(0, 10, 20, 0, 10, 20),
    pupil_interpolated = c(1, 2, 3, 100, 200, 300)
  )

  result <- smooth_gazepoint_pupil(
    data,
    window_samples = 3,
    align = "center"
  )

  expect_equal(result$pupil_smoothed[2], mean(c(1, 2, 3)))
  expect_equal(result$pupil_smoothed[5], mean(c(100, 200, 300)))
})

test_that("smooth_gazepoint_pupil can smooth globally", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P2"),
    MEDIA_ID = c("M1", "M1", "M1"),
    time = c(0, 10, 20),
    pupil_interpolated = c(1, 2, 100)
  )

  result <- smooth_gazepoint_pupil(
    data,
    group_cols = character(0),
    window_samples = 3,
    align = "center"
  )

  expect_equal(result$pupil_smoothed[2], mean(c(1, 2, 100)))
})

test_that("smooth_gazepoint_pupil supports custom grouping columns", {
  data <- tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    condition = c("A", "A", "A", "B", "B", "B"),
    time = c(0, 10, 20, 0, 10, 20),
    pupil_interpolated = c(1, 2, 3, 100, 200, 300)
  )

  result <- smooth_gazepoint_pupil(
    data,
    group_cols = "condition",
    window_samples = 3
  )

  expect_equal(result$pupil_smoothed[2], mean(c(1, 2, 3)))
  expect_equal(result$pupil_smoothed[5], mean(c(100, 200, 300)))
})

test_that("smooth_gazepoint_pupil supports baseline-corrected columns by default", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, 10, 20),
    pupil_baseline_corrected = c(-1, 0, 1),
    pupil_interpolated = c(10, 11, 12)
  )

  result <- smooth_gazepoint_pupil(data)

  expect_equal(result$pupil_smoothing_input_column[1], "pupil_baseline_corrected")
  expect_equal(result$pupil_smoothed[2], mean(c(-1, 0, 1)))
})

test_that("smooth_gazepoint_pupil supports explicit pupil and time columns", {
  data <- tibble::tibble(
    participant = rep("P1", 3),
    media_id = rep("M1", 3),
    custom_time = c(0, 10, 20),
    custom_pupil = c(1, 2, 3)
  )

  result <- smooth_gazepoint_pupil(
    data,
    pupil_col = "custom_pupil",
    time_col = "custom_time",
    window_samples = 3
  )

  expect_equal(result$pupil_smoothed[2], mean(c(1, 2, 3)))
  expect_equal(result$pupil_smoothing_input_column[1], "custom_pupil")
  expect_equal(result$pupil_smoothing_time_column[1], "custom_time")
})

test_that("smooth_gazepoint_pupil preserves row order and original columns", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1"),
    MEDIA_ID = c("M1", "M1", "M1"),
    time = c(20, 0, 10),
    pupil_interpolated = c(3, 1, 2)
  )

  result <- smooth_gazepoint_pupil(data)

  expect_equal(result$subject, data$subject)
  expect_equal(result$MEDIA_ID, data$MEDIA_ID)
  expect_equal(result$time, data$time)
  expect_equal(result$pupil_interpolated, data$pupil_interpolated)
})

test_that("smooth_gazepoint_pupil replaces pre-existing output columns", {
  data <- make_smooth_pupil_data()
  data$pupil_smoothed <- -999
  data$pupil_smoothing_status <- "old"

  result <- smooth_gazepoint_pupil(data)

  expect_equal(sum(names(result) == "pupil_smoothed"), 1)
  expect_equal(sum(names(result) == "pupil_smoothing_status"), 1)
  expect_false(any(result$pupil_smoothing_status == "old"))
  expect_false(any(result$pupil_smoothed == -999, na.rm = TRUE))
})

test_that("smooth_gazepoint_pupil validates arguments", {
  data <- make_smooth_pupil_data()

  expect_error(
    smooth_gazepoint_pupil("not a data frame"),
    "`data` must be a data frame"
  )

  expect_error(
    smooth_gazepoint_pupil(data, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    smooth_gazepoint_pupil(data, time_col = c("a", "b")),
    "`time_col` must be `NULL` or a single character string"
  )

  expect_error(
    smooth_gazepoint_pupil(data, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    smooth_gazepoint_pupil(data, window_samples = c(3, 5)),
    "`window_samples` must be a single numeric value"
  )

  expect_error(
    smooth_gazepoint_pupil(data, min_points = c(1, 2)),
    "`min_points` must be a single numeric value"
  )

  expect_error(
    smooth_gazepoint_pupil(data, preserve_missing = c(TRUE, FALSE)),
    "`preserve_missing` must be `TRUE` or `FALSE`"
  )

  expect_error(
    smooth_gazepoint_pupil(data, window_samples = 0),
    "`window_samples` must be greater than or equal to 1"
  )

  expect_error(
    smooth_gazepoint_pupil(data, min_points = 0),
    "`min_points` must be greater than or equal to 1"
  )

  expect_error(
    smooth_gazepoint_pupil(data, window_samples = 3, min_points = 4),
    "`min_points` must be less than or equal to `window_samples`"
  )

  expect_error(
    smooth_gazepoint_pupil(data, method = "mode"),
    "'arg' should be one of"
  )

  expect_error(
    smooth_gazepoint_pupil(data, align = "middle"),
    "'arg' should be one of"
  )
})

test_that("smooth_gazepoint_pupil errors when required columns are missing", {
  data <- make_smooth_pupil_data()

  no_pupil <- data
  no_pupil$pupil_interpolated <- NULL

  expect_error(
    smooth_gazepoint_pupil(no_pupil),
    "No pupil column was found"
  )

  no_time <- data
  no_time$time <- NULL

  expect_error(
    smooth_gazepoint_pupil(no_time),
    "No time column was found"
  )

  no_subject <- data
  no_subject$subject <- NULL

  expect_error(
    smooth_gazepoint_pupil(no_subject),
    "requested but not found"
  )

  expect_error(
    smooth_gazepoint_pupil(data, group_cols = "condition"),
    "requested but not found"
  )
})
