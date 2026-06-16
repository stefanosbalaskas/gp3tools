make_interpolate_pupil_data <- function() {
  tibble::tibble(
    subject = rep("P1", 7),
    MEDIA_ID = rep("M1", 7),
    time = c(0, 10, 20, 30, 40, 50, 60),
    pupil_for_preprocessing = c(1, NA, 3, NA, NA, 6, NA)
  )
}

test_that("interpolate_gazepoint_pupil adds interpolation columns", {
  data <- make_interpolate_pupil_data()

  result <- interpolate_gazepoint_pupil(data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(data))

  expected_cols <- c(
    "pupil_interpolated",
    "pupil_was_interpolated",
    "pupil_interpolation_status",
    "pupil_gap_id",
    "pupil_gap_n_samples",
    "pupil_gap_duration_ms",
    "pupil_interp_pupil_column",
    "pupil_interp_time_column",
    "pupil_interp_max_gap_ms",
    "pupil_interp_max_gap_samples",
    "pupil_interp_min_valid_points"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$pupil_interp_pupil_column[1], "pupil_for_preprocessing")
  expect_equal(result$pupil_interp_time_column[1], "time")
  expect_equal(result$pupil_interp_max_gap_ms[1], 150)
})

test_that("interpolate_gazepoint_pupil interpolates short internal gaps", {
  data <- make_interpolate_pupil_data()

  result <- interpolate_gazepoint_pupil(data)

  expect_equal(result$pupil_interpolated[1], 1)
  expect_equal(result$pupil_interpolated[2], 2)
  expect_equal(result$pupil_interpolated[3], 3)

  expect_true(result$pupil_was_interpolated[2])
  expect_equal(result$pupil_interpolation_status[2], "interpolated")

  expect_equal(result$pupil_gap_n_samples[2], 1)
  expect_equal(result$pupil_gap_duration_ms[2], 20)
})

test_that("interpolate_gazepoint_pupil interpolates multi-sample short gaps", {
  data <- make_interpolate_pupil_data()

  result <- interpolate_gazepoint_pupil(data)

  expect_equal(result$pupil_interpolated[4], 4)
  expect_equal(result$pupil_interpolated[5], 5)

  expect_true(result$pupil_was_interpolated[4])
  expect_true(result$pupil_was_interpolated[5])

  expect_equal(result$pupil_interpolation_status[4], "interpolated")
  expect_equal(result$pupil_interpolation_status[5], "interpolated")

  expect_equal(result$pupil_gap_n_samples[4], 2)
  expect_equal(result$pupil_gap_n_samples[5], 2)
  expect_equal(result$pupil_gap_duration_ms[4], 30)
  expect_equal(result$pupil_gap_duration_ms[5], 30)
})

test_that("interpolate_gazepoint_pupil does not fill trailing edge gaps", {
  data <- make_interpolate_pupil_data()

  result <- interpolate_gazepoint_pupil(data)

  expect_true(is.na(result$pupil_interpolated[7]))
  expect_false(result$pupil_was_interpolated[7])
  expect_equal(result$pupil_interpolation_status[7], "missing_edge_gap")
})

test_that("interpolate_gazepoint_pupil does not fill leading edge gaps", {
  data <- tibble::tibble(
    subject = rep("P1", 4),
    MEDIA_ID = rep("M1", 4),
    time = c(0, 10, 20, 30),
    pupil_for_preprocessing = c(NA, 2, NA, 4)
  )

  result <- interpolate_gazepoint_pupil(data)

  expect_true(is.na(result$pupil_interpolated[1]))
  expect_equal(result$pupil_interpolation_status[1], "missing_edge_gap")

  expect_equal(result$pupil_interpolated[3], 3)
  expect_equal(result$pupil_interpolation_status[3], "interpolated")
})

test_that("interpolate_gazepoint_pupil respects max_gap_ms", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, 100, 200),
    pupil_for_preprocessing = c(1, NA, 3)
  )

  result <- interpolate_gazepoint_pupil(
    data,
    max_gap_ms = 50
  )

  expect_true(is.na(result$pupil_interpolated[2]))
  expect_false(result$pupil_was_interpolated[2])
  expect_equal(result$pupil_interpolation_status[2], "missing_long_gap")
  expect_equal(result$pupil_gap_duration_ms[2], 200)
})

test_that("interpolate_gazepoint_pupil respects max_gap_samples", {
  data <- tibble::tibble(
    subject = rep("P1", 4),
    MEDIA_ID = rep("M1", 4),
    time = c(0, 10, 20, 30),
    pupil_for_preprocessing = c(1, NA, NA, 4)
  )

  result <- interpolate_gazepoint_pupil(
    data,
    max_gap_samples = 1
  )

  expect_true(all(is.na(result$pupil_interpolated[2:3])))
  expect_false(any(result$pupil_was_interpolated[2:3]))
  expect_true(all(result$pupil_interpolation_status[2:3] == "missing_long_gap"))
})

test_that("interpolate_gazepoint_pupil does not interpolate missing rows without valid time", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, NA, 20),
    pupil_for_preprocessing = c(1, NA, 3)
  )

  result <- interpolate_gazepoint_pupil(data)

  expect_true(is.na(result$pupil_interpolated[2]))
  expect_false(result$pupil_was_interpolated[2])
  expect_equal(result$pupil_interpolation_status[2], "missing_no_time")
})

test_that("interpolate_gazepoint_pupil requires enough valid points in a group", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, 10, 20),
    pupil_for_preprocessing = c(1, NA, NA)
  )

  result <- interpolate_gazepoint_pupil(data)

  expect_true(all(is.na(result$pupil_interpolated[2:3])))
  expect_true(all(result$pupil_interpolation_status[2:3] == "missing_insufficient_valid"))
})

test_that("interpolate_gazepoint_pupil keeps interpolation within groups", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    MEDIA_ID = rep("M1", 6),
    time = c(0, 10, 20, 0, 10, 20),
    pupil_for_preprocessing = c(1, NA, 3, 10, NA, 14)
  )

  result <- interpolate_gazepoint_pupil(data)

  expect_equal(result$pupil_interpolated[2], 2)
  expect_equal(result$pupil_interpolated[5], 12)

  expect_true(result$pupil_was_interpolated[2])
  expect_true(result$pupil_was_interpolated[5])
})

test_that("interpolate_gazepoint_pupil can interpolate globally", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1"),
    MEDIA_ID = c("M1", "M1", "M1"),
    time = c(0, 10, 20),
    pupil_for_preprocessing = c(1, NA, 3)
  )

  result <- interpolate_gazepoint_pupil(
    data,
    group_cols = character(0)
  )

  expect_equal(result$pupil_interpolated[2], 2)
  expect_true(result$pupil_was_interpolated[2])
})

test_that("interpolate_gazepoint_pupil supports old standardised column names", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    media_id = rep("M1", 3),
    time_ms = c(0, 10, 20),
    pupil = c(1, NA, 3)
  )

  result <- interpolate_gazepoint_pupil(data)

  expect_equal(result$pupil_interpolated[2], 2)
  expect_equal(result$pupil_interp_pupil_column[1], "pupil")
  expect_equal(result$pupil_interp_time_column[1], "time_ms")
})

test_that("interpolate_gazepoint_pupil supports explicit pupil and time columns", {
  data <- tibble::tibble(
    participant = rep("P1", 3),
    media_id = rep("M1", 3),
    custom_time = c(0, 10, 20),
    custom_pupil = c(1, NA, 3)
  )

  result <- interpolate_gazepoint_pupil(
    data,
    pupil_col = "custom_pupil",
    time_col = "custom_time"
  )

  expect_equal(result$pupil_interpolated[2], 2)
  expect_equal(result$pupil_interp_pupil_column[1], "custom_pupil")
  expect_equal(result$pupil_interp_time_column[1], "custom_time")
})

test_that("interpolate_gazepoint_pupil preserves row order and original columns", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P1"),
    MEDIA_ID = c("M1", "M1", "M1"),
    time = c(20, 0, 10),
    pupil_for_preprocessing = c(3, 1, NA)
  )

  result <- interpolate_gazepoint_pupil(data)

  expect_equal(result$subject, data$subject)
  expect_equal(result$MEDIA_ID, data$MEDIA_ID)
  expect_equal(result$time, data$time)
  expect_equal(result$pupil_for_preprocessing, data$pupil_for_preprocessing)
})

test_that("interpolate_gazepoint_pupil replaces pre-existing output columns", {
  data <- make_interpolate_pupil_data()
  data$pupil_interpolated <- -999
  data$pupil_interpolation_status <- "old"

  result <- interpolate_gazepoint_pupil(data)

  expect_equal(sum(names(result) == "pupil_interpolated"), 1)
  expect_equal(sum(names(result) == "pupil_interpolation_status"), 1)
  expect_false(any(result$pupil_interpolation_status == "old"))
  expect_false(any(result$pupil_interpolated == -999, na.rm = TRUE))
})

test_that("interpolate_gazepoint_pupil validates arguments", {
  data <- make_interpolate_pupil_data()

  expect_error(
    interpolate_gazepoint_pupil("not a data frame"),
    "`data` must be a data frame"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, time_col = c("a", "b")),
    "`time_col` must be `NULL` or a single character string"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, group_cols = "condition"),
    "`group_cols` can only contain"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, max_gap_ms = c(100, 200)),
    "`max_gap_ms` must be a single numeric value"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, max_gap_samples = c(1, 2)),
    "`max_gap_samples` must be a single numeric value"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, min_valid_points = c(2, 3)),
    "`min_valid_points` must be a single numeric value"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, max_gap_ms = -1),
    "`max_gap_ms` must be greater than or equal to 0"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, max_gap_samples = -1),
    "`max_gap_samples` must be greater than or equal to 0"
  )

  expect_error(
    interpolate_gazepoint_pupil(data, min_valid_points = 1),
    "`min_valid_points` must be greater than or equal to 2"
  )
})

test_that("interpolate_gazepoint_pupil errors when required columns are missing", {
  data <- make_interpolate_pupil_data()

  no_pupil <- data
  no_pupil$pupil_for_preprocessing <- NULL

  expect_error(
    interpolate_gazepoint_pupil(no_pupil),
    "No pupil column was found"
  )

  no_time <- data
  no_time$time <- NULL

  expect_error(
    interpolate_gazepoint_pupil(no_time),
    "No time column was found"
  )

  no_subject <- data
  no_subject$subject <- NULL

  expect_error(
    interpolate_gazepoint_pupil(no_subject),
    "requested but not found"
  )
})
