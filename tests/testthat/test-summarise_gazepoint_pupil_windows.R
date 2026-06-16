make_pupil_window_data <- function() {
  tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    time = c(0, 100, 200, 500, 700, 1000),
    pupil_smoothed = c(1, 2, NA, 4, 5, 6)
  )
}

test_that("summarise_gazepoint_pupil_windows returns expected columns", {
  data <- make_pupil_window_data()

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500, 1000)
  )

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "subject",
    "media_id",
    "window_label",
    "window_start_ms",
    "window_end_ms",
    "n_samples",
    "n_valid_pupil",
    "n_missing_pupil",
    "valid_pupil_pct",
    "missing_pupil_pct",
    "mean_pupil",
    "sd_pupil",
    "median_pupil",
    "min_pupil",
    "max_pupil",
    "q25_pupil",
    "q75_pupil",
    "pupil_auc",
    "pupil_time_span_ms",
    "pupil_window_status",
    "pupil_window_pupil_column",
    "pupil_window_time_column",
    "pupil_window_min_valid_samples",
    "pupil_window_include_end"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$pupil_window_pupil_column[1], "pupil_smoothed")
  expect_equal(result$pupil_window_time_column[1], "time")
  expect_equal(result$pupil_window_min_valid_samples[1], 1L)
  expect_false(result$pupil_window_include_end[1])
})

test_that("summarise_gazepoint_pupil_windows summarises numeric windows", {
  data <- make_pupil_window_data()

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500, 1000)
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$window_label, c("0_500ms", "500_1000ms"))
  expect_equal(result$n_samples, c(3L, 2L))
  expect_equal(result$n_valid_pupil, c(2L, 2L))
  expect_equal(result$n_missing_pupil, c(1L, 0L))
  expect_equal(result$valid_pupil_pct, c(2 / 3 * 100, 100))
  expect_equal(result$missing_pupil_pct, c(1 / 3 * 100, 0))
  expect_equal(result$mean_pupil, c(mean(c(1, 2)), mean(c(4, 5))))
  expect_equal(result$median_pupil, c(stats::median(c(1, 2)), stats::median(c(4, 5))))
  expect_equal(result$min_pupil, c(1, 4))
  expect_equal(result$max_pupil, c(2, 5))
  expect_equal(result$pupil_window_status, c("valid", "valid"))
})

test_that("summarise_gazepoint_pupil_windows computes SD, quantiles, AUC, and time span", {
  data <- make_pupil_window_data()

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500)
  )

  expect_equal(result$sd_pupil[1], stats::sd(c(1, 2)))
  expect_equal(result$q25_pupil[1], as.numeric(stats::quantile(c(1, 2), 0.25, names = FALSE)))
  expect_equal(result$q75_pupil[1], as.numeric(stats::quantile(c(1, 2), 0.75, names = FALSE)))

  expected_auc <- 100 * (1 + 2) / 2

  expect_equal(result$pupil_auc[1], expected_auc)
  expect_equal(result$pupil_time_span_ms[1], 100)
})

test_that("summarise_gazepoint_pupil_windows can include window end", {
  data <- make_pupil_window_data()

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500),
    include_window_end = TRUE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$n_samples, 4L)
  expect_equal(result$n_valid_pupil, 3L)
  expect_equal(result$mean_pupil, mean(c(1, 2, 4)))
  expect_true(result$pupil_window_include_end[1])
})

test_that("summarise_gazepoint_pupil_windows supports custom window data frames", {
  data <- make_pupil_window_data()

  windows <- tibble::tibble(
    window_label = c("early", "late"),
    window_start_ms = c(0, 500),
    window_end_ms = c(500, 1000)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = windows
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$window_label, c("early", "late"))
  expect_equal(result$window_start_ms, c(0, 500))
  expect_equal(result$window_end_ms, c(500, 1000))
})

test_that("summarise_gazepoint_pupil_windows supports custom window column names", {
  data <- make_pupil_window_data()

  windows <- tibble::tibble(
    label = c("early", "late"),
    start = c(0, 500),
    end = c(500, 1000)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = windows
  )

  expect_equal(result$window_label, c("early", "late"))
  expect_equal(result$window_start_ms, c(0, 500))
  expect_equal(result$window_end_ms, c(500, 1000))
})

test_that("summarise_gazepoint_pupil_windows respects min_valid_samples", {
  data <- make_pupil_window_data()

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500, 1000),
    min_valid_samples = 3
  )

  expect_equal(result$pupil_window_status, c("insufficient_valid_pupil", "insufficient_valid_pupil"))
})

test_that("summarise_gazepoint_pupil_windows labels no-valid-pupil windows", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, 100, 200),
    pupil_smoothed = c(NA, NA, NA)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500)
  )

  expect_equal(result$n_samples, 3L)
  expect_equal(result$n_valid_pupil, 0L)
  expect_equal(result$n_missing_pupil, 3L)
  expect_equal(result$pupil_window_status, "no_valid_pupil")
  expect_true(is.na(result$mean_pupil))
  expect_true(is.na(result$pupil_auc))
})

test_that("summarise_gazepoint_pupil_windows keeps summaries within groups", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P2", "P2"),
    MEDIA_ID = rep("M1", 4),
    time = c(0, 100, 0, 100),
    pupil_smoothed = c(1, 3, 10, 30)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500)
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$subject, c("P1", "P2"))
  expect_equal(result$mean_pupil, c(2, 20))
})

test_that("summarise_gazepoint_pupil_windows can summarise globally", {
  data <- tibble::tibble(
    subject = c("P1", "P1", "P2", "P2"),
    MEDIA_ID = rep("M1", 4),
    time = c(0, 100, 0, 100),
    pupil_smoothed = c(1, 3, 10, 30)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500),
    group_cols = character(0)
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$mean_pupil, mean(c(1, 3, 10, 30)))
  expect_equal(result$n_samples, 4L)
  expect_equal(result$n_valid_pupil, 4L)
})

test_that("summarise_gazepoint_pupil_windows supports custom grouping columns", {
  data <- tibble::tibble(
    subject = rep("P1", 4),
    MEDIA_ID = rep("M1", 4),
    condition = c("A", "A", "B", "B"),
    time = c(0, 100, 0, 100),
    pupil_smoothed = c(1, 3, 10, 30)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500),
    group_cols = "condition"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$condition, c("A", "B"))
  expect_equal(result$mean_pupil, c(2, 20))
})

test_that("summarise_gazepoint_pupil_windows supports explicit pupil and time columns", {
  data <- tibble::tibble(
    participant = rep("P1", 3),
    media_id = rep("M1", 3),
    custom_time = c(0, 100, 200),
    custom_pupil = c(1, 2, 3)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    pupil_col = "custom_pupil",
    time_col = "custom_time",
    windows = c(0, 500)
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$mean_pupil, 2)
  expect_equal(result$pupil_window_pupil_column[1], "custom_pupil")
  expect_equal(result$pupil_window_time_column[1], "custom_time")
})

test_that("summarise_gazepoint_pupil_windows auto-detects processed pupil columns", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(0, 100, 200),
    pupil_smoothed = c(1, 2, 3),
    pupil_baseline_corrected = c(10, 20, 30)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500)
  )

  expect_equal(result$pupil_window_pupil_column[1], "pupil_smoothed")
  expect_equal(result$mean_pupil, 2)
})

test_that("summarise_gazepoint_pupil_windows auto-detects relative time columns", {
  data <- tibble::tibble(
    subject = rep("P1", 3),
    MEDIA_ID = rep("M1", 3),
    time = c(1000, 1100, 1200),
    time_relative_ms = c(0, 100, 200),
    pupil_smoothed = c(1, 2, 3)
  )

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(0, 500)
  )

  expect_equal(result$pupil_window_time_column[1], "time_relative_ms")
  expect_equal(result$mean_pupil, 2)
})

test_that("summarise_gazepoint_pupil_windows returns empty tibble when no samples fall in windows", {
  data <- make_pupil_window_data()

  result <- summarise_gazepoint_pupil_windows(
    data,
    windows = c(2000, 3000)
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c("window_label", "mean_pupil", "pupil_window_status") %in% names(result)))
})

test_that("summarise_gazepoint_pupil_windows validates arguments", {
  data <- make_pupil_window_data()

  expect_error(
    summarise_gazepoint_pupil_windows("not a data frame"),
    "`data` must be a data frame"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, time_col = c("a", "b")),
    "`time_col` must be `NULL` or a single character string"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, include_window_end = c(TRUE, FALSE)),
    "`include_window_end` must be `TRUE` or `FALSE`"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, min_valid_samples = c(1, 2)),
    "`min_valid_samples` must be a single numeric value"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, min_valid_samples = 0),
    "`min_valid_samples` must be greater than or equal to 1"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, windows = c(0)),
    "Numeric `windows` must contain at least two breakpoints"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, windows = c(0, NA, 500)),
    "Numeric `windows` must not contain missing values"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, windows = c(0, 500, 500)),
    "Numeric `windows` must be strictly increasing"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, windows = "bad"),
    "`windows` must be either a numeric vector or a data frame"
  )
})

test_that("summarise_gazepoint_pupil_windows validates window data frames", {
  data <- make_pupil_window_data()

  expect_error(
    summarise_gazepoint_pupil_windows(
      data,
      windows = tibble::tibble(window_end_ms = 500)
    ),
    "Window data must contain a start column"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(
      data,
      windows = tibble::tibble(window_start_ms = 0)
    ),
    "Window data must contain an end column"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(
      data,
      windows = tibble::tibble(
        window_start_ms = 0,
        window_end_ms = NA
      )
    ),
    "Window start and end values must be numeric and non-missing"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(
      data,
      windows = tibble::tibble(
        window_label = "",
        window_start_ms = 0,
        window_end_ms = 500
      )
    ),
    "Window labels must not be missing or empty"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(
      data,
      windows = tibble::tibble(
        window_start_ms = 500,
        window_end_ms = 0
      )
    ),
    "Each window end must be greater than or equal to its start"
  )
})

test_that("summarise_gazepoint_pupil_windows errors when required columns are missing", {
  data <- make_pupil_window_data()

  no_pupil <- data
  no_pupil$pupil_smoothed <- NULL

  expect_error(
    summarise_gazepoint_pupil_windows(no_pupil),
    "No pupil column was found"
  )

  no_time <- data
  no_time$time <- NULL

  expect_error(
    summarise_gazepoint_pupil_windows(no_time),
    "No time column was found"
  )

  no_subject <- data
  no_subject$subject <- NULL

  expect_error(
    summarise_gazepoint_pupil_windows(no_subject),
    "requested but not found"
  )

  expect_error(
    summarise_gazepoint_pupil_windows(data, group_cols = "condition"),
    "requested but not found"
  )
})
