make_flag_pupil_master <- function() {
  tibble::tibble(
    subject = c("P1", "P1", "P1", "P1", "P2", "P2", "P2", "P2"),
    MEDIA_ID = c("M1", "M1", "M2", "M2", "M1", "M1", "M2", "M2"),
    time = c(0, 10, 20, 30, 0, 10, 20, 30),
    mean_pupil = c(4, NA, Inf, -1, 10, NA, 5, 100),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, FALSE)
  )
}

test_that("flag_gazepoint_pupil adds pupil flag columns", {
  master <- make_flag_pupil_master()

  result <- flag_gazepoint_pupil(
    master,
    flag_iqr_outliers = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(master))

  expected_cols <- c(
    "pupil_raw_value",
    "pupil_flag_missing",
    "pupil_flag_nonfinite",
    "pupil_flag_implausible_low",
    "pupil_flag_implausible_high",
    "pupil_flag_implausible",
    "pupil_flag_iqr_outlier",
    "pupil_flag_invalid",
    "pupil_flag_reason",
    "pupil_for_preprocessing",
    "pupil_flag_pupil_column",
    "pupil_flag_time_column",
    "pupil_flag_min_plausible",
    "pupil_flag_max_plausible",
    "pupil_flag_outlier_k"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$pupil_flag_pupil_column[1], "mean_pupil")
  expect_equal(result$pupil_flag_time_column[1], "time")
  expect_equal(result$pupil_flag_min_plausible[1], 0)
  expect_equal(result$pupil_flag_max_plausible[1], Inf)
})

test_that("flag_gazepoint_pupil flags missing, nonfinite, implausible, and valid samples", {
  master <- make_flag_pupil_master()

  result <- flag_gazepoint_pupil(
    master,
    flag_iqr_outliers = FALSE
  )

  reason_counts <- table(result$pupil_flag_reason)

  expect_equal(
    unname(as.integer(reason_counts[c(
      "implausible_low",
      "missing",
      "nonfinite",
      "valid"
    )])),
    c(1L, 2L, 1L, 4L)
  )

  expect_equal(sum(result$pupil_flag_missing), 2)
  expect_equal(sum(result$pupil_flag_nonfinite), 1)
  expect_equal(sum(result$pupil_flag_implausible_low), 1)
  expect_equal(sum(result$pupil_flag_implausible_high), 0)
  expect_equal(sum(result$pupil_flag_iqr_outlier), 0)
  expect_equal(sum(result$pupil_flag_invalid), 4)
  expect_equal(sum(!is.na(result$pupil_for_preprocessing)), 4)
})

test_that("flag_gazepoint_pupil uses custom plausible thresholds", {
  master <- make_flag_pupil_master()

  result <- flag_gazepoint_pupil(
    master,
    min_pupil = 1,
    max_pupil = 9,
    flag_iqr_outliers = FALSE
  )

  expect_equal(sum(result$pupil_flag_implausible_low), 1)
  expect_equal(sum(result$pupil_flag_implausible_high), 2)
  expect_equal(sum(result$pupil_flag_implausible), 3)
  expect_equal(sum(result$pupil_flag_invalid), 6)
  expect_equal(sum(!is.na(result$pupil_for_preprocessing)), 2)

  reason_counts <- table(result$pupil_flag_reason)

  expect_equal(
    unname(as.integer(reason_counts[c(
      "implausible_high",
      "implausible_low",
      "missing",
      "nonfinite",
      "valid"
    )])),
    c(2L, 1L, 2L, 1L, 2L)
  )
})

test_that("flag_gazepoint_pupil can flag IQR outliers", {
  master <- tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    time = seq(0, 50, by = 10),
    mean_pupil = c(4, 4.1, 4.2, 4.3, 4.4, 10),
    missing_pupil = rep(FALSE, 6)
  )

  result <- flag_gazepoint_pupil(
    master,
    group_cols = character(0)
  )

  expect_equal(sum(result$pupil_flag_iqr_outlier), 1)
  expect_equal(sum(result$pupil_flag_invalid), 1)
  expect_equal(result$pupil_flag_reason[6], "iqr_outlier")
  expect_true(is.na(result$pupil_for_preprocessing[6]))
})

test_that("flag_gazepoint_pupil can disable IQR outlier flagging", {
  master <- tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    time = seq(0, 50, by = 10),
    mean_pupil = c(4, 4.1, 4.2, 4.3, 4.4, 10),
    missing_pupil = rep(FALSE, 6)
  )

  result <- flag_gazepoint_pupil(
    master,
    group_cols = character(0),
    flag_iqr_outliers = FALSE
  )

  expect_equal(sum(result$pupil_flag_iqr_outlier), 0)
  expect_equal(sum(result$pupil_flag_invalid), 0)
  expect_true(all(result$pupil_flag_reason == "valid"))
  expect_equal(sum(!is.na(result$pupil_for_preprocessing)), 6)
})

test_that("flag_gazepoint_pupil supports old standardised column names", {
  master <- make_flag_pupil_master()

  old_master <- tibble::tibble(
    subject = master$subject,
    media_id = master$MEDIA_ID,
    time_ms = master$time,
    pupil = master$mean_pupil,
    missing_pupil = master$missing_pupil
  )

  result <- flag_gazepoint_pupil(
    old_master,
    flag_iqr_outliers = FALSE
  )

  expect_equal(nrow(result), 8)
  expect_equal(result$pupil_flag_pupil_column[1], "pupil")
  expect_equal(result$pupil_flag_time_column[1], "time_ms")
  expect_equal(sum(result$pupil_flag_missing), 2)
})

test_that("flag_gazepoint_pupil supports explicit pupil, time, and missing columns", {
  master <- tibble::tibble(
    participant = c("P1", "P1", "P2", "P2"),
    media_id = c("A", "A", "A", "A"),
    custom_time = c(0, 10, 0, 10),
    custom_pupil = c(4, NA, 5, 6),
    custom_missing = c(FALSE, TRUE, FALSE, FALSE)
  )

  result <- flag_gazepoint_pupil(
    master,
    pupil_col = "custom_pupil",
    time_col = "custom_time",
    missing_pupil_col = "custom_missing",
    flag_iqr_outliers = FALSE
  )

  expect_equal(nrow(result), 4)
  expect_equal(sum(result$pupil_flag_missing), 1)
  expect_equal(sum(result$pupil_flag_invalid), 1)
  expect_equal(sum(!is.na(result$pupil_for_preprocessing)), 3)
  expect_equal(result$pupil_flag_pupil_column[1], "custom_pupil")
  expect_equal(result$pupil_flag_time_column[1], "custom_time")
})

test_that("flag_gazepoint_pupil derives missing pupil when no missing flag exists", {
  master <- make_flag_pupil_master()
  master$missing_pupil <- NULL

  result <- flag_gazepoint_pupil(
    master,
    flag_iqr_outliers = FALSE
  )

  expect_equal(sum(result$pupil_flag_missing), 2)
  expect_equal(sum(result$pupil_flag_nonfinite), 1)
  expect_equal(sum(result$pupil_flag_implausible_low), 1)
})

test_that("flag_gazepoint_pupil preserves row order and original columns", {
  master <- make_flag_pupil_master()

  result <- flag_gazepoint_pupil(
    master,
    flag_iqr_outliers = FALSE
  )

  expect_equal(result$subject, master$subject)
  expect_equal(result$MEDIA_ID, master$MEDIA_ID)
  expect_equal(result$time, master$time)
  expect_equal(result$mean_pupil, master$mean_pupil)
})

test_that("flag_gazepoint_pupil replaces pre-existing output columns", {
  master <- make_flag_pupil_master()
  master$pupil_flag_reason <- "old"
  master$pupil_for_preprocessing <- -999

  result <- flag_gazepoint_pupil(
    master,
    flag_iqr_outliers = FALSE
  )

  expect_equal(sum(names(result) == "pupil_flag_reason"), 1)
  expect_equal(sum(names(result) == "pupil_for_preprocessing"), 1)
  expect_false(any(result$pupil_flag_reason == "old"))
  expect_false(any(result$pupil_for_preprocessing == -999, na.rm = TRUE))
})

test_that("flag_gazepoint_pupil validates arguments", {
  master <- make_flag_pupil_master()

  expect_error(
    flag_gazepoint_pupil("not a data frame"),
    "`master` must be a data frame"
  )

  expect_error(
    flag_gazepoint_pupil(master, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    flag_gazepoint_pupil(master, time_col = c("a", "b")),
    "`time_col` must be `NULL` or a single character string"
  )

  expect_error(
    flag_gazepoint_pupil(master, missing_pupil_col = c("a", "b")),
    "`missing_pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    flag_gazepoint_pupil(master, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    flag_gazepoint_pupil(master, group_cols = "trial"),
    "`group_cols` can only contain"
  )

  expect_error(
    flag_gazepoint_pupil(master, min_pupil = c(0, 1)),
    "`min_pupil` must be a single numeric value"
  )

  expect_error(
    flag_gazepoint_pupil(master, max_pupil = c(1, 2)),
    "`max_pupil` must be a single numeric value"
  )

  expect_error(
    flag_gazepoint_pupil(master, outlier_k = c(1.5, 3)),
    "`outlier_k` must be a single numeric value"
  )

  expect_error(
    flag_gazepoint_pupil(master, flag_iqr_outliers = c(TRUE, FALSE)),
    "`flag_iqr_outliers` must be `TRUE` or `FALSE`"
  )

  expect_error(
    flag_gazepoint_pupil(master, min_pupil = 10, max_pupil = 5),
    "`max_pupil` must be greater than `min_pupil`"
  )
})

test_that("flag_gazepoint_pupil errors when required columns are missing", {
  master <- make_flag_pupil_master()

  no_subject <- master
  no_subject$subject <- NULL

  expect_error(
    flag_gazepoint_pupil(no_subject),
    "No subject column was found"
  )

  no_media <- master
  no_media$MEDIA_ID <- NULL

  expect_error(
    flag_gazepoint_pupil(no_media),
    "No media/stimulus column was found"
  )

  no_pupil <- master
  no_pupil$mean_pupil <- NULL

  expect_error(
    flag_gazepoint_pupil(no_pupil),
    "No pupil column was found"
  )

  no_time <- master
  no_time$time <- NULL

  expect_error(
    flag_gazepoint_pupil(no_time),
    "No time column was found"
  )

  expect_error(
    flag_gazepoint_pupil(master, missing_pupil_col = "not_here"),
    "`missing_pupil_col` was not found in `master`"
  )
})
