make_pupil_master <- function() {
  tibble::tibble(
    subject = c("P1", "P1", "P1", "P1", "P2", "P2", "P2", "P2"),
    MEDIA_ID = c("M1", "M1", "M2", "M2", "M1", "M1", "M2", "M2"),
    time = c(0, 10, 20, 30, 0, 10, 20, 30),
    mean_pupil = c(3, NA, 4, 10, 20, NA, 21, 22),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, FALSE)
  )
}

test_that("summarise_gazepoint_pupil creates an overall summary", {
  master <- make_pupil_master()

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = character(0)
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$n_rows, 8)
  expect_equal(result$time_min_ms, 0)
  expect_equal(result$time_max_ms, 30)
  expect_equal(result$time_span_ms, 30)

  expect_equal(result$n_pupil_samples, 6)
  expect_equal(result$n_missing_pupil, 2)
  expect_equal(result$missing_pupil_pct, 25)
  expect_equal(result$valid_pupil_pct, 75)

  expect_equal(result$mean_pupil, mean(c(3, 4, 10, 20, 21, 22)))
  expect_equal(result$median_pupil, stats::median(c(3, 4, 10, 20, 21, 22)))
  expect_equal(result$min_pupil, 3)
  expect_equal(result$max_pupil, 22)

  expect_equal(result$n_implausible, 0)
  expect_equal(result$implausible_pct, 0)
  expect_equal(result$pupil_column, "mean_pupil")
  expect_equal(result$time_column, "time")
  expect_equal(result$min_plausible, 0)
  expect_equal(result$max_plausible, Inf)
})

test_that("summarise_gazepoint_pupil groups by subject and media by default", {
  master <- make_pupil_master()

  result <- summarise_gazepoint_pupil(master)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_true(all(c("subject", "media_id") %in% names(result)))

  p1_m1 <- result[result$subject == "P1" & result$media_id == "M1", ]

  expect_equal(p1_m1$n_rows, 2)
  expect_equal(p1_m1$n_pupil_samples, 1)
  expect_equal(p1_m1$n_missing_pupil, 1)
  expect_equal(p1_m1$mean_pupil, 3)
})

test_that("summarise_gazepoint_pupil can group by subject only", {
  master <- make_pupil_master()

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = "subject"
  )

  expect_equal(nrow(result), 2)

  p1 <- result[result$subject == "P1", ]
  p2 <- result[result$subject == "P2", ]

  expect_equal(p1$n_rows, 4)
  expect_equal(p1$n_pupil_samples, 3)
  expect_equal(p1$n_missing_pupil, 1)
  expect_equal(p1$mean_pupil, mean(c(3, 4, 10)))

  expect_equal(p2$n_rows, 4)
  expect_equal(p2$n_pupil_samples, 3)
  expect_equal(p2$n_missing_pupil, 1)
  expect_equal(p2$mean_pupil, mean(c(20, 21, 22)))
})

test_that("summarise_gazepoint_pupil supports custom plausible pupil thresholds", {
  master <- make_pupil_master()

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = character(0),
    min_pupil = 1,
    max_pupil = 9
  )

  expect_equal(result$n_pupil_samples, 6)
  expect_equal(result$n_below_plausible, 0)
  expect_equal(result$n_above_plausible, 4)
  expect_equal(result$n_implausible, 4)
  expect_equal(result$implausible_pct, 50)
  expect_equal(result$min_plausible, 1)
  expect_equal(result$max_plausible, 9)
})

test_that("summarise_gazepoint_pupil supports older standardised column names", {
  master <- make_pupil_master()
  old_master <- tibble::tibble(
    subject = master$subject,
    media_id = master$MEDIA_ID,
    time_ms = master$time,
    pupil = master$mean_pupil,
    missing_pupil = master$missing_pupil
  )

  result <- summarise_gazepoint_pupil(
    old_master,
    group_cols = character(0)
  )

  expect_equal(result$n_rows, 8)
  expect_equal(result$n_pupil_samples, 6)
  expect_equal(result$pupil_column, "pupil")
  expect_equal(result$time_column, "time_ms")
})

test_that("summarise_gazepoint_pupil supports explicit pupil, time, and missing columns", {
  master <- tibble::tibble(
    participant = c("P1", "P1", "P2", "P2"),
    media_id = c("A", "A", "A", "A"),
    custom_time = c(0, 10, 0, 10),
    custom_pupil = c(4, NA, 5, 6),
    custom_missing = c(FALSE, TRUE, FALSE, FALSE)
  )

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = character(0),
    pupil_col = "custom_pupil",
    time_col = "custom_time",
    missing_pupil_col = "custom_missing"
  )

  expect_equal(result$n_rows, 4)
  expect_equal(result$n_pupil_samples, 3)
  expect_equal(result$n_missing_pupil, 1)
  expect_equal(result$mean_pupil, 5)
  expect_equal(result$pupil_column, "custom_pupil")
  expect_equal(result$time_column, "custom_time")
})

test_that("summarise_gazepoint_pupil derives missing pupil when no missing flag exists", {
  master <- make_pupil_master()
  master$missing_pupil <- NULL

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = character(0)
  )

  expect_equal(result$n_rows, 8)
  expect_equal(result$n_pupil_samples, 6)
  expect_equal(result$n_missing_pupil, 2)
  expect_equal(result$missing_pupil_pct, 25)
})

test_that("summarise_gazepoint_pupil ignores values flagged as missing", {
  master <- tibble::tibble(
    subject = c("P1", "P1", "P1"),
    MEDIA_ID = c("M1", "M1", "M1"),
    time = c(0, 10, 20),
    mean_pupil = c(4, 100, 6),
    missing_pupil = c(FALSE, TRUE, FALSE)
  )

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = character(0)
  )

  expect_equal(result$n_pupil_samples, 2)
  expect_equal(result$n_missing_pupil, 1)
  expect_equal(result$mean_pupil, 5)
  expect_equal(result$max_pupil, 6)
})

test_that("summarise_gazepoint_pupil returns IQR outlier counts", {
  master <- tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    time = seq(0, 50, by = 10),
    mean_pupil = c(4, 4.1, 4.2, 4.3, 4.4, 10),
    missing_pupil = rep(FALSE, 6)
  )

  result <- summarise_gazepoint_pupil(
    master,
    group_cols = character(0)
  )

  expect_equal(result$n_pupil_samples, 6)
  expect_true(result$n_iqr_outliers >= 1)
  expect_true(result$iqr_outlier_pct > 0)
})

test_that("summarise_gazepoint_pupil validates arguments", {
  master <- make_pupil_master()

  expect_error(
    summarise_gazepoint_pupil("not a data frame"),
    "`master` must be a data frame"
  )

  expect_error(
    summarise_gazepoint_pupil(master, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    summarise_gazepoint_pupil(master, group_cols = "trial"),
    "`group_cols` can only contain"
  )

  expect_error(
    summarise_gazepoint_pupil(master, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    summarise_gazepoint_pupil(master, time_col = c("a", "b")),
    "`time_col` must be `NULL` or a single character string"
  )

  expect_error(
    summarise_gazepoint_pupil(master, missing_pupil_col = c("a", "b")),
    "`missing_pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    summarise_gazepoint_pupil(master, min_pupil = c(0, 1)),
    "`min_pupil` must be a single numeric value"
  )

  expect_error(
    summarise_gazepoint_pupil(master, max_pupil = c(1, 2)),
    "`max_pupil` must be a single numeric value"
  )

  expect_error(
    summarise_gazepoint_pupil(master, outlier_k = c(1.5, 3)),
    "`outlier_k` must be a single numeric value"
  )

  expect_error(
    summarise_gazepoint_pupil(master, min_pupil = 10, max_pupil = 5),
    "`max_pupil` must be greater than `min_pupil`"
  )
})

test_that("summarise_gazepoint_pupil errors when required columns are missing", {
  master <- make_pupil_master()

  no_subject <- master
  no_subject$subject <- NULL

  expect_error(
    summarise_gazepoint_pupil(no_subject),
    "No subject column was found"
  )

  no_media <- master
  no_media$MEDIA_ID <- NULL

  expect_error(
    summarise_gazepoint_pupil(no_media),
    "No media/stimulus column was found"
  )

  no_pupil <- master
  no_pupil$mean_pupil <- NULL

  expect_error(
    summarise_gazepoint_pupil(no_pupil),
    "No pupil column was found"
  )

  no_time <- master
  no_time$time <- NULL

  expect_error(
    summarise_gazepoint_pupil(no_time),
    "No time column was found"
  )

  expect_error(
    summarise_gazepoint_pupil(master, missing_pupil_col = "not_here"),
    "`missing_pupil_col` was not found in `master`"
  )
})
