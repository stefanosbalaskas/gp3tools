testthat::test_that("check_sampling_rate works with multiple grouping columns", {
  dat <- data.frame(
    USER_FILE = c(
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv"
    ),
    MEDIA_ID = c(0, 0, 0, 0, 0, 0),
    TIME = c(0.000, 0.016, 0.032, 0.000, 0.020, 0.040)
  )

  out <- check_sampling_rate(
    dat,
    group_cols = c("USER_FILE", "MEDIA_ID")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(c("USER_FILE", "MEDIA_ID") %in% names(out)))
  testthat::expect_true(all(c(
    "n_samples",
    "duration_sec",
    "mean_interval_ms",
    "median_interval_ms",
    "sd_interval_ms",
    "estimated_hz"
  ) %in% names(out)))

  user0 <- out[out$USER_FILE == "User 0_all_gaze.csv", ]
  user1 <- out[out$USER_FILE == "User 1_all_gaze.csv", ]

  testthat::expect_equal(user0$n_samples, 3L)
  testthat::expect_equal(user0$mean_interval_ms, 16)
  testthat::expect_equal(user0$estimated_hz, 62.5)

  testthat::expect_equal(user1$n_samples, 3L)
  testthat::expect_equal(user1$mean_interval_ms, 20)
  testthat::expect_equal(user1$estimated_hz, 50)
})
