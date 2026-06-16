testthat::test_that("summarise_tracking_quality works with multiple grouping columns", {
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
    FPOGV = c(1, 1, 0, 1, 0, 0),
    BPOGV = c(1, 1, 1, 1, 1, 0),
    LPV = c(1, 0, 1, 1, 1, 1),
    RPV = c(1, 1, 1, 0, 0, 0)
  )

  out <- summarise_tracking_quality(
    dat,
    group_cols = c("USER_FILE", "MEDIA_ID")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 2)

  testthat::expect_true(all(c(
    "USER_FILE",
    "MEDIA_ID",
    "FPOGV_valid_pct",
    "BPOGV_valid_pct",
    "LPV_valid_pct",
    "RPV_valid_pct"
  ) %in% names(out)))

  user0 <- out[out$USER_FILE == "User 0_all_gaze.csv", ]
  user1 <- out[out$USER_FILE == "User 1_all_gaze.csv", ]

  testthat::expect_equal(user0$FPOGV_valid_pct, 100 * 2 / 3)
  testthat::expect_equal(user0$BPOGV_valid_pct, 100)
  testthat::expect_equal(user0$LPV_valid_pct, 100 * 2 / 3)
  testthat::expect_equal(user0$RPV_valid_pct, 100)

  testthat::expect_equal(user1$FPOGV_valid_pct, 100 * 1 / 3)
  testthat::expect_equal(user1$BPOGV_valid_pct, 100 * 2 / 3)
  testthat::expect_equal(user1$LPV_valid_pct, 100)
  testthat::expect_equal(user1$RPV_valid_pct, 0)
})
