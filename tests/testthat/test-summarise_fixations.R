testthat::test_that("summarise_fixations computes AOI fixation metrics", {
  dat <- data.frame(
    USER_FILE = c(
      "User 0_fixations.csv",
      "User 0_fixations.csv",
      "User 0_fixations.csv",
      "User 1_fixations.csv",
      "User 1_fixations.csv"
    ),
    MEDIA_ID = c(0, 0, 0, 0, 0),
    AOI = c("AOI 1", "AOI 1", "", "AOI 2", "AOI 2"),
    FPOGD = c(0.20, 0.30, 0.40, 0.50, 0.70),
    FPOGS = c(0.10, 0.40, 0.80, 0.20, 0.90),
    stringsAsFactors = FALSE
  )

  out <- summarise_fixations(
    dat,
    group_cols = c("USER_FILE", "MEDIA_ID")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 2)

  user0 <- out[out$USER_FILE == "User 0_fixations.csv", ]
  user1 <- out[out$USER_FILE == "User 1_fixations.csv", ]

  testthat::expect_equal(user0$AOI, "AOI 1")
  testthat::expect_equal(user0$fixation_count, 2L)
  testthat::expect_equal(user0$fixation_duration_sum_sec, 0.50)
  testthat::expect_equal(user0$fixation_duration_mean_ms, 250)
  testthat::expect_equal(user0$fixation_ttff_sec, 0.10)

  testthat::expect_equal(user1$AOI, "AOI 2")
  testthat::expect_equal(user1$fixation_count, 2L)
  testthat::expect_equal(user1$fixation_duration_sum_sec, 1.20)
  testthat::expect_equal(user1$fixation_duration_mean_ms, 600)
  testthat::expect_equal(user1$fixation_ttff_sec, 0.20)
})
