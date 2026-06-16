testthat::test_that("summarise_gazepoint_aoi combines sample and fixation metrics", {
  gaze_data <- data.frame(
    USER_FILE = c(
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv"
    ),
    MEDIA_ID = c(0, 0, 0, 1, 1),
    MEDIA_NAME = c("NewMedia0", "NewMedia0", "NewMedia0", "NewMedia1", "NewMedia1"),
    AOI = c("AOI 1", "AOI 1", "AOI 1", "AOI 2", ""),
    TIME = c(0.10, 0.12, 0.14, 0.50, 0.70),
    stringsAsFactors = FALSE
  )

  fixation_data <- data.frame(
    USER_FILE = c(
      "User 0_fixations.csv",
      "User 0_fixations.csv",
      "User 1_fixations.csv"
    ),
    MEDIA_ID = c(0, 0, 1),
    MEDIA_NAME = c("NewMedia0", "NewMedia0", "NewMedia1"),
    AOI = c("AOI 1", "AOI 1", "AOI 2"),
    FPOGD = c(0.20, 0.30, 0.40),
    FPOGS = c(0.10, 0.30, 0.50),
    stringsAsFactors = FALSE
  )

  out <- summarise_gazepoint_aoi(
    gaze_data = gaze_data,
    fixation_data = fixation_data,
    sample_rate = 60
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 2)

  user0_aoi1 <- out[out$USER_ID == 0 & out$AOI == "AOI 1", ]

  testthat::expect_equal(user0_aoi1$sample_count, 3L)
  testthat::expect_equal(user0_aoi1$sample_time_viewed_sec, 3 / 60)
  testthat::expect_equal(user0_aoi1$fixation_count, 2L)
  testthat::expect_equal(user0_aoi1$fixation_duration_sum_sec, 0.50)
  testthat::expect_equal(user0_aoi1$fixation_duration_mean_ms, 250)
  testthat::expect_equal(user0_aoi1$fixation_ttff_sec, 0.10)
})
