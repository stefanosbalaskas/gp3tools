testthat::test_that("summarise_aoi_samples computes sample-level AOI viewing metrics", {
  dat <- data.frame(
    USER_FILE = c(
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv"
    ),
    MEDIA_ID = c(0, 0, 0, 0, 0, 0, 0),
    TIME = c(0.00, 0.10, 0.20, 0.30, 0.00, 0.20, 0.40),
    AOI = c("AOI 1", "AOI 1", "", "AOI 2", "AOI 1", "AOI 1", "AOI 1"),
    stringsAsFactors = FALSE
  )

  out <- summarise_aoi_samples(
    dat,
    group_cols = c("USER_FILE", "MEDIA_ID")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 3)

  user0_aoi1 <- out[
    out$USER_FILE == "User 0_all_gaze.csv" &
      out$AOI == "AOI 1",
  ]

  user0_aoi2 <- out[
    out$USER_FILE == "User 0_all_gaze.csv" &
      out$AOI == "AOI 2",
  ]

  user1_aoi1 <- out[
    out$USER_FILE == "User 1_all_gaze.csv" &
      out$AOI == "AOI 1",
  ]

  testthat::expect_equal(user0_aoi1$time_to_first_view_sec, 0.00)
  testthat::expect_equal(user0_aoi1$aoi_sample_count, 2L)
  testthat::expect_equal(user0_aoi1$approx_time_viewed_sec, 0.20)

  testthat::expect_equal(user0_aoi2$time_to_first_view_sec, 0.30)
  testthat::expect_equal(user0_aoi2$aoi_sample_count, 1L)

  testthat::expect_equal(user1_aoi1$time_to_first_view_sec, 0.00)
  testthat::expect_equal(user1_aoi1$aoi_sample_count, 3L)
  testthat::expect_equal(user1_aoi1$approx_time_viewed_sec, 0.60)
})
