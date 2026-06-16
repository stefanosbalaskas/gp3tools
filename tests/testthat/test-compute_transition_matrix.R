testthat::test_that("compute_transition_matrix counts AOI transitions by group", {
  dat <- data.frame(
    USER_FILE = c(
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 0_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv",
      "User 1_all_gaze.csv"
    ),
    MEDIA_ID = c(0, 0, 0, 0, 0, 0, 0, 0),
    TIME = c(0.00, 0.10, 0.20, 0.30, 0.00, 0.10, 0.20, 0.30),
    AOI = c(
      "AOI 1", "AOI 1", "AOI 2", "AOI 3",
      "AOI 1", "AOI 2", "AOI 2", "AOI 1"
    ),
    stringsAsFactors = FALSE
  )

  out <- compute_transition_matrix(
    dat,
    group_cols = c("USER_FILE", "MEDIA_ID")
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 4)

  testthat::expect_true(all(c(
    "USER_FILE",
    "MEDIA_ID",
    "from",
    "to",
    "n",
    "prob"
  ) %in% names(out)))

  testthat::expect_equal(
    out$n[out$USER_FILE == "User 0_all_gaze.csv" &
            out$from == "AOI 1" &
            out$to == "AOI 2"],
    1L
  )

  testthat::expect_equal(
    out$n[out$USER_FILE == "User 0_all_gaze.csv" &
            out$from == "AOI 2" &
            out$to == "AOI 3"],
    1L
  )

  testthat::expect_equal(
    out$n[out$USER_FILE == "User 1_all_gaze.csv" &
            out$from == "AOI 1" &
            out$to == "AOI 2"],
    1L
  )

  testthat::expect_equal(
    out$n[out$USER_FILE == "User 1_all_gaze.csv" &
            out$from == "AOI 2" &
            out$to == "AOI 1"],
    1L
  )

  testthat::expect_true(all(out$prob == 1))
})
