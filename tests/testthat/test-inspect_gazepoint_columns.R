testthat::test_that("inspect_gazepoint_columns identifies Gazepoint column groups", {
  dat <- data.frame(
    MEDIA_ID = c(0, 0),
    MEDIA_NAME = c("NewMedia0", "NewMedia0"),
    CNT = c(1, 2),
    TIME = c(0.000, 0.016),
    TIMETICK = c(123456, 123457),
    FPOGX = c(0.50, 0.51),
    FPOGY = c(0.40, 0.41),
    FPOGV = c(1, 0),
    BPOGX = c(0.52, 0.53),
    BPOGY = c(0.42, 0.43),
    BPOGV = c(1, 1),
    AOI = c("AOI 1", ""),
    stringsAsFactors = FALSE
  )

  out <- inspect_gazepoint_columns(dat)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), ncol(dat))

  testthat::expect_true(all(c(
    "column",
    "semantic_group",
    "dtype",
    "n_missing",
    "pct_missing"
  ) %in% names(out)))

  testthat::expect_equal(
    unname(out$semantic_group[out$column == "MEDIA_ID"]),
    "identification"
  )

  testthat::expect_equal(
    unname(out$semantic_group[out$column == "TIME"]),
    "time"
  )

  testthat::expect_equal(
    unname(out$semantic_group[out$column == "FPOGX"]),
    "fixation_gaze"
  )

  testthat::expect_equal(
    unname(out$semantic_group[out$column == "BPOGX"]),
    "best_gaze"
  )

  testthat::expect_equal(
    unname(out$semantic_group[out$column == "AOI"]),
    "derived"
  )
})
