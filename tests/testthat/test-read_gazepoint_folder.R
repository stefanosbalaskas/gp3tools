testthat::test_that("read_gazepoint_folder reads and combines matching Gazepoint files", {
  folder <- tempfile()
  dir.create(folder)

  file1 <- file.path(folder, "User 0_all_gaze.csv")
  file2 <- file.path(folder, "User 1_all_gaze.csv")

  writeLines(
    c(
      "MEDIA_ID,TIME(2026/01/01 00:00:00),TIMETICK(f=10000000),FPOGX,",
      "0,0.000,123456,0.50,",
      "0,0.016,123457,0.51,"
    ),
    file1
  )

  writeLines(
    c(
      "MEDIA_ID,TIME(2026/01/01 00:00:00),TIMETICK(f=10000000),FPOGX,",
      "1,0.000,223456,0.60,",
      "1,0.016,223457,0.61,"
    ),
    file2
  )

  out <- read_gazepoint_folder(
    folder,
    pattern = "_all_gaze\\.csv$"
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true("USER_FILE" %in% names(out))
  testthat::expect_true("TIME" %in% names(out))
  testthat::expect_true("TIMETICK" %in% names(out))
  testthat::expect_false(any(grepl("^\\.\\.\\.", names(out))))

  testthat::expect_equal(
    sort(unique(out$USER_FILE)),
    c("User 0_all_gaze.csv", "User 1_all_gaze.csv")
  )
})
