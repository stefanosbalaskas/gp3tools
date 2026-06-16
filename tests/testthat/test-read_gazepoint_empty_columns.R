testthat::test_that("read_gazepoint removes empty trailing columns", {
  path <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "MEDIA_ID,TIME(2026/01/01 00:00:00),TIMETICK(f=10000000),FPOGX,",
      "0,0.000,123456,0.50,",
      "0,0.016,123457,0.51,"
    ),
    path
  )

  out <- read_gazepoint(path)

  testthat::expect_true("MEDIA_ID" %in% names(out))
  testthat::expect_true("TIME" %in% names(out))
  testthat::expect_true("TIMETICK" %in% names(out))
  testthat::expect_true("FPOGX" %in% names(out))

  testthat::expect_false(any(grepl("^\\.\\.\\.", names(out))))
  testthat::expect_false("EMPTY_TRAILING" %in% names(out))
  testthat::expect_equal(ncol(out), 4)
})
