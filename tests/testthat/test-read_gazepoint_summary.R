testthat::test_that("read_gazepoint_summary parses AOI summary and AOI by-user sections", {
  path <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "Gazepoint Analysis,v7.2.0",
      "Processed on,Thu Jun 11 23:50:26 2026",
      "",
      "AOI Summary",
      "Media ID,Media Name,AOI ID,AOI Name,Viewers (#),",
      "0,NewMedia0,2,AOI 2,6,",
      "1,NewMedia1,0,AOI 0,5,",
      "",
      "AOI Statistics (for each user)",
      "Media ID,Media Name,AOI ID,AOI Name,User ID,User Name,Time Viewed (sec),",
      "0,NewMedia0,2,AOI 2,0,User 0,1.50,",
      "1,NewMedia1,0,AOI 0,1,User 1,2.25,"
    ),
    path
  )

  out <- read_gazepoint_summary(path)

  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("metadata", "aoi_summary", "aoi_by_user") %in% names(out)))

  testthat::expect_equal(out$metadata$gazepoint_analysis_version, "v7.2.0")
  testthat::expect_equal(out$metadata$processed_on, "Thu Jun 11 23:50:26 2026")

  testthat::expect_s3_class(out$aoi_summary, "tbl_df")
  testthat::expect_s3_class(out$aoi_by_user, "tbl_df")

  testthat::expect_equal(nrow(out$aoi_summary), 2)
  testthat::expect_equal(nrow(out$aoi_by_user), 2)

  testthat::expect_true("AOI Name" %in% names(out$aoi_summary))
  testthat::expect_true("User ID" %in% names(out$aoi_by_user))

  testthat::expect_false(any(grepl("^\\.\\.\\.", names(out$aoi_summary))))
  testthat::expect_false(any(grepl("^\\.\\.\\.", names(out$aoi_by_user))))

  testthat::expect_equal(out$aoi_summary$`AOI Name`[1], "AOI 2")
  testthat::expect_equal(out$aoi_by_user$`Time Viewed (sec)`[2], 2.25)
})
