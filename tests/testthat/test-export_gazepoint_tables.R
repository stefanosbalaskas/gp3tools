testthat::test_that("export_gazepoint_tables writes named tables to CSV files", {
  output_dir <- tempfile()
  dir.create(output_dir)

  tables <- list(
    sampling = data.frame(id = 1:2, hz = c(60, 61)),
    quality = data.frame(id = 1:2, valid_pct = c(95, 90))
  )

  written <- export_gazepoint_tables(
    tables = tables,
    output_dir = output_dir,
    prefix = "test"
  )

  testthat::expect_s3_class(written, "tbl_df")
  testthat::expect_equal(nrow(written), 2)
  testthat::expect_true(all(file.exists(written$file)))

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_sampling.csv"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_quality.csv"))
  )
})

testthat::test_that("write_gazepoint_outputs writes standard output tables", {
  output_dir <- tempfile()
  dir.create(output_dir)

  sampling <- data.frame(USER_FILE = "User 0", estimated_hz = 60)
  quality <- data.frame(USER_FILE = "User 0", FPOGV_valid_pct = 95)
  aoi_table <- data.frame(USER_ID = 0, AOI = "AOI 1", fixation_count = 3)

  written <- write_gazepoint_outputs(
    sampling = sampling,
    quality = quality,
    aoi_table = aoi_table,
    output_dir = output_dir,
    prefix = "gp3"
  )

  testthat::expect_equal(nrow(written), 3)

  testthat::expect_true(
    file.exists(file.path(output_dir, "gp3_sampling.csv"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "gp3_quality.csv"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "gp3_aoi_table.csv"))
  )
})

testthat::test_that("write_gazepoint_outputs requires at least one table", {
  output_dir <- tempfile()
  dir.create(output_dir)

  testthat::expect_error(
    write_gazepoint_outputs(output_dir = output_dir),
    "At least one output table must be provided"
  )
})
