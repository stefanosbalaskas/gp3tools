
test_that("read_gazepoint standardises names", {
  path <- system.file("extdata", "toy_all_gaze.csv", package = "gp3tools")
  dat <- read_gazepoint(path)
  expect_true("TIME" %in% names(dat))
  expect_true("TIMETICK" %in% names(dat))
  expect_false("EMPTY_TRAILING" %in% names(dat))
})

test_that("sampling rate returns expected columns", {
  path <- system.file("extdata", "toy_all_gaze.csv", package = "gp3tools")
  dat <- read_gazepoint(path)
  out <- check_sampling_rate(dat)
  expect_true("estimated_hz" %in% names(out))
})

test_that("summary parser returns sections", {
  path <- system.file("extdata", "toy_summary.csv", package = "gp3tools")
  out <- read_gazepoint_summary(path)
  expect_true("metadata" %in% names(out))
  expect_true("aoi_summary" %in% names(out))
  expect_true("aoi_by_user" %in% names(out))
})
