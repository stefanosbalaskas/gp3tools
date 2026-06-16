testthat::test_that("create_gazepoint_master creates expected master columns", {
  gaze_data <- tibble::tibble(
    USER_FILE = c("User 0_all_gaze.csv", "User 0_all_gaze.csv"),
    MEDIA_ID = c(0, 0),
    MEDIA_NAME = c("Stimulus 1", "Stimulus 1"),
    TIME = c(0, 0.016),
    BPOGX = c(0.50, 0.60),
    BPOGY = c(0.40, 0.45),
    BPOGV = c(1, 1),
    LPOGX = c(0.49, 0.59),
    LPOGY = c(0.39, 0.44),
    RPOGX = c(0.51, 0.61),
    RPOGY = c(0.41, 0.46),
    LPV = c(1, 1),
    RPV = c(1, 1),
    LPMM = c(3.1, 3.2),
    RPMM = c(3.3, 3.4),
    AOI = c("AOI 1", "")
  )

  master <- create_gazepoint_master(
    gaze_data = gaze_data,
    screen_width_px = 1920,
    screen_height_px = 1080,
    screen_width_cm = 52,
    screen_height_cm = 29,
    viewing_distance_cm = 60,
    baseline_window = c(-200, 0),
    analysis_window = c(0, 1000)
  )

  testthat::expect_s3_class(master, "tbl_df")
  testthat::expect_equal(nrow(master), 2)

  testthat::expect_true(all(c(
    "subject",
    "pID",
    "USER_FILE",
    "MEDIA_ID",
    "MEDIA_NAME",
    "trial_global",
    "time",
    "x",
    "y",
    "left_pupil",
    "right_pupil",
    "mean_pupil",
    "valid_sample",
    "missing_gaze",
    "missing_pupil",
    "aoi_current",
    "screen_width_px",
    "screen_height_px",
    "tracker_model"
  ) %in% names(master)))

  testthat::expect_equal(master$subject, c("User 0", "User 0"))
  testthat::expect_equal(master$time, c(0, 16))
  testthat::expect_equal(master$x, c(960, 1152))
  testthat::expect_equal(master$y, c(432, 486))

  testthat::expect_equal(master$aoi_current[1], "AOI 1")
  testthat::expect_equal(master$aoi_current[2], "non_aoi")

  testthat::expect_true(all(master$valid_sample))
  testthat::expect_false(any(master$missing_gaze))
  testthat::expect_false(any(master$missing_pupil))

  testthat::expect_equal(master$pupil_unit[1], "diameter_mm")
  testthat::expect_equal(master$gaze_unit[1], "pixels")
})

testthat::test_that("create_gazepoint_master handles missing gaze and pupil", {
  gaze_data <- tibble::tibble(
    USER_FILE = c("User 1_all_gaze.csv", "User 1_all_gaze.csv"),
    MEDIA_ID = c(1, 1),
    TIME = c(0, 0.016),
    BPOGX = c(0.50, NA),
    BPOGY = c(0.40, NA),
    BPOGV = c(1, 0),
    LPV = c(1, 0),
    RPV = c(1, 0),
    LPMM = c(3.1, NA),
    RPMM = c(3.2, NA),
    AOI = c("AOI 2", "")
  )

  master <- create_gazepoint_master(
    gaze_data = gaze_data,
    screen_width_px = 1920,
    screen_height_px = 1080
  )

  testthat::expect_equal(nrow(master), 2)

  testthat::expect_false(master$missing_gaze[1])
  testthat::expect_true(master$missing_gaze[2])

  testthat::expect_false(master$missing_pupil[1])
  testthat::expect_true(master$missing_pupil[2])

  testthat::expect_equal(master$aoi_current[1], "AOI 2")
  testthat::expect_equal(master$aoi_current[2], "missing")

  testthat::expect_true(master$artifact_flag[2])
  testthat::expect_equal(master$artifact_reason[2], "missing_gaze_and_pupil")
})

testthat::test_that("create_gazepoint_master keeps pixel coordinates unchanged", {
  gaze_data <- tibble::tibble(
    USER_FILE = "User 2_all_gaze.csv",
    MEDIA_ID = 0,
    TIME = 0,
    BPOGX = 960,
    BPOGY = 540,
    BPOGV = 1
  )

  master <- create_gazepoint_master(
    gaze_data = gaze_data,
    screen_width_px = 1920,
    screen_height_px = 1080
  )

  testthat::expect_equal(master$x, 960)
  testthat::expect_equal(master$y, 540)
})

testthat::test_that("create_gazepoint_master requires required columns", {
  gaze_data <- tibble::tibble(
    USER_FILE = "User 0_all_gaze.csv",
    MEDIA_ID = 0
  )

  testthat::expect_error(
    create_gazepoint_master(gaze_data),
    "Missing required columns"
  )
})

testthat::test_that("create_gazepoint_master requires data frame input", {
  testthat::expect_error(
    create_gazepoint_master("not data"),
    "`gaze_data` must be a data frame"
  )
})
