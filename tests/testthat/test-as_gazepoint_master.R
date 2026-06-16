test_that("as_gazepoint_master converts normalised Gazepoint coordinates to pixels", {
  data <- tibble::tibble(
    USER_FILE = rep("User 1_all_gaze.csv", 6),
    MEDIA_ID = rep(0, 6),
    MEDIA_NAME = rep("Stimulus_A", 6),
    TIME = c(0, 0.016, 0.033, 0.050, 0.066, 0.083),
    CNT = 0:5,
    BPOGX = c(0.50, 0.25, 0.75, 0.10, 1.10, 1.60),
    BPOGY = c(0.50, 0.25, 0.75, 0.10, 1.10, 0.20),
    BPOGV = c(1, 1, 1, 1, 1, 1),
    LPMM = c(3.0, 3.1, 3.2, 3.3, 3.4, 3.5),
    RPMM = c(4.0, 4.1, 4.2, 4.3, 4.4, 4.5),
    LPMMV = rep(1, 6),
    RPMMV = rep(1, 6),
    AOI = c("AOI_1", "AOI_1", NA, NA, "AOI_2", "AOI_2")
  )

  master <- as_gazepoint_master(
    data,
    screen_width_px = 1000,
    screen_height_px = 500
  )

  expect_s3_class(master, "tbl_df")
  expect_equal(nrow(master), 6)

  expect_equal(unique(master$coordinate_unit_detected), "normalised")
  expect_equal(unique(master$gaze_unit), "pixels")

  expect_equal(master$x[1], 500)
  expect_equal(master$y[1], 250)
  expect_equal(master$raw_x[1], 0.50)
  expect_equal(master$raw_y[1], 0.50)

  expect_equal(master$subject[1], "User 1")
  expect_equal(master$pID[1], "User 1")
  expect_equal(master$media_id[1], "0")
  expect_equal(master$media_name[1], "Stimulus_A")
  expect_equal(master$trial_global[1], "User 1_MEDIA_0")

  expect_equal(master$left_pupil[1], 3.0)
  expect_equal(master$right_pupil[1], 4.0)
  expect_equal(master$mean_pupil[1], 3.5)
  expect_equal(master$pupil[1], 3.5)
  expect_equal(unique(master$pupil_unit), "diameter_mm")

  expect_equal(sum(master$gaze_offscreen, na.rm = TRUE), 2)
  expect_equal(master$aoi_current[5], "offscreen")
  expect_equal(master$aoi_current[6], "offscreen")
  expect_equal(master$aoi_count[5], 0)
  expect_equal(master$aoi_count[6], 0)
})

test_that("as_gazepoint_master handles missing gaze and missing pupil", {
  data <- tibble::tibble(
    USER_FILE = rep("User 2_all_gaze.csv", 3),
    MEDIA_ID = rep(1, 3),
    MEDIA_NAME = rep("Stimulus_B", 3),
    TIME = c(0, 0.016, 0.033),
    CNT = 0:2,
    BPOGX = c(0.50, 0.50, NA),
    BPOGY = c(0.50, 0.50, NA),
    BPOGV = c(1, 0, 1),
    LPMM = c(3.0, NA, NA),
    RPMM = c(4.0, NA, NA),
    LPMMV = c(1, 0, 0),
    RPMMV = c(1, 0, 0),
    AOI = c("AOI_1", "AOI_1", "AOI_1")
  )

  master <- as_gazepoint_master(
    data,
    screen_width_px = 1000,
    screen_height_px = 500
  )

  expect_false(master$missing_gaze[1])
  expect_true(master$missing_gaze[2])
  expect_true(master$missing_gaze[3])

  expect_false(master$missing_pupil[1])
  expect_true(master$missing_pupil[2])
  expect_true(master$missing_pupil[3])

  expect_equal(master$aoi_current[1], "AOI_1")
  expect_equal(master$aoi_current[2], "missing")
  expect_equal(master$aoi_current[3], "missing")

  expect_equal(master$artifact_reason[2], "missing_gaze_and_pupil")
  expect_true(master$blink[2])
})

test_that("as_gazepoint_master respects pixel coordinate input", {
  data <- tibble::tibble(
    USER_FILE = rep("User 3_all_gaze.csv", 2),
    MEDIA_ID = rep(0, 2),
    MEDIA_NAME = rep("Stimulus_C", 2),
    TIME = c(0, 0.016),
    CNT = 0:1,
    BPOGX = c(500, 600),
    BPOGY = c(250, 300),
    BPOGV = c(1, 1),
    LPMM = c(3.0, 3.2),
    RPMM = c(4.0, 4.2),
    LPMMV = c(1, 1),
    RPMMV = c(1, 1)
  )

  master <- as_gazepoint_master(
    data,
    screen_width_px = 1000,
    screen_height_px = 500,
    coordinate_unit = "pixels"
  )

  expect_equal(unique(master$coordinate_unit_detected), "pixels")
  expect_equal(unique(master$gaze_unit), "pixels")

  expect_equal(master$x[1], 500)
  expect_equal(master$y[1], 250)
  expect_equal(master$raw_x[1], 500)
  expect_equal(master$raw_y[1], 250)
})

test_that("as_gazepoint_master detects event labels from messages", {
  data <- tibble::tibble(
    USER_FILE = rep("User 4_all_gaze.csv", 4),
    MEDIA_ID = rep(0, 4),
    MEDIA_NAME = rep("Stimulus_D", 4),
    TIME = c(0, 0.016, 0.033, 0.050),
    CNT = 0:3,
    BPOGX = c(0.5, 0.5, 0.5, 0.5),
    BPOGY = c(0.5, 0.5, 0.5, 0.5),
    BPOGV = c(1, 1, 1, 1),
    LPMM = c(3, 3, 3, 3),
    RPMM = c(4, 4, 4, 4),
    LPMMV = c(1, 1, 1, 1),
    RPMMV = c(1, 1, 1, 1),
    USER = c(
      "TRIAL_START_001",
      "STIMULUS_ONSET_001",
      "TARGET_ONSET_001",
      "TRIAL_END_001"
    )
  )

  master <- as_gazepoint_master(
    data,
    screen_width_px = 1000,
    screen_height_px = 500
  )

  expect_equal(
    master$event_type,
    c("trial_start", "stimulus_onset", "target_onset", "trial_end")
  )

  expect_equal(master$event_label, data$USER)
})

test_that("as_gazepoint_master applies event latency offset", {
  data <- tibble::tibble(
    USER_FILE = "User 5_all_gaze.csv",
    MEDIA_ID = 0,
    MEDIA_NAME = "Stimulus_E",
    TIME = 1,
    CNT = 0,
    BPOGX = 0.5,
    BPOGY = 0.5,
    BPOGV = 1
  )

  master <- as_gazepoint_master(
    data,
    screen_width_px = 1000,
    screen_height_px = 500,
    event_latency_offset_ms = 20
  )

  expect_equal(master$time_ms, 1020)
  expect_equal(master$time_orig_ms, 1000)
  expect_equal(master$event_latency_offset_ms, 20)
})

test_that("as_gazepoint_master errors for invalid input", {
  expect_error(
    as_gazepoint_master("not a data frame"),
    "`data` must be a data frame"
  )

  data_missing_time <- tibble::tibble(
    USER_FILE = "User 1_all_gaze.csv",
    BPOGX = 0.5,
    BPOGY = 0.5
  )

  expect_error(
    as_gazepoint_master(data_missing_time),
    "Column `TIME` was not found"
  )

  data <- tibble::tibble(
    USER_FILE = "User 1_all_gaze.csv",
    TIME = 0,
    BPOGX = 0.5,
    BPOGY = 0.5
  )

  expect_error(
    as_gazepoint_master(data, screen_width_px = c(1000, 1200)),
    "`screen_width_px` must be `NULL` or a single numeric value"
  )

  expect_error(
    as_gazepoint_master(data, screen_height_px = c(500, 600)),
    "`screen_height_px` must be `NULL` or a single numeric value"
  )

  expect_error(
    as_gazepoint_master(data, event_latency_offset_ms = c(0, 20)),
    "`event_latency_offset_ms` must be a single numeric value"
  )
})
