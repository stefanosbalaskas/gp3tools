make_test_eyetools_master <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    MEDIA_ID = c("stim1", "stim1", "stim1", "stim1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    FPOGX = c(0.20, 0.60, NA, 1.20),
    FPOGY = c(0.20, 0.60, 0.30, 0.80),
    LPOGX = c(0.19, 0.59, NA, 1.18),
    LPOGY = c(0.21, 0.61, 0.31, 0.79),
    RPOGX = c(0.21, 0.61, NA, 1.22),
    RPOGY = c(0.19, 0.59, 0.29, 0.81),
    BPOPD = c(1010, 1025, 1000, 990),
    LPOPD = c(1005, 1020, 995, 985),
    RPOPD = c(1015, 1030, 1005, 995),
    FPOGV = c(1, 1, 0, 1),
    aoi_current = c("logo", "product", "logo", "outside"),
    FPOGID = c(1, 1, 2, 2),
    event_label = c("baseline", "stimulus", "stimulus", "response"),
    condition = c("A", "A", "A", "A")
  )
}

test_that("prepare_gazepoint_eyetools_data creates a complete adapter table", {
  toy_master <- make_test_eyetools_master()

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "FPOGX",
    y_col = "FPOGY",
    left_x_col = "LPOGX",
    left_y_col = "LPOGY",
    right_x_col = "RPOGX",
    right_y_col = "RPOGY",
    pupil_col = "BPOPD",
    left_pupil_col = "LPOPD",
    right_pupil_col = "RPOPD",
    media_col = "MEDIA_ID",
    condition_col = "condition",
    aoi_col = "aoi_current",
    fixation_col = "FPOGID",
    event_col = "event_label",
    validity_cols = "FPOGV",
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1)
  )

  expect_s3_class(out, "gp3_eyetools_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "gp3_adapter"), "eyetools")
  expect_equal(attr(out, "gp3_coordinate_cols"), c("x", "y"))
  expect_equal(
    attr(out, "gp3_binocular_coordinate_cols"),
    c("left_x", "left_y", "right_x", "right_y")
  )
  expect_equal(
    attr(out, "gp3_pupil_cols"),
    c("pupil", "left_pupil", "right_pupil")
  )
  expect_s3_class(attr(out, "gp3_settings"), "tbl_df")

  expect_equal(nrow(out), 4)

  expect_true(
    all(
      c(
        "participant",
        "trial",
        "time",
        "x",
        "y",
        "left_x",
        "left_y",
        "right_x",
        "right_y",
        "pupil",
        "left_pupil",
        "right_pupil",
        "media_id",
        "condition",
        "aoi",
        "aoi_raw",
        "fixation_id",
        "event",
        "missing_gaze",
        "offscreen_gaze",
        "validity_bad",
        "trackloss",
        "valid_gaze",
        "pupil_missing",
        "eyetools_data_status"
      ) %in% names(out)
    )
  )

  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$x, c(0.20, 0.60, NA, 1.20))
  expect_equal(out$y, c(0.20, 0.60, 0.30, 0.80))
  expect_equal(out$left_x, c(0.19, 0.59, NA, 1.18))
  expect_equal(out$left_y, c(0.21, 0.61, 0.31, 0.79))
  expect_equal(out$right_x, c(0.21, 0.61, NA, 1.22))
  expect_equal(out$right_y, c(0.19, 0.59, 0.29, 0.81))
  expect_equal(out$pupil, c(1010, 1025, 1000, 990))
  expect_equal(out$left_pupil, c(1005, 1020, 995, 985))
  expect_equal(out$right_pupil, c(1015, 1030, 1005, 995))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$aoi, c("logo", "product", "logo", "outside"))
  expect_equal(out$aoi_raw, c("logo", "product", "logo", "outside"))
  expect_equal(out$fixation_id, c("1", "1", "2", "2"))
  expect_equal(out$event, c("baseline", "stimulus", "stimulus", "response"))

  expect_equal(out$missing_gaze, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$offscreen_gaze, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out$validity_bad, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$trackloss, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$valid_gaze, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(out$pupil_missing, c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(
    out$eyetools_data_status,
    c("ready", "ready", "trackloss", "offscreen_gaze")
  )

  expect_true("subject" %in% names(out))
  expect_true("MEDIA_ID" %in% names(out))
  expect_true("FPOGX" %in% names(out))
  expect_true("FPOGY" %in% names(out))
})

test_that("prepare_gazepoint_eyetools_data auto-detects common Gazepoint columns", {
  toy_master <- make_test_eyetools_master()

  out <- prepare_gazepoint_eyetools_data(toy_master)

  expect_s3_class(out, "gp3_eyetools_data")
  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$x, c(0.20, 0.60, NA, 1.20))
  expect_equal(out$y, c(0.20, 0.60, 0.30, 0.80))
  expect_equal(out$left_x, c(0.19, 0.59, NA, 1.18))
  expect_equal(out$left_y, c(0.21, 0.61, 0.31, 0.79))
  expect_equal(out$right_x, c(0.21, 0.61, NA, 1.22))
  expect_equal(out$right_y, c(0.19, 0.59, 0.29, 0.81))
  expect_equal(out$pupil, c(1010, 1025, 1000, 990))
  expect_equal(out$left_pupil, c(1005, 1020, 995, 985))
  expect_equal(out$right_pupil, c(1015, 1030, 1005, 995))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$aoi, c("logo", "product", "logo", "outside"))
  expect_equal(out$fixation_id, c("1", "1", "2", "2"))
  expect_equal(out$event, c("baseline", "stimulus", "stimulus", "response"))

  settings <- attr(out, "gp3_settings")

  expect_equal(settings$value[settings$setting == "participant_col"], "subject")
  expect_equal(settings$value[settings$setting == "trial_col"], "trial_global")
  expect_equal(settings$value[settings$setting == "time_col"], "time")
  expect_equal(settings$value[settings$setting == "x_col"], "FPOGX")
  expect_equal(settings$value[settings$setting == "y_col"], "FPOGY")
  expect_equal(settings$value[settings$setting == "left_x_col"], "LPOGX")
  expect_equal(settings$value[settings$setting == "left_y_col"], "LPOGY")
  expect_equal(settings$value[settings$setting == "right_x_col"], "RPOGX")
  expect_equal(settings$value[settings$setting == "right_y_col"], "RPOGY")
  expect_equal(settings$value[settings$setting == "pupil_col"], "BPOPD")
  expect_equal(settings$value[settings$setting == "left_pupil_col"], "LPOPD")
  expect_equal(settings$value[settings$setting == "right_pupil_col"], "RPOPD")
})

test_that("prepare_gazepoint_eyetools_data can drop original columns", {
  toy_master <- make_test_eyetools_master()

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    keep_original_cols = FALSE
  )

  expect_s3_class(out, "gp3_eyetools_data")
  expect_false("subject" %in% names(out))
  expect_false("MEDIA_ID" %in% names(out))
  expect_false("FPOGX" %in% names(out))
  expect_false("FPOGY" %in% names(out))
  expect_true("participant" %in% names(out))
  expect_true("x" %in% names(out))
  expect_true("y" %in% names(out))
})

test_that("prepare_gazepoint_eyetools_data uses media as trial fallback", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    MEDIA_ID = c("stim1", "stim2"),
    time = c(0, 16),
    FPOGX = c(0.2, 0.3),
    FPOGY = c(0.2, 0.3)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    x_col = "FPOGX",
    y_col = "FPOGY",
    media_col = "MEDIA_ID"
  )

  expect_equal(out$trial, c("stim1", "stim2"))
  expect_equal(out$media_id, c("stim1", "stim2"))
})

test_that("prepare_gazepoint_eyetools_data uses trial_1 fallback without trial or media", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    time = c(0, 16),
    FPOGX = c(0.2, 0.3),
    FPOGY = c(0.2, 0.3)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    x_col = "FPOGX",
    y_col = "FPOGY"
  )

  expect_equal(out$trial, c("trial_1", "trial_1"))
  expect_true(all(is.na(out$media_id)))
})

test_that("prepare_gazepoint_eyetools_data supports explicit trackloss columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    track_loss = c(FALSE, TRUE, FALSE)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    trackloss_col = "track_loss"
  )

  expect_equal(out$trackloss, c(FALSE, TRUE, FALSE))
  expect_equal(out$valid_gaze, c(TRUE, FALSE, TRUE))
  expect_equal(out$eyetools_data_status, c("ready", "trackloss", "ready"))
})

test_that("prepare_gazepoint_eyetools_data supports logical and character validity columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    valid_gaze = c(TRUE, FALSE, TRUE),
    validity_label = c("valid", "invalid", "ok")
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    validity_cols = c("valid_gaze", "validity_label")
  )

  expect_equal(out$validity_bad, c(FALSE, TRUE, FALSE))
  expect_equal(out$trackloss, c(FALSE, TRUE, FALSE))
  expect_equal(out$valid_gaze, c(TRUE, FALSE, TRUE))
})

test_that("prepare_gazepoint_eyetools_data detects missing gaze", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, NA, Inf),
    y = c(0.2, 0.3, 0.4)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(out$missing_gaze, c(FALSE, TRUE, TRUE))
  expect_equal(out$trackloss, c(FALSE, TRUE, TRUE))
  expect_equal(out$valid_gaze, c(TRUE, FALSE, FALSE))
  expect_equal(out$eyetools_data_status, c("ready", "trackloss", "trackloss"))
})

test_that("prepare_gazepoint_eyetools_data detects offscreen gaze", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    x = c(0.2, -0.1, 1.2, 0.5),
    y = c(0.2, 0.5, 0.5, 1.2)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1)
  )

  expect_equal(out$missing_gaze, c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(out$offscreen_gaze, c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(out$trackloss, c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(out$valid_gaze, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(
    out$eyetools_data_status,
    c("ready", "offscreen_gaze", "offscreen_gaze", "offscreen_gaze")
  )
})

test_that("prepare_gazepoint_eyetools_data supports custom screen ranges", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    x = c(960, 2500),
    y = c(540, 500)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    screen_x_range = c(0, 1920),
    screen_y_range = c(0, 1080)
  )

  expect_equal(out$offscreen_gaze, c(FALSE, TRUE))
  expect_equal(out$valid_gaze, c(TRUE, FALSE))
})

test_that("prepare_gazepoint_eyetools_data standardises missing AOI values", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    aoi_current = c("logo", NA_character_, "")
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    aoi_col = "aoi_current",
    missing_aoi_label = "missing_aoi"
  )

  expect_equal(out$aoi, c("logo", "missing_aoi", "missing_aoi"))
  expect_equal(out$aoi_raw, c("logo", NA_character_, ""))
})

test_that("prepare_gazepoint_eyetools_data computes primary coordinates from binocular coordinates", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    left_x = c(0.10, 0.20),
    left_y = c(0.30, 0.40),
    right_x = c(0.30, 0.40),
    right_y = c(0.50, 0.60)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    left_x_col = "left_x",
    left_y_col = "left_y",
    right_x_col = "right_x",
    right_y_col = "right_y"
  )

  expect_equal(out$x, c(0.20, 0.30))
  expect_equal(out$y, c(0.40, 0.50))
  expect_equal(out$valid_gaze, c(TRUE, TRUE))
})

test_that("prepare_gazepoint_eyetools_data computes primary pupil from binocular pupil", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    x = c(0.20, 0.30),
    y = c(0.20, 0.30),
    left_pupil = c(1000, 1020),
    right_pupil = c(1010, 1040)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    left_pupil_col = "left_pupil",
    right_pupil_col = "right_pupil"
  )

  expect_equal(out$pupil, c(1005, 1030))
  expect_equal(out$pupil_missing, c(FALSE, FALSE))
})

test_that("prepare_gazepoint_eyetools_data reports row status problems", {
  toy_master <- tibble::tibble(
    subject = c("S1", NA, "S3"),
    trial_global = c(1, 1, NA),
    time = c(0, 16, NA),
    x = c(0.1, 0.2, 0.3),
    y = c(0.1, 0.2, 0.3)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(
    out$eyetools_data_status,
    c("ready", "missing_participant", "missing_time")
  )
})

test_that("prepare_gazepoint_eyetools_data handles missing optional columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    x = c(0.2, 0.3),
    y = c(0.2, 0.3)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_true(all(is.na(out$left_x)))
  expect_true(all(is.na(out$left_y)))
  expect_true(all(is.na(out$right_x)))
  expect_true(all(is.na(out$right_y)))
  expect_true(all(is.na(out$pupil)))
  expect_true(all(is.na(out$left_pupil)))
  expect_true(all(is.na(out$right_pupil)))
  expect_true(all(is.na(out$media_id)))
  expect_true(all(is.na(out$condition)))
  expect_true(all(is.na(out$aoi_raw)))
  expect_equal(out$aoi, c("missing_aoi", "missing_aoi"))
  expect_true(all(is.na(out$fixation_id)))
  expect_true(all(is.na(out$event)))
  expect_equal(out$valid_gaze, c(TRUE, TRUE))
})

test_that("prepare_gazepoint_eyetools_data prefers processed pupil columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    x = c(0.2, 0.3),
    y = c(0.2, 0.3),
    pupil = c(900, 910),
    pupil_interpolated = c(950, 960),
    pupil_smoothed = c(1000, 1010)
  )

  out <- prepare_gazepoint_eyetools_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(out$pupil, c(1000, 1010))
})

test_that("prepare_gazepoint_eyetools_data checks invalid inputs", {
  toy_master <- make_test_eyetools_master()

  expect_error(
    prepare_gazepoint_eyetools_data(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(toy_master[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      x_col = "bad_x"
    ),
    "`x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      y_col = "bad_y"
    ),
    "`y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      validity_cols = "bad_validity"
    ),
    "All `validity_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      screen_x_range = c(0, NA)
    ),
    "`screen_x_range` must be a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      screen_y_range = c(0, 0)
    ),
    "`screen_y_range` must be a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      missing_aoi_label = ""
    ),
    "`missing_aoi_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetools_data(
      toy_master,
      keep_original_cols = NA
    ),
    "`keep_original_cols` must be TRUE or FALSE",
    fixed = TRUE
  )
})
