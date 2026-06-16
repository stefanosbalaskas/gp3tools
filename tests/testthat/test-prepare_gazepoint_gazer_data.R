make_test_gazer_master <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    MEDIA_ID = c("stim1", "stim1", "stim1", "stim1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    FPOGX = c(0.20, 0.60, NA, 1.20),
    FPOGY = c(0.20, 0.60, 0.30, 0.80),
    BPOPD = c(1010, 1025, 1000, 990),
    FPOGV = c(1, 1, 0, 1),
    aoi_current = c("logo", "product", "logo", "outside"),
    FPOGID = c(1, 1, 2, 2),
    condition = c("A", "A", "A", "A")
  )
}

test_that("prepare_gazepoint_gazer_data creates a complete adapter table", {
  toy_master <- make_test_gazer_master()

  out <- prepare_gazepoint_gazer_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "FPOGX",
    y_col = "FPOGY",
    pupil_col = "BPOPD",
    media_col = "MEDIA_ID",
    condition_col = "condition",
    aoi_col = "aoi_current",
    fixation_col = "FPOGID",
    validity_cols = "FPOGV",
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1)
  )

  expect_s3_class(out, "gp3_gazer_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "gp3_adapter"), "gazer")
  expect_equal(attr(out, "gp3_coordinate_cols"), c("x", "y"))
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
        "pupil",
        "media_id",
        "condition",
        "aoi",
        "aoi_raw",
        "fixation_id",
        "missing_gaze",
        "offscreen_gaze",
        "validity_bad",
        "trackloss",
        "valid_gaze",
        "gazer_data_status"
      ) %in% names(out)
    )
  )

  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$x, c(0.20, 0.60, NA, 1.20))
  expect_equal(out$y, c(0.20, 0.60, 0.30, 0.80))
  expect_equal(out$pupil, c(1010, 1025, 1000, 990))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$aoi, c("logo", "product", "logo", "outside"))
  expect_equal(out$aoi_raw, c("logo", "product", "logo", "outside"))
  expect_equal(out$fixation_id, c("1", "1", "2", "2"))

  expect_equal(out$missing_gaze, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$offscreen_gaze, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out$validity_bad, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$trackloss, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$valid_gaze, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(
    out$gazer_data_status,
    c("ready", "ready", "trackloss", "offscreen_gaze")
  )

  expect_true("subject" %in% names(out))
  expect_true("MEDIA_ID" %in% names(out))
  expect_true("FPOGX" %in% names(out))
  expect_true("FPOGY" %in% names(out))
})

test_that("prepare_gazepoint_gazer_data auto-detects common Gazepoint columns", {
  toy_master <- make_test_gazer_master()

  out <- prepare_gazepoint_gazer_data(toy_master)

  expect_s3_class(out, "gp3_gazer_data")
  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$x, c(0.20, 0.60, NA, 1.20))
  expect_equal(out$y, c(0.20, 0.60, 0.30, 0.80))
  expect_equal(out$pupil, c(1010, 1025, 1000, 990))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$aoi, c("logo", "product", "logo", "outside"))
  expect_equal(out$fixation_id, c("1", "1", "2", "2"))
  expect_equal(out$trackloss, c(FALSE, FALSE, TRUE, FALSE))

  settings <- attr(out, "gp3_settings")

  expect_equal(
    settings$value[settings$setting == "participant_col"],
    "subject"
  )
  expect_equal(
    settings$value[settings$setting == "trial_col"],
    "trial_global"
  )
  expect_equal(
    settings$value[settings$setting == "time_col"],
    "time"
  )
  expect_equal(
    settings$value[settings$setting == "x_col"],
    "FPOGX"
  )
  expect_equal(
    settings$value[settings$setting == "y_col"],
    "FPOGY"
  )
  expect_equal(
    settings$value[settings$setting == "pupil_col"],
    "BPOPD"
  )
  expect_equal(
    settings$value[settings$setting == "media_col"],
    "MEDIA_ID"
  )
  expect_equal(
    settings$value[settings$setting == "aoi_col"],
    "aoi_current"
  )
})

test_that("prepare_gazepoint_gazer_data can drop original columns", {
  toy_master <- make_test_gazer_master()

  out <- prepare_gazepoint_gazer_data(
    toy_master,
    keep_original_cols = FALSE
  )

  expect_s3_class(out, "gp3_gazer_data")
  expect_false("subject" %in% names(out))
  expect_false("MEDIA_ID" %in% names(out))
  expect_false("FPOGX" %in% names(out))
  expect_false("FPOGY" %in% names(out))
  expect_true("participant" %in% names(out))
  expect_true("x" %in% names(out))
  expect_true("y" %in% names(out))
})

test_that("prepare_gazepoint_gazer_data uses media as trial fallback", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    MEDIA_ID = c("stim1", "stim2"),
    time = c(0, 16),
    FPOGX = c(0.2, 0.3),
    FPOGY = c(0.2, 0.3)
  )

  out <- prepare_gazepoint_gazer_data(
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

test_that("prepare_gazepoint_gazer_data uses trial_1 fallback without trial or media", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    time = c(0, 16),
    FPOGX = c(0.2, 0.3),
    FPOGY = c(0.2, 0.3)
  )

  out <- prepare_gazepoint_gazer_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    x_col = "FPOGX",
    y_col = "FPOGY"
  )

  expect_equal(out$trial, c("trial_1", "trial_1"))
  expect_true(all(is.na(out$media_id)))
})

test_that("prepare_gazepoint_gazer_data supports explicit trackloss columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    track_loss = c(FALSE, TRUE, FALSE)
  )

  out <- prepare_gazepoint_gazer_data(
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
  expect_equal(out$gazer_data_status, c("ready", "trackloss", "ready"))
})

test_that("prepare_gazepoint_gazer_data supports logical and character validity columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    valid_gaze = c(TRUE, FALSE, TRUE),
    validity_label = c("valid", "invalid", "ok")
  )

  out <- prepare_gazepoint_gazer_data(
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

test_that("prepare_gazepoint_gazer_data detects missing gaze", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, NA, Inf),
    y = c(0.2, 0.3, 0.4)
  )

  out <- prepare_gazepoint_gazer_data(
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
  expect_equal(out$gazer_data_status, c("ready", "trackloss", "trackloss"))
})

test_that("prepare_gazepoint_gazer_data detects offscreen gaze", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    x = c(0.2, -0.1, 1.2, 0.5),
    y = c(0.2, 0.5, 0.5, 1.2)
  )

  out <- prepare_gazepoint_gazer_data(
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
    out$gazer_data_status,
    c("ready", "offscreen_gaze", "offscreen_gaze", "offscreen_gaze")
  )
})

test_that("prepare_gazepoint_gazer_data supports custom screen ranges", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    x = c(960, 2500),
    y = c(540, 500)
  )

  out <- prepare_gazepoint_gazer_data(
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

test_that("prepare_gazepoint_gazer_data standardises missing AOI values", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    aoi_current = c("logo", NA_character_, "")
  )

  out <- prepare_gazepoint_gazer_data(
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

test_that("prepare_gazepoint_gazer_data reports row status problems", {
  toy_master <- tibble::tibble(
    subject = c("S1", NA, "S3"),
    trial_global = c(1, 1, NA),
    time = c(0, 16, NA),
    x = c(0.1, 0.2, 0.3),
    y = c(0.1, 0.2, 0.3)
  )

  out <- prepare_gazepoint_gazer_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(
    out$gazer_data_status,
    c("ready", "missing_participant", "missing_time")
  )
})

test_that("prepare_gazepoint_gazer_data handles missing optional columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    x = c(0.2, 0.3),
    y = c(0.2, 0.3)
  )

  out <- prepare_gazepoint_gazer_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_true(all(is.na(out$pupil)))
  expect_true(all(is.na(out$media_id)))
  expect_true(all(is.na(out$condition)))
  expect_true(all(is.na(out$aoi_raw)))
  expect_equal(out$aoi, c("missing_aoi", "missing_aoi"))
  expect_true(all(is.na(out$fixation_id)))
  expect_equal(out$valid_gaze, c(TRUE, TRUE))
})

test_that("prepare_gazepoint_gazer_data prefers processed pupil columns", {
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

  out <- prepare_gazepoint_gazer_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(out$pupil, c(1000, 1010))
})

test_that("prepare_gazepoint_gazer_data checks invalid inputs", {
  toy_master <- make_test_gazer_master()

  expect_error(
    prepare_gazepoint_gazer_data(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(toy_master[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      x_col = "bad_x"
    ),
    "`x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      y_col = "bad_y"
    ),
    "`y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      validity_cols = "bad_validity"
    ),
    "All `validity_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      screen_x_range = c(0, NA)
    ),
    "`screen_x_range` must be a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      screen_y_range = c(0, 0)
    ),
    "`screen_y_range` must be a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      missing_aoi_label = ""
    ),
    "`missing_aoi_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gazer_data(
      toy_master,
      keep_original_cols = NA
    ),
    "`keep_original_cols` must be TRUE or FALSE",
    fixed = TRUE
  )
})
