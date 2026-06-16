make_test_pupillometryr_master <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    MEDIA_ID = c("stim1", "stim1", "stim1", "stim1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    pupil_smoothed = c(1010, 1025, NA, 990),
    FPOGV = c(1, 1, 0, 1),
    event_label = c("baseline", "stimulus", "stimulus", "response"),
    baseline = c(TRUE, FALSE, FALSE, FALSE),
    pupil_status = c("ok", "ok", "missing", "ok"),
    condition = c("A", "A", "A", "A")
  )
}

test_that("prepare_gazepoint_pupillometryr_data creates a complete adapter table", {
  toy_master <- make_test_pupillometryr_master()

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil_smoothed",
    media_col = "MEDIA_ID",
    condition_col = "condition",
    event_col = "event_label",
    baseline_col = "baseline",
    validity_cols = "FPOGV",
    pupil_status_col = "pupil_status"
  )

  expect_s3_class(out, "gp3_pupillometryr_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "gp3_adapter"), "pupillometryr")
  expect_equal(attr(out, "gp3_pupil_col"), "pupil_smoothed")
  expect_s3_class(attr(out, "gp3_settings"), "tbl_df")

  expect_equal(nrow(out), 4)

  expect_true(
    all(
      c(
        "participant",
        "trial",
        "time",
        "pupil",
        "media_id",
        "condition",
        "event",
        "baseline",
        "pupil_status_raw",
        "pupil_missing",
        "pupil_invalid_status",
        "trackloss",
        "pupil_valid",
        "pupillometryr_data_status"
      ) %in% names(out)
    )
  )

  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$pupil, c(1010, 1025, NA, 990))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$event, c("baseline", "stimulus", "stimulus", "response"))
  expect_equal(out$baseline, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(out$pupil_status_raw, c("ok", "ok", "missing", "ok"))

  expect_equal(out$pupil_missing, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$pupil_invalid_status, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$trackloss, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(out$pupil_valid, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(
    out$pupillometryr_data_status,
    c("ready", "ready", "trackloss", "ready")
  )

  expect_true("subject" %in% names(out))
  expect_true("MEDIA_ID" %in% names(out))
  expect_true("pupil_smoothed" %in% names(out))
})

test_that("prepare_gazepoint_pupillometryr_data auto-detects common Gazepoint columns", {
  toy_master <- make_test_pupillometryr_master()

  out <- prepare_gazepoint_pupillometryr_data(toy_master)

  expect_s3_class(out, "gp3_pupillometryr_data")
  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$pupil, c(1010, 1025, NA, 990))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$event, c("baseline", "stimulus", "stimulus", "response"))
  expect_equal(out$baseline, c(TRUE, FALSE, FALSE, FALSE))
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
    settings$value[settings$setting == "pupil_col"],
    "pupil_smoothed"
  )
  expect_equal(
    settings$value[settings$setting == "media_col"],
    "MEDIA_ID"
  )
  expect_equal(
    settings$value[settings$setting == "condition_col"],
    "condition"
  )
  expect_equal(
    settings$value[settings$setting == "event_col"],
    "event_label"
  )
  expect_equal(
    settings$value[settings$setting == "baseline_col"],
    "baseline"
  )
})

test_that("prepare_gazepoint_pupillometryr_data can drop original columns", {
  toy_master <- make_test_pupillometryr_master()

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    keep_original_cols = FALSE
  )

  expect_s3_class(out, "gp3_pupillometryr_data")
  expect_false("subject" %in% names(out))
  expect_false("MEDIA_ID" %in% names(out))
  expect_false("pupil_smoothed" %in% names(out))
  expect_true("participant" %in% names(out))
  expect_true("pupil" %in% names(out))
})

test_that("prepare_gazepoint_pupillometryr_data uses media as trial fallback", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    MEDIA_ID = c("stim1", "stim2"),
    time = c(0, 16),
    pupil = c(1000, 1010)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    pupil_col = "pupil",
    media_col = "MEDIA_ID"
  )

  expect_equal(out$trial, c("stim1", "stim2"))
  expect_equal(out$media_id, c("stim1", "stim2"))
})

test_that("prepare_gazepoint_pupillometryr_data uses trial_1 fallback without trial or media", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    time = c(0, 16),
    pupil = c(1000, 1010)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    pupil_col = "pupil"
  )

  expect_equal(out$trial, c("trial_1", "trial_1"))
  expect_true(all(is.na(out$media_id)))
})

test_that("prepare_gazepoint_pupillometryr_data supports explicit trackloss columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    pupil = c(1000, 1010, 1020),
    track_loss = c(FALSE, TRUE, FALSE)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil",
    trackloss_col = "track_loss"
  )

  expect_equal(out$trackloss, c(FALSE, TRUE, FALSE))
  expect_equal(out$pupil_valid, c(TRUE, FALSE, TRUE))
  expect_equal(
    out$pupillometryr_data_status,
    c("ready", "trackloss", "ready")
  )
})

test_that("prepare_gazepoint_pupillometryr_data supports logical and character validity columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    pupil = c(1000, 1010, 1020),
    valid_gaze = c(TRUE, FALSE, TRUE),
    validity_label = c("valid", "invalid", "ok")
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil",
    validity_cols = c("valid_gaze", "validity_label")
  )

  expect_equal(out$trackloss, c(FALSE, TRUE, FALSE))
  expect_equal(out$pupil_valid, c(TRUE, FALSE, TRUE))
  expect_equal(
    out$pupillometryr_data_status,
    c("ready", "trackloss", "ready")
  )
})

test_that("prepare_gazepoint_pupillometryr_data detects invalid pupil status values", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    pupil = c(1000, 1010, 1020, 1030),
    pupil_status = c("ok", "blink", "artifact", "valid")
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil",
    pupil_status_col = "pupil_status"
  )

  expect_equal(out$pupil_invalid_status, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(out$pupil_valid, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(
    out$pupillometryr_data_status,
    c("ready", "invalid_pupil_status", "invalid_pupil_status", "ready")
  )
})

test_that("prepare_gazepoint_pupillometryr_data supports custom invalid pupil statuses", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    pupil = c(1000, 1010, 1020),
    pupil_status = c("usable", "reject", "usable")
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil",
    pupil_status_col = "pupil_status",
    invalid_pupil_status = "reject"
  )

  expect_equal(out$pupil_invalid_status, c(FALSE, TRUE, FALSE))
  expect_equal(out$pupil_valid, c(TRUE, FALSE, TRUE))
})

test_that("prepare_gazepoint_pupillometryr_data detects missing pupil values", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    pupil = c(1000, NA, Inf)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil"
  )

  expect_equal(out$pupil_missing, c(FALSE, TRUE, TRUE))
  expect_equal(out$pupil_valid, c(TRUE, FALSE, FALSE))
  expect_equal(
    out$pupillometryr_data_status,
    c("ready", "missing_pupil", "missing_pupil")
  )
})

test_that("prepare_gazepoint_pupillometryr_data reports row status problems", {
  toy_master <- tibble::tibble(
    subject = c("S1", NA, "S3"),
    trial_global = c(1, 1, NA),
    time = c(0, 16, NA),
    pupil = c(1000, 1010, 1020)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil"
  )

  expect_equal(
    out$pupillometryr_data_status,
    c("ready", "missing_participant", "missing_time")
  )
})

test_that("prepare_gazepoint_pupillometryr_data handles missing optional columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    pupil = c(1000, 1010)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    pupil_col = "pupil"
  )

  expect_true(all(is.na(out$media_id)))
  expect_true(all(is.na(out$condition)))
  expect_true(all(is.na(out$event)))
  expect_true(all(is.na(out$baseline)))
  expect_true(all(is.na(out$pupil_status_raw)))
  expect_equal(out$trackloss, c(FALSE, FALSE))
  expect_equal(out$pupil_valid, c(TRUE, TRUE))
})

test_that("prepare_gazepoint_pupillometryr_data prefers processed pupil columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16),
    pupil = c(900, 910),
    pupil_interpolated = c(950, 960),
    pupil_smoothed = c(1000, 1010)
  )

  out <- prepare_gazepoint_pupillometryr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time"
  )

  expect_equal(out$pupil, c(1000, 1010))
  expect_equal(attr(out, "gp3_pupil_col"), "pupil_smoothed")
})

test_that("prepare_gazepoint_pupillometryr_data checks invalid inputs", {
  toy_master <- make_test_pupillometryr_master()

  expect_error(
    prepare_gazepoint_pupillometryr_data(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(toy_master[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(
      toy_master,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(
      toy_master,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(
      toy_master,
      pupil_col = "bad_pupil"
    ),
    "`pupil_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(
      toy_master,
      validity_cols = "bad_validity"
    ),
    "All `validity_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(
      toy_master,
      invalid_pupil_status = character()
    ),
    "`invalid_pupil_status` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupillometryr_data(
      toy_master,
      keep_original_cols = NA
    ),
    "`keep_original_cols` must be TRUE or FALSE",
    fixed = TRUE
  )
})
