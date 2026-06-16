make_test_trial_feature_data <- function() {
  tibble::tibble(
    subject = rep("S1", 6),
    trial_global = rep("S1_T1", 6),
    condition = rep("A", 6),
    time = c(0, 500, 1000, 1500, 2000, 2500),
    pupil_smoothed = c(1, 2, 3, 4, 5, NA),
    pupil_was_interpolated = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    pupil_artifact_flag = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
}

test_that("summarise_gazepoint_pupil_trial_features returns trial-level tibble", {
  x <- make_test_trial_feature_data()

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed",
    time_col = "time",
    early_window = c(0, 1000),
    middle_window = c(1000, 2000),
    late_window = c(2000, 3000)
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_true("subject" %in% names(out))
  expect_true("trial_global" %in% names(out))
})

test_that("summarise_gazepoint_pupil_trial_features computes core features", {
  x <- make_test_trial_feature_data()

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed",
    time_col = "time",
    early_window = c(0, 1000),
    middle_window = c(1000, 2000),
    late_window = c(2000, 3000)
  )

  expect_equal(out$n_samples, 6L)
  expect_equal(out$n_valid_pupil, 5L)
  expect_equal(out$n_missing_pupil, 1L)

  expect_equal(out$valid_sample_pct, 100 * 5 / 6)
  expect_equal(out$missing_sample_pct, 100 * 1 / 6)

  expect_equal(out$mean_pupil, 3)
  expect_equal(out$peak_pupil, 5)
  expect_equal(out$peak_time_ms, 2000)
  expect_equal(out$time_to_peak_ms, 2000)

  expect_equal(out$pupil_auc, 6000)
})

test_that("summarise_gazepoint_pupil_trial_features computes window means", {
  x <- make_test_trial_feature_data()

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed",
    time_col = "time",
    early_window = c(0, 1000),
    middle_window = c(1000, 2000),
    late_window = c(2000, 3000)
  )

  expect_equal(out$early_mean_pupil, 1.5)
  expect_equal(out$middle_mean_pupil, 3.5)
  expect_equal(out$late_mean_pupil, 5)

  expect_equal(out$n_valid_early, 2L)
  expect_equal(out$n_valid_middle, 2L)
  expect_equal(out$n_valid_late, 1L)

  expect_equal(out$early_window_start_ms, 0)
  expect_equal(out$early_window_end_ms, 1000)
  expect_equal(out$middle_window_start_ms, 1000)
  expect_equal(out$middle_window_end_ms, 2000)
  expect_equal(out$late_window_start_ms, 2000)
  expect_equal(out$late_window_end_ms, 3000)
})

test_that("summarise_gazepoint_pupil_trial_features computes interpolation and artifact percentages", {
  x <- make_test_trial_feature_data()

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed",
    time_col = "time"
  )

  expect_equal(out$n_interpolated_samples, 1L)
  expect_equal(out$interpolation_pct, 100 * 1 / 6)

  expect_equal(out$n_artifact_samples, 1L)
  expect_equal(out$artifact_pct, 100 * 1 / 6)
})

test_that("summarise_gazepoint_pupil_trial_features handles multiple trials", {
  x <- tibble::tibble(
    subject = c(rep("S1", 4), rep("S2", 4)),
    trial_global = c(rep("S1_T1", 4), rep("S2_T1", 4)),
    time = c(0, 500, 1000, 1500, 0, 500, 1000, 1500),
    pupil_smoothed = c(1, 2, 3, 4, 2, 4, 6, 8),
    pupil_was_interpolated = FALSE,
    pupil_artifact_flag = FALSE
  )

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed"
  )

  expect_equal(nrow(out), 2L)

  s1 <- out[out$subject == "S1", ]
  s2 <- out[out$subject == "S2", ]

  expect_equal(s1$mean_pupil, 2.5)
  expect_equal(s2$mean_pupil, 5)
  expect_equal(s1$peak_pupil, 4)
  expect_equal(s2$peak_pupil, 8)
})

test_that("summarise_gazepoint_pupil_trial_features detects insufficient valid samples", {
  x <- tibble::tibble(
    subject = rep("S1", 4),
    trial_global = rep("S1_T1", 4),
    time = c(0, 500, 1000, 1500),
    pupil_smoothed = c(NA, NA, 3, NA),
    pupil_was_interpolated = FALSE,
    pupil_artifact_flag = FALSE
  )

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed",
    min_valid_samples = 2
  )

  expect_equal(out$n_valid_pupil, 1L)
  expect_equal(out$pupil_feature_status, "insufficient_valid_samples")
})

test_that("summarise_gazepoint_pupil_trial_features auto-detects pupil column", {
  x <- make_test_trial_feature_data()

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global")
  )

  expect_equal(out$pupil_feature_pupil_column, "pupil_smoothed")
  expect_equal(out$pupil_feature_time_column, "time")
})

test_that("summarise_gazepoint_pupil_trial_features works without interpolation or artifact columns", {
  x <- make_test_trial_feature_data() |>
    dplyr::select(
      subject,
      trial_global,
      time,
      pupil_smoothed
    )

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed"
  )

  expect_true(is.na(out$n_interpolated_samples))
  expect_true(is.na(out$interpolation_pct))
  expect_true(is.na(out$n_artifact_samples))
  expect_true(is.na(out$artifact_pct))
})

test_that("summarise_gazepoint_pupil_trial_features supports artifact reason column", {
  x <- make_test_trial_feature_data() |>
    dplyr::select(-pupil_artifact_flag) |>
    dplyr::mutate(
      pupil_artifact_reason = c("valid", "valid", "valid", "blink", "valid", "valid")
    )

  out <- summarise_gazepoint_pupil_trial_features(
    x,
    group_cols = c("subject", "trial_global"),
    pupil_col = "pupil_smoothed",
    artifact_reason_col = "pupil_artifact_reason"
  )

  expect_equal(out$n_artifact_samples, 1L)
  expect_equal(out$artifact_pct, 100 * 1 / 6)
})

test_that("summarise_gazepoint_pupil_trial_features errors when required columns are missing", {
  x <- make_test_trial_feature_data()

  expect_error(
    summarise_gazepoint_pupil_trial_features(
      dplyr::select(x, -time),
      group_cols = c("subject", "trial_global"),
      pupil_col = "pupil_smoothed"
    ),
    "Missing required columns"
  )

  expect_error(
    summarise_gazepoint_pupil_trial_features(
      x,
      group_cols = "missing_subject",
      pupil_col = "pupil_smoothed"
    ),
    "Missing required columns"
  )
})

test_that("summarise_gazepoint_pupil_trial_features errors for invalid inputs", {
  x <- make_test_trial_feature_data()

  expect_error(
    summarise_gazepoint_pupil_trial_features("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_pupil_trial_features(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_pupil_trial_features(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_pupil_trial_features(
      x,
      early_window = c(1000, 0)
    ),
    "`early_window` must be a finite numeric vector of length 2"
  )

  expect_error(
    summarise_gazepoint_pupil_trial_features(
      x,
      min_valid_samples = NA_real_
    ),
    "`min_valid_samples` must be a finite numeric scalar",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_pupil_trial_features works with real pipeline object when available", {
  if (exists("smoothed_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "smoothed_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "subject",
      "trial_global",
      "time",
      "pupil_smoothed"
    )

    if (all(required_cols %in% names(real_data))) {
      out <- summarise_gazepoint_pupil_trial_features(
        real_data,
        group_cols = c("subject", "trial_global"),
        pupil_col = "pupil_smoothed",
        time_col = "time"
      )

      expect_s3_class(out, "tbl_df")
      expect_true("mean_pupil" %in% names(out))
      expect_true("peak_pupil" %in% names(out))
      expect_true("pupil_auc" %in% names(out))
      expect_true("pupil_feature_status" %in% names(out))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
