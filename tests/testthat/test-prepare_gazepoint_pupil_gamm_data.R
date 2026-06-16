make_test_pupil_gamm_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 10),
    condition = rep(c("A", "B"), each = 10),
    time = rep(seq(0, 450, by = 50), 2),
    pupil_smoothed = c(
      0.10, 0.15, 0.20, 0.22, 0.25,
      0.24, 0.23, 0.20, 0.18, 0.15,
      0.05, 0.08, 0.11, 0.16, 0.20,
      0.22, 0.21, 0.18, 0.14, 0.10
    ),
    gaze_x = rep(seq(0.40, 0.58, length.out = 10), 2),
    gaze_y = rep(seq(0.45, 0.55, length.out = 10), 2)
  )
}

test_that("prepare_gazepoint_pupil_gamm_data bins pupil data correctly", {
  x <- make_test_pupil_gamm_data()

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10L)

  expect_true(all(c(
    "subject",
    "condition",
    "time_bin",
    "time_bin_center_ms",
    "mean_pupil",
    "n_samples",
    "n_valid_samples",
    "mean_x",
    "mean_y",
    "AR.start",
    "gamm_data_status"
  ) %in% names(out)))

  s1 <- dplyr::filter(out, subject == "S1")

  expect_equal(s1$time_bin, 0:4)
  expect_equal(s1$time_bin_center_ms, c(50, 150, 250, 350, 450))
  expect_equal(s1$n_samples, rep(2L, 5))
  expect_equal(s1$n_valid_samples, rep(2L, 5))
  expect_equal(s1$mean_pupil, c(0.125, 0.210, 0.245, 0.215, 0.165))
  expect_equal(s1$AR.start, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(s1$gamm_data_status, rep("ok", 5))
})

test_that("prepare_gazepoint_pupil_gamm_data creates AR starts by subject and condition", {
  x <- make_test_pupil_gamm_data()

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100
  )

  ar_counts <- out |>
    dplyr::group_by(subject, condition) |>
    dplyr::summarise(
      n_ar_start = sum(AR.start),
      .groups = "drop"
    )

  expect_equal(ar_counts$n_ar_start, c(1L, 1L))
})

test_that("prepare_gazepoint_pupil_gamm_data auto-detects processed pupil columns", {
  x <- make_test_pupil_gamm_data() |>
    dplyr::rename(pupil_baseline_corrected = pupil_smoothed)

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100
  )

  expect_equal(unique(out$pupil_col), "pupil_baseline_corrected")
  expect_equal(nrow(out), 10L)
})

test_that("prepare_gazepoint_pupil_gamm_data handles missing condition columns", {
  x <- make_test_pupil_gamm_data() |>
    dplyr::select(-condition)

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100
  )

  expect_true("condition" %in% names(out))
  expect_equal(unique(out$condition), "all_data")
  expect_equal(unique(out$condition_status), "no_condition_column")
  expect_equal(unique(out$gamm_data_status), "no_condition_column")
})

test_that("prepare_gazepoint_pupil_gamm_data handles entirely missing condition values", {
  x <- make_test_pupil_gamm_data() |>
    dplyr::mutate(condition = NA_character_)

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100
  )

  expect_equal(unique(out$condition), "all_data")
  expect_equal(unique(out$condition_status), "condition_missing_all_data")
  expect_equal(unique(out$gamm_data_status), "condition_missing_all_data")
})

test_that("prepare_gazepoint_pupil_gamm_data supports time-window filtering", {
  x <- make_test_pupil_gamm_data()

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100,
    time_window = c(100, 300)
  )

  expect_true(all(out$time_bin_start_ms >= 100))
  expect_true(all(out$time_bin_start_ms <= 300))
  expect_true(nrow(out) > 0)
})

test_that("prepare_gazepoint_pupil_gamm_data filters bins by minimum valid samples", {
  x <- make_test_pupil_gamm_data()
  x$pupil_smoothed[c(1, 2, 11, 12)] <- NA_real_

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100,
    min_valid_samples = 1
  )

  expect_false(any(out$n_valid_samples < 1))

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      bin_width_ms = 100,
      min_valid_samples = 3
    ),
    "No pupil bins remain after applying `min_valid_samples`"
  )
})

test_that("prepare_gazepoint_pupil_gamm_data supports explicit grouping columns", {
  x <- make_test_pupil_gamm_data() |>
    dplyr::mutate(block = "B1")

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    group_cols = c("subject", "condition", "block"),
    bin_width_ms = 100
  )

  expect_true("block" %in% names(out))
  expect_equal(nrow(out), 10L)
})

test_that("prepare_gazepoint_pupil_gamm_data auto-detects subject fallback columns", {
  x <- make_test_pupil_gamm_data() |>
    dplyr::rename(USER_FILE = subject)

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    subject_col = "missing_subject",
    group_cols = c("subject", "condition"),
    bin_width_ms = 100
  )

  expect_true("subject" %in% names(out))
  expect_equal(sort(unique(out$subject)), c("S1", "S2"))
})

test_that("prepare_gazepoint_pupil_gamm_data works without gaze coordinates", {
  x <- make_test_pupil_gamm_data() |>
    dplyr::select(-gaze_x, -gaze_y)

  out <- prepare_gazepoint_pupil_gamm_data(
    x,
    bin_width_ms = 100
  )

  expect_true(all(is.na(out$mean_x)))
  expect_true(all(is.na(out$mean_y)))
})

test_that("prepare_gazepoint_pupil_gamm_data errors for invalid inputs", {
  x <- make_test_pupil_gamm_data()

  expect_error(
    prepare_gazepoint_pupil_gamm_data("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      pupil_col = NA_character_
    ),
    "`pupil_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      subject_col = NA_character_
    ),
    "`subject_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      condition_col = NA_character_
    ),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      bin_width_ms = 0
    ),
    "`bin_width_ms` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      time_window = c(0, NA)
    ),
    "`time_window` must be NULL or a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      min_valid_samples = 0
    ),
    "`min_valid_samples` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_pupil_gamm_data errors when required columns are unavailable", {
  x <- make_test_pupil_gamm_data()

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      dplyr::select(x, -pupil_smoothed)
    ),
    "Could not automatically detect a pupil column"
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      dplyr::select(x, -time)
    ),
    "Could not automatically detect a time column"
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      group_cols = "missing_group"
    ),
    "Missing required grouping columns"
  )

  expect_error(
    prepare_gazepoint_pupil_gamm_data(
      x,
      time_window = c(10000, 20000)
    ),
    "No rows remain after applying time filtering"
  )
})

test_that("prepare_gazepoint_pupil_gamm_data works with real smoothed_pupil object when available", {
  if (exists("smoothed_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("smoothed_pupil", envir = .GlobalEnv, inherits = TRUE)

    if (all(c("subject", "time") %in% names(real_data)) &&
        any(c(
          "pupil_smoothed",
          "pupil_baseline_corrected",
          "pupil_interpolated",
          "pupil_clean",
          "pupil_for_preprocessing"
        ) %in% names(real_data))) {
      out <- prepare_gazepoint_pupil_gamm_data(
        real_data,
        pupil_col = if ("pupil_smoothed" %in% names(real_data)) {
          "pupil_smoothed"
        } else {
          NULL
        },
        bin_width_ms = 50,
        min_valid_samples = 1
      )

      expect_s3_class(out, "tbl_df")
      expect_true(all(c(
        "subject",
        "condition",
        "time_bin_center_ms",
        "mean_pupil",
        "n_samples",
        "n_valid_samples",
        "mean_x",
        "mean_y",
        "AR.start",
        "gamm_data_status"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
