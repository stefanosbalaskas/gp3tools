make_test_baseline_data <- function() {
  tibble::tibble(
    subject = c(
      "S1", "S1", "S1", "S1", "S1", "S1",
      "S2", "S2", "S2", "S2"
    ),
    media_id = c(
      "M1", "M1", "M1", "M1", "M1", "M1",
      "M1", "M1", "M2", "M2"
    ),
    condition = c(
      "A", "A", "A", "A", "A", "A",
      "B", "B", "B", "B"
    ),
    time = c(
      0, 50, 100, 150, 250, 300,
      0, 100, 0, 100
    ),
    pupil_interpolated = c(
      3.0, 3.1, NA, 3.2, 3.3, 3.4,
      NA, 3.5, 3.2, 3.3
    ),
    pupil_was_interpolated = c(
      FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, FALSE, FALSE
    ),
    pupil_baseline_n = c(
      3, 3, 3, 3, 3, 3,
      1, 1, 2, 2
    ),
    pupil_baseline_status = c(
      "corrected", "corrected", "corrected", "corrected", "corrected", "corrected",
      "corrected", "corrected", "corrected", "corrected"
    ),
    pupil_baseline_available = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE
    ),
    pupil_baseline_used = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE
    ),
    pupil_baseline_window_start = c(
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0
    ),
    pupil_baseline_window_end = c(
      200, 200, 200, 200, 200, 200,
      200, 200, 200, 200
    ),
    pupil_artifact_flag = c(
      FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
      TRUE, TRUE, FALSE, FALSE
    ),
    pupil_artifact_reason = c(
      "valid", "valid", "blink", "valid", "valid", "valid",
      "blink", "blink", "valid", "valid"
    )
  )
}

test_that("audit_gazepoint_pupil_baseline returns a tibble and groups by subject/media", {
  x <- make_test_baseline_data()

  out <- audit_gazepoint_pupil_baseline(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3L)

  s1 <- out[out$subject == "S1" & out$media_id == "M1", ]

  expect_equal(s1$n_rows, 6L)
  expect_equal(s1$n_baseline_rows, 4L)
  expect_equal(s1$n_baseline_valid_samples, 3L)
  expect_equal(s1$n_baseline_missing_samples, 1L)
  expect_equal(s1$n_baseline_interpolated_samples, 1L)
  expect_equal(s1$n_baseline_artifact_samples, 1L)

  expect_equal(s1$baseline_missing_pct, 25, tolerance = 1e-8)
  expect_equal(s1$baseline_interpolated_pct, 25, tolerance = 1e-8)
  expect_equal(s1$baseline_artifact_pct, 25, tolerance = 1e-8)

  expect_equal(s1$baseline_n_min, 3)
  expect_equal(s1$baseline_n_mean, 3)
  expect_equal(s1$baseline_n_max, 3)
  expect_equal(s1$baseline_status, "corrected")
  expect_true(s1$baseline_available)
  expect_true(s1$baseline_used)
  expect_false(s1$no_baseline_case)
  expect_false(s1$low_quality_baseline_flag)
  expect_equal(s1$baseline_quality_reason, "ok")
})

test_that("audit_gazepoint_pupil_baseline works with subject-only grouping", {
  x <- make_test_baseline_data()

  out <- audit_gazepoint_pupil_baseline(
    x,
    group_cols = "subject"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)

  s2 <- out[out$subject == "S2", ]

  expect_equal(s2$n_rows, 4L)
  expect_equal(s2$n_baseline_rows, 4L)
  expect_equal(s2$n_baseline_valid_samples, 3L)
  expect_equal(s2$n_baseline_missing_samples, 1L)
  expect_equal(s2$n_baseline_interpolated_samples, 1L)
  expect_equal(s2$n_baseline_artifact_samples, 2L)
})

test_that("audit_gazepoint_pupil_baseline works without grouping", {
  x <- make_test_baseline_data()

  out <- audit_gazepoint_pupil_baseline(
    x,
    group_cols = character(0)
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_false("subject" %in% names(out))
  expect_false("media_id" %in% names(out))

  expect_equal(out$n_rows, 10L)
  expect_equal(out$n_baseline_rows, 8L)
  expect_equal(out$n_baseline_valid_samples, 6L)
  expect_equal(out$n_baseline_missing_samples, 2L)
  expect_equal(out$n_baseline_interpolated_samples, 2L)
  expect_equal(out$n_baseline_artifact_samples, 3L)
})

test_that("audit_gazepoint_pupil_baseline supports additional grouping columns", {
  x <- make_test_baseline_data()

  out <- audit_gazepoint_pupil_baseline(
    x,
    group_cols = c("subject", "media_id", "condition")
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3L)
  expect_true("condition" %in% names(out))
})

test_that("audit_gazepoint_pupil_baseline flags low-quality baselines", {
  x <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    media_id = c("M1", "M1", "M1", "M1"),
    time = c(0, 100, 250, 300),
    pupil_interpolated = c(NA, 3.1, 3.2, 3.3),
    pupil_was_interpolated = c(FALSE, TRUE, FALSE, FALSE),
    pupil_baseline_n = c(1, 1, 1, 1),
    pupil_baseline_status = c("corrected", "corrected", "corrected", "corrected"),
    pupil_baseline_available = c(TRUE, TRUE, TRUE, TRUE),
    pupil_baseline_used = c(TRUE, TRUE, TRUE, TRUE),
    pupil_baseline_window_start = c(0, 0, 0, 0),
    pupil_baseline_window_end = c(200, 200, 200, 200),
    pupil_artifact_flag = c(TRUE, TRUE, FALSE, FALSE),
    pupil_artifact_reason = c("blink", "blink", "valid", "valid")
  )

  out <- audit_gazepoint_pupil_baseline(
    x,
    min_baseline_samples = 2,
    max_missing_pct = 20,
    max_interpolated_pct = 20,
    max_artifact_pct = 20
  )

  expect_true(out$no_baseline_case)
  expect_true(out$low_quality_baseline_flag)
  expect_equal(out$baseline_quality_reason, "no_baseline")
  expect_equal(out$baseline_missing_pct, 50)
  expect_equal(out$baseline_interpolated_pct, 50)
  expect_equal(out$baseline_artifact_pct, 100)
})

test_that("audit_gazepoint_pupil_baseline can use an explicit baseline flag column", {
  x <- make_test_baseline_data() |>
    dplyr::mutate(
      explicit_baseline = dplyr::row_number() %in% c(1, 2, 3)
    )

  out <- audit_gazepoint_pupil_baseline(
    x,
    group_cols = c("subject", "media_id"),
    baseline_flag_col = "explicit_baseline"
  )

  s1 <- out[out$subject == "S1" & out$media_id == "M1", ]

  expect_equal(s1$n_baseline_rows, 3L)
  expect_equal(s1$n_baseline_valid_samples, 2L)
  expect_equal(s1$n_baseline_missing_samples, 1L)
})

test_that("audit_gazepoint_pupil_baseline errors when required columns are missing", {
  x <- make_test_baseline_data()

  expect_error(
    audit_gazepoint_pupil_baseline(dplyr::select(x, -pupil_baseline_n)),
    "Missing required columns"
  )

  expect_error(
    audit_gazepoint_pupil_baseline(x, group_cols = "missing_condition"),
    "Missing required columns"
  )
})

test_that("audit_gazepoint_pupil_baseline errors for invalid inputs", {
  x <- make_test_baseline_data()

  expect_error(
    audit_gazepoint_pupil_baseline("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_baseline(x, group_cols = c("subject", "subject")),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_baseline(x, time_col = NA_character_),
    "Column-name arguments must be non-missing character scalars"
  )

  expect_error(
    audit_gazepoint_pupil_baseline(x, max_missing_pct = NA_real_),
    "Quality-threshold arguments must be finite numeric scalars"
  )
})

test_that("audit_gazepoint_pupil_baseline works with real pipeline object when available", {
  if (exists("baseline_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "baseline_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "time",
      "pupil_interpolated",
      "pupil_was_interpolated",
      "pupil_baseline_n",
      "pupil_baseline_status",
      "pupil_baseline_available",
      "pupil_baseline_used",
      "pupil_baseline_window_start",
      "pupil_baseline_window_end"
    )

    if (all(required_cols %in% names(real_data))) {
      available_group_cols <- intersect(
        c("subject", "media_id"),
        names(real_data)
      )

      out <- audit_gazepoint_pupil_baseline(
        real_data,
        group_cols = available_group_cols
      )

      expect_s3_class(out, "tbl_df")
      expect_true("n_rows" %in% names(out))
      expect_true("n_baseline_rows" %in% names(out))
      expect_true("low_quality_baseline_flag" %in% names(out))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
