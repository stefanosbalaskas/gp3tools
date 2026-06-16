make_test_pupil_gap_data <- function() {
  tibble::tibble(
    subject = c(
      "S1", "S1", "S1", "S1", "S1", "S1",
      "S2", "S2", "S2", "S2", "S2", "S2"
    ),
    media_id = c(
      "M1", "M1", "M1", "M1", "M1", "M1",
      "M1", "M1", "M1", "M1", "M2", "M2"
    ),
    condition = c(
      "A", "A", "A", "A", "A", "A",
      "B", "B", "B", "B", "B", "B"
    ),
    pupil_interpolation_status = c(
      "observed",
      "observed",
      "interpolated",
      "interpolated",
      "missing_edge_gap",
      "missing_long_gap",
      "observed",
      "missing_no_time",
      "missing_insufficient_valid_samples",
      "missing_unfilled",
      "observed",
      "observed"
    ),
    pupil_gap_id = c(
      NA, NA, 1, 1, 2, 3,
      NA, 4, 5, 6,
      NA, NA
    ),
    pupil_gap_n_samples = c(
      NA, NA, 2, 2, 1, 1,
      NA, 1, 1, 1,
      NA, NA
    ),
    pupil_gap_duration_ms = c(
      NA, NA, 33.4, 33.4, 16.7, 16.7,
      NA, NA, 16.7, 16.7,
      NA, NA
    ),
    pupil_was_interpolated = c(
      FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE
    ),
    pupil_interpolated = c(
      3.00, 3.10, 3.05, 3.06, NA, NA,
      3.20, NA, NA, NA,
      3.30, 3.40
    )
  )
}

test_that("audit_gazepoint_pupil_gaps returns a tibble and groups by subject/media", {
  x <- make_test_pupil_gap_data()

  out <- audit_gazepoint_pupil_gaps(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3L)

  s1 <- out[out$subject == "S1" & out$media_id == "M1", ]

  expect_equal(s1$n_rows, 6L)
  expect_equal(s1$n_observed_samples, 2L)
  expect_equal(s1$n_interpolated_samples, 2L)
  expect_equal(s1$n_missing_edge_gap_samples, 1L)
  expect_equal(s1$n_missing_long_gap_samples, 1L)
  expect_equal(s1$n_remaining_missing_samples, 2L)
  expect_equal(s1$n_total_missing_or_gap_samples, 4L)

  expect_equal(s1$pct_observed_samples, 100 * 2 / 6, tolerance = 1e-8)
  expect_equal(s1$pct_interpolated_samples, 100 * 2 / 6, tolerance = 1e-8)
  expect_equal(s1$pct_remaining_missing_samples, 100 * 2 / 6, tolerance = 1e-8)

  expect_equal(s1$n_gaps_total, 3L)
  expect_equal(s1$n_gaps_interpolated, 1L)
  expect_equal(s1$n_gaps_edge, 1L)
  expect_equal(s1$n_gaps_long, 1L)
  expect_equal(s1$mean_gap_n_samples, mean(c(2, 1, 1)), tolerance = 1e-8)
  expect_equal(s1$max_gap_n_samples, 2)
  expect_equal(
    s1$mean_gap_duration_ms,
    mean(c(33.4, 16.7, 16.7)),
    tolerance = 1e-8
  )
  expect_equal(s1$max_gap_duration_ms, 33.4)
})

test_that("audit_gazepoint_pupil_gaps works with subject-only grouping", {
  x <- make_test_pupil_gap_data()

  out <- audit_gazepoint_pupil_gaps(
    x,
    group_cols = "subject"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)

  s2 <- out[out$subject == "S2", ]

  expect_equal(s2$n_rows, 6L)
  expect_equal(s2$n_observed_samples, 3L)
  expect_equal(s2$n_interpolated_samples, 0L)
  expect_equal(s2$n_missing_no_time_samples, 1L)
  expect_equal(s2$n_missing_insufficient_valid_samples, 1L)
  expect_equal(s2$n_missing_unfilled_samples, 1L)
  expect_equal(s2$n_remaining_missing_samples, 3L)
  expect_equal(s2$n_gaps_total, 3L)
})

test_that("audit_gazepoint_pupil_gaps works without grouping", {
  x <- make_test_pupil_gap_data()

  out <- audit_gazepoint_pupil_gaps(
    x,
    group_cols = character(0)
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_false("subject" %in% names(out))
  expect_false("media_id" %in% names(out))

  expect_equal(out$n_rows, 12L)
  expect_equal(out$n_observed_samples, 5L)
  expect_equal(out$n_interpolated_samples, 2L)
  expect_equal(out$n_remaining_missing_samples, 5L)
  expect_equal(out$n_gaps_total, 6L)

  expect_equal(out$pct_interpolated_samples, 100 * 2 / 12, tolerance = 1e-8)
  expect_equal(
    out$pct_remaining_missing_samples,
    100 * 5 / 12,
    tolerance = 1e-8
  )
})

test_that("audit_gazepoint_pupil_gaps supports additional grouping columns", {
  x <- make_test_pupil_gap_data()

  out <- audit_gazepoint_pupil_gaps(
    x,
    group_cols = c("subject", "media_id", "condition")
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3L)
  expect_true("condition" %in% names(out))
})

test_that("audit_gazepoint_pupil_gaps counts gaps by unique non-NA gap IDs", {
  x <- make_test_pupil_gap_data()

  out <- audit_gazepoint_pupil_gaps(
    x,
    group_cols = c("subject", "media_id")
  )

  s1 <- out[out$subject == "S1" & out$media_id == "M1", ]

  expect_equal(s1$n_gaps_total, 3L)
  expect_equal(s1$n_gaps_interpolated, 1L)
  expect_equal(s1$n_gaps_edge, 1L)
  expect_equal(s1$n_gaps_long, 1L)
})

test_that("audit_gazepoint_pupil_gaps errors when required columns are missing", {
  x <- make_test_pupil_gap_data()

  expect_error(
    audit_gazepoint_pupil_gaps(dplyr::select(x, -pupil_gap_id)),
    "Missing required columns"
  )

  expect_error(
    audit_gazepoint_pupil_gaps(x, group_cols = "missing_condition"),
    "Missing required columns"
  )
})

test_that("audit_gazepoint_pupil_gaps errors for invalid inputs", {
  x <- make_test_pupil_gap_data()

  expect_error(
    audit_gazepoint_pupil_gaps("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_gaps(x, group_cols = c("subject", "subject")),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_gaps(x, status_col = NA_character_),
    "Column-name arguments must be non-missing character scalars"
  )
})

test_that("audit_gazepoint_pupil_gaps works with real pipeline object when available", {
  if (exists("interpolated_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "interpolated_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "pupil_interpolation_status",
      "pupil_gap_id",
      "pupil_gap_n_samples",
      "pupil_gap_duration_ms",
      "pupil_was_interpolated",
      "pupil_interpolated"
    )

    if (all(required_cols %in% names(real_data))) {
      available_group_cols <- intersect(
        c("subject", "media_id"),
        names(real_data)
      )

      out <- audit_gazepoint_pupil_gaps(
        real_data,
        group_cols = available_group_cols
      )

      expect_s3_class(out, "tbl_df")
      expect_true("n_rows" %in% names(out))
      expect_true("n_gaps_total" %in% names(out))
      expect_true("pct_interpolated_samples" %in% names(out))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
