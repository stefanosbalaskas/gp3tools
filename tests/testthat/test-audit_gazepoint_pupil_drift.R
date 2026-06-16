make_test_drift_data <- function() {
  tibble::tibble(
    subject = c(
      rep("S1", 5),
      rep("S2", 5)
    ),
    condition = c(
      rep("A", 5),
      rep("B", 5)
    ),
    trial = c(
      1, 1, 1, 1, 1,
      2, 2, 2, 2, 2
    ),
    time = c(
      0, 1000, 2000, 3000, 4000,
      0, 1000, 2000, 3000, 4000
    ),
    pupil_smoothed = c(
      1.0, 1.1, 1.2, 1.3, 1.4,
      2.0, 2.1, 2.2, 2.3, 2.4
    ),
    excluded_trial = c(
      FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    )
  )
}

test_that("audit_gazepoint_pupil_drift returns named audit tables", {
  x <- make_test_drift_data()

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    max_abs_slope_per_min = 1
  )

  expect_type(out, "list")
  expect_named(
    out,
    c(
      "by_group",
      "by_subject",
      "by_condition",
      "condition_balance",
      "summary"
    )
  )

  expect_s3_class(out$by_group, "tbl_df")
  expect_s3_class(out$by_subject, "tbl_df")
  expect_s3_class(out$by_condition, "tbl_df")
  expect_s3_class(out$condition_balance, "tbl_df")
  expect_s3_class(out$summary, "tbl_df")
})

test_that("audit_gazepoint_pupil_drift estimates increasing drift by group", {
  x <- make_test_drift_data()

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    max_abs_slope_per_min = 1
  )

  expect_equal(nrow(out$by_group), 2L)

  s1 <- out$by_group[out$by_group$subject == "S1", ]

  expect_equal(s1$n_rows, 5L)
  expect_equal(s1$n_valid_pupil, 5L)
  expect_equal(s1$valid_pupil_pct, 100)
  expect_equal(s1$pupil_time_slope_per_ms, 0.0001, tolerance = 1e-10)
  expect_equal(s1$pupil_time_slope_per_sec, 0.1, tolerance = 1e-10)
  expect_equal(s1$pupil_time_slope_per_min, 6, tolerance = 1e-10)
  expect_equal(s1$drift_direction, "increasing")
  expect_true(s1$drift_warning)
  expect_equal(s1$drift_status, "possible_drift")
})

test_that("audit_gazepoint_pupil_drift summarises by condition", {
  x <- make_test_drift_data()

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    max_abs_slope_per_min = 1
  )

  expect_equal(nrow(out$by_condition), 2L)
  expect_true(all(c("A", "B") %in% out$by_condition$condition))

  a <- out$by_condition[out$by_condition$condition == "A", ]

  expect_equal(a$n_valid_pupil, 5L)
  expect_equal(a$pupil_time_slope_per_min, 6, tolerance = 1e-10)
  expect_equal(a$drift_direction, "increasing")
  expect_true(a$drift_warning)
})

test_that("audit_gazepoint_pupil_drift reports condition balance", {
  x <- make_test_drift_data()

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    max_abs_slope_per_min = 1,
    max_condition_order_mean_diff = 0.5
  )

  expect_equal(out$condition_balance$n_conditions, 2L)
  expect_equal(out$condition_balance$condition_time_mean_range, 0)
  expect_equal(out$condition_balance$condition_order_mean_range, 1)
  expect_false(out$condition_balance$condition_time_imbalance_warning)
  expect_true(out$condition_balance$condition_order_imbalance_warning)
  expect_true(out$condition_balance$condition_balance_warning)
  expect_equal(out$condition_balance$condition_balance_reason, "order_mean_diff")
})

test_that("audit_gazepoint_pupil_drift handles missing condition labels", {
  x <- make_test_drift_data() |>
    dplyr::mutate(condition = NA_character_)

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition"
  )

  expect_equal(nrow(out$by_condition), 0L)
  expect_equal(out$condition_balance$n_conditions, 0L)
  expect_false(out$condition_balance$condition_balance_warning)
  expect_equal(
    out$condition_balance$condition_balance_reason,
    "no_non_missing_conditions"
  )
  expect_equal(out$summary$n_conditions, 0L)
})

test_that("audit_gazepoint_pupil_drift detects insufficient valid samples", {
  x <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    condition = c("A", "A", "A"),
    trial = c(1, 1, 1),
    time = c(0, 1000, 2000),
    pupil_smoothed = c(1.0, NA, NA),
    excluded_trial = c(FALSE, FALSE, FALSE)
  )

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    min_valid_samples = 3
  )

  expect_equal(out$by_group$n_valid_pupil, 1L)
  expect_equal(out$by_group$drift_direction, "not_estimated")
  expect_false(out$by_group$drift_warning)
  expect_equal(out$by_group$drift_status, "insufficient_valid_samples")
})

test_that("audit_gazepoint_pupil_drift respects excluded rows", {
  x <- make_test_drift_data() |>
    dplyr::mutate(
      excluded_trial = subject == "S2"
    )

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    exclude_col = "excluded_trial",
    include_excluded = FALSE
  )

  expect_equal(nrow(out$by_group), 1L)
  expect_equal(out$by_group$subject, "S1")
})

test_that("audit_gazepoint_pupil_drift can include excluded rows", {
  x <- make_test_drift_data() |>
    dplyr::mutate(
      excluded_trial = subject == "S2"
    )

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    exclude_col = "excluded_trial",
    include_excluded = TRUE
  )

  expect_equal(nrow(out$by_group), 2L)
})

test_that("audit_gazepoint_pupil_drift auto-detects pupil column", {
  x <- make_test_drift_data()

  out <- audit_gazepoint_pupil_drift(
    x,
    group_cols = "subject",
    time_col = "time",
    order_col = "trial",
    condition_col = "condition"
  )

  expect_equal(out$summary$pupil_column, "pupil_smoothed")
})

test_that("audit_gazepoint_pupil_drift errors when required columns are missing", {
  x <- make_test_drift_data()

  expect_error(
    audit_gazepoint_pupil_drift(
      dplyr::select(x, -time),
      pupil_col = "pupil_smoothed",
      time_col = "time"
    ),
    "Missing required columns"
  )

  expect_error(
    audit_gazepoint_pupil_drift(
      x,
      group_cols = "missing_subject",
      pupil_col = "pupil_smoothed"
    ),
    "Missing required columns"
  )
})

test_that("audit_gazepoint_pupil_drift errors for invalid inputs", {
  x <- make_test_drift_data()

  expect_error(
    audit_gazepoint_pupil_drift("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_drift(x, group_cols = c("subject", "subject")),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_drift(x, time_col = NA_character_),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_drift(x, max_abs_slope_per_min = NA_real_),
    "Threshold arguments must be finite numeric scalars"
  )
})

test_that("audit_gazepoint_pupil_drift works with real pipeline object when available", {
  if (exists("smoothed_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "smoothed_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "subject",
      "time",
      "trial",
      "condition",
      "pupil_smoothed"
    )

    if (all(required_cols %in% names(real_data))) {
      out <- audit_gazepoint_pupil_drift(
        real_data,
        group_cols = "subject",
        pupil_col = "pupil_smoothed",
        time_col = "time",
        order_col = "trial",
        condition_col = "condition"
      )

      expect_type(out, "list")
      expect_s3_class(out$by_group, "tbl_df")
      expect_s3_class(out$summary, "tbl_df")
      expect_true("drift_warning" %in% names(out$by_group))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
