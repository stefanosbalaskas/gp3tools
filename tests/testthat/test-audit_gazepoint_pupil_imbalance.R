make_test_imbalance_data <- function() {
  tibble::tibble(
    condition = c(
      rep("A", 10),
      rep("B", 10)
    ),
    subject = c(
      rep("S1", 10),
      rep("S2", 10)
    ),
    pupil_interpolated = c(
      rep(3.1, 9), NA,
      rep(3.2, 5), rep(NA, 5)
    ),
    pupil_was_interpolated = c(
      rep(FALSE, 8), TRUE, FALSE,
      rep(FALSE, 10)
    ),
    pupil_interpolation_status = c(
      rep("observed", 8),
      "interpolated",
      "missing_edge_gap",
      rep("observed", 5),
      rep("missing_long_gap", 5)
    ),
    pupil_artifact_flag = c(
      rep(FALSE, 9), TRUE,
      rep(TRUE, 5), rep(FALSE, 5)
    ),
    pupil_artifact_reason = c(
      rep("valid", 9), "blink",
      rep("blink", 5), rep("valid", 5)
    )
  )
}

test_that("audit_gazepoint_pupil_imbalance returns a tibble by condition", {
  x <- make_test_imbalance_data()

  out <- audit_gazepoint_pupil_imbalance(
    x,
    group_cols = "condition"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)

  a <- out[out$condition == "A", ]
  b <- out[out$condition == "B", ]

  expect_equal(a$n_rows, 10L)
  expect_equal(a$n_valid_samples, 9L)
  expect_equal(a$n_interpolated_samples, 1L)
  expect_equal(a$n_artifact_samples, 1L)
  expect_equal(a$n_remaining_missing_samples, 1L)
  expect_equal(a$n_observed_samples, 8L)
  expect_equal(a$n_missing_edge_gap_samples, 1L)
  expect_equal(a$n_missing_long_gap_samples, 0L)

  expect_equal(b$n_rows, 10L)
  expect_equal(b$n_valid_samples, 5L)
  expect_equal(b$n_interpolated_samples, 0L)
  expect_equal(b$n_artifact_samples, 5L)
  expect_equal(b$n_remaining_missing_samples, 5L)
  expect_equal(b$n_observed_samples, 5L)
  expect_equal(b$n_missing_edge_gap_samples, 0L)
  expect_equal(b$n_missing_long_gap_samples, 5L)
})

test_that("audit_gazepoint_pupil_imbalance computes percentages and warnings", {
  x <- make_test_imbalance_data()

  out <- audit_gazepoint_pupil_imbalance(
    x,
    group_cols = "condition",
    max_valid_pct_diff = 10,
    max_artifact_pct_diff = 10,
    max_missing_pct_diff = 10,
    max_interpolated_pct_diff = 10
  )

  a <- out[out$condition == "A", ]
  b <- out[out$condition == "B", ]

  expect_equal(a$valid_sample_pct, 90)
  expect_equal(a$interpolated_sample_pct, 10)
  expect_equal(a$artifact_sample_pct, 10)
  expect_equal(a$remaining_missing_sample_pct, 10)

  expect_equal(b$valid_sample_pct, 50)
  expect_equal(b$interpolated_sample_pct, 0)
  expect_equal(b$artifact_sample_pct, 50)
  expect_equal(b$remaining_missing_sample_pct, 50)

  expect_equal(a$valid_sample_pct_range, 40)
  expect_equal(a$artifact_sample_pct_range, 40)
  expect_equal(a$remaining_missing_sample_pct_range, 40)
  expect_equal(a$interpolated_sample_pct_range, 10)

  expect_true(a$preprocessing_imbalance_warning)
  expect_true(b$preprocessing_imbalance_warning)
  expect_equal(
    a$preprocessing_imbalance_reason,
    "valid_pct_diff;artifact_pct_diff;missing_pct_diff"
  )
})

test_that("audit_gazepoint_pupil_imbalance can return no warning for balanced data", {
  x <- tibble::tibble(
    condition = c(rep("A", 10), rep("B", 10)),
    pupil_interpolated = c(rep(3.1, 8), rep(NA, 2), rep(3.2, 8), rep(NA, 2)),
    pupil_was_interpolated = c(rep(FALSE, 9), TRUE, rep(FALSE, 9), TRUE),
    pupil_interpolation_status = c(
      rep("observed", 8), "missing_edge_gap", "interpolated",
      rep("observed", 8), "missing_edge_gap", "interpolated"
    ),
    pupil_artifact_flag = c(
      rep(FALSE, 9), TRUE,
      rep(FALSE, 9), TRUE
    ),
    pupil_artifact_reason = c(
      rep("valid", 9), "blink",
      rep("valid", 9), "blink"
    )
  )

  out <- audit_gazepoint_pupil_imbalance(x)

  expect_false(any(out$preprocessing_imbalance_warning))
  expect_true(all(out$preprocessing_imbalance_reason == "ok"))
  expect_equal(unique(out$valid_sample_pct_range), 0)
  expect_equal(unique(out$artifact_sample_pct_range), 0)
  expect_equal(unique(out$remaining_missing_sample_pct_range), 0)
})

test_that("audit_gazepoint_pupil_imbalance works without grouping", {
  x <- make_test_imbalance_data()

  out <- audit_gazepoint_pupil_imbalance(
    x,
    group_cols = character(0)
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_false("condition" %in% names(out))

  expect_equal(out$n_rows, 20L)
  expect_equal(out$n_valid_samples, 14L)
  expect_equal(out$n_interpolated_samples, 1L)
  expect_equal(out$n_artifact_samples, 6L)
  expect_equal(out$n_remaining_missing_samples, 6L)
})

test_that("audit_gazepoint_pupil_imbalance supports additional grouping columns", {
  x <- make_test_imbalance_data()

  out <- audit_gazepoint_pupil_imbalance(
    x,
    group_cols = c("condition", "subject")
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)
  expect_true("subject" %in% names(out))
})

test_that("audit_gazepoint_pupil_imbalance detects small group sizes", {
  x <- make_test_imbalance_data()

  out <- audit_gazepoint_pupil_imbalance(
    x,
    group_cols = "condition",
    min_group_n = 11,
    max_valid_pct_diff = 100,
    max_artifact_pct_diff = 100,
    max_missing_pct_diff = 100,
    max_interpolated_pct_diff = 100
  )

  expect_true(any(out$preprocessing_imbalance_warning))
  expect_equal(unique(out$preprocessing_imbalance_reason), "small_group_n")
})

test_that("audit_gazepoint_pupil_imbalance errors when required columns are missing", {
  x <- make_test_imbalance_data()

  expect_error(
    audit_gazepoint_pupil_imbalance(dplyr::select(x, -pupil_interpolated)),
    "Missing required columns"
  )

  expect_error(
    audit_gazepoint_pupil_imbalance(x, group_cols = "missing_condition"),
    "Missing required columns"
  )
})

test_that("audit_gazepoint_pupil_imbalance errors for invalid inputs", {
  x <- make_test_imbalance_data()

  expect_error(
    audit_gazepoint_pupil_imbalance("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_imbalance(x, group_cols = c("condition", "condition")),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_imbalance(x, pupil_col = NA_character_),
    "Column-name arguments must be non-missing character scalars"
  )

  expect_error(
    audit_gazepoint_pupil_imbalance(x, max_valid_pct_diff = NA_real_),
    "Threshold arguments must be finite numeric scalars"
  )
})

test_that("audit_gazepoint_pupil_imbalance works with real pipeline object when available", {
  if (exists("interpolated_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "interpolated_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "condition",
      "pupil_interpolated",
      "pupil_was_interpolated",
      "pupil_interpolation_status"
    )

    if (all(required_cols %in% names(real_data))) {
      out <- audit_gazepoint_pupil_imbalance(
        real_data,
        group_cols = "condition"
      )

      expect_s3_class(out, "tbl_df")
      expect_true("n_valid_samples" %in% names(out))
      expect_true("n_artifact_samples" %in% names(out))
      expect_true("preprocessing_imbalance_warning" %in% names(out))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
