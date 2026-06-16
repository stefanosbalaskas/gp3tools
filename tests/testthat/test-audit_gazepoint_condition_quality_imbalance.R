make_test_condition_quality_data <- function() {
  tibble::tibble(
    subject = c("S1", "S2", "S3", "S4", "S5", "S6"),
    condition = c("A", "A", "A", "B", "B", "B"),
    gaze_valid_prop = c(0.90, 0.88, 0.92, 0.62, 0.65, 0.60),
    missing_gaze_prop = c(0.05, 0.06, 0.04, 0.25, 0.22, 0.28),
    pupil_valid_prop = c(0.85, 0.86, 0.84, 0.80, 0.78, 0.79)
  )
}

test_that("audit_gazepoint_condition_quality_imbalance creates a complete audit", {
  toy_quality <- make_test_condition_quality_data()

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "subject",
    quality_cols = c(
      "gaze_valid_prop",
      "missing_gaze_prop",
      "pupil_valid_prop"
    ),
    min_units_per_condition = 2,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_s3_class(out, "gp3_condition_quality_imbalance_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "condition_summary",
      "metric_summary",
      "flagged_metrics",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_true(is.data.frame(out$condition_summary))
  expect_s3_class(out$metric_summary, "tbl_df")
  expect_s3_class(out$flagged_metrics, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 6)
  expect_equal(out$overview$n_conditions, 2)
  expect_equal(out$overview$n_quality_metrics, 3)
  expect_equal(out$overview$n_flagged_metrics, 2)
  expect_equal(out$overview$n_low_n_conditions, 0)
  expect_equal(out$overview$condition_quality_imbalance_status, "review")

  expect_equal(nrow(out$condition_summary), 2)
  expect_equal(nrow(out$metric_summary), 3)
  expect_equal(nrow(out$flagged_metrics), 2)

  expect_equal(
    out$flagged_metrics$quality_metric,
    c("gaze_valid_prop", "missing_gaze_prop")
  )

  gaze_row <- out$metric_summary[
    out$metric_summary$quality_metric == "gaze_valid_prop",
    ,
    drop = FALSE
  ]

  expect_equal(round(gaze_row$mean_difference, 3), 0.277)
  expect_equal(gaze_row$worst_condition, "B")
  expect_equal(
    gaze_row$condition_quality_imbalance_status,
    "mean_difference_imbalance"
  )

  missing_row <- out$metric_summary[
    out$metric_summary$quality_metric == "missing_gaze_prop",
    ,
    drop = FALSE
  ]

  expect_equal(round(missing_row$condition_ratio, 3), 5)
  expect_equal(missing_row$worst_condition, "B")
  expect_equal(missing_row$metric_direction, "lower_is_better")
})

test_that("audit_gazepoint_condition_quality_imbalance reports ok for balanced quality", {
  toy_quality <- tibble::tibble(
    subject = c("S1", "S2", "S3", "S4", "S5", "S6"),
    condition = c("A", "A", "A", "B", "B", "B"),
    gaze_valid_prop = c(0.90, 0.88, 0.92, 0.89, 0.90, 0.91),
    missing_gaze_prop = c(0.05, 0.06, 0.04, 0.05, 0.04, 0.06),
    pupil_valid_prop = c(0.85, 0.86, 0.84, 0.85, 0.84, 0.86)
  )

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "subject",
    quality_cols = c(
      "gaze_valid_prop",
      "missing_gaze_prop",
      "pupil_valid_prop"
    ),
    min_units_per_condition = 2,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$n_flagged_metrics, 0)
  expect_equal(out$overview$n_low_n_conditions, 0)
  expect_equal(out$overview$condition_quality_imbalance_status, "ok")
  expect_equal(nrow(out$flagged_metrics), 0)
  expect_true(all(out$metric_summary$condition_quality_imbalance_status == "ok"))
  expect_true(all(out$condition_summary$condition_n_status == "ok"))
})

test_that("audit_gazepoint_condition_quality_imbalance detects low condition counts", {
  toy_quality <- tibble::tibble(
    subject = c("S1", "S2", "S3", "S4"),
    condition = c("A", "A", "A", "B"),
    gaze_valid_prop = c(0.90, 0.88, 0.92, 0.89)
  )

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "subject",
    quality_cols = "gaze_valid_prop",
    min_units_per_condition = 2,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$n_low_n_conditions, 1)
  expect_equal(out$overview$condition_quality_imbalance_status, "review")

  b_row <- out$condition_summary[
    out$condition_summary$condition == "B",
    ,
    drop = FALSE
  ]

  expect_equal(b_row$n_units, 1)
  expect_equal(b_row$condition_n_status, "too_few_units")
})

test_that("audit_gazepoint_condition_quality_imbalance detects ratio imbalance", {
  toy_quality <- tibble::tibble(
    subject = c("S1", "S2", "S3", "S4"),
    condition = c("A", "A", "B", "B"),
    excluded_prop = c(0.01, 0.01, 0.10, 0.10)
  )

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "subject",
    quality_cols = "excluded_prop",
    min_units_per_condition = 2,
    max_mean_difference = 0.20,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$n_flagged_metrics, 1)
  expect_equal(out$flagged_metrics$quality_metric, "excluded_prop")
  expect_equal(out$flagged_metrics$condition_ratio, 10)
  expect_equal(
    out$flagged_metrics$condition_quality_imbalance_status,
    "condition_ratio_imbalance"
  )
})

test_that("audit_gazepoint_condition_quality_imbalance detects quality columns automatically", {
  toy_quality <- make_test_condition_quality_data()

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "subject",
    min_units_per_condition = 2,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$n_quality_metrics, 3)
  expect_equal(
    out$settings$value[out$settings$setting == "quality_cols"],
    "gaze_valid_prop, missing_gaze_prop, pupil_valid_prop"
  )
})

test_that("audit_gazepoint_condition_quality_imbalance supports USER_FILE alias", {
  toy_quality <- tibble::tibble(
    USER_FILE = c("S1", "S2", "S3", "S4"),
    condition = c("A", "A", "B", "B"),
    gaze_valid_prop = c(0.90, 0.91, 0.90, 0.91)
  )

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "USER_FILE",
    quality_cols = "gaze_valid_prop",
    min_units_per_condition = 2,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$condition_quality_imbalance_status, "ok")
  expect_equal(
    out$settings$value[out$settings$setting == "subject_col"],
    "subject"
  )
})

test_that("audit_gazepoint_condition_quality_imbalance works without subject column", {
  toy_quality <- make_test_condition_quality_data()

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = NULL,
    quality_cols = "gaze_valid_prop",
    min_units_per_condition = 2,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$n_conditions, 2)
  expect_true(is.na(out$condition_summary$n_subjects[1]))
  expect_true(is.na(out$settings$value[out$settings$setting == "subject_col"]))
})

test_that("audit_gazepoint_condition_quality_imbalance handles insufficient data", {
  toy_quality <- tibble::tibble(
    subject = c("S1", "S2"),
    condition = c("A", "B"),
    gaze_valid_prop = c(NA_real_, NA_real_)
  )

  out <- audit_gazepoint_condition_quality_imbalance(
    toy_quality,
    condition_col = "condition",
    subject_col = "subject",
    quality_cols = "gaze_valid_prop",
    min_units_per_condition = 1,
    max_mean_difference = 0.10,
    max_condition_ratio = 2
  )

  expect_equal(out$overview$n_flagged_metrics, 1)
  expect_equal(
    out$metric_summary$condition_quality_imbalance_status,
    "insufficient_data"
  )
})

test_that("audit_gazepoint_condition_quality_imbalance checks invalid inputs", {
  toy_quality <- make_test_condition_quality_data()

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      subject_col = "bad_subject"
    ),
    "`subject_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      quality_cols = "bad_metric"
    ),
    "All `quality_cols` must be present in `data`",
    fixed = TRUE
  )

  bad_quality <- toy_quality
  bad_quality$bad_metric <- as.character(bad_quality$gaze_valid_prop)

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      bad_quality,
      quality_cols = "bad_metric"
    ),
    "All `quality_cols` must be numeric",
    fixed = TRUE
  )

  no_quality <- tibble::tibble(
    subject = c("S1", "S2"),
    condition = c("A", "B")
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      no_quality
    ),
    "No quality columns were detected. Supply `quality_cols` explicitly",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      quality_cols = character()
    ),
    "`quality_cols` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      min_units_per_condition = 0
    ),
    "`min_units_per_condition` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      max_mean_difference = -0.1
    ),
    "`max_mean_difference` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      max_condition_ratio = 0
    ),
    "`max_condition_ratio` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_condition_quality_imbalance(
      toy_quality,
      lower_is_better = character()
    ),
    "`lower_is_better` must be a non-empty character vector",
    fixed = TRUE
  )
})
