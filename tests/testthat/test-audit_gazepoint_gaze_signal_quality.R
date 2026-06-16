make_test_gaze_signal_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 8),
    MEDIA_ID = rep(1, 16),
    trial_global = rep(rep(1:2, each = 4), 2),
    condition = rep(c("A", "B", "A", "B"), each = 4),
    FPOGX = c(
      0.40, 0.42, 0.43, 0.41,
      0.50, 0.52, NA, 0.51,
      0.30, 0.31, 1.20, 0.32,
      0.60, NA, NA, 0.61
    ),
    FPOGY = c(
      0.50, 0.52, 0.53, 0.51,
      0.40, 0.41, NA, 0.39,
      0.60, 0.61, 0.62, 0.63,
      0.50, NA, NA, 0.52
    ),
    FPOGV = c(
      1, 1, 1, 1,
      1, 1, 0, 1,
      1, 1, 1, 1,
      1, 0, 0, 1
    ),
    pupil = c(
      3.1, 3.2, 3.2, 3.1,
      3.0, 3.1, NA, 3.0,
      3.3, 3.4, 3.5, 3.4,
      3.2, NA, NA, 3.1
    )
  )
}

test_that("audit_gazepoint_gaze_signal_quality creates a complete gaze-signal audit", {
  toy_gaze <- make_test_gaze_signal_data()

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    x_col = "FPOGX",
    y_col = "FPOGY",
    validity_cols = "FPOGV",
    pupil_col = "pupil",
    min_gaze_valid_prop = 0.70,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.25,
    min_pupil_valid_prop = 0.70
  )

  expect_s3_class(out, "gp3_gaze_signal_quality_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "unit_summary",
      "subject_summary",
      "condition_summary",
      "signal_issue_summary",
      "flagged_units",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_true(is.data.frame(out$unit_summary))
  expect_s3_class(out$subject_summary, "tbl_df")
  expect_s3_class(out$condition_summary, "tbl_df")
  expect_s3_class(out$signal_issue_summary, "tbl_df")
  expect_true(is.data.frame(out$flagged_units))
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 16)
  expect_equal(out$overview$n_units, 4)
  expect_equal(out$overview$n_subjects, 2)
  expect_equal(out$overview$n_flagged_units, 1)
  expect_equal(out$overview$x_col, "FPOGX")
  expect_equal(out$overview$y_col, "FPOGY")
  expect_equal(out$overview$validity_cols, "FPOGV")
  expect_equal(out$overview$pupil_col, "pupil")
  expect_equal(out$overview$has_gaze_coordinates, TRUE)
  expect_equal(out$overview$has_validity_cols, TRUE)
  expect_equal(out$overview$has_pupil_col, TRUE)
  expect_equal(out$overview$gaze_signal_quality_status, "review")

  expect_equal(nrow(out$unit_summary), 4)
  expect_equal(sum(out$unit_summary$gaze_signal_status == "ok"), 3)
  expect_equal(sum(out$unit_summary$gaze_signal_status == "low_gaze_validity"), 1)

  expect_equal(nrow(out$flagged_units), 1)
  expect_equal(out$flagged_units$subject, "S2")
  expect_equal(out$flagged_units$condition, "B")
  expect_equal(out$flagged_units$gaze_valid_prop, 0.5)
  expect_equal(out$flagged_units$pupil_valid_prop, 0.5)
  expect_equal(out$flagged_units$gaze_signal_status, "low_gaze_validity")
})

test_that("audit_gazepoint_gaze_signal_quality reports ok for clean data", {
  toy_gaze <- tibble::tibble(
    subject = rep(c("S1", "S2"), each = 4),
    MEDIA_ID = rep(1, 8),
    trial_global = rep(rep(1:2, each = 2), 2),
    condition = rep(c("A", "B", "A", "B"), each = 2),
    FPOGX = rep(c(0.40, 0.45), 4),
    FPOGY = rep(c(0.50, 0.55), 4),
    FPOGV = rep(1, 8),
    pupil = rep(3.2, 8)
  )

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    min_gaze_valid_prop = 0.70,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.30,
    min_pupil_valid_prop = 0.70
  )

  expect_equal(out$overview$n_flagged_units, 0)
  expect_equal(out$overview$gaze_signal_quality_status, "ok")
  expect_equal(nrow(out$flagged_units), 0)
  expect_true(all(out$unit_summary$gaze_signal_status == "ok"))
  expect_true(all(out$subject_summary$subject_signal_status == "ok"))
  expect_true(all(out$condition_summary$condition_signal_status == "ok"))
})

test_that("audit_gazepoint_gaze_signal_quality detects high missing gaze", {
  toy_gaze <- tibble::tibble(
    subject = rep("S1", 4),
    MEDIA_ID = rep(1, 4),
    trial_global = rep(1, 4),
    condition = rep("A", 4),
    FPOGX = c(0.40, NA, NA, 0.42),
    FPOGY = c(0.50, NA, NA, 0.52),
    pupil = c(3.1, 3.2, 3.1, 3.2)
  )

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    x_col = "FPOGX",
    y_col = "FPOGY",
    pupil_col = "pupil",
    min_gaze_valid_prop = 0.50,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.30,
    min_pupil_valid_prop = 0.70
  )

  expect_equal(out$overview$gaze_signal_quality_status, "review")
  expect_equal(out$unit_summary$missing_gaze_prop, 0.5)
  expect_equal(out$unit_summary$gaze_signal_status, "high_missing_gaze")
})

test_that("audit_gazepoint_gaze_signal_quality detects high off-screen gaze", {
  toy_gaze <- tibble::tibble(
    subject = rep("S1", 4),
    MEDIA_ID = rep(1, 4),
    trial_global = rep(1, 4),
    condition = rep("A", 4),
    FPOGX = c(0.40, 1.20, 1.30, 0.42),
    FPOGY = c(0.50, 0.60, 0.70, 0.52),
    pupil = c(3.1, 3.2, 3.1, 3.2)
  )

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    x_col = "FPOGX",
    y_col = "FPOGY",
    pupil_col = "pupil",
    min_gaze_valid_prop = 0.50,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.25,
    min_pupil_valid_prop = 0.70
  )

  expect_equal(out$overview$gaze_signal_quality_status, "review")
  expect_equal(out$unit_summary$offscreen_prop, 0.5)
  expect_equal(out$unit_summary$gaze_signal_status, "high_offscreen_gaze")
})

test_that("audit_gazepoint_gaze_signal_quality detects low pupil validity", {
  toy_gaze <- tibble::tibble(
    subject = rep("S1", 4),
    MEDIA_ID = rep(1, 4),
    trial_global = rep(1, 4),
    condition = rep("A", 4),
    FPOGX = c(0.40, 0.41, 0.42, 0.43),
    FPOGY = c(0.50, 0.51, 0.52, 0.53),
    pupil = c(3.1, NA, NA, 3.2)
  )

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    x_col = "FPOGX",
    y_col = "FPOGY",
    pupil_col = "pupil",
    min_gaze_valid_prop = 0.70,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.30,
    min_pupil_valid_prop = 0.70
  )

  expect_equal(out$overview$gaze_signal_quality_status, "review")
  expect_equal(out$unit_summary$pupil_valid_prop, 0.5)
  expect_equal(out$unit_summary$gaze_signal_status, "low_pupil_validity")
})

test_that("audit_gazepoint_gaze_signal_quality supports validity-only workflows", {
  toy_gaze <- tibble::tibble(
    subject = rep("S1", 4),
    MEDIA_ID = rep(1, 4),
    trial_global = rep(1, 4),
    condition = rep("A", 4),
    FPOGV = c(1, 1, 0, 1)
  )

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    validity_cols = "FPOGV",
    min_gaze_valid_prop = 0.70,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.30
  )

  expect_equal(out$overview$has_gaze_coordinates, FALSE)
  expect_equal(out$overview$has_validity_cols, TRUE)
  expect_equal(out$overview$gaze_signal_quality_status, "ok")
  expect_equal(out$unit_summary$gaze_valid_prop, 0.75)
  expect_equal(out$unit_summary$gaze_signal_status, "ok")
})

test_that("audit_gazepoint_gaze_signal_quality supports aliases and automatic detection", {
  toy_gaze <- tibble::tibble(
    USER_FILE = rep(c("S1", "S2"), each = 2),
    MEDIA_ID = rep(1, 4),
    trial_global = rep(1, 4),
    condition = rep(c("A", "B"), each = 2),
    FPOGX = c(0.4, 0.5, 0.4, 0.5),
    FPOGY = c(0.5, 0.6, 0.5, 0.6),
    FPOGV = c(1, 1, 1, 1),
    pupil = c(3.1, 3.2, 3.3, 3.4)
  )

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "USER_FILE",
    condition_col = "condition",
    group_cols = c("USER_FILE", "MEDIA_ID", "trial_global")
  )

  expect_equal(out$overview$x_col, "FPOGX")
  expect_equal(out$overview$y_col, "FPOGY")
  expect_equal(out$overview$validity_cols, "FPOGV")
  expect_equal(out$overview$pupil_col, "pupil")
  expect_true("subject" %in% names(out$unit_summary))
  expect_true("media_id" %in% names(out$unit_summary))
  expect_equal(out$overview$gaze_signal_quality_status, "ok")
})

test_that("audit_gazepoint_gaze_signal_quality works without condition summaries", {
  toy_gaze <- make_test_gaze_signal_data()

  out <- audit_gazepoint_gaze_signal_quality(
    toy_gaze,
    subject_col = "subject",
    condition_col = NULL,
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    x_col = "FPOGX",
    y_col = "FPOGY",
    validity_cols = "FPOGV",
    pupil_col = "pupil"
  )

  expect_equal(out$overview$n_units, 4)
  expect_equal(nrow(out$condition_summary), 0)
  expect_true(is.na(out$settings$value[out$settings$setting == "condition_col"]))
})

test_that("audit_gazepoint_gaze_signal_quality checks invalid inputs", {
  toy_gaze <- make_test_gaze_signal_data()

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      subject_col = "bad_subject"
    ),
    "`subject_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      x_col = "bad_x"
    ),
    "`x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      y_col = "bad_y"
    ),
    "`y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      validity_cols = "bad_validity"
    ),
    "All `validity_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      validity_cols = character()
    ),
    "`validity_cols` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      screen_x_range = c(1, 0)
    ),
    "`screen_x_range` must be a numeric length-2 vector with lower < upper",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      screen_y_range = c(1, 1)
    ),
    "`screen_y_range` must be a numeric length-2 vector with lower < upper",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      min_gaze_valid_prop = -0.1
    ),
    "`min_gaze_valid_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      max_missing_gaze_prop = 1.1
    ),
    "`max_missing_gaze_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      max_offscreen_prop = NA_real_
    ),
    "`max_offscreen_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_gaze_signal_quality(
      toy_gaze,
      min_pupil_valid_prop = 2
    ),
    "`min_pupil_valid_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )
})
