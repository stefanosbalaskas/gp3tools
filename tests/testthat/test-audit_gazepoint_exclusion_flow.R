make_test_exclusion_flow_data <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S2", "S2", "S2"),
    MEDIA_ID = c(1, 2, 3, 1, 2, 3),
    trial_global = c(1, 2, 3, 1, 2, 3),
    condition = c("A", "A", "B", "A", "B", "B"),
    analysis_status = c(
      "included",
      "included",
      "included",
      "included",
      "excluded",
      "included"
    ),
    exclusion_reason = c(
      NA,
      NA,
      NA,
      NA,
      "low_tracking_quality",
      NA
    )
  )
}

test_that("audit_gazepoint_exclusion_flow creates a complete exclusion-flow audit", {
  toy_exclusion <- make_test_exclusion_flow_data()

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_s3_class(out, "gp3_exclusion_flow_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "unit_flow",
      "reason_summary",
      "condition_summary",
      "subject_summary",
      "flagged_units",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_true(is.data.frame(out$unit_flow))
  expect_s3_class(out$reason_summary, "tbl_df")
  expect_s3_class(out$condition_summary, "tbl_df")
  expect_s3_class(out$subject_summary, "tbl_df")
  expect_true(is.data.frame(out$flagged_units))
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 6)
  expect_equal(out$overview$n_units, 6)
  expect_equal(out$overview$n_subjects, 2)
  expect_equal(out$overview$n_retained_units, 5)
  expect_equal(out$overview$n_excluded_units, 1)
  expect_equal(out$overview$n_flagged_units, 1)
  expect_equal(out$overview$n_exclusion_reasons, 1)
  expect_true(is.infinite(out$overview$condition_exclusion_ratio))
  expect_equal(out$overview$exclusion_flow_status, "review")

  expect_equal(nrow(out$unit_flow), 6)
  expect_equal(sum(out$unit_flow$retained), 5)
  expect_equal(sum(out$unit_flow$exclusion_flow_status == "excluded"), 1)

  expect_equal(nrow(out$flagged_units), 1)
  expect_equal(out$flagged_units$subject, "S2")
  expect_equal(out$flagged_units$condition, "B")
  expect_equal(out$flagged_units$media_id, 2)
  expect_equal(out$flagged_units$trial_global, 2)
  expect_equal(out$flagged_units$exclusion_reason, "low_tracking_quality")
  expect_equal(out$flagged_units$exclusion_flow_status, "excluded")

  expect_equal(nrow(out$reason_summary), 1)
  expect_equal(out$reason_summary$exclusion_reason, "low_tracking_quality")
  expect_equal(out$reason_summary$n_units, 1)
  expect_equal(out$reason_summary$reason_prop, 1)
})

test_that("audit_gazepoint_exclusion_flow reports ok when all units are retained", {
  toy_exclusion <- make_test_exclusion_flow_data()
  toy_exclusion$analysis_status <- "included"
  toy_exclusion$exclusion_reason <- NA_character_

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_equal(out$overview$n_retained_units, 6)
  expect_equal(out$overview$n_excluded_units, 0)
  expect_equal(out$overview$n_flagged_units, 0)
  expect_equal(out$overview$condition_exclusion_ratio, 1)
  expect_equal(out$overview$exclusion_flow_status, "ok")

  expect_equal(nrow(out$flagged_units), 0)
  expect_equal(nrow(out$reason_summary), 0)
  expect_true(all(out$unit_flow$exclusion_flow_status == "retained"))
  expect_true(all(out$condition_summary$condition_exclusion_status == "ok"))
  expect_true(all(out$subject_summary$subject_exclusion_status == "ok"))
})

test_that("audit_gazepoint_exclusion_flow supports logical include columns", {
  toy_exclusion <- make_test_exclusion_flow_data()
  toy_exclusion$include_trial <- toy_exclusion$analysis_status == "included"

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    include_col = "include_trial",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_equal(out$overview$n_retained_units, 5)
  expect_equal(out$overview$n_excluded_units, 1)
  expect_equal(out$flagged_units$exclusion_reason, "low_tracking_quality")
})

test_that("audit_gazepoint_exclusion_flow supports numeric exclude columns", {
  toy_exclusion <- make_test_exclusion_flow_data()
  toy_exclusion$exclude_trial <- ifelse(
    toy_exclusion$analysis_status == "excluded",
    1,
    0
  )

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    exclude_col = "exclude_trial",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_equal(out$overview$n_retained_units, 5)
  expect_equal(out$overview$n_excluded_units, 1)
  expect_equal(out$flagged_units$exclusion_flow_status, "excluded")
})

test_that("audit_gazepoint_exclusion_flow detects conflicting unit flags", {
  toy_exclusion <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    MEDIA_ID = c(1, 1, 2),
    trial_global = c(1, 1, 2),
    condition = c("A", "A", "A"),
    analysis_status = c("included", "excluded", "included"),
    exclusion_reason = c(NA, "manual_exclusion", NA)
  )

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.50,
    max_condition_exclusion_ratio = 2
  )

  expect_true("conflicting_flags" %in% out$unit_flow$exclusion_flow_status)
  expect_true("conflicting_flags" %in% out$flagged_units$exclusion_flow_status)
  expect_equal(out$overview$exclusion_flow_status, "review")
})

test_that("audit_gazepoint_exclusion_flow detects unclear status", {
  toy_exclusion <- make_test_exclusion_flow_data()
  toy_exclusion$analysis_status[1] <- "unknown_status"

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_true("unclear_status" %in% out$unit_flow$exclusion_flow_status)
  expect_true("unclear_status" %in% out$flagged_units$exclusion_flow_status)
  expect_equal(out$overview$exclusion_flow_status, "review")
})

test_that("audit_gazepoint_exclusion_flow supports MEDIA_ID and USER_FILE aliases", {
  toy_exclusion <- tibble::tibble(
    USER_FILE = c("S1", "S1", "S2", "S2"),
    MEDIA_ID = c(1, 2, 1, 2),
    trial_global = c(1, 2, 1, 2),
    condition = c("A", "B", "A", "B"),
    analysis_status = c("included", "included", "included", "included")
  )

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "USER_FILE",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status"
  )

  expect_equal(out$overview$exclusion_flow_status, "ok")
  expect_true("subject" %in% names(out$unit_flow))
  expect_true("media_id" %in% names(out$unit_flow))

  expect_equal(
    out$settings$value[out$settings$setting == "unit_cols"],
    "media_id, trial_global"
  )
})

test_that("audit_gazepoint_exclusion_flow works without condition summaries", {
  toy_exclusion <- make_test_exclusion_flow_data()

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = NULL,
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    reason_col = "exclusion_reason",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_equal(out$overview$n_units, 6)
  expect_equal(out$overview$n_retained_units, 5)
  expect_equal(out$overview$n_excluded_units, 1)
  expect_true(is.na(out$overview$condition_exclusion_ratio))
  expect_equal(nrow(out$condition_summary), 0)
})

test_that("audit_gazepoint_exclusion_flow can use subject-condition units", {
  toy_exclusion <- tibble::tibble(
    subject = c("S1", "S1", "S2", "S2"),
    condition = c("A", "B", "A", "B"),
    analysis_status = c("included", "excluded", "included", "included")
  )

  out <- audit_gazepoint_exclusion_flow(
    toy_exclusion,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = NULL,
    status_col = "analysis_status",
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
  )

  expect_equal(out$overview$n_units, 4)
  expect_equal(out$overview$n_retained_units, 3)
  expect_equal(out$overview$n_excluded_units, 1)
  expect_equal(out$overview$exclusion_flow_status, "review")
})

test_that("audit_gazepoint_exclusion_flow checks invalid inputs", {
  toy_exclusion <- make_test_exclusion_flow_data()

  expect_error(
    audit_gazepoint_exclusion_flow(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      subject_col = "bad_subject",
      status_col = "analysis_status"
    ),
    "`subject_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      condition_col = "bad_condition",
      status_col = "analysis_status"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      status_col = "bad_status"
    ),
    "`status_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion
    ),
    "One of `include_col`, `exclude_col`, or `status_col` must be supplied",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      status_col = "analysis_status",
      included_values = character()
    ),
    "`included_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      status_col = "analysis_status",
      excluded_values = character()
    ),
    "`excluded_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      status_col = "analysis_status",
      min_retained_prop = 0
    ),
    "`min_retained_prop` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      status_col = "analysis_status",
      min_retained_prop = 1.5
    ),
    "`min_retained_prop` must be between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_exclusion,
      status_col = "analysis_status",
      max_condition_exclusion_ratio = 0
    ),
    "`max_condition_exclusion_ratio` must be a positive numeric scalar",
    fixed = TRUE
  )

  toy_bad_flag <- toy_exclusion
  toy_bad_flag$include_trial <- c("yes", "bad", "yes", "yes", "no", "yes")

  expect_error(
    audit_gazepoint_exclusion_flow(
      toy_bad_flag,
      include_col = "include_trial"
    ),
    "`include_col` character values must be interpretable as inclusion/exclusion flags",
    fixed = TRUE
  )
})
