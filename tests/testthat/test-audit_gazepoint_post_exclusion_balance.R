make_test_post_exclusion_balance_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2", "S3"), each = 4),
    MEDIA_ID = rep(1, 12),
    trial_global = rep(c(1, 2, 3, 4), 3),
    condition = rep(c("A", "A", "B", "B"), 3),
    analysis_status = c(
      "included", "included", "included", "included",
      "included", "included", "excluded", "excluded",
      "included", "included", "included", "included"
    )
  )
}

test_that("audit_gazepoint_post_exclusion_balance creates a complete audit", {
  toy_post <- make_test_post_exclusion_balance_data()

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 2,
    min_retained_units_per_subject_condition = 1,
    max_condition_count_ratio = 2,
    max_subject_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_s3_class(out, "gp3_post_exclusion_balance_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "unit_flow",
      "cell_summary",
      "condition_summary",
      "subject_summary",
      "flagged_cells",
      "flagged_subjects",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_true(is.data.frame(out$unit_flow))
  expect_s3_class(out$cell_summary, "tbl_df")
  expect_s3_class(out$condition_summary, "tbl_df")
  expect_s3_class(out$subject_summary, "tbl_df")
  expect_s3_class(out$flagged_cells, "tbl_df")
  expect_s3_class(out$flagged_subjects, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 12)
  expect_equal(out$overview$n_units, 12)
  expect_equal(out$overview$n_retained_units, 10)
  expect_equal(out$overview$n_excluded_units, 2)
  expect_equal(out$overview$n_subjects, 3)
  expect_equal(out$overview$n_conditions, 2)
  expect_equal(out$overview$n_problem_units, 0)
  expect_equal(out$overview$n_flagged_cells, 1)
  expect_equal(out$overview$n_flagged_subjects, 1)
  expect_equal(out$overview$n_flagged_conditions, 0)
  expect_equal(out$overview$condition_count_ratio, 1.5)
  expect_equal(out$overview$condition_ratio_status, "ok")
  expect_equal(out$overview$post_exclusion_balance_status, "review")

  expect_equal(nrow(out$unit_flow), 12)
  expect_equal(sum(out$unit_flow$retained), 10)
  expect_equal(sum(out$unit_flow$post_exclusion_unit_status == "excluded"), 2)

  expect_equal(nrow(out$cell_summary), 6)
  expect_equal(nrow(out$flagged_cells), 1)
  expect_equal(out$flagged_cells$subject, "S2")
  expect_equal(out$flagged_cells$condition, "B")
  expect_equal(out$flagged_cells$n_total_units, 2)
  expect_equal(out$flagged_cells$n_retained_units, 0)
  expect_equal(
    out$flagged_cells$post_exclusion_cell_status,
    "missing_retained_condition"
  )

  expect_equal(nrow(out$flagged_subjects), 1)
  expect_equal(out$flagged_subjects$subject, "S2")
  expect_equal(
    out$flagged_subjects$post_exclusion_subject_status,
    "missing_retained_condition"
  )
})

test_that("audit_gazepoint_post_exclusion_balance reports ok when retained sample is balanced", {
  toy_post <- make_test_post_exclusion_balance_data()
  toy_post$analysis_status <- "included"

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 2,
    min_retained_units_per_subject_condition = 1,
    max_condition_count_ratio = 2,
    max_subject_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$n_retained_units, 12)
  expect_equal(out$overview$n_excluded_units, 0)
  expect_equal(out$overview$n_flagged_cells, 0)
  expect_equal(out$overview$n_flagged_subjects, 0)
  expect_equal(out$overview$n_flagged_conditions, 0)
  expect_equal(out$overview$condition_count_ratio, 1)
  expect_equal(out$overview$post_exclusion_balance_status, "ok")

  expect_equal(nrow(out$flagged_cells), 0)
  expect_equal(nrow(out$flagged_subjects), 0)
  expect_true(all(out$cell_summary$post_exclusion_cell_status == "ok"))
  expect_true(all(out$subject_summary$post_exclusion_subject_status == "ok"))
  expect_true(all(out$condition_summary$post_exclusion_condition_status == "ok"))
})

test_that("audit_gazepoint_post_exclusion_balance defaults to all retained when no flag column is supplied", {
  toy_post <- make_test_post_exclusion_balance_data()
  toy_post$analysis_status <- NULL

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 2,
    min_retained_units_per_subject_condition = 1
  )

  expect_equal(out$overview$n_retained_units, 12)
  expect_equal(out$overview$n_excluded_units, 0)
  expect_equal(out$overview$post_exclusion_balance_status, "ok")
  expect_true(all(out$unit_flow$retained))
  expect_true(is.na(out$settings$value[out$settings$setting == "status_col"]))
})

test_that("audit_gazepoint_post_exclusion_balance supports logical retained columns", {
  toy_post <- make_test_post_exclusion_balance_data()
  toy_post$retained_trial <- toy_post$analysis_status == "included"

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    retained_col = "retained_trial",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 2,
    min_retained_units_per_subject_condition = 1
  )

  expect_equal(out$overview$n_retained_units, 10)
  expect_equal(out$overview$n_excluded_units, 2)
  expect_equal(out$flagged_cells$subject, "S2")
  expect_equal(out$flagged_cells$condition, "B")
})

test_that("audit_gazepoint_post_exclusion_balance supports numeric exclude columns", {
  toy_post <- make_test_post_exclusion_balance_data()
  toy_post$exclude_trial <- ifelse(
    toy_post$analysis_status == "excluded",
    1,
    0
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    exclude_col = "exclude_trial",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 2,
    min_retained_units_per_subject_condition = 1
  )

  expect_equal(out$overview$n_retained_units, 10)
  expect_equal(out$overview$n_excluded_units, 2)
  expect_true("excluded" %in% out$unit_flow$post_exclusion_unit_status)
})

test_that("audit_gazepoint_post_exclusion_balance detects missing expected retained conditions", {
  toy_post <- tibble::tibble(
    subject = c("S1", "S1", "S2", "S2"),
    MEDIA_ID = c(1, 1, 1, 1),
    trial_global = c(1, 2, 1, 2),
    condition = c("A", "A", "A", "A"),
    analysis_status = c("included", "included", "included", "included")
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 1,
    min_retained_units_per_subject_condition = 1,
    max_condition_count_ratio = 2,
    max_subject_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$post_exclusion_balance_status, "review")
  expect_true("missing_retained_condition" %in% out$cell_summary$post_exclusion_cell_status)
  expect_true("missing_retained_condition" %in% out$subject_summary$post_exclusion_subject_status)
  expect_true(is.infinite(out$overview$condition_count_ratio))
  expect_equal(out$overview$condition_ratio_status, "condition_count_imbalance")

  b_cells <- out$cell_summary[
    out$cell_summary$condition == "B",
    ,
    drop = FALSE
  ]

  expect_true(all(b_cells$n_retained_units == 0))
  expect_true(all(b_cells$post_exclusion_cell_status == "missing_retained_condition"))
})

test_that("audit_gazepoint_post_exclusion_balance detects condition-count imbalance", {
  toy_post <- tibble::tibble(
    subject = c(rep("S1", 4), rep("S2", 4)),
    MEDIA_ID = rep(1, 8),
    trial_global = rep(1:4, 2),
    condition = c(
      "A", "A", "A", "B",
      "A", "A", "A", "B"
    ),
    analysis_status = rep("included", 8)
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 1,
    min_retained_units_per_subject_condition = 1,
    max_condition_count_ratio = 2,
    max_subject_condition_ratio = 10,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$n_flagged_cells, 0)
  expect_equal(out$overview$n_flagged_subjects, 0)
  expect_equal(out$overview$condition_count_ratio, 3)
  expect_equal(out$overview$condition_ratio_status, "condition_count_imbalance")
  expect_equal(out$overview$post_exclusion_balance_status, "review")
})

test_that("audit_gazepoint_post_exclusion_balance detects within-subject retained imbalance", {
  toy_post <- tibble::tibble(
    subject = c(rep("S1", 4), rep("S2", 4)),
    MEDIA_ID = rep(1, 8),
    trial_global = rep(1:4, 2),
    condition = c(
      "A", "A", "A", "B",
      "A", "A", "B", "B"
    ),
    analysis_status = rep("included", 8)
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = c("A", "B"),
    min_retained_units_per_condition = 1,
    min_retained_units_per_subject_condition = 1,
    max_condition_count_ratio = 10,
    max_subject_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$n_flagged_subjects, 1)

  s1 <- out$subject_summary[
    out$subject_summary$subject == "S1",
    ,
    drop = FALSE
  ]

  expect_equal(s1$retained_condition_ratio, 3)
  expect_equal(
    s1$post_exclusion_subject_status,
    "retained_condition_imbalance"
  )
})

test_that("audit_gazepoint_post_exclusion_balance detects conflicting unit flags", {
  toy_post <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    MEDIA_ID = c(1, 1, 1),
    trial_global = c(1, 1, 2),
    condition = c("A", "A", "A"),
    analysis_status = c("included", "excluded", "included")
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = "A",
    min_retained_units_per_condition = 1,
    min_retained_units_per_subject_condition = 1
  )

  expect_true("conflicting_flags" %in% out$unit_flow$post_exclusion_unit_status)
  expect_equal(out$overview$n_problem_units, 1)
  expect_equal(out$overview$post_exclusion_balance_status, "review")
})

test_that("audit_gazepoint_post_exclusion_balance detects unclear status", {
  toy_post <- tibble::tibble(
    subject = c("S1", "S1"),
    MEDIA_ID = c(1, 1),
    trial_global = c(1, 2),
    condition = c("A", "A"),
    analysis_status = c("unknown_status", "included")
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = "A",
    min_retained_units_per_condition = 1,
    min_retained_units_per_subject_condition = 1
  )

  expect_true("unclear_status" %in% out$unit_flow$post_exclusion_unit_status)
  expect_equal(out$overview$n_problem_units, 1)
  expect_equal(out$overview$post_exclusion_balance_status, "review")
})

test_that("audit_gazepoint_post_exclusion_balance supports USER_FILE and MEDIA_ID aliases", {
  toy_post <- tibble::tibble(
    USER_FILE = rep(c("S1", "S2"), each = 4),
    MEDIA_ID = rep(1, 8),
    trial_global = rep(c(1, 2, 3, 4), 2),
    condition = rep(c("A", "A", "B", "B"), 2),
    analysis_status = rep("included", 8)
  )

  out <- audit_gazepoint_post_exclusion_balance(
    toy_post,
    subject_col = "USER_FILE",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    status_col = "analysis_status",
    expected_conditions = c("A", "B")
  )

  expect_equal(out$overview$post_exclusion_balance_status, "ok")
  expect_true("subject" %in% names(out$unit_flow))
  expect_true("media_id" %in% names(out$unit_flow))
  expect_equal(
    out$settings$value[out$settings$setting == "unit_cols"],
    "media_id, trial_global"
  )
})

test_that("audit_gazepoint_post_exclusion_balance checks invalid inputs", {
  toy_post <- make_test_post_exclusion_balance_data()

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      subject_col = "bad_subject"
    ),
    "`subject_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      retained_col = "bad_retained"
    ),
    "`retained_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      include_col = "bad_include"
    ),
    "`include_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      exclude_col = "bad_exclude"
    ),
    "`exclude_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      status_col = "bad_status"
    ),
    "`status_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      expected_conditions = character()
    ),
    "`expected_conditions` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      included_values = character()
    ),
    "`included_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      excluded_values = character()
    ),
    "`excluded_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      min_retained_units_per_condition = 0
    ),
    "`min_retained_units_per_condition` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      min_retained_units_per_subject_condition = 0
    ),
    "`min_retained_units_per_subject_condition` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      max_condition_count_ratio = 0
    ),
    "`max_condition_count_ratio` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      max_subject_condition_ratio = 0
    ),
    "`max_subject_condition_ratio` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      toy_post,
      require_all_conditions_per_subject = NA
    ),
    "`require_all_conditions_per_subject` must be TRUE or FALSE",
    fixed = TRUE
  )

  bad_flag <- toy_post
  bad_flag$retained_flag <- c(
    "yes", "yes", "yes", "yes",
    "yes", "maybe", "no", "no",
    "yes", "yes", "yes", "yes"
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      bad_flag,
      retained_col = "retained_flag"
    ),
    "`retained_col` character values must be interpretable as retained/excluded flags",
    fixed = TRUE
  )

  bad_numeric <- toy_post
  bad_numeric$exclude_flag <- c(
    0, 0, 0, 0,
    0, 2, 1, 1,
    0, 0, 0, 0
  )

  expect_error(
    audit_gazepoint_post_exclusion_balance(
      bad_numeric,
      exclude_col = "exclude_flag"
    ),
    "`exclude_col` numeric values must be 0, 1, or NA",
    fixed = TRUE
  )
})
