make_test_design_balance_data <- function() {
  tibble::tibble(
    subject = c(
      rep("S1", 6),
      rep("S2", 5),
      rep("S3", 4)
    ),
    MEDIA_ID = c(
      1, 2, 3, 4, 5, 6,
      1, 2, 3, 4, 5,
      1, 2, 3, 4
    ),
    trial_global = c(
      1, 2, 3, 4, 5, 6,
      1, 2, 3, 4, 5,
      1, 2, 3, 4
    ),
    condition = c(
      "A", "A", "A", "B", "B", "B",
      "A", "A", "A", "A", "B",
      "A", "A", "B", "B"
    )
  )
}

test_that("audit_gazepoint_design_balance creates a complete design-balance audit", {
  toy_design <- make_test_design_balance_data()

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    expected_conditions = c("A", "B"),
    min_units_per_condition = 2,
    max_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_s3_class(out, "gp3_design_balance_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "subject_summary",
      "condition_summary",
      "cell_summary",
      "imbalance_summary",
      "flagged_cells",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$subject_summary, "tbl_df")
  expect_s3_class(out$condition_summary, "tbl_df")
  expect_s3_class(out$cell_summary, "tbl_df")
  expect_s3_class(out$imbalance_summary, "tbl_df")
  expect_s3_class(out$flagged_cells, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 15)
  expect_equal(out$overview$n_units, 15)
  expect_equal(out$overview$n_subjects, 3)
  expect_equal(out$overview$n_conditions, 2)
  expect_equal(out$overview$n_flagged_subjects, 1)
  expect_equal(out$overview$n_flagged_cells, 1)
  expect_equal(out$overview$design_balance_status, "review")

  expect_equal(nrow(out$subject_summary), 3)
  expect_equal(nrow(out$condition_summary), 2)
  expect_equal(nrow(out$cell_summary), 6)
  expect_equal(nrow(out$flagged_cells), 1)

  expect_equal(out$flagged_cells$subject, "S2")
  expect_equal(out$flagged_cells$condition, "B")
  expect_equal(out$flagged_cells$n_units, 1)
  expect_equal(out$flagged_cells$design_cell_status, "too_few_units")

  s2 <- out$subject_summary[
    out$subject_summary$subject == "S2",
    ,
    drop = FALSE
  ]

  expect_equal(s2$n_low_count_conditions, 1)
  expect_equal(s2$design_balance_status, "too_few_units")
})

test_that("audit_gazepoint_design_balance reports ok for balanced data", {
  toy_design <- tibble::tibble(
    subject = rep(c("S1", "S2", "S3"), each = 4),
    MEDIA_ID = rep(1:4, 3),
    trial_global = rep(1:4, 3),
    condition = rep(c("A", "A", "B", "B"), 3)
  )

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    expected_conditions = c("A", "B"),
    min_units_per_condition = 2,
    max_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$n_flagged_subjects, 0)
  expect_equal(out$overview$n_flagged_cells, 0)
  expect_equal(out$overview$design_balance_status, "ok")
  expect_equal(nrow(out$flagged_cells), 0)
  expect_true(all(out$subject_summary$design_balance_status == "ok"))
  expect_true(all(out$cell_summary$design_cell_status == "ok"))
})

test_that("audit_gazepoint_design_balance detects missing expected conditions", {
  toy_design <- tibble::tibble(
    subject = c("S1", "S1", "S2", "S2", "S3", "S3"),
    MEDIA_ID = c(1, 2, 1, 2, 1, 2),
    trial_global = c(1, 2, 1, 2, 1, 2),
    condition = c("A", "B", "A", "B", "A", "A")
  )

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    expected_conditions = c("A", "B"),
    min_units_per_condition = 1,
    max_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$design_balance_status, "review")
  expect_true("missing_condition" %in% out$subject_summary$design_balance_status)
  expect_true("missing_condition" %in% out$cell_summary$design_cell_status)

  missing_cell <- out$cell_summary[
    out$cell_summary$subject == "S3" &
      out$cell_summary$condition == "B",
    ,
    drop = FALSE
  ]

  expect_equal(missing_cell$n_units, 0)
  expect_equal(missing_cell$design_cell_status, "missing_condition")
})

test_that("audit_gazepoint_design_balance detects condition-count imbalance", {
  toy_design <- tibble::tibble(
    subject = c(rep("S1", 6), rep("S2", 4)),
    MEDIA_ID = c(1:6, 1:4),
    trial_global = c(1:6, 1:4),
    condition = c(
      "A", "A", "A", "A", "B", "B",
      "A", "A", "B", "B"
    )
  )

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    expected_conditions = c("A", "B"),
    min_units_per_condition = 1,
    max_condition_ratio = 1.5,
    require_all_conditions_per_subject = TRUE
  )

  expect_true(
    "condition_count_imbalance" %in%
      out$subject_summary$design_balance_status
  )

  s1 <- out$subject_summary[
    out$subject_summary$subject == "S1",
    ,
    drop = FALSE
  ]

  expect_equal(s1$condition_count_ratio, 2)
  expect_equal(s1$design_balance_status, "condition_count_imbalance")
})

test_that("audit_gazepoint_design_balance supports observed conditions when expected not supplied", {
  toy_design <- make_test_design_balance_data()

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    min_units_per_condition = 1,
    max_condition_ratio = 10,
    require_all_conditions_per_subject = TRUE
  )

  expect_equal(out$overview$n_conditions, 2)
  expect_equal(sort(unique(out$cell_summary$condition)), c("A", "B"))
})

test_that("audit_gazepoint_design_balance supports MEDIA_ID and USER_FILE aliases", {
  toy_design <- tibble::tibble(
    USER_FILE = rep(c("S1", "S2"), each = 4),
    MEDIA_ID = rep(1:4, 2),
    trial_global = rep(1:4, 2),
    condition = rep(c("A", "A", "B", "B"), 2)
  )

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "USER_FILE",
    condition_col = "condition",
    unit_cols = c("MEDIA_ID", "trial_global"),
    expected_conditions = c("A", "B"),
    min_units_per_condition = 2
  )

  expect_equal(out$overview$design_balance_status, "ok")
  expect_true("subject" %in% names(out$subject_summary))
  expect_true("media_id, trial_global" %in% out$settings$value)
})

test_that("audit_gazepoint_design_balance can count subject-condition rows without unit columns", {
  toy_design <- tibble::tibble(
    subject = c("S1", "S1", "S2", "S2"),
    condition = c("A", "B", "A", "B")
  )

  out <- audit_gazepoint_design_balance(
    toy_design,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = NULL,
    expected_conditions = c("A", "B"),
    min_units_per_condition = 1
  )

  expect_equal(out$overview$n_units, 4)
  expect_equal(out$overview$design_balance_status, "ok")
  expect_equal(
    out$settings$value[out$settings$setting == "unit_cols"],
    ""
  )
})

test_that("audit_gazepoint_design_balance checks invalid inputs", {
  toy_design <- make_test_design_balance_data()

  expect_error(
    audit_gazepoint_design_balance(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design,
      subject_col = "bad_subject"
    ),
    "`subject_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design,
      min_units_per_condition = 0
    ),
    "`min_units_per_condition` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design,
      max_condition_ratio = 0
    ),
    "`max_condition_ratio` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design,
      require_all_conditions_per_subject = NA
    ),
    "`require_all_conditions_per_subject` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_design_balance(
      toy_design,
      expected_conditions = character()
    ),
    "`expected_conditions` must be a non-empty character vector",
    fixed = TRUE
  )
})
