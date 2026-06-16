make_test_pupil_reliability_data <- function() {
  set.seed(123)

  tidyr::expand_grid(
    subject = paste0("S", 1:12),
    trial = 1:12
  ) |>
    dplyr::mutate(
      trial_global = paste(subject, trial, sep = "_T"),
      condition = rep(c("A", "B"), length.out = dplyr::n()),
      window = rep(c("early", "late"), each = 6, length.out = dplyr::n()),
      subject_score = rep(seq(0.05, 0.60, length.out = 12), each = 12),
      auc_pupil_0_2000 = subject_score + rnorm(dplyr::n(), 0, 0.03),
      mean_pupil_0_2000 = subject_score / 2 + rnorm(dplyr::n(), 0, 0.02),
      peak_pupil_0_2000 = subject_score + 0.20 + rnorm(dplyr::n(), 0, 0.04)
    )
}

test_that("audit_gazepoint_pupil_reliability creates a complete split-half audit", {
  toy_data <- make_test_pupil_reliability_data()

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = c(
      "auc_pupil_0_2000",
      "mean_pupil_0_2000",
      "peak_pupil_0_2000"
    ),
    participant_col = "subject",
    trial_col = "trial",
    split_method = "odd_even",
    aggregate_function = "mean",
    correlation_method = "pearson",
    min_trials_per_split = 2,
    name = "toy_pupil_reliability"
  )

  expect_s3_class(out, "gp3_pupil_reliability_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "split_data",
      "split_summary",
      "reliability_pairs",
      "reliability_summary",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$split_data, "tbl_df")
  expect_s3_class(out$split_summary, "tbl_df")
  expect_s3_class(out$reliability_pairs, "tbl_df")
  expect_s3_class(out$reliability_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_pupil_reliability")
  expect_equal(out$overview$n_input_rows, nrow(toy_data))
  expect_equal(out$overview$n_rows_used, nrow(toy_data))
  expect_equal(out$overview$n_participants, 12)
  expect_equal(out$overview$n_outcomes, 3)
  expect_equal(out$overview$n_by_groups, 1)
  expect_equal(out$overview$n_reliability_rows, 3)
  expect_equal(out$overview$n_ready_reliability_rows, 3)
  expect_equal(out$overview$split_method, "odd_even")
  expect_equal(out$overview$aggregate_function, "mean")
  expect_equal(out$overview$correlation_method, "pearson")
})

test_that("audit_gazepoint_pupil_reliability creates expected split summaries and pairs", {
  toy_data <- make_test_pupil_reliability_data()

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = c("auc_pupil_0_2000", "mean_pupil_0_2000"),
    participant_col = "subject",
    trial_col = "trial",
    split_method = "odd_even",
    min_trials_per_split = 2
  )

  expect_equal(nrow(out$split_summary), 12 * 2 * 2)
  expect_equal(sort(unique(as.character(out$split_summary$split))), c("even", "odd"))

  expect_true(all(out$split_summary$n_trials == 6))
  expect_true(all(out$split_summary$n_valid == 6))

  expect_equal(nrow(out$reliability_pairs), 12 * 2)
  expect_true(all(out$reliability_pairs$split1_label == "odd"))
  expect_true(all(out$reliability_pairs$split2_label == "even"))
  expect_true(all(out$reliability_pairs$complete_pair))

  expect_equal(nrow(out$reliability_summary), 2)
  expect_true(all(out$reliability_summary$n_complete_pairs == 12))
  expect_true(all(out$reliability_summary$reliability_status == "ready"))
  expect_true(all(!is.na(out$reliability_summary$split_half_correlation)))
  expect_true(all(!is.na(out$reliability_summary$spearman_brown_reliability)))
})

test_that("audit_gazepoint_pupil_reliability auto-detects common columns", {
  toy_data <- make_test_pupil_reliability_data()

  out <- audit_gazepoint_pupil_reliability(toy_data)

  expect_s3_class(out, "gp3_pupil_reliability_audit")
  expect_equal(out$overview$n_participants, 12)
  expect_true(out$overview$n_outcomes >= 3)

  expect_true(grepl(
    "auc_pupil_0_2000",
    out$settings$value[out$settings$setting == "outcome_cols"],
    fixed = TRUE
  ))
  expect_equal(out$settings$value[out$settings$setting == "participant_col"], "subject")
  expect_equal(out$settings$value[out$settings$setting == "trial_col"], "trial_global")
})

test_that("audit_gazepoint_pupil_reliability supports by_cols", {
  toy_data <- make_test_pupil_reliability_data()

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    by_cols = "condition",
    split_method = "odd_even",
    min_trials_per_split = 2
  )

  expect_true("condition" %in% names(out$split_summary))
  expect_true("condition" %in% names(out$reliability_pairs))
  expect_true("condition" %in% names(out$reliability_summary))

  expect_equal(out$overview$n_by_groups, 2)
  expect_equal(nrow(out$reliability_summary), 2)
  expect_equal(sort(unique(out$reliability_summary$condition)), c("A", "B"))
})

test_that("audit_gazepoint_pupil_reliability supports first-second splits", {
  toy_data <- make_test_pupil_reliability_data()

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    split_method = "first_second",
    min_trials_per_split = 2
  )

  expect_equal(sort(unique(as.character(out$split_summary$split))), c("first", "second"))
  expect_true(all(out$reliability_pairs$split1_label == "first"))
  expect_true(all(out$reliability_pairs$split2_label == "second"))
  expect_equal(out$overview$split_method, "first_second")
})

test_that("audit_gazepoint_pupil_reliability supports median aggregation and spearman correlation", {
  toy_data <- make_test_pupil_reliability_data()

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    aggregate_function = "median",
    correlation_method = "spearman",
    min_trials_per_split = 2
  )

  expect_equal(out$overview$aggregate_function, "median")
  expect_equal(out$overview$correlation_method, "spearman")
  expect_equal(out$settings$value[out$settings$setting == "aggregate_function"], "median")
  expect_equal(out$settings$value[out$settings$setting == "correlation_method"], "spearman")
  expect_true(out$reliability_summary$reliability_status %in% c("ready", "correlation_unavailable"))
})

test_that("audit_gazepoint_pupil_reliability supports predefined split columns", {
  toy_data <- make_test_pupil_reliability_data() |>
    dplyr::mutate(split_half = ifelse(trial <= 6, "first", "second"))

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    split_col = "split_half",
    min_trials_per_split = 2
  )

  expect_equal(out$overview$split_method, "predefined_split_col")
  expect_equal(sort(unique(as.character(out$split_summary$split))), c("first", "second"))
  expect_true(all(out$reliability_pairs$split1_label == "first"))
  expect_true(all(out$reliability_pairs$split2_label == "second"))
})

test_that("audit_gazepoint_pupil_reliability handles missing outcome values", {
  toy_data <- make_test_pupil_reliability_data()
  toy_data$auc_pupil_0_2000[toy_data$subject == "S1" & toy_data$trial %in% c(1, 3, 5, 7)] <- NA_real_

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    split_method = "odd_even",
    min_trials_per_split = 4
  )

  s1_pair <- out$reliability_pairs[out$reliability_pairs$participant == "S1", ]

  expect_equal(s1_pair$split1_n_valid, 2)
  expect_equal(s1_pair$split2_n_valid, 6)
  expect_false(s1_pair$complete_pair)

  expect_equal(out$reliability_summary$n_complete_pairs, 11)
  expect_equal(out$reliability_summary$reliability_status, "ready")
})

test_that("audit_gazepoint_pupil_reliability flags too few complete pairs", {
  toy_data <- make_test_pupil_reliability_data() |>
    dplyr::filter(subject %in% c("S1", "S2"))

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    split_method = "odd_even",
    min_trials_per_split = 2
  )

  expect_equal(out$reliability_summary$n_complete_pairs, 2)
  expect_equal(out$reliability_summary$reliability_status, "too_few_complete_pairs")
  expect_true(is.na(out$reliability_summary$split_half_correlation))
  expect_true(is.na(out$reliability_summary$spearman_brown_reliability))
})

test_that("audit_gazepoint_pupil_reliability flags constant split values", {
  toy_data <- make_test_pupil_reliability_data() |>
    dplyr::mutate(auc_pupil_0_2000 = 1)

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    trial_col = "trial",
    split_method = "odd_even",
    min_trials_per_split = 2
  )

  expect_equal(out$reliability_summary$reliability_status, "constant_split_values")
  expect_true(is.na(out$reliability_summary$split_half_correlation))
})

test_that("audit_gazepoint_pupil_reliability can use row order when trial_col is absent", {
  toy_data <- make_test_pupil_reliability_data() |>
    dplyr::select(-trial, -trial_global)

  out <- audit_gazepoint_pupil_reliability(
    toy_data,
    outcome_cols = "auc_pupil_0_2000",
    participant_col = "subject",
    split_method = "odd_even",
    min_trials_per_split = 2
  )

  expect_s3_class(out, "gp3_pupil_reliability_audit")
  expect_equal(out$overview$n_participants, 12)
  expect_true(all(c("odd", "even") %in% unique(as.character(out$split_summary$split))))
  expect_true(is.na(out$settings$value[out$settings$setting == "trial_col"]))
})

test_that("audit_gazepoint_pupil_reliability checks invalid inputs", {
  toy_data <- make_test_pupil_reliability_data()

  expect_error(
    audit_gazepoint_pupil_reliability(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      trial_col = "bad_trial"
    ),
    "`trial_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      outcome_cols = "bad_outcome"
    ),
    "All `outcome_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      by_cols = "bad_group"
    ),
    "All `by_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      split_col = "bad_split"
    ),
    "`split_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      min_trials_per_split = 0
    ),
    "`min_trials_per_split` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_pupil_reliability requires numeric outcomes", {
  toy_data <- make_test_pupil_reliability_data() |>
    dplyr::mutate(bad_outcome = as.character(auc_pupil_0_2000)) |>
    dplyr::select(subject, trial, bad_outcome)

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      outcome_cols = "bad_outcome",
      participant_col = "subject",
      trial_col = "trial"
    ),
    "`outcome_cols` could not be detected and must include at least one numeric column",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_pupil_reliability requires predefined split column to have two levels", {
  toy_data <- make_test_pupil_reliability_data() |>
    dplyr::mutate(split_half = "only_one")

  expect_error(
    audit_gazepoint_pupil_reliability(
      toy_data,
      outcome_cols = "auc_pupil_0_2000",
      participant_col = "subject",
      trial_col = "trial",
      split_col = "split_half"
    ),
    "`split_col` must contain exactly two non-missing split levels",
    fixed = TRUE
  )
})
