make_test_divergence_data <- function(
    n_subjects = 20,
    times = seq(0, 1000, by = 100),
    onset = 500,
    effect_size = 0.35,
    noise_sd = 0.08,
    seed = 123
) {
  set.seed(seed)

  base <- tidyr::expand_grid(
    subject = paste0("S", seq_len(n_subjects)),
    condition = c("control", "treatment"),
    time = times
  )

  subject_shift <- stats::rnorm(n_subjects)
  names(subject_shift) <- paste0("S", seq_len(n_subjects))

  base |>
    dplyr::mutate(
      subject_shift = subject_shift[.data$subject],
      effect = dplyr::if_else(
        .data$condition == "treatment" & .data$time >= onset,
        effect_size,
        0
      ),
      outcome = 1 + .data$subject_shift * 0.05 + .data$effect +
        stats::rnorm(dplyr::n(), 0, noise_sd)
    )
}

make_test_divergence_trial_data <- function() {
  set.seed(456)

  tidyr::expand_grid(
    subject = paste0("S", 1:8),
    trial = paste0("T", 1:4),
    condition = c("control", "treatment"),
    time = seq(0, 800, by = 100)
  ) |>
    dplyr::mutate(
      subject_shift = stats::rnorm(8)[match(.data$subject, paste0("S", 1:8))],
      effect = dplyr::if_else(
        .data$condition == "treatment" & .data$time >= 400,
        0.45,
        0
      ),
      outcome = 1 + .data$subject_shift * 0.04 + .data$effect +
        stats::rnorm(dplyr::n(), 0, 0.06)
    )
}

test_that("estimate_gazepoint_divergence_point detects a known participant-level onset", {
  toy_data <- make_test_divergence_data()

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 100,
    ci = 0.95,
    consecutive_points = 2,
    seed = 123,
    name = "toy_divergence"
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "divergence_point",
      "observed_curve",
      "difference_summary",
      "bootstrap_onsets",
      "bootstrap_differences",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$divergence_point, "tbl_df")
  expect_s3_class(out$observed_curve, "tbl_df")
  expect_s3_class(out$difference_summary, "tbl_df")
  expect_s3_class(out$bootstrap_onsets, "tbl_df")
  expect_s3_class(out$bootstrap_differences, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_divergence")
  expect_equal(out$overview$detector_status, "complete")
  expect_equal(out$overview$comparison_reference, "control")
  expect_equal(out$overview$comparison_test, "treatment")
  expect_equal(out$overview$difference_label, "treatment - control")
  expect_equal(out$overview$bootstrap_unit, "participant")
  expect_equal(out$overview$n_boot, 100)
  expect_equal(out$overview$consecutive_points, 2)

  expect_equal(out$divergence_point$divergence_time, 500)
  expect_equal(out$divergence_point$detector_status, "complete")
  expect_equal(out$divergence_point$observed_direction, "positive")
  expect_true(out$divergence_point$observed_difference_at_onset > 0)
  expect_true(out$divergence_point$bootstrap_onset_detection_rate > 0.90)

  expect_equal(nrow(out$bootstrap_onsets), 100)
  expect_equal(nrow(out$bootstrap_differences), 100 * length(unique(toy_data$time)))
  expect_true(any(out$difference_summary$reliable))
})

test_that("estimate_gazepoint_divergence_point returns observed curves and difference summaries", {
  toy_data <- make_test_divergence_data()

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 50,
    consecutive_points = 2,
    seed = 123
  )

  expect_true(all(c("condition", "time", "estimate", "n") %in% names(out$observed_curve)))
  expect_true(all(c("time", "lower_ci", "upper_ci", "observed_difference", "reliable") %in% names(out$difference_summary)))

  expect_equal(
    sort(unique(out$observed_curve$condition)),
    c("control", "treatment")
  )

  expect_equal(
    sort(unique(out$difference_summary$time)),
    sort(unique(toy_data$time))
  )

  post_onset <- out$difference_summary |>
    dplyr::filter(.data$time >= 500)

  pre_onset <- out$difference_summary |>
    dplyr::filter(.data$time < 500)

  expect_true(mean(post_onset$observed_difference) > mean(pre_onset$observed_difference))
})

test_that("estimate_gazepoint_divergence_point supports row-level bootstrap", {
  toy_data <- make_test_divergence_data(n_subjects = 12)

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    comparison = c("control", "treatment"),
    bootstrap_unit = "row",
    n_boot = 40,
    consecutive_points = 2,
    seed = 99,
    name = "row_bootstrap_divergence"
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_equal(out$overview$bootstrap_unit, "row")
  expect_equal(out$overview$detector_status, "complete")
  expect_equal(nrow(out$bootstrap_onsets), 40)
  expect_equal(out$divergence_point$divergence_time, 500)
})

test_that("estimate_gazepoint_divergence_point supports trial-level bootstrap", {
  toy_data <- make_test_divergence_trial_data()

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    trial_col = "trial",
    comparison = c("control", "treatment"),
    bootstrap_unit = "trial",
    n_boot = 40,
    consecutive_points = 2,
    seed = 456,
    name = "trial_bootstrap_divergence"
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_equal(out$overview$bootstrap_unit, "trial")
  expect_equal(out$overview$detector_status, "complete")
  expect_equal(out$divergence_point$divergence_time, 400)
  expect_equal(nrow(out$bootstrap_onsets), 40)
})

test_that("estimate_gazepoint_divergence_point supports median summaries", {
  toy_data <- make_test_divergence_data()

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    summary_function = "median",
    n_boot = 50,
    consecutive_points = 2,
    seed = 123
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_equal(out$settings$value[out$settings$setting == "summary_function"], "median")
  expect_equal(out$overview$detector_status, "complete")
  expect_equal(out$divergence_point$divergence_time, 500)
})

test_that("estimate_gazepoint_divergence_point supports directional testing", {
  positive_data <- make_test_divergence_data()

  positive_out <- estimate_gazepoint_divergence_point(
    positive_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    direction = "positive",
    n_boot = 50,
    consecutive_points = 2,
    seed = 123
  )

  expect_equal(positive_out$overview$detector_status, "complete")
  expect_equal(positive_out$divergence_point$observed_direction, "positive")
  expect_equal(positive_out$divergence_point$divergence_time, 500)

  negative_data <- positive_data |>
    dplyr::mutate(
      outcome = dplyr::if_else(
        .data$condition == "treatment" & .data$time >= 500,
        .data$outcome - 0.70,
        .data$outcome
      )
    )

  negative_out <- estimate_gazepoint_divergence_point(
    negative_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    direction = "negative",
    n_boot = 50,
    consecutive_points = 2,
    seed = 123
  )

  expect_equal(negative_out$overview$detector_status, "complete")
  expect_equal(negative_out$divergence_point$observed_direction, "negative")
  expect_equal(negative_out$divergence_point$divergence_time, 500)
})

test_that("estimate_gazepoint_divergence_point supports minimum effect thresholds", {
  toy_data <- make_test_divergence_data(effect_size = 0.20)

  no_onset <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 50,
    consecutive_points = 2,
    min_abs_difference = 0.50,
    seed = 123
  )

  expect_equal(no_onset$overview$detector_status, "no_reliable_divergence")
  expect_true(is.na(no_onset$divergence_point$divergence_time))
  expect_true(is.na(no_onset$divergence_point$observed_difference_at_onset))

  onset <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 50,
    consecutive_points = 2,
    min_abs_difference = 0.05,
    seed = 123
  )

  expect_equal(onset$overview$detector_status, "complete")
  expect_equal(onset$divergence_point$divergence_time, 500)
})

test_that("estimate_gazepoint_divergence_point handles no reliable divergence", {
  toy_data <- make_test_divergence_data(effect_size = 0, noise_sd = 0.08)

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 50,
    consecutive_points = 2,
    seed = 123,
    name = "no_divergence"
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_equal(out$overview$detector_status, "no_reliable_divergence")
  expect_true(is.na(out$overview$divergence_time))
  expect_true(is.na(out$divergence_point$divergence_time))
  expect_true(is.na(out$divergence_point$divergence_time_lower_ci))
  expect_true(is.na(out$divergence_point$divergence_time_upper_ci))
})

test_that("estimate_gazepoint_divergence_point can omit bootstrap differences", {
  toy_data <- make_test_divergence_data()

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 30,
    consecutive_points = 2,
    seed = 123,
    keep_bootstrap = FALSE
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_null(out$bootstrap_differences)
  expect_s3_class(out$bootstrap_onsets, "tbl_df")
  expect_equal(nrow(out$bootstrap_onsets), 30)
  expect_equal(
    out$settings$value[out$settings$setting == "keep_bootstrap"],
    "FALSE"
  )
})

test_that("estimate_gazepoint_divergence_point uses supplied comparison order", {
  toy_data <- make_test_divergence_data()

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("treatment", "control"),
    bootstrap_unit = "participant",
    direction = "negative",
    n_boot = 50,
    consecutive_points = 2,
    seed = 123
  )

  expect_equal(out$overview$comparison_reference, "treatment")
  expect_equal(out$overview$comparison_test, "control")
  expect_equal(out$overview$difference_label, "control - treatment")
  expect_equal(out$divergence_point$observed_direction, "negative")
  expect_equal(out$divergence_point$divergence_time, 500)
})

test_that("estimate_gazepoint_divergence_point restores reproducibility with seed", {
  toy_data <- make_test_divergence_data()

  out_1 <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 30,
    consecutive_points = 2,
    seed = 321
  )

  out_2 <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 30,
    consecutive_points = 2,
    seed = 321
  )

  expect_equal(out_1$bootstrap_onsets, out_2$bootstrap_onsets)
  expect_equal(out_1$difference_summary, out_2$difference_summary)
})

test_that("estimate_gazepoint_divergence_point handles missing rows before analysis", {
  toy_data <- make_test_divergence_data()
  toy_data$outcome[1:5] <- NA_real_
  toy_data$time[6] <- NA_real_

  out <- estimate_gazepoint_divergence_point(
    toy_data,
    outcome_col = "outcome",
    time_col = "time",
    condition_col = "condition",
    participant_col = "subject",
    comparison = c("control", "treatment"),
    bootstrap_unit = "participant",
    n_boot = 30,
    consecutive_points = 2,
    seed = 123
  )

  expect_s3_class(out, "gp3_divergence_point_analysis")
  expect_equal(out$overview$n_input_rows, nrow(toy_data))
  expect_equal(out$overview$n_rows_used, nrow(toy_data) - 6)
})

test_that("estimate_gazepoint_divergence_point checks invalid inputs", {
  toy_data <- make_test_divergence_data()

  expect_error(
    estimate_gazepoint_divergence_point(
      list(),
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data[0, ],
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "bad_outcome",
      time_col = "time",
      condition_col = "condition"
    ),
    "`outcome_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "bad_time",
      condition_col = "condition"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      bootstrap_unit = "participant"
    ),
    "`participant_col` is required when `bootstrap_unit = 'participant'`",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      bootstrap_unit = "trial"
    ),
    "`trial_col` is required when `bootstrap_unit = 'trial'`",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      comparison = c("control", "missing"),
      bootstrap_unit = "participant"
    ),
    "All values in `comparison` must be present in `condition_col`",
    fixed = TRUE
  )

  three_condition_data <- dplyr::bind_rows(
    toy_data,
    toy_data |>
      dplyr::mutate(condition = "third")
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      three_condition_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant"
    ),
    "`condition_col` must contain exactly two conditions unless `comparison` is supplied",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      comparison = "control",
      bootstrap_unit = "participant"
    ),
    "`comparison` must be NULL or a character vector of two condition values",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      n_boot = 0
    ),
    "`n_boot` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      ci = 1
    ),
    "`ci` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      consecutive_points = 0
    ),
    "`consecutive_points` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      min_abs_difference = -0.1
    ),
    "`min_abs_difference` must be a finite non-negative number",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      keep_bootstrap = NA
    ),
    "`keep_bootstrap` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      seed = NA_real_
    ),
    "`seed` must be NULL or a finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    estimate_gazepoint_divergence_point(
      toy_data,
      outcome_col = "outcome",
      time_col = "time",
      condition_col = "condition",
      participant_col = "subject",
      bootstrap_unit = "participant",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
