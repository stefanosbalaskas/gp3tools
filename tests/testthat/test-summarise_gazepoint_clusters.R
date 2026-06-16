make_test_cluster_summary_result <- function(effect = TRUE) {
  subjects <- paste0("S", 1:12)
  conditions <- c("control", "treatment")
  trials <- paste0("T", 1:3)
  times <- seq(0, 950, by = 50)

  dat <- expand.grid(
    subject = subjects,
    condition = conditions,
    trial_global = trials,
    time = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat <- tibble::as_tibble(dat)

  subject_num <- as.numeric(factor(dat$subject))
  trial_num <- as.numeric(factor(dat$trial_global))

  dat$cluster_window <- dat$time >= 300 & dat$time <= 550
  dat$subject_shift <- subject_num * 0.01
  dat$time_shift <- dat$time / 10000

  subject_cluster_effect <- 0.34 + subject_num * 0.01

  dat$outcome <- dat$subject_shift +
    dat$time_shift +
    trial_num * 0.002 +
    ifelse(
      effect & dat$condition == "treatment" & dat$cluster_window,
      subject_cluster_effect,
      0
    )

  cluster_data <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "outcome",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 950),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE,
    outcome_label = "toy_outcome"
  )

  run_gazepoint_cluster_permutation(
    cluster_data,
    condition_order = c("control", "treatment"),
    n_permutations = 49,
    cluster_threshold = 2,
    tail = "two_sided",
    cluster_stat = "sum_abs_t",
    min_time_bins = 1,
    seed = 123
  )
}

test_that("summarise_gazepoint_clusters summarises significant cluster results", {
  result <- make_test_cluster_summary_result(effect = TRUE)

  out <- summarise_gazepoint_clusters(
    result,
    alpha = 0.05,
    round_digits = 3
  )

  expect_s3_class(out, "gp3_cluster_summary")

  expect_true(all(c(
    "overview",
    "clusters",
    "significant_clusters",
    "timecourse_summary",
    "permutation_summary",
    "settings",
    "warning",
    "model_status",
    "timecourse"
  ) %in% names(out)))

  expect_equal(nrow(out$overview), 1)
  expect_equal(out$overview$report_status, "significant_cluster_evidence")
  expect_equal(out$overview$n_subjects, 12)
  expect_equal(out$overview$n_time_bins, 20)
  expect_equal(out$overview$bin_step_ms, 50)
  expect_equal(out$overview$n_permutations, 49)
  expect_equal(out$overview$condition_1, "control")
  expect_equal(out$overview$condition_2, "treatment")
  expect_equal(out$overview$difference, "treatment - control")

  expect_true(nrow(out$clusters) >= 1)
  expect_true(all(c(
    "cluster_id",
    "cluster_label",
    "cluster_direction",
    "start_time_bin",
    "end_time_bin",
    "cluster_duration_ms",
    "n_time_bins",
    "cluster_statistic",
    "max_abs_statistic",
    "mean_difference",
    "p_value",
    "significant_alpha",
    "report_status"
  ) %in% names(out$clusters)))

  expect_false(".gp3_cluster_id" %in% names(out$clusters))
  expect_true(any(out$clusters$significant_alpha))
  expect_true(nrow(out$significant_clusters) >= 1)
  expect_true(all(out$significant_clusters$report_status == "significant"))
})

test_that("summarise_gazepoint_clusters creates timecourse and permutation summaries", {
  result <- make_test_cluster_summary_result(effect = TRUE)

  out <- summarise_gazepoint_clusters(
    result,
    alpha = 0.05,
    round_digits = 3
  )

  expect_equal(nrow(out$timecourse_summary), 1)
  expect_equal(out$timecourse_summary$n_time_bins, 20)
  expect_equal(out$timecourse_summary$start_time_bin, 0)
  expect_equal(out$timecourse_summary$end_time_bin, 950)
  expect_equal(out$timecourse_summary$min_n_subjects, 12)
  expect_equal(out$timecourse_summary$max_n_subjects, 12)
  expect_true(out$timecourse_summary$n_candidate_time_bins >= 1)
  expect_true(out$timecourse_summary$n_clustered_time_bins >= 1)

  expect_equal(nrow(out$permutation_summary), 1)
  expect_equal(out$permutation_summary$n_permutations, 49)
  expect_true(is.finite(out$permutation_summary$median_max_cluster_statistic))
  expect_true(is.finite(out$permutation_summary$mean_max_cluster_statistic))
  expect_true(is.finite(out$permutation_summary$p95_max_cluster_statistic))
})

test_that("summarise_gazepoint_clusters records settings and warning", {
  result <- make_test_cluster_summary_result(effect = TRUE)

  out <- summarise_gazepoint_clusters(result)

  expect_true(all(c("parameter", "value") %in% names(out$settings)))
  expect_true("condition_order" %in% out$settings$parameter)
  expect_true("n_permutations" %in% out$settings$parameter)
  expect_true("cluster_threshold" %in% out$settings$parameter)

  expect_equal(nrow(out$warning), 1)
  expect_match(
    out$warning$warning,
    "do not use them to select a confirmatory window"
  )
})

test_that("summarise_gazepoint_clusters can omit full timecourse table", {
  result <- make_test_cluster_summary_result(effect = TRUE)

  out <- summarise_gazepoint_clusters(
    result,
    include_timecourse = FALSE
  )

  expect_false("timecourse" %in% names(out))
  expect_true("timecourse_summary" %in% names(out))
})

test_that("summarise_gazepoint_clusters handles no-cluster results", {
  result <- make_test_cluster_summary_result(effect = FALSE)

  out <- summarise_gazepoint_clusters(result)

  expect_s3_class(out, "gp3_cluster_summary")
  expect_equal(out$overview$report_status, "no_observed_clusters")
  expect_equal(out$overview$n_observed_clusters, 0)
  expect_equal(out$overview$n_significant_clusters, 0)
  expect_equal(nrow(out$clusters), 0)
  expect_equal(nrow(out$significant_clusters), 0)
})

test_that("summarise_gazepoint_clusters respects alpha", {
  result <- make_test_cluster_summary_result(effect = TRUE)

  out_strict <- summarise_gazepoint_clusters(
    result,
    alpha = 0.001
  )

  out_liberal <- summarise_gazepoint_clusters(
    result,
    alpha = 0.999
  )

  expect_true(nrow(out_liberal$significant_clusters) >=
                nrow(out_strict$significant_clusters))
  expect_equal(out_strict$overview$alpha, 0.001)
  expect_equal(out_liberal$overview$alpha, 0.999)
})

test_that("summarise_gazepoint_clusters checks result structure", {
  expect_error(
    summarise_gazepoint_clusters(data.frame(x = 1)),
    "`result` is missing required element\\(s\\)"
  )

  result <- make_test_cluster_summary_result(effect = TRUE)
  result$timecourse <- NULL

  expect_error(
    summarise_gazepoint_clusters(result),
    "`result` is missing required element\\(s\\): timecourse"
  )
})

test_that("summarise_gazepoint_clusters checks timecourse columns", {
  result <- make_test_cluster_summary_result(effect = TRUE)
  result$timecourse$statistic <- NULL

  expect_error(
    summarise_gazepoint_clusters(result),
    "`result\\$timecourse` is missing required column\\(s\\): statistic"
  )
})

test_that("summarise_gazepoint_clusters checks cluster columns", {
  result <- make_test_cluster_summary_result(effect = TRUE)
  result$clusters$p_value <- NULL

  expect_error(
    summarise_gazepoint_clusters(result),
    "`result\\$clusters` is missing required column\\(s\\): p_value"
  )
})

test_that("summarise_gazepoint_clusters checks permutation columns", {
  result <- make_test_cluster_summary_result(effect = TRUE)
  result$permutation_distribution$max_cluster_statistic <- NULL

  expect_error(
    summarise_gazepoint_clusters(result),
    "`result\\$permutation_distribution` is missing required column\\(s\\): max_cluster_statistic"
  )
})

test_that("summarise_gazepoint_clusters checks scalar arguments", {
  result <- make_test_cluster_summary_result(effect = TRUE)

  expect_error(
    summarise_gazepoint_clusters(result, alpha = 0),
    "`alpha` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_clusters(result, alpha = 1),
    "`alpha` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_clusters(result, round_digits = -1),
    "`round_digits` must be NULL or a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_clusters(result, include_timecourse = NA),
    "`include_timecourse` must be TRUE or FALSE",
    fixed = TRUE
  )
})
