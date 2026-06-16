make_test_cluster_permutation_data <- function(effect = TRUE) {
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

  prepare_gazepoint_cluster_data(
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
}

test_that("run_gazepoint_cluster_permutation detects a known positive cluster", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  out <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 99,
    cluster_threshold = 2,
    tail = "two_sided",
    cluster_stat = "sum_abs_t",
    min_time_bins = 1,
    seed = 123
  )

  expect_s3_class(out, "gp3_cluster_permutation")
  expect_equal(out$model_status, "significant_clusters")
  expect_equal(out$n_subjects, 12)
  expect_equal(out$n_time_bins, 20)

  expect_equal(nrow(out$permutation_distribution), 99)
  expect_true(all(c(
    "permutation",
    "max_cluster_statistic"
  ) %in% names(out$permutation_distribution)))

  expect_true(nrow(out$clusters) >= 1)
  expect_true(all(c(
    "cluster_id",
    "cluster_direction",
    "start_time_bin",
    "end_time_bin",
    "n_time_bins",
    "cluster_statistic",
    "max_abs_statistic",
    "mean_difference",
    "p_value",
    "significant"
  ) %in% names(out$clusters)))

  expect_false(".gp3_cluster_id" %in% names(out$clusters))
  expect_true(any(out$clusters$cluster_direction == "positive"))
  expect_true(any(out$clusters$start_time_bin <= 550))
  expect_true(any(out$clusters$end_time_bin >= 300))
  expect_true(any(out$clusters$significant))

  expect_true(all(c(
    ".gp3_cluster_time_bin",
    "n_subjects",
    "mean_difference",
    "sd_difference",
    "statistic",
    "cluster_id",
    "point_candidate",
    "condition_1",
    "condition_2",
    "difference_label"
  ) %in% names(out$timecourse)))
})

test_that("run_gazepoint_cluster_permutation records settings", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  out <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 25,
    cluster_threshold = 2.5,
    tail = "greater",
    cluster_stat = "size",
    min_time_bins = 2,
    seed = 99
  )

  expect_equal(out$settings$condition_order, c("control", "treatment"))
  expect_equal(out$settings$condition_1, "control")
  expect_equal(out$settings$condition_2, "treatment")
  expect_equal(out$settings$difference, "treatment - control")
  expect_equal(out$settings$n_permutations, 25)
  expect_equal(out$settings$cluster_threshold, 2.5)
  expect_equal(out$settings$tail, "greater")
  expect_equal(out$settings$cluster_stat, "size")
  expect_equal(out$settings$min_time_bins, 2)
  expect_equal(out$settings$seed, 99)
  expect_true(out$settings$paired)
})

test_that("run_gazepoint_cluster_permutation supports greater and less tails", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  out_greater <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 25,
    cluster_threshold = 2,
    tail = "greater",
    seed = 123
  )

  out_less <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("treatment", "control"),
    n_permutations = 25,
    cluster_threshold = 2,
    tail = "less",
    seed = 123
  )

  expect_true(out_greater$model_status %in% c(
    "significant_clusters",
    "clusters_not_significant"
  ))
  expect_true(out_less$model_status %in% c(
    "significant_clusters",
    "clusters_not_significant"
  ))

  if (nrow(out_greater$clusters) > 0) {
    expect_true(all(out_greater$clusters$cluster_direction == "positive"))
  }

  if (nrow(out_less$clusters) > 0) {
    expect_true(all(out_less$clusters$cluster_direction == "negative"))
  }
})

test_that("run_gazepoint_cluster_permutation supports different cluster statistics", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  out_sum_t <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 25,
    cluster_threshold = 2,
    cluster_stat = "sum_t",
    seed = 123
  )

  out_size <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 25,
    cluster_threshold = 2,
    cluster_stat = "size",
    seed = 123
  )

  expect_true(out_sum_t$model_status %in% c(
    "significant_clusters",
    "clusters_not_significant",
    "no_clusters"
  ))
  expect_true(out_size$model_status %in% c(
    "significant_clusters",
    "clusters_not_significant",
    "no_clusters"
  ))

  expect_equal(out_sum_t$settings$cluster_stat, "sum_t")
  expect_equal(out_size$settings$cluster_stat, "size")

  if (nrow(out_size$clusters) > 0) {
    expect_equal(
      out_size$clusters$cluster_statistic,
      out_size$clusters$n_time_bins
    )
  }
})

test_that("run_gazepoint_cluster_permutation reports no clusters when there is no condition difference", {
  dat <- make_test_cluster_permutation_data(effect = FALSE)

  out <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 25,
    cluster_threshold = 2,
    seed = 123
  )

  expect_equal(out$model_status, "no_clusters")
  expect_equal(nrow(out$clusters), 0)
  expect_true(all(is.na(out$timecourse$cluster_id)))
  expect_false(any(out$timecourse$point_candidate))
})

test_that("run_gazepoint_cluster_permutation requires exactly two conditions", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  dat_one_condition <- dat[
    dat$.gp3_cluster_condition == "control",
    ,
    drop = FALSE
  ]

  expect_error(
    run_gazepoint_cluster_permutation(dat_one_condition),
    "Cluster permutation requires exactly two conditions"
  )
})

test_that("run_gazepoint_cluster_permutation checks requested conditions", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  expect_error(
    run_gazepoint_cluster_permutation(
      dat,
      condition_order = c("control", "missing_condition")
    ),
    "Requested condition\\(s\\) not found in data"
  )
})

test_that("run_gazepoint_cluster_permutation checks required columns", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)
  dat$.gp3_cluster_outcome <- NULL

  expect_error(
    run_gazepoint_cluster_permutation(dat),
    "Missing required cluster-data columns: .gp3_cluster_outcome",
    fixed = TRUE
  )
})

test_that("run_gazepoint_cluster_permutation checks scalar arguments", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  expect_error(
    run_gazepoint_cluster_permutation(dat, n_permutations = 0),
    "`n_permutations` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_cluster_permutation(dat, cluster_threshold = 0),
    "`cluster_threshold` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_cluster_permutation(dat, min_time_bins = 0),
    "`min_time_bins` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_cluster_permutation(dat, seed = NA_real_),
    "`seed` must be NULL or a finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_cluster_permutation(dat, paired = FALSE),
    "Only paired within-subject cluster permutation is currently supported",
    fixed = TRUE
  )
})

test_that("run_gazepoint_cluster_permutation includes circularity warning", {
  dat <- make_test_cluster_permutation_data(effect = TRUE)

  out <- run_gazepoint_cluster_permutation(
    dat,
    condition_order = c("control", "treatment"),
    n_permutations = 10,
    seed = 123
  )

  expect_match(out$warning, "do not use them to select a confirmatory window")
})
