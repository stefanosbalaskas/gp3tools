.make_gp3_cluster_extension_demo <- function(n_subjects = 8, n_times = 8) {
  grid <- expand.grid(
    subject = sprintf("S%02d", seq_len(n_subjects)),
    condition = c("control", "treatment"),
    time = seq_len(n_times),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  subject_shift <- rep(seq(-0.2, 0.2, length.out = n_subjects), each = 2 * n_times)
  treatment_effect <- ifelse(
    grid$condition == "treatment" & grid$time %in% 4:6,
    2.5,
    0
  )

  grid$value <- subject_shift + treatment_effect + stats::rnorm(nrow(grid), 0, 0.05)
  grid
}


test_that("prepare_gazepoint_timecourse_test_data creates cluster columns", {
  dat <- .make_gp3_cluster_extension_demo()

  prepared <- prepare_gazepoint_timecourse_test_data(
    dat,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    outcome_col = "value",
    condition_order = c("control", "treatment")
  )

  expect_s3_class(prepared, "gp3_timecourse_test_data")
  expect_true(all(c(
    ".gp3_cluster_subject",
    ".gp3_cluster_condition",
    ".gp3_cluster_time_bin",
    ".gp3_cluster_outcome",
    ".gp3_cluster_status"
  ) %in% names(prepared)))
  expect_equal(length(unique(prepared$.gp3_cluster_condition)), 2)
  expect_true(all(prepared$.gp3_cluster_status == "ok"))
})


test_that("prepare_gazepoint_timecourse_test_data aggregates duplicate cells", {
  dat <- .make_gp3_cluster_extension_demo(n_subjects = 2, n_times = 3)
  dat <- rbind(dat, dat)

  prepared <- prepare_gazepoint_timecourse_test_data(
    dat,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    outcome_col = "value",
    condition_order = c("control", "treatment")
  )

  expect_equal(nrow(prepared), 2 * 2 * 3)
})


test_that("prepare_gazepoint_timecourse_test_data rejects one-condition data", {
  dat <- .make_gp3_cluster_extension_demo()
  dat <- dat[dat$condition == "control", , drop = FALSE]

  expect_error(
    prepare_gazepoint_timecourse_test_data(
      dat,
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time",
      outcome_col = "value"
    ),
    "exactly two conditions"
  )
})


test_that("summarize_gazepoint_time_clusters summarizes cluster results", {
  set.seed(1)
  dat <- .make_gp3_cluster_extension_demo()

  prepared <- prepare_gazepoint_timecourse_test_data(
    dat,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    outcome_col = "value",
    condition_order = c("control", "treatment")
  )

  result <- run_gazepoint_cluster_permutation(
    prepared,
    condition_order = c("control", "treatment"),
    n_permutations = 19,
    cluster_threshold = 1,
    min_time_bins = 1,
    seed = 1
  )

  summary <- summarize_gazepoint_time_clusters(result)

  expect_true(is.data.frame(summary))
  expect_true(all(c(
    "cluster_id",
    "start_time_bin",
    "end_time_bin",
    "p_value",
    "cluster_significant"
  ) %in% names(summary)))
})


test_that("plot_gazepoint_cluster_permutation wraps existing plot helper", {
  set.seed(2)
  dat <- .make_gp3_cluster_extension_demo()

  prepared <- prepare_gazepoint_timecourse_test_data(
    dat,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    outcome_col = "value",
    condition_order = c("control", "treatment")
  )

  result <- run_gazepoint_cluster_permutation(
    prepared,
    condition_order = c("control", "treatment"),
    n_permutations = 19,
    cluster_threshold = 1,
    min_time_bins = 1,
    seed = 2
  )

  p <- plot_gazepoint_cluster_permutation(
    result,
    plot_type = "difference",
    significant_only = FALSE
  )

  expect_s3_class(p, "ggplot")
})
