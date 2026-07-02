test_that("simulate_gazepoint_cluster_timecourse_data returns two-condition data", {
  dat <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 6,
    n_time_bins = 10,
    seed = 1
  )

  expect_s3_class(dat, "gp3_cluster_simulated_timecourse")
  expect_equal(length(unique(dat$condition)), 2)
  expect_equal(length(unique(dat$subject)), 6)
  expect_equal(length(unique(dat$time_bin)), 10)
  expect_true(is.numeric(dat$outcome))
})


test_that("audit_gazepoint_timecourse_grid reports ready prepared data", {
  raw <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 5,
    n_time_bins = 6,
    seed = 2
  )

  prepared <- prepare_gazepoint_timecourse_test_data(
    raw,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time_bin",
    outcome_col = "outcome"
  )

  audit <- audit_gazepoint_timecourse_grid(prepared)

  expect_s3_class(audit, "gp3_timecourse_grid_audit")
  expect_equal(audit$audit_status, "ready")
  expect_true(all(audit$readiness$passed))
})


test_that("diagnose_gazepoint_cluster_design returns diagnostic rows", {
  raw <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 5,
    n_time_bins = 6,
    seed = 3
  )

  prepared <- prepare_gazepoint_timecourse_test_data(
    raw,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time_bin",
    outcome_col = "outcome"
  )

  diagnosis <- diagnose_gazepoint_cluster_design(prepared)

  expect_s3_class(diagnosis, "gp3_cluster_design_diagnostic")
  expect_true(all(c("diagnostic", "passed", "interpretation") %in% names(diagnosis)))
  expect_true(any(diagnosis$diagnostic == "validated_scope"))
})


test_that("report_gazepoint_cluster_permutation returns cautious report text", {
  raw <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 6,
    n_time_bins = 8,
    effect_start = 3,
    effect_end = 5,
    effect_size = 1.5,
    seed = 4
  )

  prepared <- prepare_gazepoint_timecourse_test_data(
    raw,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time_bin",
    outcome_col = "outcome"
  )

  result <- run_gazepoint_cluster_permutation(
    prepared,
    n_permutations = 19,
    cluster_threshold = 1,
    min_time_bins = 1,
    seed = 4
  )

  report <- report_gazepoint_cluster_permutation(result)

  expect_s3_class(report, "gp3_cluster_permutation_report")
  expect_true(is.data.frame(report$cluster_table))
  expect_true(is.character(report$report_text))
  expect_true(nchar(report$report_text) > 20)
})


test_that("plot_gazepoint_cluster_null_distribution returns a ggplot when null vector exists", {
  result <- list(
    null_distribution = stats::rnorm(50),
    clusters = data.frame(
      cluster_id = 1,
      start_time_bin = 2,
      end_time_bin = 4,
      cluster_statistic = 3,
      p_value = 0.04
    )
  )

  p <- plot_gazepoint_cluster_null_distribution(result)

  expect_s3_class(p, "ggplot")
})


test_that("run_gazepoint_cluster_threshold_sensitivity returns summaries", {
  raw <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 6,
    n_time_bins = 8,
    effect_start = 3,
    effect_end = 5,
    effect_size = 1.5,
    seed = 5
  )

  prepared <- prepare_gazepoint_timecourse_test_data(
    raw,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time_bin",
    outcome_col = "outcome"
  )

  out <- run_gazepoint_cluster_threshold_sensitivity(
    prepared,
    thresholds = c(1, 1.5),
    n_permutations = 19,
    min_time_bins = 1,
    seed = 5
  )

  expect_s3_class(out, "gp3_cluster_threshold_sensitivity")
  expect_equal(nrow(out$summary), 2)
  expect_equal(length(out$results), 2)
})


test_that("export_gazepoint_cluster_results writes files", {
  result <- list(
    null_distribution = stats::rnorm(20),
    clusters = data.frame(
      cluster_id = 1,
      start_time_bin = 2,
      end_time_bin = 4,
      cluster_statistic = 3,
      p_value = 0.04
    ),
    settings = list(n_permutations = 19, cluster_threshold = 1)
  )

  outdir <- tempfile("gp3_cluster_results_")

  written <- export_gazepoint_cluster_results(
    result,
    outdir = outdir
  )

  expect_true(is.data.frame(written))
  expect_true(all(file.exists(written$file)))
  expect_true(any(written$file_type == "cluster_summary"))
  expect_true(any(written$file_type == "report_text"))
})


test_that("export_gazepoint_cluster_results protects existing directories", {
  result <- list(
    clusters = data.frame(
      cluster_id = integer(0),
      start_time_bin = numeric(0),
      end_time_bin = numeric(0),
      p_value = numeric(0)
    )
  )

  outdir <- tempfile("gp3_cluster_existing_")
  dir.create(outdir)

  expect_error(
    export_gazepoint_cluster_results(result, outdir = outdir),
    "already exists"
  )
})
