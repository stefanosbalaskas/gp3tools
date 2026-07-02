test_that("advanced cluster guardrails fail safely", {
  guardrails <- list(
    run_gazepoint_cluster_permutation_anova,
    run_gazepoint_cluster_permutation_lmer,
    run_gazepoint_tfce,
    run_gazepoint_multidimensional_cluster_permutation,
    estimate_gazepoint_cluster_onset,
    estimate_gazepoint_cluster_offset,
    run_gazepoint_cluster_permutation_covariate_adjusted,
    run_gazepoint_cluster_permutation_parallel
  )

  for (fn in guardrails) {
    expect_error(
      fn(),
      "outside the current validated scope"
    )
  }
})


test_that("cluster guardrails point users to the validated workflow", {
  expect_error(
    run_gazepoint_tfce(),
    "prepare_gazepoint_timecourse_test_data"
  )

  expect_error(
    estimate_gazepoint_cluster_onset(),
    "exact onset claims"
  )

  expect_error(
    estimate_gazepoint_cluster_offset(),
    "exact offset claims"
  )
})
