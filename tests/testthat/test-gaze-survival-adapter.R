test_that("survival adapter requires explicit estimator choices", {
  trials <- data.frame(
    participant_id = "P1",
    trial_id = "T1",
    start_time = 0,
    end_time = 5
  )
  events <- data.frame(
    participant_id = "P1",
    trial_id = "T1",
    start_time = 1,
    aoi_id = "target",
    episode_type = "fixation"
  )

  expect_error(
    run_gazepoint_latency_analysis(
      trials,
      events,
      "target",
      "condition"
    ),
    "cox_structure.*specified explicitly"
  )
  expect_error(
    run_gazepoint_latency_analysis(
      trials,
      events,
      "target",
      "condition",
      cox_structure = "cluster_robust"
    ),
    "aft_distribution.*specified explicitly"
  )
})

test_that("Gazepoint preparation delegates to eyeprocess and retains censoring", {
  skip_if_not_installed("eyeprocess")
  raw <- eyeprocess::simulate_gaze_survival_inputs(
    "disclosure",
    seed = 20260918,
    n_participants = 12,
    trials_per_participant = 3
  )
  d <- prepare_gazepoint_survival_data(
    raw$trials,
    raw$events,
    target_aoi = "disclosure",
    event_type = "first_fixation",
    condition_col = "condition_id",
    event_detector = "synthetic_truth",
    aoi_specification = "fixed synthetic disclosure AOI",
    quality_rules = list(valid_fraction_min = .9)
  )

  expect_equal(nrow(d), nrow(raw$trials))
  expect_true(any(d$event_observed == 0, na.rm = TRUE))
  expect_true(all(
    d$analysis_time[d$event_observed == 0] ==
      d$censor_time[d$event_observed == 0]
  ))
})

test_that("workflow delegates models and preserves explicit specification", {
  skip_if_not_installed("eyeprocess")
  skip_if_not_installed("survival")
  raw <- eyeprocess::simulate_gaze_survival_inputs(
    "disclosure",
    seed = 11,
    n_participants = 40,
    trials_per_participant = 3
  )

  expect_warning(
    out <- run_gazepoint_latency_analysis(
      raw$trials,
      raw$events,
      target_aoi = "disclosure",
      formula = "condition",
      participant_col = "participant_id",
      cox_structure = "cluster_robust",
      aft_distribution = "weibull",
      km_group = "condition",
      event_type = "first_fixation",
      condition_col = "condition_id",
      event_detector = "synthetic_truth",
      aoi_specification = "fixed synthetic disclosure AOI",
      quality_rules = list(valid_fraction_min = .9)
    ),
    "not directly comparable"
  )

  expect_s3_class(out, "gazepoint_survival_analysis")
  expect_identical(out$specification$engine, "eyeprocess")
  expect_identical(out$specification$cox_structure, "cluster_robust")
  expect_identical(out$specification$aft_distribution, "weibull")
  expect_match(out$cox$repeated_structure, "cluster_robust")
  expect_equal(out$aft$model_family, "aft_weibull")
  expect_true(nrow(out$kaplan_meier) > 0)
  expect_true(nrow(out$ph_diagnostics) > 0)
})
