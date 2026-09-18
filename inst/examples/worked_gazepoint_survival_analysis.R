# Synthetic, reproducible Gazepoint adapter demonstration.
# Scientific survival logic is provided by eyeprocess.
if (requireNamespace("eyeprocess", quietly = TRUE)) {
  raw <- eyeprocess::simulate_gaze_survival_inputs(
    "disclosure",
    seed = 20260918,
    n_participants = 36,
    trials_per_participant = 3
  )

  result <- run_gazepoint_latency_analysis(
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
  )

  print(result$censoring)
  print(result$report)
}
