# Synthetic Gazepoint evidence-verification survival adapter example.
# Scientific survival logic remains in eyeprocess.
if (requireNamespace("eyeprocess", quietly = TRUE)) {
  raw <- eyeprocess::simulate_gaze_survival_inputs(
    "verification",
    seed = 20260918,
    n_participants = 36,
    trials_per_participant = 3
  )

  result <- run_gazepoint_latency_analysis(
    raw$trials,
    raw$events,
    target_aoi = "source_evidence",
    formula = "condition",
    participant_col = "participant_id",
    cox_structure = "cluster_robust",
    aft_distribution = "weibull",
    km_group = "condition",
    event_type = "first_aoi_entry",
    condition_col = "condition_id",
    event_detector = "synthetic_truth",
    aoi_specification = "fixed synthetic source/evidence AOI",
    quality_rules = list(valid_fraction_min = .90)
  )

  print(result$censoring)
  print(result$report$effects)
  print(result$ph_diagnostics)
  eyeprocess::plot_gaze_survival_curve(result$data, group = "condition")
}
