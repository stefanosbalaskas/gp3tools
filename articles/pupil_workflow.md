# Pupil preprocessing and analysis workflow

## Overview

This vignette shows a practical pupil workflow for Gazepoint GP3 /
Gazepoint Analysis exports using `gp3tools`.

The workflow covers:

1.  creating a master table;
2.  running a light pupil preprocessing branch;
3.  running a conservative artifact-cleaned pupil branch;
4.  auditing pupil preprocessing quality;
5.  summarising pupil windows;
6.  fitting confirmatory pupil-window models;
7.  running time-course models and sensitivity checks.

The vignette chunks are not evaluated during package checks, so the file
can serve as a readable workflow template.

The first section uses built-in example data; the optional import chunk
shows how to replace those objects with data from a private Gazepoint
export folder.

When adapting the code, replace the placeholder paths and column names
with those used in your own study.

## 1. Load example data or import your own exports

For a quick read-through, start with the lightweight synthetic example
data included in the package.

These objects make the later workflow easier to follow because they
already use the column names expected by the examples.

``` r

library(gp3tools)

data("gazepoint_example_master")
data("gazepoint_example_pupil_windows")

master <- gazepoint_example_master
pupil_windows <- gazepoint_example_pupil_windows
```

For a real study, replace the example objects above by importing a
Gazepoint export folder.

This optional chunk creates `all_gaze` and then rebuilds `master` from
the imported rows:

``` r

export_dir <- "C:/Users/YourName/Desktop/gp3_test_exports"
output_dir <- "C:/Users/YourName/Desktop/gp3_outputs"

results <- run_gazepoint_workflow(
  export_dir = export_dir,
  output_dir = output_dir,
  prefix = "study1",
  save_plots = TRUE,
  create_report = TRUE
)

all_gaze <- results$all_gaze
master <- create_gazepoint_master(all_gaze)
```

## 2. Validate the master table

``` r

master_audit <- audit_gazepoint_master(master)

validation <- validate_gazepoint_master(master)

master_audit$overview
validation$summary
validation$checks
```

Use the validation output before continuing to pupil preprocessing.

If you imported real Gazepoint exports, this is the point where you
should confirm that the expected participant, trial, time, gaze, and
pupil columns were created correctly.

## 3. Light pupil preprocessing branch

Use the light branch when you need a transparent minimal preprocessing
pipeline.

``` r

pupil_summary <- summarise_gazepoint_pupil(master)

flagged_pupil <- flag_gazepoint_pupil(
  master,
  pupil_col = "pupil"
)

interpolated_pupil <- interpolate_gazepoint_pupil(
  flagged_pupil,
  pupil_col = "pupil_for_preprocessing",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

baseline_corrected <- baseline_correct_gazepoint_pupil(
  interpolated_pupil,
  pupil_col = "pupil_interpolated",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  baseline_window = c(0, 200),
  min_baseline_samples = 1
)

smoothed_pupil <- smooth_gazepoint_pupil(
  baseline_corrected,
  pupil_col = "pupil_baseline_corrected",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  window_samples = 5,
  method = "mean",
  align = "center",
  min_points = 2
)
```

Create pupil-window summaries.

These summaries are useful for confirmatory window analyses, where the
time window should be specified before testing the main condition
effect.

``` r

pupil_windows <- summarise_gazepoint_pupil_windows(
  smoothed_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  windows = c(0, 500, 1000, 2000, 5000),
  group_cols = c("subject", "MEDIA_ID", "trial_global", "condition"),
  min_valid_samples = 1
)

pupil_windows
```

## 4. Conservative artifact-cleaned branch

Use the conservative branch when blink/trackloss padding, pupil-speed
artifacts, or stricter preprocessing decisions are important.

``` r

registry <- create_gazepoint_preprocessing_registry(
  blink_padding_ms = 50,
  interpolation_max_gap_ms = 150,
  smoothing_window_samples = 5,
  baseline_window = c(0, 200)
)

artifact_pupil <- flag_gazepoint_pupil_artifacts(
  master,
  registry = registry,
  pupil_col = "pupil",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

interpolated_artifact_pupil <- interpolate_gazepoint_pupil(
  artifact_pupil,
  pupil_col = "pupil_clean",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

baseline_artifact_pupil <- baseline_correct_gazepoint_pupil(
  interpolated_artifact_pupil,
  pupil_col = "pupil_interpolated",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  baseline_window = c(0, 200),
  min_baseline_samples = 1
)

smoothed_artifact_pupil <- smooth_gazepoint_pupil(
  baseline_artifact_pupil,
  pupil_col = "pupil_baseline_corrected",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  window_samples = 5,
  method = "mean",
  align = "center",
  min_points = 2
)

pupil_windows_conservative <- summarise_gazepoint_pupil_windows(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  windows = c(0, 500, 1000, 2000, 5000),
  group_cols = c("subject", "MEDIA_ID", "trial_global", "condition"),
  min_valid_samples = 1
)
```

## 5. Pupil quality audits

``` r

gap_audit <- audit_gazepoint_pupil_gaps(
  interpolated_artifact_pupil,
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

baseline_audit <- audit_gazepoint_pupil_baseline(
  baseline_artifact_pupil,
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

imbalance_audit <- audit_gazepoint_pupil_imbalance(
  smoothed_artifact_pupil,
  group_cols = "condition"
)

drift_audit <- audit_gazepoint_pupil_drift(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

overlap_audit <- audit_gazepoint_pupil_overlap_risk(
  master,
  time_col = "time",
  event_col = "event_label",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

gap_audit$overview
baseline_audit$overview
imbalance_audit$overview
drift_audit$overview
overlap_audit$overview
```

## 6. Reliability and sensitivity checks

The checks below use the conservative pupil-window summary. In a real
analysis, compare this branch with the lighter preprocessing branch to
assess whether conclusions depend on preprocessing choices.

``` r

pupil_reliability <- audit_gazepoint_pupil_reliability(
  pupil_windows_conservative,
  subject_col = "subject",
  outcome_col = "mean_pupil",
  condition_col = "condition"
)

pchip_pupil <- interpolate_gazepoint_pupil_pchip(
  artifact_pupil,
  pupil_col = "pupil_clean",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

hampel_pupil <- flag_gazepoint_pupil_hampel(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  grouping_cols = c("subject", "MEDIA_ID", "trial_global"),
  window_size_samples = 7,
  k = 3,
  min_valid_samples = 3,
  corrected_col = "pupil_hampel_corrected"
)

pupil_reliability$overview
attr(pchip_pupil, "gp3_pchip_overview")
attr(hampel_pupil, "gp3_hampel_overview")
```

## 7. Confirmatory pupil-window LMM

Prepare pupil-window model data:

``` r

pupil_window_model_data <- prepare_gazepoint_pupil_window_model_data(
  pupil_windows_conservative,
  outcome_col = "mean_pupil",
  subject_col = "subject",
  condition_col = "condition",
  window_col = "window_label",
  trial_col = "trial_global",
  valid_samples_col = "n_valid_samples",
  total_samples_col = "n_samples",
  min_valid_samples = 1
)

dplyr::count(
  pupil_window_model_data,
  pupil_window_condition,
  pupil_window_label,
  pupil_window_model_status
)
```

Fit the confirmatory model:

``` r

pupil_window_lmm <- fit_gazepoint_pupil_window_lmm(
  pupil_window_model_data,
  random_window_slopes = FALSE,
  use_weights = TRUE,
  REML = FALSE
)

pupil_window_lmm$model_status
pupil_window_lmm$formula
pupil_window_lmm$fixed_effects
pupil_window_lmm$comparison
```

Run sensitivity checks:

``` r

pupil_window_sensitivity <- fit_gazepoint_pupil_window_sensitivity(
  pupil_window_model_data,
  model_types = c(
    "unweighted_lmm",
    "weighted_lmm",
    "fixed_lm",
    "weighted_lm"
  ),
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE
)

pupil_window_sensitivity$comparison
pupil_window_sensitivity$fixed_effects
```

## 8. Pupil time-course models

Prepare binned pupil time-course data:

``` r

pupil_gamm_data <- prepare_gazepoint_pupil_gamm_data(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  subject_col = "subject",
  condition_col = "condition",
  trial_col = "trial_global",
  bin_width_ms = 50,
  min_valid_samples = 1
)
```

Fit the main pupil GAMM:

``` r

pupil_gamm_fit <- fit_gazepoint_pupil_gamm(
  pupil_gamm_data,
  n_time_basis = 10,
  discrete = TRUE
)

pupil_gamm_fit$model_status
pupil_gamm_fit$formula
```

Run a gaze-position/PFE sensitivity GAMM:

``` r

pupil_pfe_fit <- fit_gazepoint_pupil_pfe_gamm(
  pupil_gamm_data,
  n_time_basis = 10,
  n_position_basis = 8,
  discrete = TRUE
)

pupil_pfe_fit$sensitivity_status
pupil_pfe_fit$comparison
```

Prepare and fit a Growth Curve Analysis model:

``` r

pupil_gca_data <- prepare_gazepoint_gca_data(
  pupil_gamm_data,
  pupil_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition",
  degree = 3,
  orthogonal = TRUE,
  valid_samples_col = "n_valid_samples",
  min_valid_samples = 1
)

pupil_gca_fit <- fit_gazepoint_gca(
  pupil_gca_data,
  REML = FALSE
)

pupil_gca_fit$model_status
pupil_gca_fit$comparison
```

## 9. Cluster permutation and divergence point

Cluster-permutation and divergence-point analyses are exploratory
time-course tools. Use enough permutations or bootstrap samples for the
final analysis, and avoid using exploratory results to define a later
confirmatory window on the same data.

``` r

cluster_data <- prepare_gazepoint_cluster_data(
  pupil_gamm_data,
  outcome_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition"
)

cluster_results <- run_gazepoint_cluster_permutation(
  cluster_data,
  condition_levels = c("control", "treatment"),
  n_permutations = 1000,
  seed = 123
)

cluster_summary <- summarise_gazepoint_clusters(cluster_results)

cluster_summary$overview
cluster_summary$significant_clusters
```

Estimate the earliest reliable divergence point:

``` r

divergence <- estimate_gazepoint_divergence_point(
  data = pupil_gamm_data,
  outcome_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  condition_col = "condition",
  participant_col = "subject",
  comparison = c("control", "treatment"),
  bootstrap_unit = "participant",
  n_boot = 1000,
  consecutive_points = 2,
  seed = 123
)

divergence$overview
divergence$divergence_point
```

Do not use a detected exploratory cluster to define a confirmatory
window and then retest that same window as if it had been specified a
priori.

## 10. Reporting

Run diagnostics and create manuscript-ready tables:

``` r

pupil_diagnostics <- diagnose_gazepoint_glmm(
  pupil_window_lmm,
  model_name = "pupil_window_lmm",
  use_dharma = FALSE
)

pupil_model_summary <- tidy_gazepoint_model_summary(
  pupil_window_lmm,
  model_name = "pupil_window_lmm"
)

pupil_diagnostics$overview
pupil_model_summary$fixed_effects
```

Create a reporting checklist:

``` r

reporting <- create_gazepoint_reporting_checklist(
  data = master,
  objects = list(
    validation = validation,
    gap_audit = gap_audit,
    baseline_audit = baseline_audit,
    pupil_model = pupil_window_lmm,
    diagnostics = pupil_diagnostics
  ),
  analysis_type = "pupil",
  study_title = "Gazepoint pupil study"
)

reporting$overview
reporting$checklist
```
