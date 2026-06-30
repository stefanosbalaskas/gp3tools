# AOI analysis workflow

## Overview

This vignette shows a practical AOI workflow for Gazepoint GP3 /
Gazepoint Analysis exports using `gp3tools`.

The workflow covers:

1.  creating and validating a master table;
2.  checking AOI geometry and coding;
3.  creating AOI-entry and AOI-window summaries;
4.  fitting AOI-window GLMMs;
5.  extracting fixation, AOI, and transition features;
6.  fitting AOI time-course GAMMs;
7.  running model diagnostics and reporting checks.

The vignette chunks are not evaluated during package checks, so the file
can serve as a readable workflow template.

The first section uses built-in example data; the optional import chunk
shows how to replace those objects with data from a private Gazepoint
export folder.

When adapting the code, replace the placeholder paths, AOI labels, time
windows, and column names with those used in your own study.

## 1. Load example data or import your own exports

For a quick read-through, start with the lightweight synthetic example
data included in the package.

These objects make the later workflow easier to follow because they
already use the column names expected by the examples.

``` r

library(gp3tools)

data("gazepoint_example_master")
data("gazepoint_example_fixations")
data("gazepoint_example_aoi_geometry")
data("gazepoint_example_aoi_windows")

master <- gazepoint_example_master
all_fix <- gazepoint_example_fixations
aoi_geometry <- gazepoint_example_aoi_geometry
aoi_windows <- gazepoint_example_aoi_windows
```

For a real study, replace the example objects above by importing a
Gazepoint export folder.

This optional chunk creates `all_gaze` and `all_fix`, then rebuilds
`master` from the imported gaze rows:

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
all_fix <- results$all_fix
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

Use the validation output before continuing to AOI coding, AOI-window
summaries, and model fitting. If you imported real Gazepoint exports,
this is the point where you should confirm that the expected
participant, trial, time, gaze, fixation, and AOI-related columns were
created correctly.

## 3. AOI geometry and coding checks

The example data already includes an `aoi_geometry` object.

For a real study, create or import a geometry table with one row per AOI
and stimulus.

A minimal geometry template looks like this:

``` r

aoi_geometry_template <- tibble::tibble(
  media_id = c("stim1", "stim1"),
  aoi = c("logo", "product"),
  x_min = c(0.10, 0.50),
  y_min = c(0.10, 0.50),
  x_max = c(0.30, 0.70),
  y_max = c(0.30, 0.70)
)

aoi_geometry_template
```

Audit AOI geometry:

``` r

aoi_geometry_audit <- audit_gazepoint_aoi_geometry(
  aoi_geometry,
  aoi_col = "aoi",
  stimulus_col = "media_id",
  x_min_col = "x_min",
  y_min_col = "y_min",
  x_max_col = "x_max",
  y_max_col = "y_max",
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1)
)

aoi_geometry_audit$overview
aoi_geometry_audit$flagged_aois
```

Audit AOI overlap:

``` r

aoi_overlap_audit <- audit_gazepoint_aoi_overlap(
  aoi_geometry,
  aoi_col = "aoi",
  stimulus_col = "media_id",
  x_min_col = "x_min",
  y_min_col = "y_min",
  x_max_col = "x_max",
  y_max_col = "y_max",
  min_overlap_area = 0,
  min_overlap_prop = 0
)

aoi_overlap_audit$overview
aoi_overlap_audit$flagged_overlaps
```

Validate observed AOI labels against geometry-derived labels:

``` r

aoi_coding_audit <- audit_gazepoint_aoi_coding_matrix(
  gaze_data = master,
  aoi_geometry = aoi_geometry,
  observed_aoi_col = "aoi_current",
  gaze_x_col = "x",
  gaze_y_col = "y",
  gaze_stimulus_col = "MEDIA_ID",
  sample_id_cols = c("subject", "MEDIA_ID", "trial_global"),
  geometry_aoi_col = "aoi",
  geometry_stimulus_col = "media_id",
  x_min_col = "x_min",
  y_min_col = "y_min",
  x_max_col = "x_max",
  y_max_col = "y_max"
)

aoi_coding_audit$overview
aoi_coding_audit$coding_matrix
```

Create a visual AOI verification plot:

``` r

aoi_verification_plot <- plot_gazepoint_aoi_verification(
  aoi_geometry = aoi_geometry,
  gaze_data = master,
  geometry_aoi_col = "aoi",
  geometry_stimulus_col = "media_id",
  x_min_col = "x_min",
  y_min_col = "y_min",
  x_max_col = "x_max",
  y_max_col = "y_max",
  gaze_x_col = "x",
  gaze_y_col = "y",
  gaze_stimulus_col = "MEDIA_ID"
)

aoi_verification_plot
```

## 4. AOI entries and AOI-window summaries

The examples assume that each gaze sample has already been assigned to
an AOI-state column called `aoi_current`. For your own data, check that
the AOI labels used in `target_aoi_values` and `distractor_aoi_values`
match your coding scheme.

Create ordered AOI-entry episodes:

``` r

aoi_entries <- summarise_gazepoint_aoi_entries(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

dplyr::glimpse(aoi_entries)
```

Create AOI-window summaries:

``` r

aoi_windows <- summarise_gazepoint_aoi_windows(
  master,
  windows = c(0, 500, 1000, 2000, 5000, 10000),
  time_col = "time",
  aoi_col = "aoi_current",
  subject_col = "subject",
  condition_col = "condition",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

dplyr::glimpse(aoi_windows)
```

Inspect target-looking proportions:

``` r

dplyr::select(
  aoi_windows,
  subject,
  condition,
  MEDIA_ID,
  trial_global,
  window_label,
  n_window_samples,
  n_target_samples,
  n_distractor_samples,
  n_valid_denominator_samples,
  target_sample_prop_valid,
  aoi_window_status
)
```

## 5. AOI-window GLMM

Use the AOI-window GLMM for predefined confirmatory windows. Before
fitting the model, check the AOI-window denominators so that sparse or
imbalanced windows are visible.

Audit denominator adequacy:

``` r

aoi_window_denominator_audit <- audit_gazepoint_aoi_window_denominators(
  aoi_windows,
  min_denominator_samples = 5,
  min_valid_denominator_prop = 0.70,
  max_denominator_cv = 0.25,
  max_condition_ratio = 2
)

aoi_window_denominator_audit$overview
aoi_window_denominator_audit$window_summary
aoi_window_denominator_audit$condition_window_summary
```

Prepare binomial success/failure data:

``` r

aoi_glmm_data <- prepare_gazepoint_aoi_glmm_data(
  aoi_windows,
  success_col = "n_target_samples",
  denominator = "valid",
  subject_col = "subject",
  condition_col = "condition",
  window_col = "window_label",
  window_start_col = "window_start_ms",
  window_end_col = "window_end_ms",
  min_denominator_samples = 5,
  outcome_label = "target"
)

dplyr::count(
  aoi_glmm_data,
  aoi_glmm_condition,
  aoi_glmm_window,
  aoi_glmm_status
)
```

Fit the main AOI-window GLMM:

``` r

aoi_glmm_fit <- fit_gazepoint_aoi_window_glmm(
  aoi_glmm_data,
  random_window_slopes = FALSE
)

aoi_glmm_fit$model_status
aoi_glmm_fit$formula
aoi_glmm_fit$comparison
aoi_glmm_fit$fixed_effects
```

Transform AOI proportions into empirical logits for sensitivity models:

``` r

aoi_emp_logit <- transform_gazepoint_aoi_empirical_logit(
  aoi_glmm_data,
  numerator_col = "aoi_glmm_success",
  denominator_col = "aoi_glmm_denominator",
  correction = 0.5
)

attr(aoi_emp_logit, "gp3_empirical_logit_overview")
```

Run model-family sensitivity checks:

``` r

aoi_sensitivity <- fit_gazepoint_aoi_model_sensitivity(
  aoi_glmm_data,
  model_types = c(
    "binomial_glmm",
    "empirical_logit_lmm",
    "proportion_lmm",
    "quasibinomial_glm"
  ),
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE,
  random_intercept = TRUE
)

aoi_sensitivity$comparison
aoi_sensitivity$fixed_effects
```

## 6. AOI, fixation, and transition features

The next helpers create descriptive trial-level and sequence-level
features. You do not need every feature for every study; select the
summaries that match your research question.

Create transition-ready AOI sequences:

``` r

aoi_sequences <- prepare_gazepoint_aoi_sequences(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

dplyr::glimpse(aoi_sequences)
```

Summarise AOI transitions:

``` r

aoi_transition_summary <- summarise_gazepoint_aoi_transitions(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

aoi_transition_summary
```

Create transition matrices:

``` r

aoi_transition_matrix <- compute_gazepoint_aoi_transition_matrix(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

aoi_transition_matrix$count_matrix
aoi_transition_matrix$probability_matrix
aoi_transition_matrix$long_table
```

Create time-varying transition matrices:

``` r

time_varying_transition_matrix <- compute_gazepoint_time_varying_transition_matrix(
  master,
  time_col = "time",
  state_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  windows = c(0, 500, 1000, 2000, 5000)
)

time_varying_transition_matrix$overview
time_varying_transition_matrix$transition_summary
```

Create trial-level AOI features:

``` r

aoi_trial_features <- summarise_gazepoint_aoi_trial_features(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

aoi_trial_features
```

Create trial-level fixation features from Gazepoint fixation exports:

``` r

fixation_trial_features <- summarise_gazepoint_fixation_trials(
  all_fix,
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

fixation_trial_features
```

Create fixation-, saccade-, or AOI-contingent aligned data:

``` r

fixation_aligned <- prepare_gazepoint_fixation_aligned_data(
  master,
  time_col = "time",
  participant_col = "subject",
  trial_col = "trial_global",
  aoi_col = "aoi_current",
  target_aoi = "AOI 2",
  fixation_col = "is_fixation",
  alignment_event = "first_fixation_to_target",
  baseline_window = c(-200, 0),
  analysis_window = c(0, 800)
)

fixation_aligned$overview
fixation_aligned$event_table
fixation_aligned$trial_summary
```

## 7. Advanced sequence-model preparation

These preparation helpers are optional. Use them when you plan
Markov-chain, semi-Markov, or HMM-style sequence analyses; otherwise,
this section can be skipped.

Prepare dependency-free sequence-model objects:

``` r

markov_obj <- create_gazepoint_markovchain_object(
  aoi_sequences,
  state_col = "aoi"
)

semimarkov_data <- prepare_gazepoint_semimarkov_data(
  aoi_sequences,
  state_col = "aoi",
  time_col = "time"
)

hmm_data <- prepare_gazepoint_hmm_data(
  aoi_sequences,
  state_col = "aoi",
  observation_cols = c("x", "y", "pupil")
)

markov_obj$overview
semimarkov_data$overview
hmm_data$overview
```

These helpers prepare sequence and transition data for inspection,
reporting, export, or specialist modelling packages. They do not
directly fit Markov-chain, semi-Markov, or hidden Markov models.

## 8. AOI time-course GAMM

Prepare AOI-GAMM data from an AOI-state column:

``` r

aoi_gamm_data <- prepare_gazepoint_aoi_gamm_data(
  master,
  aoi_col = "aoi_current",
  target_aoi_values = "AOI 2",
  subject_col = "subject",
  condition_col = "condition",
  time_col = "time",
  trial_col = "trial_global",
  time_window = c(0, 2000),
  bin_size_ms = 50,
  denominator = "valid",
  min_denominator_samples = 1,
  outcome_label = "target_aoi"
)

dplyr::count(
  aoi_gamm_data,
  .gp3_aoi_gamm_condition,
  .gp3_aoi_gamm_condition_status,
  .gp3_aoi_gamm_status
)
```

Fit the AOI time-course GAMM:

``` r

aoi_gamm_fit <- fit_gazepoint_aoi_gamm(
  aoi_gamm_data,
  include_condition = TRUE,
  condition_smooths = TRUE,
  random_subject = TRUE,
  random_subject_time = FALSE,
  time_k = 10
)

aoi_gamm_fit$model_status
aoi_gamm_fit$condition_status
aoi_gamm_fit$formula_text
aoi_gamm_fit$diagnostics
aoi_gamm_fit$smooth_table
```

Plot observed and fitted AOI trajectories:

``` r

aoi_gamm_plot <- plot_gazepoint_aoi_gamm(
  aoi_gamm_fit,
  n_time_points = 100,
  include_observed = TRUE,
  include_fitted = TRUE,
  show_ci = TRUE
)

aoi_gamm_plot
```

AOI time-course GAMMs are complementary to AOI-window GLMMs. Use
AOI-window GLMMs for predefined confirmatory windows and AOI-GAMMs when
the research question concerns smooth target-looking trajectories.

## 9. Diagnostics and reporting

Finish by checking the fitted model and collecting reporting-ready
summaries. These outputs are intended to support manuscript reporting
rather than replace statistical interpretation.

Run model diagnostics:

``` r

aoi_diagnostics <- diagnose_gazepoint_glmm(
  aoi_glmm_fit,
  model_name = "aoi_window_glmm",
  use_dharma = FALSE
)

aoi_diagnostics$overview
aoi_diagnostics$convergence
aoi_diagnostics$singularity
aoi_diagnostics$overdispersion
```

Create model-summary tables:

``` r

aoi_model_summary <- tidy_gazepoint_model_summary(
  aoi_glmm_fit,
  model_name = "aoi_window_glmm",
  exponentiate = TRUE,
  use_dharma = FALSE
)

aoi_model_summary$overview
aoi_model_summary$fixed_effects
```

Estimate marginal means when `emmeans` is installed:

``` r

aoi_emmeans <- summarise_gazepoint_emmeans(
  aoi_glmm_fit,
  specs = "aoi_glmm_condition",
  by = "aoi_glmm_window",
  model_name = "aoi_window_glmm",
  type = "response"
)

aoi_emmeans$overview
aoi_emmeans$emmeans
aoi_emmeans$contrasts
```

Export manuscript-ready tables:

``` r

model_table_files <- export_gazepoint_model_tables(
  model_summary = aoi_model_summary,
  emmeans_summary = aoi_emmeans,
  output_dir = output_dir,
  prefix = "aoi_window_glmm"
)

model_table_files
```

Create a reporting checklist:

``` r

reporting <- create_gazepoint_reporting_checklist(
  data = master,
  objects = list(
    validation = validation,
    aoi_geometry_audit = aoi_geometry_audit,
    aoi_denominator_audit = aoi_window_denominator_audit,
    aoi_model = aoi_glmm_fit,
    diagnostics = aoi_diagnostics
  ),
  analysis_type = "aoi",
  study_title = "Gazepoint AOI study"
)

reporting$overview
reporting$checklist
```
