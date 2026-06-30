# Plot gallery

This article is a website-only showcase of representative visual outputs
in `gp3tools`. It is intended for users, reviewers, and readers who want
to see the visual diagnostics and reporting plots produced by the
package.

The gallery uses only package-safe synthetic or example data. It should
not be read as an empirical analysis of real participants.

``` r

library(gp3tools)

master <- gp3tools::gazepoint_example_master
fixations <- gp3tools::gazepoint_example_fixations
aoi_geometry <- gp3tools::gazepoint_example_aoi_geometry
aoi_windows <- gp3tools::gazepoint_example_aoi_windows

pupil_demo <- master
pupil_demo$pupil_clean <- ifelse(pupil_demo$artifact, NA_real_, pupil_demo$pupil)
pupil_demo$pupil_interpolated <- ifelse(
  is.na(pupil_demo$pupil_clean),
  ave(
    pupil_demo$pupil,
    pupil_demo$subject,
    pupil_demo$trial_global,
    FUN = function(z) mean(z, na.rm = TRUE)
  ),
  pupil_demo$pupil_clean
)
pupil_demo$pupil_was_interpolated <- is.na(pupil_demo$pupil_clean) &
  !is.na(pupil_demo$pupil_interpolated)
pupil_demo$pupil_interpolation_status <- ifelse(
  pupil_demo$pupil_was_interpolated,
  'interpolated',
  'observed'
)
pupil_demo$pupil_baseline_corrected <- pupil_demo$pupil_interpolated - ave(
  pupil_demo$pupil_interpolated,
  pupil_demo$subject,
  pupil_demo$trial_global,
  FUN = function(z) mean(z[seq_len(min(5, length(z)))], na.rm = TRUE)
)
pupil_demo$pupil_smoothed <- ave(
  pupil_demo$pupil_baseline_corrected,
  pupil_demo$subject,
  pupil_demo$trial_global,
  FUN = function(z) {
    zz <- as.numeric(stats::filter(z, rep(1 / 3, 3), sides = 2))
    zz[is.na(zz)] <- z[is.na(zz)]
    zz
  }
)

make_gca_demo <- function() {
  base <- expand.grid(
    subject = paste0('S', 1:8),
    condition = c('control', 'treatment'),
    gca_time = seq(0, 900, by = 100),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  base <- base[order(base$subject, base$condition, base$gca_time), ]
  time_poly <- stats::poly(base$gca_time, degree = 3)
  subject_shift <- stats::setNames(
    seq(-0.20, 0.20, length.out = 8),
    paste0('S', 1:8)
  )
  base$time_poly_1 <- as.numeric(time_poly[, 1])
  base$time_poly_2 <- as.numeric(time_poly[, 2])
  base$time_poly_3 <- as.numeric(time_poly[, 3])
  base$gca_pupil <- 0.20 +
    ifelse(base$condition == 'treatment', 0.10, 0) +
    0.60 * base$time_poly_1 -
    0.35 * base$time_poly_2 +
    0.15 * base$time_poly_3 +
    ifelse(base$condition == 'treatment', 0.15 * base$time_poly_1, 0) +
    subject_shift[base$subject]
  base
}

gca_demo <- make_gca_demo()
```

## Plot functions covered

``` r

plot_functions <- sort(grep(
  '^plot_',
  getNamespaceExports('gp3tools'),
  value = TRUE
))

data.frame(
  plot_function = plot_functions,
  row.names = NULL
)
#>                           plot_function
#> 1               plot_gazepoint_aoi_gamm
#> 2  plot_gazepoint_aoi_transition_matrix
#> 3       plot_gazepoint_aoi_verification
#> 4        plot_gazepoint_cluster_results
#> 5                    plot_gazepoint_gca
#> 6      plot_gazepoint_model_predictions
#> 7     plot_gazepoint_multiverse_results
#> 8    plot_gazepoint_pupil_preprocessing
#> 9           plot_gazepoint_pupil_status
#> 10      plot_gazepoint_pupil_timecourse
#> 11                   plot_sampling_rate
#> 12                plot_tracking_quality
#> 13              plot_transition_heatmap
```

## Sampling rate

The built-in demonstration data are sampled every 50 ms, corresponding
to approximately 20 Hz. This differs from a typical 60 Hz GP3 recording,
but is sufficient for a compact website example.

``` r

sampling <- check_sampling_rate(
  master,
  group_cols = c('USER_FILE', 'MEDIA_ID'),
  time_col = 'TIME'
)

plot_sampling_rate(
  sampling,
  user_col = 'USER_FILE',
  media_col = 'MEDIA_ID',
  expected_hz = 20,
  hz_tolerance = 2
)
```

![](plot-gallery_files/figure-html/sampling-rate-plot-1.png)

## Tracking quality

The tracking-quality plot summarises valid gaze percentage by
participant file and stimulus. A simple review flag is created here for
demonstration.

``` r

quality <- summarise_tracking_quality(
  master,
  group_cols = c('USER_FILE', 'MEDIA_ID')
)
quality$review_required <- quality$BPOGV_valid_pct < 70

plot_tracking_quality(
  quality,
  metric_cols = 'BPOGV_valid_pct',
  user_col = 'USER_FILE',
  media_col = 'MEDIA_ID',
  review_col = 'review_required',
  min_valid_pct = 70
)
```

![](plot-gallery_files/figure-html/tracking-quality-plot-1.png)

## Pupil status

The pupil-status plot shows whether samples are observed or
interpolated.

``` r

plot_gazepoint_pupil_status(
  pupil_demo,
  time_col = 'time',
  pupil_col = 'pupil_interpolated',
  status_col = 'pupil_interpolation_status',
  interpolated_col = 'pupil_was_interpolated',
  group_cols = c('subject', 'trial_global'),
  plot_type = 'summary'
)
```

![](plot-gallery_files/figure-html/pupil-status-plot-1.png)

## Pupil preprocessing

The preprocessing plot compares raw, cleaned, interpolated,
baseline-corrected, and smoothed pupil series for one synthetic trial.

``` r

plot_gazepoint_pupil_preprocessing(
  pupil_demo,
  subject = 'S01',
  trial_global = 'control_T1',
  time_col = 'time',
  raw_pupil_col = 'pupil',
  clean_pupil_col = 'pupil_clean',
  interpolated_pupil_col = 'pupil_interpolated',
  baseline_pupil_col = 'pupil_baseline_corrected',
  smoothed_pupil_col = 'pupil_smoothed',
  status_col = 'pupil_interpolation_status',
  plot_style = 'faceted'
)
```

![](plot-gallery_files/figure-html/pupil-preprocessing-plot-1.png)

## Pupil time course

The pupil time-course plot shows condition-level pupil trajectories
after binning the synthetic sample-level data.

``` r

plot_gazepoint_pupil_timecourse(
  master,
  pupil_col = 'pupil',
  time_col = 'time',
  condition_col = 'condition',
  bin_width_ms = 100
)
```

![](plot-gallery_files/figure-html/pupil-timecourse-plot-1.png)

## AOI verification

The AOI verification plot overlays gaze samples on the synthetic AOI
geometry. This helps users inspect whether AOI coordinates and gaze
coordinates are on the same scale before computing AOI summaries.

``` r

plot_gazepoint_aoi_verification(
  aoi_geometry = aoi_geometry,
  gaze_data = master,
  geometry_aoi_col = 'aoi',
  geometry_stimulus_col = 'media_id',
  gaze_x_col = 'x',
  gaze_y_col = 'y',
  gaze_stimulus_col = 'MEDIA_ID',
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  facet_by_stimulus = TRUE,
  show_labels = TRUE,
  show_gaze = TRUE
)
```

![](plot-gallery_files/figure-html/aoi-verification-plot-1.png)

## AOI transition matrix

The AOI transition matrix summarises transitions among AOI states by
condition.

``` r

aoi_transitions <- compute_gazepoint_aoi_transition_matrix(
  master,
  aoi_col = 'AOI',
  time_col = 'time',
  group_cols = c('subject', 'MEDIA_ID', 'trial_global'),
  by_cols = 'condition',
  include_non_aoi = TRUE,
  include_self_transitions = FALSE
)

plot_gazepoint_aoi_transition_matrix(
  aoi_transitions,
  value = 'prob',
  by_cols = 'condition',
  facet = TRUE,
  title = 'AOI transition probabilities by condition'
)
```

![](plot-gallery_files/figure-html/aoi-transition-matrix-plot-1.png)

## Transition heatmap

The legacy transition heatmap provides a compact view of transitions
computed from fixation-level AOI sequences.

``` r

legacy_transitions <- safe_value(compute_transition_matrix(
  fixations,
  group_cols = 'MEDIA_ID',
  aoi_col = 'AOI',
  time_col = 'FPOGS'
))

if (!is.null(legacy_transitions)) {
  safe_plot(plot_transition_heatmap(legacy_transitions))
}
```

![](plot-gallery_files/figure-html/transition-heatmap-plot-1.png)

## GCA model plot

The GCA plot uses a compact synthetic growth-curve data set to
illustrate observed and fitted trajectories.

``` r

gca_fit <- safe_value(fit_gazepoint_gca(
  gca_demo,
  random_slopes = FALSE,
  REML = FALSE
))

if (!is.null(gca_fit)) {
  safe_plot(plot_gazepoint_gca(
    gca_fit,
    title = 'Synthetic GCA pupil trajectories'
  ))
}
```

## Model-prediction plot

The model-prediction plot shows fitted trends from a simple model and
observed condition-level summaries.

``` r

prediction_model <- stats::lm(
  gca_pupil ~ gca_time * condition,
  data = gca_demo
)

safe_plot(plot_gazepoint_model_predictions(
  gca_demo,
  model = prediction_model,
  x_col = 'gca_time',
  outcome_col = 'gca_pupil',
  condition_col = 'condition'
))
```

![](plot-gallery_files/figure-html/model-prediction-plot-1.png)

## AOI GAMM plot

The AOI GAMM plot is included when the optional modelling dependencies
and example fit are available in the website build environment.

``` r

aoi_gamm_fit <- safe_value(fit_gazepoint_aoi_gamm(
  aoi_windows,
  outcome_col = 'target_sample_prop_valid',
  time_col = 'window_start_ms',
  condition_col = 'condition',
  subject_col = 'subject'
))

if (!is.null(aoi_gamm_fit)) {
  safe_plot(plot_gazepoint_aoi_gamm(
    aoi_gamm_fit,
    title = 'Synthetic AOI GAMM trajectory'
  ))
}
```

## Cluster-results plot

The cluster-results plot is rendered when a compatible
cluster-permutation result can be computed from the compact synthetic
data.

``` r

cluster_result <- safe_value(run_gazepoint_cluster_permutation(
  gca_demo,
  time_col = 'gca_time',
  outcome_col = 'gca_pupil',
  condition_col = 'condition',
  subject_col = 'subject',
  n_permutations = 99
))

if (!is.null(cluster_result)) {
  safe_plot(plot_gazepoint_cluster_results(
    cluster_result,
    plot_type = 'both',
    title = 'Synthetic cluster-permutation result'
  ))
}
```

## Multiverse-results plot

The multiverse plot summarises alternative preprocessing decisions when
a compact multiverse object can be created in the website build
environment.

``` r

multiverse_result <- safe_value(run_gazepoint_pupil_multiverse(
  pupil_demo,
  subject_col = 'subject',
  trial_col = 'trial_global',
  time_col = 'time',
  pupil_col = 'pupil'
))

if (!is.null(multiverse_result)) {
  safe_plot(plot_gazepoint_multiverse_results(
    multiverse_result,
    plot = 'status',
    family = 'pupil'
  ))
}
```
