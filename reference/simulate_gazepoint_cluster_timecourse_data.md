# Simulate simple Gazepoint cluster time-course data

Generate synthetic two-condition within-subject time-course data for
examples and tests. The simulation is intentionally simple and should
not be treated as a realistic model of gaze, pupil, or biometric
time-series data.

## Usage

``` r
simulate_gazepoint_cluster_timecourse_data(
  n_subjects = 20,
  n_time_bins = 60,
  conditions = c("control", "treatment"),
  effect_start = 25,
  effect_end = 40,
  effect_size = 0.5,
  subject_sd = 0.3,
  noise_sd = 0.4,
  seed = NULL
)
```

## Arguments

- n_subjects:

  Number of subjects.

- n_time_bins:

  Number of time bins.

- conditions:

  Two condition labels.

- effect_start:

  First time bin with an injected treatment effect.

- effect_end:

  Last time bin with an injected treatment effect.

- effect_size:

  Added treatment effect inside the effect window.

- subject_sd:

  Standard deviation of subject random shifts.

- noise_sd:

  Standard deviation of observation noise.

- seed:

  Optional random seed.

## Value

A long-format data frame.
