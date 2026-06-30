# Estimate a bootstrapped divergence point between two Gazepoint time courses

Estimate the earliest time point at which two condition-level time
courses reliably diverge. The helper computes observed condition curves,
bootstraps the condition difference, identifies the first time point
where the bootstrap confidence interval excludes the null value for a
requested number of consecutive time points, and returns a bootstrap
uncertainty interval for the divergence onset.

## Usage

``` r
estimate_gazepoint_divergence_point(
  data,
  outcome_col,
  time_col,
  condition_col,
  participant_col = NULL,
  trial_col = NULL,
  comparison = NULL,
  bootstrap_unit = c("participant", "trial", "row"),
  summary_function = c("mean", "median"),
  n_boot = 1000L,
  ci = 0.95,
  consecutive_points = 1L,
  null_value = 0,
  min_abs_difference = 0,
  direction = c("two_sided", "positive", "negative"),
  seed = NULL,
  keep_bootstrap = TRUE,
  name = "gazepoint_divergence_point"
)
```

## Arguments

- data:

  A data frame containing time-course observations.

- outcome_col:

  Outcome column, for example pupil size, fixation probability, gaze
  proportion, or AOI time-course value.

- time_col:

  Time column.

- condition_col:

  Condition column. Exactly two conditions are compared unless
  `comparison` is supplied.

- participant_col:

  Optional participant column used for participant-level bootstrap
  resampling.

- trial_col:

  Optional trial column used for trial-level bootstrap resampling.

- comparison:

  Optional character vector of two condition values. The estimated
  difference is `comparison[2] - comparison[1]`.

- bootstrap_unit:

  Resampling unit. Options are `"participant"`, `"trial"`, and `"row"`.

- summary_function:

  Function used to summarise observations within condition-by-time
  cells. Options are `"mean"` and `"median"`.

- n_boot:

  Number of bootstrap resamples.

- ci:

  Confidence level for bootstrap intervals.

- consecutive_points:

  Number of consecutive time points required before declaring
  divergence.

- null_value:

  Null difference value. Default is `0`.

- min_abs_difference:

  Optional minimum absolute observed difference required at a time
  point.

- direction:

  Direction of divergence. `"two_sided"` checks whether the bootstrap
  interval excludes `null_value` in either direction. `"positive"`
  checks whether `comparison[2] > comparison[1]`. `"negative"` checks
  whether `comparison[2] < comparison[1]`.

- seed:

  Optional random seed for reproducible bootstrap resampling.

- keep_bootstrap:

  Logical. If `TRUE`, return bootstrap differences for each time point.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_divergence_point_analysis`.

## Details

This helper complements cluster-permutation analysis. Cluster
permutation asks where a reliable time window exists; divergence-point
analysis asks when the condition difference first emerges.
