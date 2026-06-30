# Summarise Gazepoint pupil trial-level features

Convert sample-level Gazepoint pupil time series into trial-level pupil
features for statistical modelling.

## Usage

``` r
summarise_gazepoint_pupil_trial_features(
  data,
  group_cols = c("subject", "trial_global"),
  pupil_col = NULL,
  time_col = "time",
  interpolated_col = "pupil_was_interpolated",
  artifact_col = NULL,
  artifact_reason_col = NULL,
  early_window = c(0, 500),
  middle_window = c(500, 1500),
  late_window = c(1500, 3000),
  min_valid_samples = 1
)
```

## Arguments

- data:

  A Gazepoint pupil data frame.

- group_cols:

  Character vector of grouping columns. The default is
  `c("subject", "trial_global")`.

- pupil_col:

  Name of the processed pupil column to summarise. If `NULL`, the
  function tries `pupil_smoothed`, `pupil_baseline_corrected`,
  `pupil_baseline_percent_change`, `pupil_interpolated`, `pupil_clean`,
  and `pupil`.

- time_col:

  Name of the time column.

- interpolated_col:

  Optional logical interpolation flag column.

- artifact_col:

  Optional artifact flag column. If `NULL`, the function tries to detect
  `pupil_artifact_flag`, `pupil_flag_invalid`, or `artifact_flag`.

- artifact_reason_col:

  Optional artifact-reason column. If `NULL`, the function tries to
  detect `pupil_artifact_reason`, `pupil_flag_reason`, or
  `artifact_reason`.

- early_window:

  Numeric vector of length 2 defining the early window in milliseconds.

- middle_window:

  Numeric vector of length 2 defining the middle window in milliseconds.

- late_window:

  Numeric vector of length 2 defining the late window in milliseconds.

- min_valid_samples:

  Minimum number of valid pupil samples required for a trial to be
  labelled `"ok"`.

## Value

A tibble with one row per trial/group.

## Details

The function summarises one row per trial or other user-defined
grouping. It computes mean pupil, peak pupil, time-to-peak, AUC,
early/middle/late window means, valid-sample percentage, interpolation
percentage, artifact percentage, and missingness summaries.
