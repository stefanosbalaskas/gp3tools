# Audit Gazepoint pupil baseline quality

Summarise baseline quality after
[`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md).

## Usage

``` r
audit_gazepoint_pupil_baseline(
  data,
  group_cols = c("subject", "media_id"),
  time_col = "time",
  pupil_col = "pupil_interpolated",
  baseline_n_col = "pupil_baseline_n",
  baseline_status_col = "pupil_baseline_status",
  baseline_available_col = "pupil_baseline_available",
  baseline_used_col = "pupil_baseline_used",
  baseline_window_start_col = "pupil_baseline_window_start",
  baseline_window_end_col = "pupil_baseline_window_end",
  baseline_flag_col = NULL,
  interpolated_col = "pupil_was_interpolated",
  artifact_col = NULL,
  artifact_reason_col = NULL,
  min_baseline_samples = 1,
  max_missing_pct = 50,
  max_interpolated_pct = 50,
  max_artifact_pct = 50
)
```

## Arguments

- data:

  A data frame returned by
  [`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md)
  or a later pupil-preprocessing step.

- group_cols:

  Character vector of grouping columns. Use `character(0)` for an
  overall audit.

- time_col:

  Name of the time column.

- pupil_col:

  Name of the pupil column used to evaluate missingness.

- baseline_n_col:

  Name of the baseline valid-sample count column.

- baseline_status_col:

  Name of the baseline-status column.

- baseline_available_col:

  Name of the baseline-availability column.

- baseline_used_col:

  Name of the logical column indicating whether a row used a baseline
  value.

- baseline_window_start_col:

  Name of the baseline-window start column.

- baseline_window_end_col:

  Name of the baseline-window end column.

- baseline_flag_col:

  Optional logical column identifying baseline rows. If `NULL`, baseline
  rows are detected from the time column and baseline window start/end
  columns.

- interpolated_col:

  Name of the logical interpolation flag column.

- artifact_col:

  Optional artifact flag column. If `NULL`, the function tries to detect
  `pupil_artifact_flag`, `pupil_flag_invalid`, or `artifact_flag`.

- artifact_reason_col:

  Optional artifact-reason column. If `NULL`, the function tries to
  detect `pupil_artifact_reason`, `pupil_flag_reason`, or
  `artifact_reason`.

- min_baseline_samples:

  Minimum acceptable number of valid baseline samples before a group is
  flagged as low quality.

- max_missing_pct:

  Maximum acceptable percentage of missing baseline samples.

- max_interpolated_pct:

  Maximum acceptable percentage of interpolated baseline samples.

- max_artifact_pct:

  Maximum acceptable percentage of artifact-flagged baseline samples.

## Value

A tibble with one row per group.

## Details

The function reports baseline-row counts, valid/missing baseline
samples, interpolated baseline samples, artifact-flagged baseline
samples, no-baseline cases, and low-quality baseline flags by subject,
media, trial, condition, or any selected grouping variables.
