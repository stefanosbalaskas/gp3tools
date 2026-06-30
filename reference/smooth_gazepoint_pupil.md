# Smooth Gazepoint pupil data

Applies sample-based rolling smoothing to a Gazepoint pupil time series,
typically after
[`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md),
[`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md),
and optionally
[`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md).
The function preserves the original pupil column and adds
smoothed-output columns.

## Usage

``` r
smooth_gazepoint_pupil(
  data,
  pupil_col = NULL,
  time_col = NULL,
  group_cols = c("subject", "media_id"),
  window_samples = 5,
  method = c("mean", "median"),
  align = c("center", "right", "left"),
  min_points = 1,
  preserve_missing = TRUE
)
```

## Arguments

- data:

  A Gazepoint master table, preferably after pupil preprocessing.

- pupil_col:

  Optional name of the pupil column to smooth. If `NULL`, the function
  detects one of `pupil_baseline_corrected`,
  `pupil_baseline_percent_change`, `pupil_interpolated`,
  `pupil_for_preprocessing`, `mean_pupil`, `pupil`, `pupil_raw`,
  `left_pupil`, or `right_pupil`.

- time_col:

  Optional name of the time column. If `NULL`, the function detects one
  of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.

- group_cols:

  Character vector of grouping columns used to keep smoothing within
  independent time series. Defaults to `c("subject", "media_id")`.
  Columns such as `"trial"` or `"trial_global"` can be added when
  available. Use `character(0)` for global smoothing.

- window_samples:

  Number of samples in the rolling smoothing window. Defaults to `5`.

- method:

  Smoothing statistic. One of `"mean"` or `"median"`. Defaults to
  `"mean"`.

- align:

  Window alignment. One of `"center"`, `"right"`, or `"left"`. Defaults
  to `"center"`.

- min_points:

  Minimum number of finite values required inside a window to return a
  smoothed value. Defaults to `1`.

- preserve_missing:

  Logical. If `TRUE`, rows with missing/non-finite input remain missing
  in `pupil_smoothed`. Defaults to `TRUE`.

## Value

A tibble containing the original data plus pupil-smoothing columns.

## Examples

``` r
if (FALSE) { # \dontrun{
smoothed <- smooth_gazepoint_pupil(
  baseline_corrected,
  pupil_col = "pupil_baseline_corrected",
  window_samples = 5,
  method = "mean"
)

dplyr::count(smoothed, pupil_smoothing_status)
} # }
```
