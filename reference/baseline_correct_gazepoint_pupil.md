# Baseline-correct Gazepoint pupil data

Computes baseline-corrected pupil columns from a Gazepoint pupil time
series, typically after
[`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md)
and
[`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md).
Baselines can be defined either by a time window, such as `c(-200, 0)`,
or by a user-supplied logical baseline/pre-stimulus flag column.

## Usage

``` r
baseline_correct_gazepoint_pupil(
  data,
  pupil_col = NULL,
  time_col = NULL,
  baseline_time_col = NULL,
  baseline_window = c(-200, 0),
  baseline_flag_col = NULL,
  group_cols = c("subject", "media_id"),
  baseline_method = c("mean", "median"),
  min_baseline_samples = 1
)
```

## Arguments

- data:

  A Gazepoint master table, preferably after
  [`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md).

- pupil_col:

  Optional name of the pupil column to baseline-correct. If `NULL`, the
  function detects one of `pupil_interpolated`,
  `pupil_for_preprocessing`, `mean_pupil`, `pupil`, `pupil_raw`,
  `left_pupil`, or `right_pupil`.

- time_col:

  Optional name of the main time column. If `NULL`, the function detects
  one of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.

- baseline_time_col:

  Optional name of the time column used for selecting baseline samples.
  If `NULL`, the function detects relative-time columns first, then
  falls back to `time_col`.

- baseline_window:

  Numeric vector of length two giving the baseline window in
  milliseconds. Defaults to `c(-200, 0)`. This can also be set to
  post-onset or early-window values such as `c(0, 200)` when no
  pre-stimulus period is available.

- baseline_flag_col:

  Optional logical column identifying baseline samples. If supplied,
  this takes priority over `baseline_window`.

- group_cols:

  Character vector of grouping columns used to compute one baseline per
  independent time series. Defaults to `c("subject", "media_id")`.
  Columns such as `"trial"` or `"trial_global"` can be added when
  available. Use `character(0)` for one global baseline.

- baseline_method:

  Baseline statistic. One of `"mean"` or `"median"`. Defaults to
  `"mean"`.

- min_baseline_samples:

  Minimum number of valid baseline samples required to compute a
  baseline. Defaults to `1`.

## Value

A tibble containing the original data plus baseline-correction columns.

## Examples

``` r
if (FALSE) { # \dontrun{
flagged <- flag_gazepoint_pupil(master)
interpolated <- interpolate_gazepoint_pupil(flagged)

corrected <- baseline_correct_gazepoint_pupil(
  interpolated,
  baseline_window = c(-200, 0)
)

corrected <- baseline_correct_gazepoint_pupil(
  interpolated,
  baseline_window = c(0, 200)
)
} # }
```
