# Summarise Gazepoint pupil responses within time windows

Aggregates processed Gazepoint pupil data into user-defined analysis
windows, typically after
[`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md),
[`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md),
[`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md),
and
[`smooth_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_pupil.md).
The function can summarise raw, interpolated, baseline-corrected,
percent-change, or smoothed pupil columns.

## Usage

``` r
summarise_gazepoint_pupil_windows(
  data,
  pupil_col = NULL,
  time_col = NULL,
  windows = c(0, 500, 1000, 2000),
  group_cols = c("subject", "media_id"),
  include_window_end = FALSE,
  min_valid_samples = 1
)
```

## Arguments

- data:

  A Gazepoint master table or processed pupil table.

- pupil_col:

  Optional name of the pupil column to summarise. If `NULL`, the
  function detects one of `pupil_smoothed`, `pupil_baseline_corrected`,
  `pupil_baseline_percent_change`, `pupil_interpolated`,
  `pupil_for_preprocessing`, `mean_pupil`, `pupil`, `pupil_raw`,
  `left_pupil`, or `right_pupil`.

- time_col:

  Optional name of the time column used for assigning samples to
  windows. If `NULL`, the function detects one of `time_relative_ms`,
  `relative_time_ms`, `event_time_ms`, `time_ms`, `time`, `time_orig`,
  or `time_orig_ms`.

- windows:

  Window definitions. Either a numeric vector of breakpoints, such as
  `c(0, 500, 1000, 2000)`, or a data frame with window start and end
  columns. Supported names include `window_start_ms`, `window_start`,
  `start_ms`, or `start`, and `window_end_ms`, `window_end`, `end_ms`,
  or `end`. A `window_label` or `label` column is optional.

- group_cols:

  Character vector of grouping columns. Standard roles such as
  `"subject"`, `"media_id"`, `"trial"`, and `"trial_global"` are
  internally standardised when available. Other columns, such as
  `"condition"` or `"AOI"`, can also be used if present in `data`. Use
  `character(0)` for overall window summaries.

- include_window_end:

  Logical. If `FALSE`, windows are left-closed and right-open:
  `[start, end)`. If `TRUE`, the end point is included: `[start, end]`.
  Defaults to `FALSE`.

- min_valid_samples:

  Minimum number of finite pupil samples required for a window to be
  labelled `"valid"`. Defaults to `1`.

## Value

A tibble with one row per group-by-window combination present in the
data.

## Examples

``` r
if (FALSE) { # \dontrun{
pupil_windows <- summarise_gazepoint_pupil_windows(
  smoothed_pupil,
  pupil_col = "pupil_smoothed",
  windows = c(0, 500, 1000, 2000),
  group_cols = c("subject", "media_id")
)
} # }
```
