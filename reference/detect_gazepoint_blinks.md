# Detect blink intervals from pupil measurements

Detects blink-like periods using missing or non-positive pupil samples
and optional robust drop/recovery rules. Multiple pupil columns can be
supplied; their row-wise mean is used for detection.

## Usage

``` r
detect_gazepoint_blinks(
  all_gaze,
  pupil_col = NULL,
  ts_col = "TIME",
  id_col = "USER_ID",
  group_cols = NULL,
  min_duration = 50,
  z_thresh = 4,
  zero_threshold = 0,
  merge_gap_ms = 20,
  time_unit = c("auto", "seconds", "milliseconds"),
  include_rapid_changes = TRUE,
  return = c("events", "samples", "both")
)
```

## Arguments

- all_gaze:

  A data frame containing sample-level pupil data.

- pupil_col:

  Pupil column or columns. When `NULL`, common gp3tools and Gazepoint
  pupil names are detected automatically.

- ts_col:

  Timestamp column.

- id_col:

  Participant identifier column.

- group_cols:

  Optional additional grouping columns.

- min_duration:

  Minimum retained blink duration in milliseconds.

- z_thresh:

  Robust threshold for low values and rapid changes.

- zero_threshold:

  Values at or below this threshold are invalid.

- merge_gap_ms:

  Merge blink candidates separated by no more than this duration.

- time_unit:

  Timestamp unit.

- include_rapid_changes:

  Include robust rapid drop and recovery flags.

- return:

  Return event intervals, sample labels, or both.

## Value

A blink-event tibble, a labelled sample table, or both.

## Examples

``` r
pupil <- data.frame(
  USER_ID = "P01",
  TIME = seq(0, 0.19, by = 0.01),
  mean_pupil = c(rep(3.2, 7), NA, NA, NA, rep(3.2, 10))
)
detect_gazepoint_blinks(pupil, min_duration = 20)
#> # A tibble: 1 × 9
#>   USER_ID blink_id start_time end_time duration duration_ms n_samples reason 
#>   <chr>      <int>      <dbl>    <dbl>    <dbl>       <dbl>     <int> <chr>  
#> 1 P01            1       0.07     0.09       30          30         3 missing
#> # ℹ 1 more variable: pupil_columns <chr>
```
