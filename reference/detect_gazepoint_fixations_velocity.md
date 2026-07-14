# Detect fixations with a velocity-threshold algorithm

Converts sample-level gaze coordinates into fixation events using an
I-VT-style velocity threshold. The function is intended as a high-level
event-table companion to
[`detect_gazepoint_fixations_ivt()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_fixations_ivt.md).

## Usage

``` r
detect_gazepoint_fixations_velocity(
  all_gaze,
  id_col = "USER_ID",
  x_col = "FPOGX",
  y_col = "FPOGY",
  ts_col = "TIME",
  vmax = 10,
  min_duration = 50,
  group_cols = NULL,
  time_unit = c("auto", "seconds", "milliseconds"),
  x_scale = 1,
  y_scale = 1,
  return = c("events", "samples", "both"),
  keep_single_sample = FALSE
)
```

## Arguments

- all_gaze:

  A data frame containing sample-level gaze data.

- id_col:

  Participant identifier column.

- x_col:

  Horizontal gaze-coordinate column.

- y_col:

  Vertical gaze-coordinate column.

- ts_col:

  Timestamp column.

- vmax:

  Maximum velocity classified as fixation. The threshold is in scaled
  coordinate units per second.

- min_duration:

  Minimum fixation duration in milliseconds.

- group_cols:

  Optional additional grouping columns, such as stimulus or trial
  identifiers.

- time_unit:

  Timestamp unit. `"auto"` infers seconds versus milliseconds from
  positive timestamp differences.

- x_scale, y_scale:

  Multipliers applied to coordinate differences before velocity is
  calculated. Use these to convert native coordinates to visual degrees
  when an appropriate conversion is available.

- return:

  Return fixation `"events"`, sample labels, or `"both"`.

- keep_single_sample:

  Retain single-sample events when they satisfy `min_duration`.

## Value

A tibble of fixation events, a labelled sample table, or a list
containing both.

## Examples

``` r
gaze <- data.frame(
  USER_ID = "P01",
  TIME = seq(0, 0.19, by = 0.01),
  FPOGX = c(rep(0.25, 10), rep(0.75, 10)),
  FPOGY = 0.50
)
detect_gazepoint_fixations_velocity(
  gaze,
  vmax = 5,
  min_duration = 40
)
#> # A tibble: 2 × 13
#>   USER_ID fixation_id start_time end_time duration duration_ms n_samples mean_x
#>   <chr>         <int>      <dbl>    <dbl>    <dbl>       <dbl>     <int>  <dbl>
#> 1 P01               1       0        0.09      100         100        10   0.25
#> 2 P01               2       0.11     0.19       90          90         9   0.75
#> # ℹ 5 more variables: mean_y <dbl>, median_velocity <dbl>, max_velocity <dbl>,
#> #   velocity_threshold <dbl>, algorithm <chr>
```
