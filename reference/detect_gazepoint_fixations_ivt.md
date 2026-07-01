# Detect simple I-VT fixations from gaze samples

Apply a lightweight velocity-threshold fixation detector to ordered gaze
samples. This helper is intended for exploratory checks and teaching; it
does not replace dedicated event-detection packages or Gazepoint's
native fixation export.

## Usage

``` r
detect_gazepoint_fixations_ivt(
  data,
  x_col,
  y_col,
  time_col,
  group_cols = NULL,
  velocity_threshold = 0.01,
  min_duration_ms = 60,
  distance_scale = 1,
  time_scale = 1
)
```

## Arguments

- data:

  A sample-level gaze data frame.

- x_col:

  Horizontal gaze coordinate column.

- y_col:

  Vertical gaze coordinate column.

- time_col:

  Time column, in milliseconds by default.

- group_cols:

  Optional columns defining independent recordings/trials.

- velocity_threshold:

  Maximum velocity for a sample-to-sample interval to be treated as
  fixation-like.

- min_duration_ms:

  Minimum fixation duration.

- distance_scale:

  Multiplicative scale applied to coordinate distances.

- time_scale:

  Multiplicative scale applied to time differences.

## Value

A fixation-level data frame.
