# Compute basic saccade metrics from fixation coordinates

Compute between-fixation displacement metrics from ordered fixation
coordinates. The function does not perform raw event detection; it
assumes that fixation-level coordinates are already available, for
example from Gazepoint fixation exports.

## Usage

``` r
compute_gazepoint_saccade_metrics(
  data,
  x_col,
  y_col,
  group_cols = NULL,
  time_col = NULL,
  start_time_col = NULL,
  end_time_col = NULL,
  distance_scale = 1,
  drop_missing = TRUE
)
```

## Arguments

- data:

  A fixation-level data frame.

- x_col:

  Name of the horizontal fixation-coordinate column.

- y_col:

  Name of the vertical fixation-coordinate column.

- group_cols:

  Optional columns defining independent scanpaths, such as participant
  and trial.

- time_col:

  Optional column used to order fixations and compute inter-fixation
  time differences.

- start_time_col:

  Optional fixation-start column.

- end_time_col:

  Optional fixation-end column. If both start and end columns are
  supplied, saccade duration is computed as the next fixation start
  minus the current fixation end.

- distance_scale:

  Multiplicative scale applied to coordinate distances.

- drop_missing:

  Should rows with missing coordinates be dropped?

## Value

A data frame with one row per between-fixation movement.
