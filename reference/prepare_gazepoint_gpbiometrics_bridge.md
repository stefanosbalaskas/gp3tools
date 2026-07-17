# Prepare gp3tools gaze output for a gpbiometrics workflow

Standardises participant, trial, time, gaze, AOI, pupil, and validity
fields without changing the underlying measurements.

## Usage

``` r
prepare_gazepoint_gpbiometrics_bridge(
  gaze_data,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  time_unit = c("auto", "seconds", "milliseconds"),
  x_col = NULL,
  y_col = NULL,
  aoi_col = NULL,
  pupil_col = NULL,
  validity_col = NULL,
  keep_cols = NULL
)
```

## Arguments

- gaze_data:

  Sample-level gp3tools gaze/master data.

- participant_col, trial_col, time_col:

  Explicit source columns.

- time_unit:

  Time unit for the source time column.

- x_col, y_col, aoi_col, pupil_col, validity_col:

  Optional source columns.

- keep_cols:

  Additional source columns to retain.

## Value

A data frame of class `"gazepoint_gpbiometrics_bridge"`.
