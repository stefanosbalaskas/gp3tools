# Offline gaze recalibration using known target coordinates

Apply an offline drift-correction shift to Gazepoint gaze coordinates
using known fixation/check-target coordinates. For each group, the
helper estimates the horizontal and vertical gaze offset from valid
target samples and applies the correction to all gaze samples in the
same group.

## Usage

``` r
recalibrate_gazepoint_gaze(
  data,
  x_col,
  y_col,
  target_x_col,
  target_y_col,
  time_col = NULL,
  grouping_cols = NULL,
  calibration_col = NULL,
  calibration_value = NULL,
  method = c("median_shift", "mean_shift"),
  min_valid_points = 3L,
  max_shift = NULL,
  output_x_col = "gaze_x_recalibrated",
  output_y_col = "gaze_y_recalibrated",
  dx_col = "gaze_recalibration_dx",
  dy_col = "gaze_recalibration_dy",
  shift_col = "gaze_recalibration_shift",
  error_before_col = "gaze_error_before_recalibration",
  error_after_col = "gaze_error_after_recalibration",
  status_col = "gaze_recalibration_status",
  overwrite = FALSE,
  name = "gazepoint_gaze_recalibration"
)
```

## Arguments

- data:

  A data frame containing gaze and target coordinates.

- x_col:

  Horizontal gaze coordinate column.

- y_col:

  Vertical gaze coordinate column.

- target_x_col:

  Known horizontal target coordinate column.

- target_y_col:

  Known vertical target coordinate column.

- time_col:

  Optional time column used only for stable ordering.

- grouping_cols:

  Optional grouping columns used to estimate one correction per
  participant, trial, block, stimulus, or other unit.

- calibration_col:

  Optional column identifying rows to use for estimating the correction.

- calibration_value:

  Optional value in `calibration_col` identifying calibration/check
  rows. If `calibration_col` is supplied and `calibration_value = NULL`,
  logical `TRUE` rows are used.

- method:

  Shift estimator. `"median_shift"` uses median target-minus-gaze
  offsets; `"mean_shift"` uses mean offsets.

- min_valid_points:

  Minimum valid target/gaze pairs required per group.

- max_shift:

  Optional maximum Euclidean correction shift. If exceeded, the shift is
  reported but not applied.

- output_x_col:

  Corrected horizontal gaze output column.

- output_y_col:

  Corrected vertical gaze output column.

- dx_col:

  Estimated horizontal correction column.

- dy_col:

  Estimated vertical correction column.

- shift_col:

  Estimated Euclidean shift-distance column.

- error_before_col:

  Row-wise gaze-to-target error before correction.

- error_after_col:

  Row-wise gaze-to-target error after correction.

- status_col:

  Row-level recalibration status column.

- overwrite:

  Logical. If `FALSE`, existing output columns are protected.

- name:

  Character label stored in object attributes.

## Value

A tibble with recalibrated gaze columns and recalibration attributes.

## Details

This helper is useful only when known target coordinates are available,
for example from calibration checks, fixation targets, validation
targets, or drift-check trials.
