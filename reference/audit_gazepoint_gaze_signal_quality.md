# Audit Gazepoint gaze-signal quality

Create a publication-level audit of gaze coordinate availability,
validity, off-screen samples, and optional pupil availability.

## Usage

``` r
audit_gazepoint_gaze_signal_quality(
  data,
  subject_col = "subject",
  condition_col = NULL,
  group_cols = c("subject", "media_id", "trial_global"),
  x_col = NULL,
  y_col = NULL,
  validity_cols = NULL,
  pupil_col = NULL,
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  min_gaze_valid_prop = 0.7,
  max_missing_gaze_prop = 0.3,
  max_offscreen_prop = 0.3,
  min_pupil_valid_prop = 0.7
)
```

## Arguments

- data:

  A sample-level Gazepoint data frame.

- subject_col:

  Subject/participant identifier column.

- condition_col:

  Optional condition column.

- group_cols:

  Columns defining a recording, stimulus, trial, or analysis unit.

- x_col:

  Optional gaze-x coordinate column. If `NULL`, common Gazepoint aliases
  are detected.

- y_col:

  Optional gaze-y coordinate column. If `NULL`, common Gazepoint aliases
  are detected.

- validity_cols:

  Optional gaze-validity columns. If `NULL`, common Gazepoint validity
  columns are detected.

- pupil_col:

  Optional pupil column. If `NULL`, common pupil aliases are detected.

- screen_x_range:

  Numeric length-2 vector defining plausible on-screen x range.

- screen_y_range:

  Numeric length-2 vector defining plausible on-screen y range.

- min_gaze_valid_prop:

  Minimum acceptable gaze-validity proportion.

- max_missing_gaze_prop:

  Maximum acceptable missing-gaze proportion.

- max_offscreen_prop:

  Maximum acceptable off-screen proportion.

- min_pupil_valid_prop:

  Minimum acceptable valid-pupil proportion when a pupil column is
  available.

## Value

A list with class `gp3_gaze_signal_quality_audit` containing overview,
unit_summary, subject_summary, condition_summary, signal_issue_summary,
flagged_units, and settings tables.
