# Prepare Gazepoint master data for eyetools-style workflows

Convert a `gp3tools` master table into a dependency-free,
eyetools-friendly sample-level table. The returned data frame keeps one
row per sample and creates standard participant, trial, time,
gaze-coordinate, binocular coordinate, pupil, AOI, fixation, event,
validity, trackloss, and status columns.

## Usage

``` r
prepare_gazepoint_eyetools_data(
  data,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  x_col = NULL,
  y_col = NULL,
  left_x_col = NULL,
  left_y_col = NULL,
  right_x_col = NULL,
  right_y_col = NULL,
  pupil_col = NULL,
  left_pupil_col = NULL,
  right_pupil_col = NULL,
  media_col = NULL,
  condition_col = NULL,
  aoi_col = NULL,
  fixation_col = NULL,
  event_col = NULL,
  validity_cols = NULL,
  trackloss_col = NULL,
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  missing_aoi_label = "missing_aoi",
  keep_original_cols = TRUE
)
```

## Arguments

- data:

  A Gazepoint master table or sample-level gaze data frame.

- participant_col:

  Participant/subject identifier column.

- trial_col:

  Trial identifier column. If `NULL`, a trial identifier is created from
  `media_col` when available.

- time_col:

  Sample time column.

- x_col:

  Optional primary gaze x-coordinate column.

- y_col:

  Optional primary gaze y-coordinate column.

- left_x_col:

  Optional left-eye x-coordinate column.

- left_y_col:

  Optional left-eye y-coordinate column.

- right_x_col:

  Optional right-eye x-coordinate column.

- right_y_col:

  Optional right-eye y-coordinate column.

- pupil_col:

  Optional primary pupil column.

- left_pupil_col:

  Optional left-eye pupil column.

- right_pupil_col:

  Optional right-eye pupil column.

- media_col:

  Optional media/stimulus identifier column.

- condition_col:

  Optional condition/grouping column.

- aoi_col:

  Optional AOI label/state column.

- fixation_col:

  Optional fixation identifier column.

- event_col:

  Optional event/marker column.

- validity_cols:

  Optional validity columns used to define trackloss.

- trackloss_col:

  Optional existing trackloss column. If supplied, it is used directly
  after coercion to logical.

- screen_x_range:

  Numeric length-2 vector defining the screen x range.

- screen_y_range:

  Numeric length-2 vector defining the screen y range.

- missing_aoi_label:

  Label used for missing AOI values.

- keep_original_cols:

  Logical. If `TRUE`, original columns are retained after the standard
  adapter columns.

## Value

A tibble with class `gp3_eyetools_data`.
