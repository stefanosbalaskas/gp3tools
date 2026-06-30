# Prepare Gazepoint master data for pupillometryR-style workflows

Convert a `gp3tools` master table into a dependency-free,
pupillometryR-friendly sample-level pupil table. The returned data frame
keeps one row per sample and creates standard participant, trial, time,
pupil, condition, event, baseline, validity, trackloss, and status
columns.

## Usage

``` r
prepare_gazepoint_pupillometryr_data(
  data,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  pupil_col = NULL,
  media_col = NULL,
  condition_col = NULL,
  event_col = NULL,
  baseline_col = NULL,
  validity_cols = NULL,
  pupil_status_col = NULL,
  trackloss_col = NULL,
  invalid_pupil_status = c("missing", "artifact", "blink", "trackloss", "track_loss",
    "invalid", "excluded", "bad", "outlier"),
  keep_original_cols = TRUE
)
```

## Arguments

- data:

  A Gazepoint master table or sample-level gaze/pupil data frame.

- participant_col:

  Participant/subject identifier column.

- trial_col:

  Trial identifier column. If `NULL`, a trial identifier is created from
  `media_col` when available.

- time_col:

  Sample time column.

- pupil_col:

  Pupil column to export.

- media_col:

  Optional media/stimulus identifier column.

- condition_col:

  Optional condition/grouping column.

- event_col:

  Optional event/marker column.

- baseline_col:

  Optional baseline-period indicator column.

- validity_cols:

  Optional validity columns used to define trackloss.

- pupil_status_col:

  Optional pupil-status column used to mark invalid pupil samples.

- trackloss_col:

  Optional existing trackloss column. If supplied, it is used directly
  after coercion to logical.

- invalid_pupil_status:

  Character values in `pupil_status_col` treated as invalid pupil
  samples.

- keep_original_cols:

  Logical. If `TRUE`, original columns are retained after the standard
  adapter columns.

## Value

A tibble with class `gp3_pupillometryr_data`.
