# Audit Gazepoint pupil-response overlap risk

Check whether event-related pupil-response windows may overlap.

## Usage

``` r
audit_gazepoint_pupil_overlap_risk(
  data,
  group_cols = "subject",
  trial_col = "trial_global",
  time_col = "time",
  event_time_cols = c("stimulus_onset_time", "target_onset_time", "response_time"),
  window_start_ms = 0,
  window_end_ms = 2000,
  min_event_gap_ms = 1000,
  exclude_col = "excluded_trial",
  include_excluded = FALSE
)
```

## Arguments

- data:

  A Gazepoint sample-level data frame.

- group_cols:

  Character vector of grouping columns, usually `"subject"`.

- trial_col:

  Name of the trial identifier column.

- time_col:

  Name of the within-trial time column.

- event_time_cols:

  Character vector of event-time columns, in ms.

- window_start_ms:

  Response-window start relative to each event, in ms.

- window_end_ms:

  Response-window end relative to each event, in ms.

- min_event_gap_ms:

  Minimum acceptable event-to-event gap in ms.

- exclude_col:

  Optional logical exclusion column.

- include_excluded:

  Logical. If `FALSE`, rows marked by `exclude_col` are removed before
  the audit when that column exists.

## Value

A named list containing `events`, `event_gaps`, `by_trial`, and
`summary` tibbles.

## Details

This function is designed as a deconvolution/readiness gate. It checks
whether events within the same trial are too close together and whether
their response windows overlap. If no usable event-time values are
found, the function returns a clean audit with
`overlap_assessment_status = "no_usable_event_times"`.
