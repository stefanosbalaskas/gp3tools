# Audit Gazepoint event and timing synchronisation

Create a publication-level audit of event timing, trial timing, and
event availability in a Gazepoint master table or sample-level export.

## Usage

``` r
audit_gazepoint_event_sync(
  data,
  time_col = "time",
  event_col = NULL,
  group_cols = c("subject", "media_id", "trial_global"),
  condition_col = NULL,
  expected_event_labels = NULL,
  onset_event_label = NULL,
  response_event_label = NULL,
  min_samples_per_unit = 1L,
  max_time_gap_ms = NULL
)
```

## Arguments

- data:

  A data frame containing sample-level Gazepoint data.

- time_col:

  Name of the time column.

- event_col:

  Optional event-label column. If `NULL`, the function tries to detect a
  common event column.

- group_cols:

  Columns defining a trial or recording unit.

- condition_col:

  Optional condition column.

- expected_event_labels:

  Optional character vector of expected event labels.

- onset_event_label:

  Optional event label identifying trial/stimulus onset.

- response_event_label:

  Optional event label identifying response events.

- min_samples_per_unit:

  Minimum number of samples expected per unit.

- max_time_gap_ms:

  Optional maximum allowed within-unit time gap in milliseconds.

## Value

A list with class `gp3_event_sync_audit` containing overview,
unit_summary, event_summary, expected_event_summary, flagged_units, and
settings tables.
