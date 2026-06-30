# Compute time-varying Gazepoint transition matrices

Compute transition-count and transition-probability matrices across time
windows. This helper is a convenience wrapper for studies where
AOI/state transitions are expected to vary over the course of a
stimulus, trial, or analysis window.

## Usage

``` r
compute_gazepoint_time_varying_transition_matrix(
  data,
  from_col = NULL,
  to_col = NULL,
  time_col = NULL,
  window_col = NULL,
  window_size_ms = NULL,
  by_cols = NULL,
  count_col = NULL,
  states = NULL,
  complete_states = TRUE,
  drop_self_transitions = FALSE,
  normalise = c("row", "global", "none"),
  name = "gazepoint_time_varying_transition_matrix"
)
```

## Arguments

- data:

  A data frame containing transition-level rows.

- from_col:

  Transition origin column. If `NULL`, common origin columns are
  detected automatically.

- to_col:

  Transition destination column. If `NULL`, common destination columns
  are detected automatically.

- time_col:

  Optional numeric time column used to construct windows when
  `window_col = NULL`.

- window_col:

  Optional existing time-window column.

- window_size_ms:

  Numeric window size used when `window_col = NULL`.

- by_cols:

  Optional grouping columns, such as subject, condition, trial, or
  stimulus.

- count_col:

  Optional count/weight column. If `NULL`, each row contributes one
  transition.

- states:

  Optional character vector of allowed states/AOIs. If `NULL`, states
  are detected from `from_col` and `to_col`.

- complete_states:

  If `TRUE`, complete all state-pair combinations within each time
  window and group.

- drop_self_transitions:

  If `TRUE`, remove transitions where origin and destination are the
  same.

- normalise:

  Probability normalisation. Options are `"row"`, `"global"`, and
  `"none"`.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_time_varying_transition_matrix`.
