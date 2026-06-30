# Prepare Gazepoint AOI sequences for semi-Markov modelling

Convert ordered AOI/state observations into state-visit and
transition-level semi-Markov data. Consecutive repeated states can be
collapsed into dwell episodes, producing one row per state visit with
dwell duration and next-state information.

## Usage

``` r
prepare_gazepoint_semimarkov_data(
  data,
  state_col = NULL,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  duration_col = NULL,
  sequence_id_cols = NULL,
  covariate_cols = NULL,
  exclude_states = c("missing", "missing_aoi", "missing_coordinate", "trackloss",
    "track_loss"),
  missing_state_label = NULL,
  collapse_repeated_states = TRUE,
  include_terminal_states = TRUE,
  terminal_next_state_label = "END",
  name = "gazepoint_semimarkov_data"
)
```

## Arguments

- data:

  A data frame containing ordered AOI/state observations.

- state_col:

  AOI/state column. If `NULL`, common AOI/state columns are detected
  automatically.

- participant_col:

  Optional participant/subject column.

- trial_col:

  Optional trial/sequence column.

- time_col:

  Optional time/order column.

- duration_col:

  Optional sample-duration column. If supplied, dwell durations are
  computed by summing this column within each state visit.

- sequence_id_cols:

  Optional character vector of columns defining separate sequences. If
  `NULL`, participant and trial columns are used when available.

- covariate_cols:

  Optional character vector of covariate columns to carry into the
  state-visit and transition tables using the first value within each
  state visit.

- exclude_states:

  Character vector of states to exclude before creating state visits.

- missing_state_label:

  Optional label used to retain missing states. If `NULL`, missing/blank
  states are removed.

- collapse_repeated_states:

  Logical. If `TRUE`, consecutive repeated states within a sequence are
  collapsed into a single dwell episode.

- include_terminal_states:

  Logical. If `TRUE`, the final state visit in each sequence is retained
  as a transition to `terminal_next_state_label`.

- terminal_next_state_label:

  Label used for the terminal next state.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_semimarkov_data`.
