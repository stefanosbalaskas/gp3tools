# Prepare Gazepoint AOI/state sequences for HMM-style workflows

Convert ordered Gazepoint AOI/state observations into a dependency-free
hidden-Markov-model-ready structure. The helper creates ordered sequence
data, transition tables, initial-state probabilities,
transition-probability matrices, and observation/emission summaries. It
does not fit an HMM and does not import external HMM packages.

## Usage

``` r
prepare_gazepoint_hmm_data(
  data,
  state_col = NULL,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  observation_cols = NULL,
  sequence_id_cols = NULL,
  covariate_cols = NULL,
  state_order = NULL,
  exclude_states = c("missing", "missing_aoi", "missing_coordinate", "trackloss",
    "track_loss"),
  missing_state_label = NULL,
  scale_numeric_observations = FALSE,
  include_terminal_state = FALSE,
  terminal_state_label = "END",
  name = "gazepoint_hmm_data"
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

- observation_cols:

  Optional observation columns to carry into the HMM data. If `NULL`,
  common gaze, pupil, fixation, and validity columns are detected
  automatically.

- sequence_id_cols:

  Optional character vector of columns defining separate sequences. If
  `NULL`, participant and trial columns are used when available.

- covariate_cols:

  Optional covariate columns to carry into the HMM data.

- state_order:

  Optional preferred hidden-state order.

- exclude_states:

  Character vector of states to exclude before sequence construction.

- missing_state_label:

  Optional label used to retain missing states. If `NULL`, missing/blank
  states are removed.

- scale_numeric_observations:

  Logical. If `TRUE`, z-scored versions of numeric observation columns
  are added with suffix `_z`.

- include_terminal_state:

  Logical. If `TRUE`, each sequence contributes a final transition to
  `terminal_state_label`.

- terminal_state_label:

  Terminal-state label.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_hmm_data`.
