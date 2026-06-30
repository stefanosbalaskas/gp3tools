# Create a Gazepoint AOI Markov-chain object

Create a dependency-free Markov-chain-style object from AOI/state
sequences. The function computes transition counts, transition
probabilities, and matrix representations from ordered gaze/AOI states.
It does not require the external `markovchain` package; instead, it
returns a lightweight `gp3tools` object that can be inspected, exported,
or converted later.

## Usage

``` r
create_gazepoint_markovchain_object(
  data,
  state_col = NULL,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  sequence_id_cols = NULL,
  state_order = NULL,
  exclude_states = c("missing", "missing_aoi", "missing_coordinate", "trackloss",
    "track_loss"),
  missing_state_label = NULL,
  include_self_transitions = TRUE,
  laplace = 0,
  empty_state_handling = c("self", "zero", "NA"),
  name = "gazepoint_markovchain"
)
```

## Arguments

- data:

  A data frame containing ordered AOI/state observations.

- state_col:

  AOI/state column. If `NULL`, common AOI/state column names are
  detected automatically.

- participant_col:

  Optional participant/subject column.

- trial_col:

  Optional trial/sequence column.

- time_col:

  Optional time/order column.

- sequence_id_cols:

  Optional character vector of columns defining separate sequences. If
  `NULL`, participant and trial columns are used when available.

- state_order:

  Optional character vector giving the preferred state order in the
  output matrices.

- exclude_states:

  Character vector of states to exclude before transition calculation.

- missing_state_label:

  Optional label used to retain missing states. If `NULL`, missing/blank
  states are removed.

- include_self_transitions:

  Logical. If `FALSE`, transitions from a state to the same state are
  removed.

- laplace:

  Numeric smoothing value added to all transition cells when computing
  probabilities.

- empty_state_handling:

  How to handle states with no outgoing transitions: `"self"` creates an
  absorbing self-transition, `"zero"` leaves a zero row, and `"NA"`
  returns `NA` probabilities for that row.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_markovchain_object`.
