# Compute Gazepoint AOI transition matrices

Compute AOI transition count and probability matrices from sample-level
Gazepoint AOI data, AOI-entry tables, or AOI-sequence tables. The
function returns both matrix and long-table forms.

## Usage

``` r
compute_gazepoint_aoi_transition_matrix(
  data,
  aoi_col = NULL,
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  by_cols = NULL,
  include_non_aoi = TRUE,
  include_self_transitions = TRUE,
  states = NULL,
  time_window = NULL,
  non_aoi_values = c("non_aoi", "none", "background", "outside", "outside_aoi",
    "missing", "missing_aoi"),
  missing_aoi_label = "missing_aoi"
)
```

## Arguments

- data:

  A Gazepoint sample-level data frame, AOI-entry table, or AOI-sequence
  table.

- aoi_col:

  Name of the AOI-state column. Used only when `data` is sample-level
  data. If `NULL`, the function tries `aoi_current`, `AOI`, and
  `aoi_state`.

- time_col:

  Name of the time column, in milliseconds. Used only when `data` is
  sample-level data.

- group_cols:

  Character vector of columns defining independent AOI sequences,
  usually subject/media/trial.

- by_cols:

  Optional character vector of columns used to compute separate
  matrices, for example `condition` or `MEDIA_ID`.

- include_non_aoi:

  Logical. If `TRUE`, non-AOI/background states are included in the
  transition matrix.

- include_self_transitions:

  Logical. If `TRUE`, same-state transitions are retained. These can
  occur after non-AOI states are removed.

- states:

  Optional character vector giving the desired row/column order for the
  transition matrices.

- time_window:

  Optional numeric vector of length 2 giving an entry-start time window
  in milliseconds.

- non_aoi_values:

  Character vector of AOI labels treated as background or non-AOI
  states.

- missing_aoi_label:

  Label used when the AOI value is missing.

## Value

A list containing count matrices, probability matrices, and long-form
transition counts/probabilities.
