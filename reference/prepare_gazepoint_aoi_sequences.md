# Prepare Gazepoint AOI sequences

Create ordered AOI-state sequences from sample-level Gazepoint AOI data
or from the output of
[`summarise_gazepoint_aoi_entries()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_entries.md).
The output is transition-ready and includes the current AOI state,
previous state, next state, dwell time before transition, and
self-transition flags.

## Usage

``` r
prepare_gazepoint_aoi_sequences(
  data,
  aoi_col = NULL,
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  include_non_aoi = TRUE,
  non_aoi_values = c("non_aoi", "none", "background", "outside", "outside_aoi",
    "missing", "missing_aoi"),
  missing_aoi_label = "missing_aoi",
  include_terminal = TRUE
)
```

## Arguments

- data:

  A Gazepoint sample-level data frame or an AOI-entry table created by
  [`summarise_gazepoint_aoi_entries()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_entries.md).

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

- include_non_aoi:

  Logical. If `TRUE`, non-AOI/background states are retained. If
  `FALSE`, non-AOI/background states are removed before sequence and
  transition fields are computed.

- non_aoi_values:

  Character vector of AOI labels treated as background or non-AOI
  states.

- missing_aoi_label:

  Label used when the AOI value is missing.

- include_terminal:

  Logical. If `TRUE`, the final state of each sequence is retained with
  `next_state = NA`. If `FALSE`, terminal states are removed so that
  each output row represents an observed transition.

## Value

A tibble with ordered AOI sequence and transition fields.
