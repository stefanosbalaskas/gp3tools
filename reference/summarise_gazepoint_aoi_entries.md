# Summarise Gazepoint AOI entry episodes

Convert sample-level AOI states into AOI entry episodes. An entry starts
whenever the AOI state changes within a subject, media, trial, or other
grouping unit.

## Usage

``` r
summarise_gazepoint_aoi_entries(
  data,
  aoi_col = NULL,
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  include_non_aoi = TRUE,
  non_aoi_values = c("non_aoi", "none", "background", "outside", "outside_aoi",
    "missing", "missing_aoi"),
  missing_aoi_label = "missing_aoi"
)
```

## Arguments

- data:

  A Gazepoint master/sample-level data frame.

- aoi_col:

  Name of the AOI-state column. If `NULL`, the function tries
  `aoi_current`, `AOI`, and `aoi_state`.

- time_col:

  Name of the time column, in milliseconds.

- group_cols:

  Character vector of columns defining independent sequences, usually
  subject/media/trial.

- include_non_aoi:

  Logical. If `TRUE`, non-AOI/background episodes are retained. If
  `FALSE`, they are removed after entry order and neighbouring states
  have been computed.

- non_aoi_values:

  Character vector of AOI labels treated as background or non-AOI
  states.

- missing_aoi_label:

  Label used when the AOI value is missing.

## Value

A tibble with one row per AOI entry episode.
