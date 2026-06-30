# Compute an AOI transition matrix

Compute an AOI transition matrix

## Usage

``` r
compute_transition_matrix(
  data,
  group_cols = "MEDIA_ID",
  aoi_col = "AOI",
  time_col = "TIME",
  collapse_repeats = TRUE
)
```

## Arguments

- data:

  A Gazepoint data frame with AOI labels.

- group_cols:

  Columns defining independent sequences.

- aoi_col:

  AOI column name.

- time_col:

  Time column name.

- collapse_repeats:

  If `TRUE`, consecutive identical AOI labels are reduced to one visit
  before transitions are counted.

## Value

A tibble with `from`, `to`, `n`, and `prob`.
