# Compute AOI sequence complexity metrics

Compute compact sequence-complexity summaries from AOI labels. This
helper complements transition and entropy summaries by returning simple,
interpretable indices such as type-token ratio, transition density,
normalized entropy, and a combined complexity index.

## Usage

``` r
compute_gazepoint_sequence_complexity(
  data = NULL,
  sequence = NULL,
  aoi_col = NULL,
  group_cols = NULL,
  time_col = NULL,
  include_missing = FALSE,
  missing_label = "missing",
  collapse_repeats = FALSE
)
```

## Arguments

- data:

  Optional data frame containing AOI observations.

- sequence:

  Optional AOI sequence vector. Used when `data` is not supplied.

- aoi_col:

  Name of the AOI column when `data` is supplied.

- group_cols:

  Optional grouping columns.

- time_col:

  Optional time/order column.

- include_missing:

  Should missing AOI labels be retained as a state?

- missing_label:

  Label used when retaining missing AOIs.

- collapse_repeats:

  Should consecutive repeated AOI labels be collapsed?

## Value

A data frame with sequence length, unique-state count, entropy,
transition density, type-token ratio, and complexity index.
