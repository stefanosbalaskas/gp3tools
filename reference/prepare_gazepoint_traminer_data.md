# Prepare AOI sequences for TraMineR-style workflows

Convert long AOI observations into one row per sequence and one column
per ordered state position. If TraMineR is installed and
`as_traminer = TRUE`, the function also returns a TraMineR sequence
object.

## Usage

``` r
prepare_gazepoint_traminer_data(
  data,
  aoi_col,
  sequence_cols,
  time_col = NULL,
  include_missing = FALSE,
  missing_label = "missing",
  collapse_repeats = FALSE,
  state_prefix = "state_",
  as_traminer = FALSE
)
```

## Arguments

- data:

  Long-format AOI data.

- aoi_col:

  AOI/state column.

- sequence_cols:

  Columns defining each sequence.

- time_col:

  Optional ordering column.

- include_missing:

  Should missing AOIs be kept as a state?

- missing_label:

  Label used for retained missing AOIs.

- collapse_repeats:

  Should consecutive repeated states be collapsed?

- state_prefix:

  Prefix for wide state columns.

- as_traminer:

  Should TraMineR::seqdef() be called if available?

## Value

A list containing wide data, state columns, alphabet, and optionally a
TraMineR sequence object.
