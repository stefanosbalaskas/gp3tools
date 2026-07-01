# Flag unusual AOI sequences

Identify grouped AOI sequences that are unusually short, unusually long,
have high missingness, or contain very few unique AOI states. The
function is intended as a lightweight quality-control helper rather than
a definitive exclusion rule.

## Usage

``` r
flag_gazepoint_sequence_anomalies(
  data,
  aoi_col,
  group_cols,
  time_col = NULL,
  min_length = 2,
  max_length = NULL,
  max_missing_prop = 0.5,
  z_threshold = 3,
  min_unique_aoi = 1
)
```

## Arguments

- data:

  A data frame containing AOI observations.

- aoi_col:

  Name of the AOI column.

- group_cols:

  Columns defining each sequence.

- time_col:

  Optional time/order column.

- min_length:

  Minimum acceptable non-missing sequence length.

- max_length:

  Optional maximum acceptable non-missing sequence length.

- max_missing_prop:

  Maximum acceptable missing AOI proportion.

- z_threshold:

  Absolute z-score threshold for unusual sequence length.

- min_unique_aoi:

  Minimum number of unique AOI labels expected.

## Value

A data frame with sequence diagnostics and anomaly flags.
