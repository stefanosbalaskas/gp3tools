# Compute simple categorical sequence recurrence metrics

Compute lightweight recurrence metrics from AOI/state sequences. This is
a compact categorical recurrence helper, not a full replacement for
dedicated CRQA/RQA packages.

## Usage

``` r
compute_gazepoint_sequence_recurrence(
  data = NULL,
  sequence = NULL,
  aoi_col = NULL,
  group_cols = NULL,
  time_col = NULL,
  min_line = 2,
  include_missing = FALSE,
  missing_label = "missing"
)
```

## Arguments

- data:

  Optional long-format data frame.

- sequence:

  Optional AOI/state vector used when `data` is absent.

- aoi_col:

  AOI/state column when `data` is supplied.

- group_cols:

  Optional grouping columns.

- time_col:

  Optional ordering column.

- min_line:

  Minimum diagonal-line length for determinism.

- include_missing:

  Should missing states be retained?

- missing_label:

  Label used for retained missing states.

## Value

A data frame of recurrence metrics.
