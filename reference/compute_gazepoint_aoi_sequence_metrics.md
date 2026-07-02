# Compute AOI sequence metrics

Computes compact scanpath-style descriptors from Gazepoint AOI
sequences, including sequence length, AOI visits, transitions, revisits,
first and last AOI, dominant AOI, and run-length summaries.

## Usage

``` r
compute_gazepoint_aoi_sequence_metrics(
  data,
  aoi_col,
  group_cols = NULL,
  time_col = NULL,
  include_missing = FALSE,
  missing_label = "missing",
  collapse_repeats = TRUE
)
```

## Arguments

- data:

  A data frame containing AOI observations.

- aoi_col:

  Character scalar. Column containing AOI labels.

- group_cols:

  Optional character vector of grouping columns.

- time_col:

  Optional character scalar. If supplied, observations are ordered by
  this column within each group.

- include_missing:

  Logical. If `TRUE`, missing or empty AOI labels are retained as
  `missing_label`; otherwise they are removed.

- missing_label:

  Character scalar used when `include_missing = TRUE`.

- collapse_repeats:

  Logical. If `TRUE`, consecutive identical AOI labels are collapsed
  before visit, transition, and revisit metrics are computed.

## Value

A data frame with one row per group and sequence-metric columns.

## Examples

``` r
dat <- data.frame(
  subject = "S01",
  trial = "T01",
  time = 1:6,
  AOI = c("A", "A", "B", "A", "C", "C")
)

compute_gazepoint_aoi_sequence_metrics(
  dat,
  aoi_col = "AOI",
  group_cols = c("subject", "trial"),
  time_col = "time"
)
#> Warning: internal error 1 in R_decompress1 with libdeflate
#> Error: lazy-load database 'C:/Users/Stefanos-PC/AppData/Local/R/win-library/4.6/gp3tools/R/gp3tools.rdb' is corrupt
```
