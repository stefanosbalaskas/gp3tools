# Compute AOI entropy metrics

Computes spatial AOI entropy, directed transition entropy, and
conditional transition entropy for Gazepoint-style AOI sequences. The
function is useful for quantifying how concentrated, dispersed, or
predictable gaze allocation is across Areas of Interest.

## Usage

``` r
compute_gazepoint_aoi_entropy(
  data,
  aoi_col,
  group_cols = NULL,
  time_col = NULL,
  include_missing = FALSE,
  missing_label = "missing",
  collapse_repeats = FALSE,
  log_base = 2
)
```

## Arguments

- data:

  A data frame containing AOI observations.

- aoi_col:

  Character scalar. Column containing AOI labels.

- group_cols:

  Optional character vector of grouping columns, such as participant,
  trial, stimulus, or condition columns.

- time_col:

  Optional character scalar. If supplied, observations are ordered by
  this column within each group before transitions are computed.

- include_missing:

  Logical. If `TRUE`, missing or empty AOI labels are retained as
  `missing_label`; otherwise they are removed.

- missing_label:

  Character scalar used when `include_missing = TRUE`.

- collapse_repeats:

  Logical. If `TRUE`, consecutive identical AOI labels are collapsed
  before transition entropy is computed.

- log_base:

  Numeric scalar. Base of the logarithm used for entropy.

## Value

A data frame with one row per group and entropy/count columns.

## Examples

``` r
dat <- data.frame(
  subject = "S01",
  trial = "T01",
  time = 1:6,
  AOI = c("A", "A", "B", "C", "B", "A")
)

compute_gazepoint_aoi_entropy(
  dat,
  aoi_col = "AOI",
  group_cols = c("subject", "trial"),
  time_col = "time"
)
#>   subject trial n_observations n_aoi spatial_entropy spatial_entropy_norm
#> 1     S01   T01              6     3        1.459148            0.9206198
#>   n_transitions n_transition_types transition_entropy transition_entropy_norm
#> 1             5                  5           2.321928                       1
#>   conditional_transition_entropy conditional_transition_entropy_norm
#> 1                            0.8                           0.5047438
#>   entropy_status
#> 1             ok
```
