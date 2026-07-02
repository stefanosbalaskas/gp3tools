# Compute AOI sequence distance

Computes a lightweight edit distance between two AOI sequences using a
vector-based Levenshtein distance. This provides a simple scanpath
dissimilarity measure without requiring heavy sequence-analysis
dependencies.

## Usage

``` r
compute_gazepoint_sequence_distance(
  sequence_a,
  sequence_b,
  ignore_missing = TRUE,
  missing_label = "missing",
  collapse_repeats = FALSE,
  substitution_cost = 1,
  insertion_cost = 1,
  deletion_cost = 1
)
```

## Arguments

- sequence_a:

  Character, factor, or atomic vector representing the first AOI
  sequence.

- sequence_b:

  Character, factor, or atomic vector representing the second AOI
  sequence.

- ignore_missing:

  Logical. If `TRUE`, missing and empty labels are removed.

- missing_label:

  Character scalar used when `ignore_missing = FALSE`.

- collapse_repeats:

  Logical. If `TRUE`, consecutive identical labels are collapsed before
  distance is computed.

- substitution_cost:

  Numeric scalar substitution cost.

- insertion_cost:

  Numeric scalar insertion cost.

- deletion_cost:

  Numeric scalar deletion cost.

## Value

A one-row data frame with edit distance, normalized distance, and
sequence lengths.

## Examples

``` r
compute_gazepoint_sequence_distance(
  sequence_a = c("Claim", "Evidence", "CTA"),
  sequence_b = c("Claim", "CTA", "Evidence")
)
#> Warning: internal error 1 in R_decompress1 with libdeflate
#> Error: lazy-load database 'C:/Users/Stefanos-PC/AppData/Local/R/win-library/4.6/gp3tools/R/gp3tools.rdb' is corrupt
```
