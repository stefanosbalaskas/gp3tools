# Summarise cluster-based permutation results

Create compact reporting tables from the output of
[`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).
The function returns an overview table, all observed clusters,
significant clusters, time-course summary, permutation-distribution
summary, settings table, and circularity warning.

## Usage

``` r
summarise_gazepoint_clusters(
  result,
  alpha = 0.05,
  round_digits = NULL,
  include_timecourse = TRUE
)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- alpha:

  Cluster-level significance threshold.

- round_digits:

  Optional number of digits for rounding numeric reporting columns. If
  `NULL`, no rounding is applied.

- include_timecourse:

  Logical. If `TRUE`, include the full observed time-course table in the
  returned object.

## Value

A list of summary tables.

## Details

Cluster-based permutation tests are intended for time-course inference.
They should not be used to discover a confirmatory time window and then
test that same window again in a second confirmatory model.
