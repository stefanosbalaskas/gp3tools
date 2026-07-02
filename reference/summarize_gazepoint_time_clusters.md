# Summarize Gazepoint time clusters

`summarize_gazepoint_time_clusters()` provides a compact US-spelling
summary of cluster-permutation output from
[`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

## Usage

``` r
summarize_gazepoint_time_clusters(result, alpha = 0.05)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- alpha:

  Significance threshold used for the descriptive `cluster_significant`
  flag.

## Value

A data frame with one row per cluster. If no clusters are present, an
empty data frame with the expected columns is returned.

## Examples

``` r
# See run_gazepoint_cluster_permutation() for the inferential workflow.
```
