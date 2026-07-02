# Run threshold-sensitivity checks for Gazepoint cluster permutation

Re-run
[`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md)
across a small set of cluster-forming thresholds and summarize how many
clusters are detected.

## Usage

``` r
run_gazepoint_cluster_threshold_sensitivity(
  data,
  thresholds = c(1.5, 2, 2.5),
  ...
)
```

## Arguments

- data:

  Prepared cluster-permutation data.

- thresholds:

  Numeric vector of cluster-forming thresholds.

- ...:

  Additional arguments passed to
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

## Value

A list containing threshold-level summaries and full result objects.
