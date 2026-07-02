# Plot a Gazepoint cluster-permutation result

`plot_gazepoint_cluster_permutation()` is a compatibility wrapper around
[`plot_gazepoint_cluster_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_cluster_results.md).
It uses the existing validated gp3tools plotting engine while providing
a name aligned with the cluster-permutation workflow.

## Usage

``` r
plot_gazepoint_cluster_permutation(result, ...)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- ...:

  Additional arguments passed to
  [`plot_gazepoint_cluster_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_cluster_results.md).

## Value

A ggplot object.
