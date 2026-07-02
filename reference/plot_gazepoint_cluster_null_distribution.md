# Plot the cluster-permutation null distribution

Plot the null distribution of maximum cluster statistics from a
cluster-permutation result when available.

## Usage

``` r
plot_gazepoint_cluster_null_distribution(
  result,
  observed_line = TRUE,
  title = NULL
)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- observed_line:

  Should observed cluster statistics be added when available?

- title:

  Optional plot title.

## Value

A ggplot object.
