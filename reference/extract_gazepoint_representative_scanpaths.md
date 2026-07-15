# Extract representative scanpaths

Select representative observed scanpaths by minimizing mean distance to
other members of the same fitted cluster.

## Usage

``` r
extract_gazepoint_representative_scanpaths(x, n_per_cluster = 1L)
```

## Arguments

- x:

  An object returned by
  [`cluster_gazepoint_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/cluster_gazepoint_scanpaths.md).

- n_per_cluster:

  Number of representatives to return per cluster.

## Value

A data frame containing cluster, representative rank, sequence
identifier, mean within-cluster distance, cluster size, and PAM medoid
status.

## Examples

``` r
d <- matrix(
  c(
    0, 0.1, 1, 1,
    0.1, 0, 1, 1,
    1, 1, 0, 0.1,
    1, 1, 0.1, 0
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(LETTERS[1:4], LETTERS[1:4])
)

fit <- cluster_gazepoint_scanpaths(d, k = 2)
extract_gazepoint_representative_scanpaths(fit)
#>   cluster representative_rank sequence_id mean_within_cluster_distance
#> 1       1                   1           A                          0.1
#> 2       2                   1           C                          0.1
#>   cluster_size is_model_medoid
#> 1            2           FALSE
#> 2            2           FALSE
```
