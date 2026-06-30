# Run paired cluster-based permutation tests

Run a paired cluster-based permutation test on time-course data prepared
by
[`prepare_gazepoint_cluster_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_cluster_data.md).
The function tests whether two conditions diverge over time while
controlling cluster-level inference using a permutation distribution of
maximum cluster statistics.

## Usage

``` r
run_gazepoint_cluster_permutation(
  data,
  condition_order = NULL,
  n_permutations = 1000,
  cluster_threshold = 2,
  tail = c("two_sided", "greater", "less"),
  cluster_stat = c("sum_abs_t", "sum_t", "size"),
  min_time_bins = 1,
  seed = NULL,
  paired = TRUE
)
```

## Arguments

- data:

  Cluster-ready data produced by
  [`prepare_gazepoint_cluster_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_cluster_data.md).

- condition_order:

  Optional character vector of length 2 defining the two conditions and
  their order. The tested difference is condition 2 minus condition 1.

- n_permutations:

  Number of sign-flip permutations.

- cluster_threshold:

  Absolute t-statistic threshold for forming candidate clusters. For
  `tail = "greater"` or `tail = "less"`, the same positive threshold is
  used in the requested direction.

- tail:

  Direction of the test. `"two_sided"` tests positive and negative
  clusters. `"greater"` tests condition 2 greater than condition 1.
  `"less"` tests condition 2 less than condition 1.

- cluster_stat:

  Cluster statistic. `"sum_abs_t"` sums absolute t-statistics within a
  cluster. `"sum_t"` sums signed t-statistics and then uses the absolute
  value for cluster-level inference. `"size"` uses the number of time
  bins.

- min_time_bins:

  Minimum number of adjacent time bins required for a cluster to be
  retained.

- seed:

  Optional random seed for reproducible permutations.

- paired:

  Logical. Currently only paired within-subject sign-flip permutation is
  supported.

## Value

A list containing observed time-course statistics, observed clusters,
the permutation distribution, settings, and status fields.

## Details

Cluster-based permutation tests are intended for time-course inference.
They should not be used to discover a confirmatory time window and then
test that same window again in a second confirmatory model.
