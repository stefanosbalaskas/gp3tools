# Report a Gazepoint cluster-permutation result

Create a compact, cautious, text-ready report from a cluster-permutation
result. The report avoids exact onset/offset claims and describes
detected clusters as time ranges surviving the specified cluster
procedure.

## Usage

``` r
report_gazepoint_cluster_permutation(result, alpha = 0.05)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- alpha:

  Significance threshold.

## Value

A list with cluster table, settings, and report text.
