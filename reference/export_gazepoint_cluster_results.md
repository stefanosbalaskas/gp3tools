# Export Gazepoint cluster-permutation results

Export cluster tables, optional null distributions, settings, and
cautious report text to a folder of CSV/TXT files.

## Usage

``` r
export_gazepoint_cluster_results(result, outdir, overwrite = FALSE)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- outdir:

  Output directory.

- overwrite:

  Should an existing directory be reused?

## Value

A data frame listing written files.
