# Select the number of scanpath clusters

Compare candidate cluster counts using mean silhouette width. Input
formats and sequence-preparation arguments match
[`cluster_gazepoint_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/cluster_gazepoint_scanpaths.md).

## Usage

``` r
select_gazepoint_scanpath_clusters(
  x,
  k_values = NULL,
  method = c("hierarchical", "pam"),
  linkage = c("average", "complete", "single", "ward.D2", "ward.D", "mcquitty", "median",
    "centroid"),
  aoi_col = NULL,
  group_cols = NULL,
  time_col = NULL,
  distance_col = "normalized_distance",
  include_missing = FALSE,
  missing_label = "missing",
  collapse_repeats = FALSE,
  max_sequences = 200
)
```

## Arguments

- x:

  Long-format AOI data, a pairwise-distance data frame, a square numeric
  distance matrix, or a `dist` object.

- k_values:

  Candidate cluster counts. When `NULL`, values from 2 through the
  smaller of 6 or one fewer than the number of scanpaths are evaluated.

- method:

  Clustering method: `"hierarchical"` or `"pam"`.

- linkage:

  Hierarchical linkage method passed to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html).

- aoi_col:

  AOI column when `x` is long-format AOI data.

- group_cols:

  Columns identifying independent scanpaths.

- time_col:

  Optional ordering column.

- distance_col:

  Distance column for pairwise-distance data.

- include_missing:

  Should missing AOI labels be retained?

- missing_label:

  Label used for retained missing AOIs.

- collapse_repeats:

  Should consecutive repeated AOIs be collapsed?

- max_sequences:

  Maximum number of scanpaths permitted when pairwise distances must be
  calculated.

## Value

An object of class `gp3_scanpath_cluster_selection` containing candidate
diagnostics, the recommended number of clusters, all fitted solutions,
the recommended fit, and the distance object.

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

if (requireNamespace("cluster", quietly = TRUE)) {
  result <- select_gazepoint_scanpath_clusters(
    d,
    k_values = 2:3
  )
  result$diagnostics
}
#>   k mean_silhouette_width n_clusters       method
#> 2 2                  0.90          2 hierarchical
#> 3 3                  0.45          3 hierarchical
```
