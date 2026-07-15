# Cluster Gazepoint AOI scanpaths

Cluster scanpaths from long-format AOI observations, pairwise scanpath
distances, a numeric distance matrix, or a `"dist"` object.

## Usage

``` r
cluster_gazepoint_scanpaths(
  x,
  k = 3,
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

  One of:

  - a long-format AOI data frame;

  - output from
    [`compute_gazepoint_scanpath_similarity`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_scanpath_similarity.md);

  - a square numeric distance matrix; or

  - a `"dist"` object.

- k:

  Integer number of clusters. Must be at least 2 and smaller than the
  number of scanpaths.

- method:

  Clustering method: `"hierarchical"` or `"pam"`.

- linkage:

  Hierarchical linkage method passed to
  [`hclust`](https://rdrr.io/r/stats/hclust.html).

- aoi_col:

  AOI column when `x` is long-format AOI data.

- group_cols:

  Columns identifying independent scanpaths when `x` is long-format AOI
  data.

- time_col:

  Optional ordering column for long-format AOI data.

- distance_col:

  Distance column when `x` is a pairwise-distance data frame. Defaults
  to `"normalized_distance"`.

- include_missing:

  Should missing AOI labels be retained as a state?

- missing_label:

  Label used when retaining missing AOIs.

- collapse_repeats:

  Should consecutive repeated AOI labels be collapsed before pairwise
  distances are calculated?

- max_sequences:

  Maximum number of grouped scanpaths permitted when pairwise distances
  must be calculated.

## Value

An object of class `"gp3_scanpath_clusters"` containing:

- `assignments`: scanpath identifiers and cluster assignments;

- `distance`: the clustering distance object;

- `model`: the fitted hierarchical or PAM model;

- `medoids`: PAM medoid identifiers, when applicable;

- `silhouette`: scanpath-level silhouette diagnostics when cluster is
  available;

- clustering settings and status fields.

## Details

Long-format AOI data are converted to pairwise normalized edit distances
with
[`compute_gazepoint_scanpath_similarity`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_scanpath_similarity.md).
Hierarchical clustering uses base R. Partitioning around medoids
requires the optional cluster package.

## Examples

``` r
distance_matrix <- matrix(
  c(
    0, 1, 5, 6,
    1, 0, 6, 5,
    5, 6, 0, 1,
    6, 5, 1, 0
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(
    c("scanpath_1", "scanpath_2", "scanpath_3", "scanpath_4"),
    c("scanpath_1", "scanpath_2", "scanpath_3", "scanpath_4")
  )
)

result <- cluster_gazepoint_scanpaths(
  distance_matrix,
  k = 2,
  method = "hierarchical"
)

result$assignments
#>   sequence_id cluster
#> 1  scanpath_1       1
#> 2  scanpath_2       1
#> 3  scanpath_3       2
#> 4  scanpath_4       2
```
