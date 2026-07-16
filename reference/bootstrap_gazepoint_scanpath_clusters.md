# Bootstrap scanpath-cluster stability

Evaluate the stability of scanpath clustering by repeatedly subsampling
scanpaths, refitting the clustering solution, and recording
co-clustering, adjusted Rand agreement, and representative-scanpath
selection.

## Usage

``` r
bootstrap_gazepoint_scanpath_clusters(
  x,
  k = 3L,
  n_boot = 200L,
  sample_fraction = 0.8,
  method = c("hierarchical", "pam"),
  linkages = "average",
  seed = NULL,
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

  Long-format AOI data, pairwise distance data, a square numeric
  distance matrix, or a `"dist"` object.

- k:

  Number of clusters.

- n_boot:

  Number of subsampling iterations per specification.

- sample_fraction:

  Proportion of scanpaths retained in each iteration. The resolved
  sample size is always at least `k + 1`.

- method:

  Clustering method: `"hierarchical"` or `"pam"`.

- linkages:

  Hierarchical linkage methods to compare. Ignored for PAM.

- seed:

  Optional integer seed. The caller's random-number state is restored on
  exit.

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

An object of class `"gp3_scanpath_cluster_bootstrap"` containing
full-data reference fits, co-clustering and pair-coverage matrices,
iteration-level adjusted Rand results, representative stability, the
reusable distance object, and resolved settings.

## Details

The routine reuses the distance formats accepted by
[`cluster_gazepoint_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/cluster_gazepoint_scanpaths.md).
Hierarchical solutions can be compared across multiple linkage methods.
PAM remains optional through cluster.

## Examples

``` r
latent <- rep(1:3, each = 2)
d <- outer(
  latent,
  latent,
  FUN = function(x, y) ifelse(x == y, 0.1, 1)
)
diag(d) <- 0
dimnames(d) <- list(LETTERS[1:6], LETTERS[1:6])

stability <- bootstrap_gazepoint_scanpath_clusters(
  d,
  k = 3,
  n_boot = 10,
  seed = 1
)

stability$iteration_summary
#>           specification       method linkage iteration n_sampled
#> 1  hierarchical_average hierarchical average         1         5
#> 2  hierarchical_average hierarchical average         2         5
#> 3  hierarchical_average hierarchical average         3         5
#> 4  hierarchical_average hierarchical average         4         5
#> 5  hierarchical_average hierarchical average         5         5
#> 6  hierarchical_average hierarchical average         6         5
#> 7  hierarchical_average hierarchical average         7         5
#> 8  hierarchical_average hierarchical average         8         5
#> 9  hierarchical_average hierarchical average         9         5
#> 10 hierarchical_average hierarchical average        10         5
#>    adjusted_rand_index mean_silhouette_width
#> 1                    1                  0.72
#> 2                    1                  0.72
#> 3                    1                  0.72
#> 4                    1                  0.72
#> 5                    1                  0.72
#> 6                    1                  0.72
#> 7                    1                  0.72
#> 8                    1                  0.72
#> 9                    1                  0.72
#> 10                   1                  0.72
```
