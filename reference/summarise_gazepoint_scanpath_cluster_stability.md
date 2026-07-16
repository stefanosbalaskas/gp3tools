# Summarise scanpath-cluster stability

Convert bootstrap scanpath-clustering output into overview,
sequence-level, pairwise, and representative-stability tables.

## Usage

``` r
summarise_gazepoint_scanpath_cluster_stability(
  x,
  min_pair_coverage = 0.5,
  stable_threshold = 0.75
)
```

## Arguments

- x:

  An object returned by
  [`bootstrap_gazepoint_scanpath_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/bootstrap_gazepoint_scanpath_clusters.md).

- min_pair_coverage:

  Minimum proportion of iterations in which a pair must co-occur before
  it contributes to summaries.

- stable_threshold:

  Within-reference-cluster co-clustering threshold used to count stable
  scanpaths.

## Value

An object of class `"gp3_scanpath_cluster_stability_summary"` containing
overview, sequence, pairwise, and representative tables.

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

summarise_gazepoint_scanpath_cluster_stability(
  stability
)
#> $overview
#>          specification       method linkage k n_boot sample_size
#> 1 hierarchical_average hierarchical average 3     10           5
#>   mean_adjusted_rand_index sd_adjusted_rand_index min_adjusted_rand_index
#> 1                        1                      0                       1
#>   mean_within_cluster_coclustering mean_between_cluster_coclustering
#> 1                                1                                 0
#>   mean_sequence_stability min_sequence_stability pct_sequences_stable
#> 1                       1                      1             66.66667
#>   stability_status
#> 1           stable
#> 
#> $sequence_summary
#>          specification sequence_id reference_cluster within_cluster_stability
#> 1 hierarchical_average           A                 1                        1
#> 2 hierarchical_average           B                 1                        1
#> 3 hierarchical_average           C                 2                        1
#> 4 hierarchical_average           D                 2                        1
#> 5 hierarchical_average           E                 3                       NA
#> 6 hierarchical_average           F                 3                       NA
#>   between_cluster_coclustering stability_separation n_within_pairs
#> 1                            0                    1              1
#> 2                            0                    1              1
#> 3                            0                    1              1
#> 4                            0                    1              1
#> 5                            0                   NA              0
#> 6                            0                   NA              0
#>   n_between_pairs mean_pair_coverage stable
#> 1               4               0.64   TRUE
#> 2               4               0.72   TRUE
#> 3               4               0.72   TRUE
#> 4               4               0.80   TRUE
#> 5               4               0.56  FALSE
#> 6               4               0.56  FALSE
#> 
#> $pairwise_summary
#>           specification sequence_a sequence_b co_clustering_probability
#> 1  hierarchical_average          A          B                         1
#> 2  hierarchical_average          A          C                         0
#> 3  hierarchical_average          B          C                         0
#> 4  hierarchical_average          A          D                         0
#> 5  hierarchical_average          B          D                         0
#> 6  hierarchical_average          C          D                         1
#> 7  hierarchical_average          A          E                         0
#> 8  hierarchical_average          B          E                         0
#> 9  hierarchical_average          C          E                         0
#> 10 hierarchical_average          D          E                         0
#> 11 hierarchical_average          A          F                         0
#> 12 hierarchical_average          B          F                         0
#> 13 hierarchical_average          C          F                         0
#> 14 hierarchical_average          D          F                         0
#> 15 hierarchical_average          E          F                         1
#>    pair_coverage same_reference_cluster included_in_summary
#> 1            0.7                   TRUE                TRUE
#> 2            0.7                  FALSE                TRUE
#> 3            0.8                  FALSE                TRUE
#> 4            0.8                  FALSE                TRUE
#> 5            0.9                  FALSE                TRUE
#> 6            0.9                   TRUE                TRUE
#> 7            0.5                  FALSE                TRUE
#> 8            0.6                  FALSE                TRUE
#> 9            0.6                  FALSE                TRUE
#> 10           0.7                  FALSE                TRUE
#> 11           0.5                  FALSE                TRUE
#> 12           0.6                  FALSE                TRUE
#> 13           0.6                  FALSE                TRUE
#> 14           0.7                  FALSE                TRUE
#> 15           0.4                   TRUE               FALSE
#> 
#> $representative_stability
#>          specification sequence_id reference_cluster n_included
#> 1 hierarchical_average           A                 1          8
#> 2 hierarchical_average           B                 1          9
#> 3 hierarchical_average           C                 2          9
#> 4 hierarchical_average           D                 2         10
#> 5 hierarchical_average           E                 3          7
#> 6 hierarchical_average           F                 3          7
#>   n_selected_as_representative representative_rate_when_included
#> 1                            8                         1.0000000
#> 2                            2                         0.2222222
#> 3                            9                         1.0000000
#> 4                            1                         0.1000000
#> 5                            7                         1.0000000
#> 6                            3                         0.4285714
#> 
#> $settings
#> $settings$min_pair_coverage
#> [1] 0.5
#> 
#> $settings$stable_threshold
#> [1] 0.75
#> 
#> 
#> attr(,"class")
#> [1] "gp3_scanpath_cluster_stability_summary"
#> [2] "list"                                  
```
