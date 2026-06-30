# Plot cluster-based permutation results

Create a publication-ready time-course plot from the output of
[`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).
The plot can show the mean condition difference, the time-wise test
statistic, or both. Candidate time bins and cluster-level significant
windows can be highlighted.

## Usage

``` r
plot_gazepoint_cluster_results(
  result,
  plot_type = c("both", "difference", "statistic"),
  alpha = 0.05,
  significant_only = TRUE,
  show_clusters = TRUE,
  show_candidates = TRUE,
  show_threshold = TRUE,
  show_zero_line = TRUE,
  title = NULL,
  subtitle = NULL,
  x_label = "Time (ms)",
  y_label = NULL,
  line_width = 0.7,
  point_size = 1.8,
  cluster_alpha = 0.12
)
```

## Arguments

- result:

  A result object returned by
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).

- plot_type:

  Character. One of `"both"`, `"difference"`, or `"statistic"`.

- alpha:

  Cluster-level significance threshold used to decide which clusters are
  significant for plotting.

- significant_only:

  Logical. If `TRUE`, only significant clusters are shaded. If `FALSE`,
  all observed clusters are shaded.

- show_clusters:

  Logical. If `TRUE`, shade cluster windows.

- show_candidates:

  Logical. If `TRUE`, mark time bins exceeding the cluster-forming
  threshold.

- show_threshold:

  Logical. If `TRUE`, show the cluster-forming threshold on the
  statistic panel.

- show_zero_line:

  Logical. If `TRUE`, add a horizontal zero reference line.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- x_label:

  X-axis label.

- y_label:

  Optional y-axis label. If `NULL`, a label is chosen automatically.

- line_width:

  Width of the time-course line.

- point_size:

  Size of candidate-bin points.

- cluster_alpha:

  Transparency for shaded cluster windows.

## Value

A `ggplot` object.

## Details

Cluster-based permutation tests are intended for time-course inference.
They should not be used to discover a confirmatory time window and then
test that same window again in a second confirmatory model.
