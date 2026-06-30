# Plot Gazepoint AOI transition matrix

Plot a heatmap of AOI transition counts or probabilities from the output
of
[`compute_gazepoint_aoi_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_transition_matrix.md)
or from a compatible long-form transition table.

## Usage

``` r
plot_gazepoint_aoi_transition_matrix(
  transitions,
  value = c("prob", "n"),
  state_order = NULL,
  by_cols = NULL,
  include_zero = TRUE,
  show_labels = TRUE,
  label_digits = 2,
  label_size = 3,
  facet = TRUE,
  title = NULL
)
```

## Arguments

- transitions:

  A `gp3_aoi_transition_matrix` object, a long-form transition table
  with `from`, `to`, `n`, and/or `prob` columns, or a numeric matrix
  with AOI states as row and column names.

- value:

  Which value to plot: `"prob"` for transition probabilities or `"n"`
  for transition counts.

- state_order:

  Optional character vector defining the AOI order on the heatmap axes.

- by_cols:

  Optional character vector of grouping columns to facet by. If `NULL`,
  the function uses grouping columns stored in a
  `gp3_aoi_transition_matrix` object, when available.

- include_zero:

  Logical. If `TRUE`, all possible state-to-state cells are shown, with
  missing transitions displayed as zero.

- show_labels:

  Logical. If `TRUE`, cell values are printed inside tiles.

- label_digits:

  Number of digits used when labelling probabilities.

- label_size:

  Text size for cell labels.

- facet:

  Logical. If `TRUE`, grouped transition tables are faceted.

- title:

  Optional plot title.

## Value

A `ggplot2` plot object.
