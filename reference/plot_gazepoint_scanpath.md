# Plot a fixation scanpath

Plot fixation coordinates connected in temporal order. This is a static
ggplot2 scanpath helper for fixation-level Gazepoint exports. It does
not require a stimulus image, but the returned plot can be extended by
users with additional ggplot2 layers.

## Usage

``` r
plot_gazepoint_scanpath(
  data,
  x_col,
  y_col,
  group_cols = NULL,
  time_col = NULL,
  fixation_index_col = NULL,
  label_col = NULL,
  point_size = 2,
  line_width = 0.4,
  show_order = TRUE,
  add_arrows = TRUE,
  reverse_y = FALSE,
  title = NULL
)
```

## Arguments

- data:

  A fixation-level data frame.

- x_col:

  Name of the horizontal fixation-coordinate column.

- y_col:

  Name of the vertical fixation-coordinate column.

- group_cols:

  Optional columns defining scanpaths.

- time_col:

  Optional column used to order fixations.

- fixation_index_col:

  Optional fixation index column used for labels.

- label_col:

  Optional label column. If supplied, labels use this column.

- point_size:

  Size of fixation points.

- line_width:

  Width of connecting path lines.

- show_order:

  Should fixation order labels be drawn?

- add_arrows:

  Should arrows be added to scanpath lines?

- reverse_y:

  Should the y-axis be reversed for screen-coordinate plots?

- title:

  Optional plot title.

## Value

A ggplot object.
