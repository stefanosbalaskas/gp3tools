# Plot multiple Gazepoint scanpaths

Creates a descriptive multi-scanpath plot from gaze coordinates. This
helper is intended for visual quality review, participant/trial
inspection, and documentation examples. It should not be interpreted as
an inferential scanpath-comparison method.

## Usage

``` r
plot_gazepoint_scanpaths(
  data,
  x_col,
  y_col,
  order_col = NULL,
  group_cols = NULL,
  colour_col = NULL,
  facet_col = NULL,
  screen_width = NULL,
  screen_height = NULL,
  reverse_y = TRUE,
  show_points = TRUE,
  alpha = 0.45,
  linewidth = 0.4,
  point_size = 0.7,
  title = NULL
)
```

## Arguments

- data:

  A data frame.

- x_col, y_col:

  Character names of gaze coordinate columns.

- order_col:

  Optional column used to order gaze samples before plotting.

- group_cols:

  Optional character vector used to define separate paths.

- colour_col:

  Optional column used for colour.

- facet_col:

  Optional column used for faceting.

- screen_width, screen_height:

  Optional screen dimensions. If supplied, axis limits are set to the
  screen bounds.

- reverse_y:

  If `TRUE`, reverses the y-axis so the origin is displayed at the
  top-left, matching common screen-coordinate conventions.

- show_points:

  If `TRUE`, sample points are added on top of paths.

- alpha:

  Line opacity.

- linewidth:

  Line width.

- point_size:

  Point size when `show_points = TRUE`.

- title:

  Optional plot title.

## Value

A ggplot object.

## Examples

``` r
x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
plot_gazepoint_scanpaths(
  x,
  x_col = "gaze_x",
  y_col = "gaze_y",
  order_col = "time_bin",
  group_cols = c("subject", "trial"),
  colour_col = "condition"
)
```
