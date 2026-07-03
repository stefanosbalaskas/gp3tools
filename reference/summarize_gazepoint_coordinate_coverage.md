# Summarize gaze-coordinate coverage over a screen grid

Summarizes how much of the screen area is represented by valid gaze
coordinates. The helper reports valid-coordinate rates, coordinate
ranges, and the proportion of occupied grid cells. It is intended for
quality review and documentation, not for inference about attention.

## Usage

``` r
summarize_gazepoint_coordinate_coverage(
  data,
  x_col,
  y_col,
  screen_width,
  screen_height,
  group_cols = NULL,
  grid_n_x = 10,
  grid_n_y = 10,
  include_out_of_bounds = FALSE
)
```

## Arguments

- data:

  A data frame.

- x_col, y_col:

  Character names of gaze-coordinate columns.

- screen_width, screen_height:

  Numeric screen or stimulus dimensions.

- group_cols:

  Optional grouping columns.

- grid_n_x, grid_n_y:

  Number of grid cells along the x and y dimensions.

- include_out_of_bounds:

  If `TRUE`, finite out-of-bounds coordinates are included in range
  summaries but not in grid-occupancy calculations.

## Value

A data frame with one row per group.

## Examples

``` r
x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
summarize_gazepoint_coordinate_coverage(
  x,
  x_col = "gaze_x",
  y_col = "gaze_y",
  screen_width = 1920,
  screen_height = 1080,
  group_cols = "condition"
)
#>    group_id n_rows n_finite_coordinates n_inside_screen finite_coordinate_rate
#> 1   control     10                   10              10                      1
#> 2 treatment     10                   10              10                      1
#>   inside_screen_rate    x_min    x_max    y_min    y_max    x_mean   y_mean
#> 1                  1 743.4050 1197.648 418.1147 634.2470  967.3951 539.3253
#> 2                  1 834.7038 1248.194 492.8383 636.6294 1023.7187 538.2429
#>   occupied_grid_cells total_grid_cells occupied_grid_rate
#> 1                   7              100               0.07
#> 2                   6              100               0.06
```
