# Prepare gaze or fixation coordinates for heatmap plotting

`prepare_gazepoint_heatmap_data()` standardises gaze or fixation
coordinates for spatial heatmap visualisation. It supports normalised
Gazepoint-style coordinates in the range 0–1 and pixel coordinates.

## Usage

``` r
prepare_gazepoint_heatmap_data(
  data,
  x_col,
  y_col,
  weight_col = NULL,
  display_width = NULL,
  display_height = NULL,
  coordinate_space = c("auto", "normalized", "pixel")
)
```

## Arguments

- data:

  A data frame containing gaze or fixation coordinates.

- x_col, y_col:

  Character strings giving the x and y coordinate columns.

- weight_col:

  Optional character string giving a non-negative weight column, such as
  fixation duration. If `NULL`, each point receives equal weight.

- display_width, display_height:

  Display width and height in pixels. For normalised coordinates, these
  values are used to convert coordinates to pixel space. If omitted for
  normalised coordinates, a unit display is used. If omitted for pixel
  coordinates, bounds are inferred from the observed coordinates.

- coordinate_space:

  One of `"auto"`, `"normalized"`, or `"pixel"`. `"auto"` treats
  coordinates as normalised only when all finite x and y values fall
  between 0 and 1.

## Value

A data frame with the original retained rows and standardised columns
`.gp3_x_px`, `.gp3_y_px`, and `.gp3_weight`.

## Examples

``` r
gaze <- data.frame(
  x = c(0.20, 0.25, 0.75),
  y = c(0.30, 0.35, 0.60),
  duration = c(120, 200, 80)
)

prepare_gazepoint_heatmap_data(
  gaze,
  x_col = "x",
  y_col = "y",
  weight_col = "duration",
  display_width = 1920,
  display_height = 1080
)
#>      x    y duration .gp3_x_source .gp3_y_source .gp3_x_px .gp3_y_px
#> 1 0.20 0.30      120          0.20          0.30       384       324
#> 2 0.25 0.35      200          0.25          0.35       480       378
#> 3 0.75 0.60       80          0.75          0.60      1440       648
#>   .gp3_weight .gp3_coordinate_space .gp3_display_width .gp3_display_height
#> 1         120            normalized               1920                1080
#> 2         200            normalized               1920                1080
#> 3          80            normalized               1920                1080
```
