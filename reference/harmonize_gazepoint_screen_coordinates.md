# Harmonize Gazepoint screen coordinates across resolutions

Rescales gaze coordinates from one screen or stimulus resolution to
another. This is a deterministic coordinate transformation for
harmonizing exports before plotting, AOI checks, or descriptive
summaries. It does not recalibrate gaze data or correct measurement
error.

## Usage

``` r
harmonize_gazepoint_screen_coordinates(
  data,
  x_col,
  y_col,
  from_width,
  from_height,
  to_width,
  to_height,
  output_x_col = "gaze_x_harmonized",
  output_y_col = "gaze_y_harmonized",
  keep_original = TRUE
)
```

## Arguments

- data:

  A data frame.

- x_col, y_col:

  Character names of source coordinate columns.

- from_width, from_height:

  Original screen or stimulus dimensions.

- to_width, to_height:

  Target screen or stimulus dimensions.

- output_x_col, output_y_col:

  Names of the rescaled output columns.

- keep_original:

  If `TRUE`, original coordinate columns are retained. If `FALSE`, the
  original columns are removed when output column names differ.

## Value

A copy of `data` with harmonized coordinate columns.

## Examples

``` r
x <- data.frame(gaze_x = c(0, 960, 1920), gaze_y = c(0, 540, 1080))
harmonize_gazepoint_screen_coordinates(
  x,
  x_col = "gaze_x",
  y_col = "gaze_y",
  from_width = 1920,
  from_height = 1080,
  to_width = 1280,
  to_height = 720
)
#>   gaze_x gaze_y gaze_x_harmonized gaze_y_harmonized
#> 1      0      0                 0                 0
#> 2    960    540               640               360
#> 3   1920   1080              1280               720
```
