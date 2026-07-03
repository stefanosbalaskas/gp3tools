# Audit AOI coverage against screen bounds

Checks rectangular AOIs against declared screen or stimulus dimensions.
The helper reports missing geometry, invalid rectangle order, off-screen
AOIs, raw AOI area, clipped on-screen area, and screen-coverage rates.
Coverage summaries are descriptive and do not correct for AOI overlap.

## Usage

``` r
audit_gazepoint_aoi_screen_coverage(
  data,
  screen_width,
  screen_height,
  aoi_col = NULL,
  x_min_col = "x_min",
  x_max_col = "x_max",
  y_min_col = "y_min",
  y_max_col = "y_max",
  margin = 0
)
```

## Arguments

- data:

  A data frame containing AOI geometry.

- screen_width, screen_height:

  Numeric screen or stimulus dimensions.

- aoi_col:

  Optional AOI identifier column.

- x_min_col, x_max_col, y_min_col, y_max_col:

  Character names of rectangle boundary columns.

- margin:

  Numeric tolerance around screen bounds.

## Value

A list with AOI-level summary, overall summary, and settings.

## Examples

``` r
aoi <- data.frame(
  aoi = c("left", "right"),
  x_min = c(100, 1200),
  x_max = c(500, 1700),
  y_min = c(100, 100),
  y_max = c(400, 400)
)
audit_gazepoint_aoi_screen_coverage(aoi, 1920, 1080, aoi_col = "aoi")
#> $aoi_summary
#>   aoi_id x_min x_max y_min y_max width height raw_area clipped_area
#> 1   left   100   500   100   400   400    300   120000       120000
#> 2  right  1200  1700   100   400   500    300   150000       150000
#>   raw_screen_coverage clipped_screen_coverage missing_geometry
#> 1          0.05787037              0.05787037            FALSE
#> 2          0.07233796              0.07233796            FALSE
#>   invalid_rectangle outside_screen offscreen_left offscreen_right offscreen_top
#> 1             FALSE          FALSE          FALSE           FALSE         FALSE
#> 2             FALSE          FALSE          FALSE           FALSE         FALSE
#>   offscreen_bottom
#> 1            FALSE
#> 2            FALSE
#> 
#> $overall_summary
#>   n_aois n_missing_geometry n_invalid_rectangles n_outside_screen
#> 1      2                  0                    0                0
#>   total_raw_area total_clipped_area total_raw_screen_coverage
#> 1         270000             270000                 0.1302083
#>   total_clipped_screen_coverage
#> 1                     0.1302083
#>                                                       coverage_note
#> 1 Coverage sums are descriptive and do not correct for AOI overlap.
#> 
#> $settings
#> $settings$screen_width
#> [1] 1920
#> 
#> $settings$screen_height
#> [1] 1080
#> 
#> $settings$aoi_col
#> [1] "aoi"
#> 
#> $settings$x_min_col
#> [1] "x_min"
#> 
#> $settings$x_max_col
#> [1] "x_max"
#> 
#> $settings$y_min_col
#> [1] "y_min"
#> 
#> $settings$y_max_col
#> [1] "y_max"
#> 
#> $settings$margin
#> [1] 0
#> 
#> 
```
