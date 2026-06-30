# Example AOI geometry table

A lightweight synthetic AOI geometry table for AOI-verification
examples. Coordinates are normalised to a 0–1 screen coordinate system.

## Usage

``` r
gazepoint_example_aoi_geometry
```

## Format

A tibble with one row per stimulus and AOI, including:

- media_id:

  Synthetic stimulus identifier.

- aoi:

  Synthetic AOI label.

- x_min, y_min, x_max, y_max:

  Normalised rectangular AOI boundaries.

## Examples

``` r
data(gazepoint_example_aoi_geometry)
gazepoint_example_aoi_geometry
#> # A tibble: 6 × 6
#>   media_id aoi   x_min y_min x_max y_max
#>   <chr>    <chr> <dbl> <dbl> <dbl> <dbl>
#> 1 stim1    AOI 0  0.15  0.35  0.35  0.65
#> 2 stim1    AOI 1  0.4   0.35  0.6   0.65
#> 3 stim1    AOI 2  0.65  0.35  0.85  0.65
#> 4 stim2    AOI 0  0.15  0.35  0.35  0.65
#> 5 stim2    AOI 1  0.4   0.35  0.6   0.65
#> 6 stim2    AOI 2  0.65  0.35  0.85  0.65
```
