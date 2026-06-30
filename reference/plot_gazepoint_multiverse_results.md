# Plot Gazepoint preprocessing multiverse results

Create diagnostic plots from preprocessing multiverse summaries or from
pupil/AOI multiverse result objects.

## Usage

``` r
plot_gazepoint_multiverse_results(
  x,
  plot = c("status", "rows", "pupil_parameters", "aoi_denominators"),
  family = c("all", "pupil", "aoi"),
  title = NULL,
  show_labels = TRUE
)
```

## Arguments

- x:

  A `gp3_multiverse_summary_results`, `gp3_pupil_multiverse_results`, or
  `gp3_aoi_multiverse_results` object.

- plot:

  Character. Plot type. One of `"status"`, `"rows"`,
  `"pupil_parameters"`, or `"aoi_denominators"`.

- family:

  Character. Which family to show. One of `"all"`, `"pupil"`, or
  `"aoi"`.

- title:

  Optional plot title.

- show_labels:

  Logical. If `TRUE`, show branch labels on the y-axis.

## Value

A `ggplot` object.
