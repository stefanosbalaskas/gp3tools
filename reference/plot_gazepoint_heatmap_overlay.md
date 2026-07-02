# Plot a Gazepoint heatmap over a background image

`plot_gazepoint_heatmap_overlay()` overlays a binned gaze or fixation
heatmap on a PNG background image, such as a stimulus screenshot. The
`png` package is used only when this helper is called.

## Usage

``` r
plot_gazepoint_heatmap_overlay(
  data,
  background_image,
  x_col = NULL,
  y_col = NULL,
  weight_col = NULL,
  display_width = NULL,
  display_height = NULL,
  coordinate_space = c("auto", "normalized", "pixel"),
  bins = 60,
  heatmap_alpha = 0.7,
  background_alpha = 1,
  normalize = TRUE,
  show_legend = TRUE
)
```

## Arguments

- data:

  A data frame or prepared heatmap data.

- background_image:

  Path to a PNG background image.

- x_col, y_col:

  Character strings giving x and y coordinate columns.

- weight_col:

  Optional non-negative weight column.

- display_width, display_height:

  Display width and height in pixels. If omitted, the PNG image
  dimensions are used.

- coordinate_space:

  One of `"auto"`, `"normalized"`, or `"pixel"`.

- bins:

  Number of heatmap bins. Either one integer or two integers.

- heatmap_alpha:

  Heatmap layer transparency.

- background_alpha:

  Background image transparency.

- normalize:

  Logical. If `TRUE`, bin intensities are scaled to 0–1.

- show_legend:

  Logical. If `TRUE`, the fill legend is shown.

## Value

A ggplot object.

## Examples

``` r
gaze <- data.frame(
  x = c(0.20, 0.25, 0.70),
  y = c(0.30, 0.35, 0.60),
  duration = c(120, 200, 80)
)

if (requireNamespace("png", quietly = TRUE)) {
  bg <- tempfile(fileext = ".png")
  img <- array(1, dim = c(200, 300, 3))
  png::writePNG(img, bg)

  plot_gazepoint_heatmap_overlay(
    gaze,
    background_image = bg,
    x_col = "x",
    y_col = "y",
    weight_col = "duration"
  )
}
```
