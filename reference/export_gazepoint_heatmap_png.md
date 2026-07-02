# Export a Gazepoint heatmap plot to PNG

`export_gazepoint_heatmap_png()` saves a ggplot heatmap to a PNG file.

## Usage

``` r
export_gazepoint_heatmap_png(
  plot,
  filename,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  create_dir = TRUE,
  ...
)
```

## Arguments

- plot:

  A ggplot object, usually returned by
  [`plot_gazepoint_heatmap()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_heatmap.md)
  or
  [`plot_gazepoint_heatmap_overlay()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_heatmap_overlay.md).

- filename:

  Output file path.

- width, height:

  Plot size passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- units:

  Units passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- dpi:

  Resolution passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- create_dir:

  Logical. If `TRUE`, the output directory is created when needed.

- ...:

  Additional arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

Invisibly returns the output path.

## Examples

``` r
gaze <- data.frame(
  x = c(0.20, 0.25, 0.70),
  y = c(0.30, 0.35, 0.60)
)

p <- plot_gazepoint_heatmap(
  gaze,
  x_col = "x",
  y_col = "y",
  bins = 10
)

out <- tempfile(fileext = ".png")
export_gazepoint_heatmap_png(p, out, width = 4, height = 3)
```
