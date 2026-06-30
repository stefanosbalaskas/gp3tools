# Plot AOI geometry for visual verification

Create a visual verification plot of AOI rectangles, with optional gaze
samples overlaid.

## Usage

``` r
plot_gazepoint_aoi_verification(
  aoi_geometry,
  gaze_data = NULL,
  geometry_aoi_col = NULL,
  geometry_stimulus_col = NULL,
  x_min_col = NULL,
  y_min_col = NULL,
  x_max_col = NULL,
  y_max_col = NULL,
  x_col = NULL,
  y_col = NULL,
  width_col = NULL,
  height_col = NULL,
  gaze_x_col = NULL,
  gaze_y_col = NULL,
  gaze_stimulus_col = NULL,
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  facet_by_stimulus = TRUE,
  show_labels = TRUE,
  show_gaze = TRUE,
  invert_y = TRUE,
  point_alpha = 0.35,
  point_size = 1.2,
  line_width = 0.8,
  label_size = 3
)
```

## Arguments

- aoi_geometry:

  A data frame containing AOI geometry definitions.

- gaze_data:

  Optional data frame containing gaze samples to overlay.

- geometry_aoi_col:

  AOI label/name column in `aoi_geometry`.

- geometry_stimulus_col:

  Optional stimulus/media column in `aoi_geometry`.

- x_min_col:

  Optional AOI left/x-min column.

- y_min_col:

  Optional AOI top/y-min column.

- x_max_col:

  Optional AOI right/x-max column.

- y_max_col:

  Optional AOI bottom/y-max column.

- x_col:

  Optional AOI left/x column used with `width_col`.

- y_col:

  Optional AOI top/y column used with `height_col`.

- width_col:

  Optional AOI width column.

- height_col:

  Optional AOI height column.

- gaze_x_col:

  Optional gaze x-coordinate column.

- gaze_y_col:

  Optional gaze y-coordinate column.

- gaze_stimulus_col:

  Optional gaze stimulus/media column.

- screen_x_range:

  Numeric length-2 vector defining the screen x range.

- screen_y_range:

  Numeric length-2 vector defining the screen y range.

- facet_by_stimulus:

  Logical. If `TRUE`, facet by stimulus/media when a stimulus column is
  available.

- show_labels:

  Logical. If `TRUE`, draw AOI labels at AOI centres.

- show_gaze:

  Logical. If `TRUE`, overlay gaze samples when `gaze_data` is supplied.

- invert_y:

  Logical. If `TRUE`, reverse the y-axis so screen origin is at the
  top-left.

- point_alpha:

  Alpha transparency for gaze points.

- point_size:

  Size of gaze points.

- line_width:

  Width of AOI rectangle borders.

- label_size:

  Size of AOI labels.

## Value

A `ggplot` object.
