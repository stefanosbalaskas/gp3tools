# Audit AOI geometry definitions

Create a publication-level audit of AOI geometry definitions, including
AOI size, area, coordinate validity, screen-bound checks, and duplicate
geometry.

## Usage

``` r
audit_gazepoint_aoi_geometry(
  data,
  aoi_col = NULL,
  stimulus_col = NULL,
  x_min_col = NULL,
  y_min_col = NULL,
  x_max_col = NULL,
  y_max_col = NULL,
  x_col = NULL,
  y_col = NULL,
  width_col = NULL,
  height_col = NULL,
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  min_width = 0,
  min_height = 0,
  min_area = 0,
  max_area_prop = 1,
  require_within_screen = TRUE
)
```

## Arguments

- data:

  A data frame containing AOI geometry definitions.

- aoi_col:

  AOI label/name column. If `NULL`, common AOI-name aliases are detected
  automatically.

- stimulus_col:

  Optional stimulus/media column.

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

- screen_x_range:

  Numeric length-2 vector defining the screen x range.

- screen_y_range:

  Numeric length-2 vector defining the screen y range.

- min_width:

  Minimum acceptable AOI width.

- min_height:

  Minimum acceptable AOI height.

- min_area:

  Minimum acceptable AOI area.

- max_area_prop:

  Maximum acceptable AOI area as a proportion of screen area.

- require_within_screen:

  Logical. If `TRUE`, AOIs extending outside the screen range are
  flagged.

## Value

A list with class `gp3_aoi_geometry_audit` containing overview,
geometry_summary, size_summary, flagged_aois, duplicate_geometry, and
settings tables.
