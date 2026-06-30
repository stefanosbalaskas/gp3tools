# Audit AOI overlap

Create a publication-level audit of pairwise AOI overlap within each
stimulus.

## Usage

``` r
audit_gazepoint_aoi_overlap(
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
  min_overlap_area = 0,
  min_overlap_prop = 0,
  ignore_invalid_geometry = TRUE
)
```

## Arguments

- data:

  A data frame containing AOI geometry definitions.

- aoi_col:

  AOI label/name column. If `NULL`, common AOI-name aliases are detected
  automatically by
  [`audit_gazepoint_aoi_geometry()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_geometry.md).

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

- min_overlap_area:

  Minimum overlap area above which an AOI pair is flagged.

- min_overlap_prop:

  Minimum overlap proportion above which an AOI pair is flagged. This is
  computed relative to the smaller AOI in the pair.

- ignore_invalid_geometry:

  Logical. If `TRUE`, AOIs with invalid geometry are excluded from
  pairwise overlap calculations.

## Value

A list with class `gp3_aoi_overlap_audit` containing overview,
geometry_summary, pairwise_overlap, overlap_summary, flagged_overlaps,
and settings tables.
