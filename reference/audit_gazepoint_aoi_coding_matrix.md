# Audit AOI coding against geometry

Validate observed sample-level AOI labels against AOI labels derived
from gaze coordinates and AOI geometry.

## Usage

``` r
audit_gazepoint_aoi_coding_matrix(
  gaze_data,
  aoi_geometry,
  observed_aoi_col = NULL,
  gaze_x_col = NULL,
  gaze_y_col = NULL,
  gaze_stimulus_col = NULL,
  sample_id_cols = NULL,
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
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  tie_method = c("ambiguous", "first"),
  outside_label = "outside",
  ambiguous_label = "ambiguous",
  missing_label = "missing_coordinate",
  observed_outside_values = c("outside", "none", "no_aoi", "non_aoi", "background",
    "off_aoi"),
  max_mismatch_prop = 0.05,
  max_ambiguous_prop = 0.05,
  max_missing_coordinate_prop = 0.2,
  ignore_invalid_geometry = TRUE
)
```

## Arguments

- gaze_data:

  A data frame containing gaze samples and observed AOI labels.

- aoi_geometry:

  A data frame containing AOI geometry definitions.

- observed_aoi_col:

  Observed AOI label column in `gaze_data`.

- gaze_x_col:

  Gaze x-coordinate column. If `NULL`, common aliases are detected
  automatically.

- gaze_y_col:

  Gaze y-coordinate column. If `NULL`, common aliases are detected
  automatically.

- gaze_stimulus_col:

  Optional gaze stimulus/media column.

- sample_id_cols:

  Optional columns to carry into the sample-level coding audit table.

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

- screen_x_range:

  Numeric length-2 vector defining the screen x range.

- screen_y_range:

  Numeric length-2 vector defining the screen y range.

- tie_method:

  How to handle samples falling in multiple AOIs. Use `"ambiguous"` to
  label them as ambiguous or `"first"` to use the first matching AOI.

- outside_label:

  Label used for samples outside all AOIs.

- ambiguous_label:

  Label used for samples inside multiple AOIs when
  `tie_method = "ambiguous"`.

- missing_label:

  Label used for samples with missing/non-finite gaze coordinates.

- observed_outside_values:

  Character values in `observed_aoi_col` treated as outside/non-AOI
  labels.

- max_mismatch_prop:

  Maximum acceptable proportion of comparable samples where observed and
  geometry-derived AOI labels differ.

- max_ambiguous_prop:

  Maximum acceptable proportion of samples with ambiguous
  geometry-derived AOI assignment.

- max_missing_coordinate_prop:

  Maximum acceptable proportion of samples with missing/non-finite gaze
  coordinates.

- ignore_invalid_geometry:

  Logical. If `TRUE`, AOIs with invalid coordinates or dimensions are
  excluded before coding validation.

## Value

A list with class `gp3_aoi_coding_matrix_audit` containing overview,
geometry_summary, sample_coding, coding_matrix, observed_summary,
derived_summary, flagged_samples, and settings tables.
