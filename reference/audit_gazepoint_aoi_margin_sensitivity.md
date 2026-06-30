# Audit AOI margin sensitivity

Audit whether sample-level AOI assignments are sensitive to small
expansions or shrinkages of AOI boundaries.

## Usage

``` r
audit_gazepoint_aoi_margin_sensitivity(
  gaze_data,
  aoi_geometry,
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
  margins = c(-0.02, 0, 0.02),
  screen_x_range = c(0, 1),
  screen_y_range = c(0, 1),
  tie_method = c("ambiguous", "first"),
  outside_label = "outside",
  ambiguous_label = "ambiguous",
  missing_label = "missing_coordinate",
  max_margin_change_prop = 0.1,
  max_ambiguous_prop = 0.05,
  ignore_invalid_geometry = TRUE
)
```

## Arguments

- gaze_data:

  A data frame containing gaze samples.

- aoi_geometry:

  A data frame containing AOI geometry definitions.

- gaze_x_col:

  Gaze x-coordinate column. If `NULL`, common aliases are detected
  automatically.

- gaze_y_col:

  Gaze y-coordinate column. If `NULL`, common aliases are detected
  automatically.

- gaze_stimulus_col:

  Optional gaze stimulus/media column.

- sample_id_cols:

  Optional columns to carry into the sample-level audit table.

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

- margins:

  Numeric vector of AOI boundary margins. Positive values expand AOIs;
  negative values shrink AOIs. A zero-margin baseline is always added.

- screen_x_range:

  Numeric length-2 vector defining the screen x range.

- screen_y_range:

  Numeric length-2 vector defining the screen y range.

- tie_method:

  How to handle samples falling in multiple AOIs. Use `"ambiguous"` to
  label them as ambiguous or `"first"` to use the first matching AOI.

- outside_label:

  Label for samples outside all AOIs.

- ambiguous_label:

  Label for samples inside multiple AOIs when
  `tie_method = "ambiguous"`.

- missing_label:

  Label for samples with missing/non-finite gaze coordinates.

- max_margin_change_prop:

  Maximum acceptable proportion of samples whose AOI assignment changes
  from the zero-margin baseline.

- max_ambiguous_prop:

  Maximum acceptable proportion of ambiguous samples.

- ignore_invalid_geometry:

  Logical. If `TRUE`, AOIs with invalid coordinates or dimensions are
  excluded before margin coding.

## Value

A list with class `gp3_aoi_margin_sensitivity_audit` containing
overview, geometry_summary, sample_sensitivity, margin_summary,
aoi_margin_summary, flagged_samples, and settings tables.
