# Prepare Gazepoint master data for eyetrackingR-style workflows

Convert a `gp3tools` master table into a dependency-free,
eyetrackingR-friendly sample-level table. The returned data frame keeps
one row per gaze sample and creates standard participant, trial, time,
gaze-coordinate, AOI, trackloss, and AOI-indicator columns.

## Usage

``` r
prepare_gazepoint_eyetrackingr_data(
  data,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  aoi_col = NULL,
  x_col = NULL,
  y_col = NULL,
  media_col = NULL,
  condition_col = NULL,
  validity_cols = NULL,
  aoi_values = NULL,
  aoi_prefix = "aoi_",
  missing_aoi_label = "missing_aoi",
  non_aoi_values = c("outside", "none", "no_aoi", "non_aoi", "background", "off_aoi",
    "missing", "NA"),
  trackloss_col = NULL,
  keep_original_cols = TRUE
)
```

## Arguments

- data:

  A Gazepoint master table or sample-level gaze data frame.

- participant_col:

  Participant/subject identifier column.

- trial_col:

  Trial identifier column. If `NULL`, a trial identifier is created from
  `media_col` when available.

- time_col:

  Sample time column.

- aoi_col:

  AOI label/state column.

- x_col:

  Optional gaze x-coordinate column.

- y_col:

  Optional gaze y-coordinate column.

- media_col:

  Optional media/stimulus identifier column.

- condition_col:

  Optional condition/grouping column.

- validity_cols:

  Optional validity columns used to define trackloss.

- aoi_values:

  Optional AOI values for which logical indicator columns should be
  created. If `NULL`, values are detected from `aoi_col`.

- aoi_prefix:

  Prefix for generated AOI indicator columns.

- missing_aoi_label:

  Label used for missing AOI values.

- non_aoi_values:

  Character values treated as non-AOI/background states.

- trackloss_col:

  Optional existing trackloss column. If supplied, it is used directly
  after coercion to logical.

- keep_original_cols:

  Logical. If `TRUE`, original columns are retained after the standard
  adapter columns.

## Value

A tibble with class `gp3_eyetrackingr_data`.
