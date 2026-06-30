# Summarise Gazepoint fixation trial features

Create trial-level fixation features from Gazepoint fixation-level data.
The function supports common Gazepoint fixation export columns such as
`FPOGS`, `FPOGD`, `FPOGX`, `FPOGY`, `FPOGID`, `FPOGV`, and `AOI`, as
well as already-standardised columns.

## Usage

``` r
summarise_gazepoint_fixation_trials(
  data,
  group_cols = NULL,
  fixation_id_col = NULL,
  start_col = NULL,
  duration_col = NULL,
  x_col = NULL,
  y_col = NULL,
  valid_col = NULL,
  aoi_col = NULL,
  start_time_unit = c("auto", "ms", "s"),
  duration_unit = c("auto", "ms", "s"),
  valid_only = TRUE,
  include_non_aoi = TRUE,
  target_aoi_values = NULL,
  distractor_aoi_values = NULL,
  non_aoi_values = c("non_aoi", "none", "background", "outside", "outside_aoi",
    "missing", "missing_aoi"),
  missing_aoi_label = "missing_aoi"
)
```

## Arguments

- data:

  A Gazepoint fixation-level data frame.

- group_cols:

  Character vector of columns defining independent trials. If `NULL`,
  the function tries to infer sensible grouping columns from
  participant, media, and trial columns.

- fixation_id_col:

  Optional fixation ID column. If `NULL`, the function tries `FPOGID`,
  `fixation_id`, and related names.

- start_col:

  Optional fixation start-time column. If `NULL`, the function tries
  `FPOGS`, `fixation_start_time`, `time`, `TIME`, and `TIMETICK`.

- duration_col:

  Optional fixation-duration column. If `NULL`, the function tries
  `FPOGD`, `fixation_duration_ms`, `fixation_duration`, and related
  names.

- x_col:

  Optional fixation x-coordinate column.

- y_col:

  Optional fixation y-coordinate column.

- valid_col:

  Optional fixation-validity column. If detected and
  `valid_only = TRUE`, invalid fixations are removed.

- aoi_col:

  Optional AOI column. If `NULL`, the function tries `AOI`,
  `aoi_current`, and `aoi_state`.

- start_time_unit:

  Unit for the start-time column: `"auto"`, `"ms"`, or `"s"`.

- duration_unit:

  Unit for the duration column: `"auto"`, `"ms"`, or `"s"`.

- valid_only:

  Logical. If `TRUE`, invalid fixations are removed when a validity
  column is available.

- include_non_aoi:

  Logical. If `TRUE`, non-AOI/background fixations are included. If
  `FALSE`, they are removed before summaries are computed.

- target_aoi_values:

  Optional character vector defining target AOI labels.

- distractor_aoi_values:

  Optional character vector defining distractor AOI labels.

- non_aoi_values:

  Character vector of AOI labels treated as background or non-AOI
  states.

- missing_aoi_label:

  Label used when the AOI value is missing.

## Value

A tibble with one row per trial/group and fixation-level features.
