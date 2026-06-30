# Summarise Gazepoint AOI samples within predefined time windows

Summarise sample-level AOI states into predefined analysis windows. This
is intended for confirmatory AOI window modelling, especially binomial
target-looking models where target samples are modelled relative to a
denominator such as all valid window samples.

## Usage

``` r
summarise_gazepoint_aoi_windows(
  data,
  windows,
  time_col = "time",
  aoi_col = NULL,
  subject_col = "subject",
  condition_col = "condition",
  group_cols = NULL,
  target_aoi_values = NULL,
  distractor_aoi_values = NULL,
  non_aoi_values = c("non_aoi", "none", "background", "outside", "outside_aoi",
    "missing", "missing_aoi"),
  window_label_col = "window_label",
  window_start_col = "window_start_ms",
  window_end_col = "window_end_ms",
  include_right_endpoint = FALSE,
  missing_condition_label = "all_data",
  missing_aoi_label = "missing_aoi"
)
```

## Arguments

- data:

  A Gazepoint master table or sample-level gaze data.

- windows:

  Numeric breakpoints, for example `c(0, 500, 1000, 2000)`, or a data
  frame with window labels and start/end columns.

- time_col:

  Name of the time column.

- aoi_col:

  Optional AOI-state column. If `NULL`, the function attempts to detect
  `aoi_current`, `AOI`, or `aoi_state`.

- subject_col:

  Name of the subject column.

- condition_col:

  Optional condition column.

- group_cols:

  Additional grouping columns. Defaults to subject, condition, media,
  trial-global, and trial columns when available.

- target_aoi_values:

  Character vector identifying target AOIs.

- distractor_aoi_values:

  Character vector identifying distractor AOIs.

- non_aoi_values:

  Character vector identifying background/non-AOI states.

- window_label_col:

  Window label column when `windows` is a data frame.

- window_start_col:

  Window start column when `windows` is a data frame.

- window_end_col:

  Window end column when `windows` is a data frame.

- include_right_endpoint:

  Logical. If `TRUE`, include the right endpoint of each window.

- missing_condition_label:

  Label used when condition is missing.

- missing_aoi_label:

  Label used when AOI state is missing.

## Value

A tibble with one row per group and AOI window.
