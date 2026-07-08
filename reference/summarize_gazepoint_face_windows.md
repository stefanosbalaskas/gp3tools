# Summarise external facial-behaviour data within analysis windows

Summarises numeric external facial-behaviour variables within time
windows. The helper can use a separate window table or data rows that
already contain window labels. It is intended for external face-analysis
data imported, standardised, and optionally synchronised with Gazepoint
data. It does not infer facial expressions or emotional states.

## Usage

``` r
summarize_gazepoint_face_windows(
  data,
  windows = NULL,
  time_col = NULL,
  window_start_col = "window_start_sec",
  window_end_col = "window_end_sec",
  group_cols = NULL,
  window_id_col = NULL,
  window_label_col = NULL,
  measure_cols = NULL,
  validity_col = NULL,
  confidence_col = NULL,
  require_valid = TRUE,
  include_empty_windows = TRUE
)
```

## Arguments

- data:

  A face-analysis data frame, usually returned by
  [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md)
  or
  [`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md).

- windows:

  Optional data frame defining windows. When supplied, it must contain
  start and end columns.

- time_col:

  Time column in `data`, in seconds. Auto-detected when possible.

- window_start_col:

  Window-start column in seconds.

- window_end_col:

  Window-end column in seconds.

- group_cols:

  Optional grouping columns shared by `data` and `windows`.

- window_id_col:

  Optional window identifier column.

- window_label_col:

  Optional human-readable window label column.

- measure_cols:

  Numeric facial-behaviour columns to summarise. When `NULL`, likely
  facial-behaviour columns are detected automatically.

- validity_col:

  Optional validity column. Auto-detected when possible.

- confidence_col:

  Optional confidence column. Auto-detected when possible.

- require_valid:

  Should measure summaries use only rows where the validity column is
  `TRUE` when such a column is available?

- include_empty_windows:

  Should windows with no matching rows be kept in the output?

## Value

A tibble with one row per group/window and summary columns for each
measure. The returned object has class `gp3_face_window_summary`.

## Examples

``` r
face <- data.frame(
  participant_id = "P001",
  face_time_sec = c(0.00, 0.05, 0.10),
  face_confidence = c(0.95, 0.94, 0.93),
  face_valid = c(TRUE, TRUE, TRUE),
  AU12_r = c(0.1, 0.2, 0.3)
)

windows <- data.frame(
  participant_id = "P001",
  window = c("baseline", "response"),
  window_start_sec = c(0.00, 0.05),
  window_end_sec = c(0.05, 0.15)
)

summarize_gazepoint_face_windows(
  face,
  windows = windows,
  group_cols = "participant_id",
  window_label_col = "window"
)
#> # A tibble: 2 × 18
#>   participant_id face_window_id face_window_label window_start_sec
#>   <chr>                   <int> <chr>                        <dbl>
#> 1 P001                        1 baseline                      0   
#> 2 P001                        2 response                      0.05
#> # ℹ 14 more variables: window_end_sec <dbl>, n_rows <int>, n_used <int>,
#> #   n_valid <int>, n_invalid <int>, valid_percent <dbl>,
#> #   face_confidence_mean <dbl>, face_confidence_median <dbl>, AU12_r_n <int>,
#> #   AU12_r_mean <dbl>, AU12_r_median <dbl>, AU12_r_sd <dbl>, AU12_r_min <dbl>,
#> #   AU12_r_max <dbl>
```
