# Summarise facial-behaviour reactivity between two windows

Computes baseline-to-response differences from
[`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md)
output. The function returns one row per group and measure. Reactivity
is reported as response minus baseline. The helper summarises technical
facial-behaviour measures only and does not infer emotional states.

## Usage

``` r
summarize_gazepoint_face_reactivity(
  data,
  baseline_window,
  response_window,
  group_cols = NULL,
  window_col = NULL,
  measure_cols = NULL,
  statistic = c("mean", "median")
)
```

## Arguments

- data:

  A window-summary table returned by
  [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md).

- baseline_window:

  Value in `window_col` identifying the baseline window.

- response_window:

  Value in `window_col` identifying the response window.

- group_cols:

  Optional grouping columns.

- window_col:

  Column identifying window labels or IDs. Auto-detected when possible.

- measure_cols:

  Measure names or summary columns. If `NULL`, columns ending in the
  selected statistic suffix are detected automatically.

- statistic:

  Statistic used for reactivity. One of `"mean"` or `"median"`.

## Value

A tibble with one row per group and measure. The returned object has
class `gp3_face_reactivity_summary`.

## Examples

``` r
face <- data.frame(
  participant_id = "P001",
  face_time_sec = c(0.00, 0.05, 0.10),
  face_valid = c(TRUE, TRUE, TRUE),
  AU12_r = c(0.1, 0.2, 0.3)
)

windows <- data.frame(
  participant_id = "P001",
  window = c("baseline", "response"),
  window_start_sec = c(0.00, 0.05),
  window_end_sec = c(0.05, 0.15)
)

summary <- summarize_gazepoint_face_windows(
  face,
  windows = windows,
  group_cols = "participant_id",
  window_label_col = "window"
)

summarize_gazepoint_face_reactivity(
  summary,
  baseline_window = "baseline",
  response_window = "response",
  group_cols = "participant_id"
)
#> # A tibble: 1 × 12
#>   participant_id measure statistic baseline_window response_window
#>   <chr>          <chr>   <chr>     <chr>           <chr>          
#> 1 P001           AU12_r  mean      baseline        response       
#> # ℹ 7 more variables: baseline_value <dbl>, response_value <dbl>,
#> #   reactivity <dbl>, absolute_reactivity <dbl>, percent_reactivity <dbl>,
#> #   n_baseline_windows <int>, n_response_windows <int>
```
