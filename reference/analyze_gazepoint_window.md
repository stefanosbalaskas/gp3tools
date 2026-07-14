# Summarise gaze or pupil measures in sliding time windows

Creates overlapping or non-overlapping time windows and calculates
selected summary statistics for numeric gaze or pupil columns.

## Usage

``` r
analyze_gazepoint_window(
  et_data,
  window_size = 50,
  step = 10,
  summary_stats = c("mean", "sd"),
  by = "USER_ID",
  condition_col = NULL,
  value_cols = NULL,
  ts_col = "TIME",
  window_unit = c("milliseconds", "seconds", "native"),
  time_unit = c("auto", "seconds", "milliseconds"),
  include_partial = FALSE
)
```

## Arguments

- et_data:

  A sample-level data frame.

- window_size:

  Window width.

- step:

  Distance between consecutive window starts.

- summary_stats:

  Statistics to calculate. Supported values are `"mean"`, `"sd"`,
  `"median"`, `"min"`, `"max"`, `"sum"`, and `"valid_prop"`.

- by:

  Grouping columns defining independent time series.

- condition_col:

  Optional condition column appended to `by`.

- value_cols:

  Numeric columns to summarise. When `NULL`, common gaze and pupil
  columns are detected.

- ts_col:

  Timestamp column.

- window_unit:

  Unit used by `window_size` and `step`.

- time_unit:

  Unit of the timestamp column.

- include_partial:

  Include a final window that is shorter than `window_size`.

## Value

A tibble with one row per group and time window.

## Examples

``` r
pupil <- data.frame(
  USER_ID = "P01",
  TIME = seq(0, 0.99, by = 0.01),
  mean_pupil = sin(seq(0, 2 * pi, length.out = 100))
)
analyze_gazepoint_window(
  pupil,
  window_size = 100,
  step = 50,
  value_cols = "mean_pupil"
)
#> # A tibble: 18 × 10
#>    USER_ID window_start window_end window_mid window_size window_step
#>    <chr>          <dbl>      <dbl>      <dbl>       <dbl>       <dbl>
#>  1 P01             0          0.1        0.05         100          50
#>  2 P01             0.05       0.15       0.1          100          50
#>  3 P01             0.1        0.2        0.15         100          50
#>  4 P01             0.15       0.25       0.2          100          50
#>  5 P01             0.2        0.3        0.25         100          50
#>  6 P01             0.25       0.35       0.3          100          50
#>  7 P01             0.3        0.4        0.35         100          50
#>  8 P01             0.35       0.45       0.4          100          50
#>  9 P01             0.4        0.5        0.45         100          50
#> 10 P01             0.45       0.55       0.5          100          50
#> 11 P01             0.5        0.6        0.55         100          50
#> 12 P01             0.55       0.65       0.6          100          50
#> 13 P01             0.6        0.7        0.65         100          50
#> 14 P01             0.65       0.75       0.7          100          50
#> 15 P01             0.7        0.8        0.75         100          50
#> 16 P01             0.75       0.85       0.8          100          50
#> 17 P01             0.8        0.9        0.85         100          50
#> 18 P01             0.85       0.95       0.9          100          50
#> # ℹ 4 more variables: window_unit <chr>, n_samples <int>,
#> #   mean_pupil_mean <dbl>, mean_pupil_sd <dbl>
```
