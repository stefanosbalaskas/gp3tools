# Example pupil-window summary table

A lightweight synthetic pupil-window summary table created from
`gazepoint_example_master`. It can be used in examples for pupil-window
model-data preparation and confirmatory pupil-window modelling.

## Usage

``` r
gazepoint_example_pupil_windows
```

## Format

A tibble with one row per participant, stimulus/trial, condition, and
pupil time window.

## Examples

``` r
data(gazepoint_example_pupil_windows)
head(gazepoint_example_pupil_windows)
#> # A tibble: 6 × 26
#>   subject MEDIA_ID trial_global condition window_label window_start_ms
#>   <chr>   <chr>    <chr>        <chr>     <chr>                  <dbl>
#> 1 S01     stim1    control_T1   control   0_500ms                    0
#> 2 S01     stim1    treatment_T1 treatment 0_500ms                    0
#> 3 S01     stim2    control_T2   control   0_500ms                    0
#> 4 S01     stim2    treatment_T2 treatment 0_500ms                    0
#> 5 S02     stim1    control_T1   control   0_500ms                    0
#> 6 S02     stim1    treatment_T1 treatment 0_500ms                    0
#> # ℹ 20 more variables: window_end_ms <dbl>, n_samples <int>,
#> #   n_valid_pupil <int>, n_missing_pupil <int>, valid_pupil_pct <dbl>,
#> #   missing_pupil_pct <dbl>, mean_pupil <dbl>, sd_pupil <dbl>,
#> #   median_pupil <dbl>, min_pupil <dbl>, max_pupil <dbl>, q25_pupil <dbl>,
#> #   q75_pupil <dbl>, pupil_auc <dbl>, pupil_time_span_ms <dbl>,
#> #   pupil_window_status <chr>, pupil_window_pupil_column <chr>,
#> #   pupil_window_time_column <chr>, pupil_window_min_valid_samples <int>, …
```
