# Example AOI-window summary table

A lightweight synthetic AOI-window summary table created from
`gazepoint_example_master`. It can be used in examples for AOI-window
denominator checks, GLMM preparation, and AOI-window modelling.

## Usage

``` r
gazepoint_example_aoi_windows
```

## Format

A tibble with one row per participant, stimulus/trial, and AOI time
window.

## Examples

``` r
data(gazepoint_example_aoi_windows)
head(gazepoint_example_aoi_windows)
#> # A tibble: 6 × 26
#>   subject MEDIA_ID trial_global condition window_label window_start_ms
#>   <chr>   <chr>    <chr>        <chr>     <chr>                  <dbl>
#> 1 S01     stim1    control_T1   control   0_500ms                    0
#> 2 S01     stim1    control_T1   control   500_1000ms               500
#> 3 S01     stim1    control_T1   control   1000_1500ms             1000
#> 4 S01     stim1    control_T1   control   1500_2000ms             1500
#> 5 S01     stim1    treatment_T1 treatment 0_500ms                    0
#> 6 S01     stim1    treatment_T1 treatment 500_1000ms               500
#> # ℹ 20 more variables: window_end_ms <dbl>, n_window_samples <int>,
#> #   n_target_samples <int>, n_distractor_samples <int>,
#> #   n_non_aoi_samples <int>, n_missing_aoi_samples <int>,
#> #   n_other_aoi_samples <int>, n_unique_aoi_states <int>,
#> #   first_aoi_state <chr>, last_aoi_state <chr>, n_aoi_samples <int>,
#> #   n_valid_denominator_samples <int>, target_sample_prop_all <dbl>,
#> #   target_sample_prop_valid <dbl>, target_sample_prop_aoi <dbl>, …
```
