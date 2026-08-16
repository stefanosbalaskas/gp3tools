# Stress-test binocular reconstruction across missingness levels

Repeats artificial monocular-loss validation across declared missingness
levels and random/contiguous masking modes to show where cross-eye
reconstruction begins to degrade.

## Usage

``` r
stress_test_gazepoint_binocular_reconstruction(
  data,
  left_col,
  right_col,
  time_col = NULL,
  group_cols = NULL,
  gap_group_cols = NULL,
  fallback_group_cols = NULL,
  missingness = c(0.05, 0.1, 0.2, 0.3),
  mask_modes = c("random", "contiguous"),
  block_size = 6L,
  repeats = 3L,
  seed = 1L,
  min_pairs = 30L,
  min_unique = 5L,
  min_r2 = NULL,
  max_gap_ms = Inf,
  valid_min = NULL,
  valid_max = NULL
)
```

## Arguments

- data, left_col, right_col, time_col, group_cols, gap_group_cols,
  fallback_group_cols:

  See
  [`validate_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_binocular_reconstruction.md).

- missingness:

  Numeric proportions strictly between 0 and 1.

- mask_modes:

  One or both of `"random"` and `"contiguous"`.

- block_size:

  Contiguous mask size in samples.

- repeats:

  Repeats per stress-test cell.

- seed:

  Base seed; deterministic offsets are used across cells.

- min_pairs, min_unique, min_r2, max_gap_ms, valid_min, valid_max:

  Passed to validation.

## Value

A `gp3_binocular_stress_test` object containing cell-level `results` and
individual validation objects.

## See also

[`validate_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_binocular_reconstruction.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 22)
stress_test_gazepoint_binocular_reconstruction(
  dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
  group_cols = "subject", missingness = c(0.05, 0.10),
  mask_modes = "random", repeats = 1, min_pairs = 20
)
#> $results
#> # A tibble: 4 × 13
#>   mask_mode missingness direction       repeats total_requested total_predicted
#>   <chr>           <dbl> <chr>             <int>           <int>           <int>
#> 1 random           0.05 left_from_right       1               9               9
#> 2 random           0.05 right_from_left       1              15              15
#> 3 random           0.1  left_from_right       1              22              22
#> 4 random           0.1  right_from_left       1              25              25
#> # ℹ 7 more variables: prediction_rate <dbl>, rmse <dbl>, mae <dbl>, bias <dbl>,
#> #   median_error <dbl>, error_mad <dbl>, correlation <dbl>
#> 
#> $validations
#> $validations$random_0.05
#> $summary
#> # A tibble: 2 × 11
#>   direction       repeats total_requested total_predicted prediction_rate   rmse
#>   <chr>             <int>           <int>           <int>           <dbl>  <dbl>
#> 1 left_from_right       1               9               9               1 0.0781
#> 2 right_from_left       1              15              15               1 0.102 
#> # ℹ 5 more variables: mae <dbl>, bias <dbl>, median_error <dbl>,
#> #   error_mad <dbl>, correlation <dbl>
#> 
#> $metrics
#> # A tibble: 2 × 11
#>   repeat_id direction      n_requested n_predicted prediction_rate   rmse    mae
#>       <int> <chr>                <int>       <int>           <dbl>  <dbl>  <dbl>
#> 1         1 left_from_rig…           9           9               1 0.0781 0.0659
#> 2         1 right_from_le…          15          15               1 0.102  0.0817
#> # ℹ 4 more variables: bias <dbl>, median_error <dbl>, error_mad <dbl>,
#> #   correlation <dbl>
#> 
#> $predictions
#> # A tibble: 24 × 14
#>    repeat_id row_id direction       observed predicted    error status  model_id
#>        <int>  <int> <chr>              <dbl>     <dbl>    <dbl> <chr>   <chr>   
#>  1         1     18 left_from_right     3.37      3.36 -0.00270 left_r… binoc_0…
#>  2         1     35 left_from_right     3.28      3.38  0.0999  left_r… binoc_0…
#>  3         1    128 left_from_right     4.04      4.12  0.0864  left_r… binoc_0…
#>  4         1    151 left_from_right     4.11      4.11 -0.00527 left_r… binoc_0…
#>  5         1    164 left_from_right     4.09      4.15  0.0595  left_r… binoc_0…
#>  6         1    230 left_from_right     4.24      4.17 -0.0659  left_r… binoc_0…
#>  7         1    296 left_from_right     3.77      3.81  0.0356  left_r… binoc_0…
#>  8         1    439 left_from_right     3.70      3.59 -0.114   left_r… binoc_0…
#>  9         1    464 left_from_right     3.78      3.66 -0.124   left_r… binoc_0…
#> 10         1      1 right_from_left     3.25      3.40  0.148   right_… binoc_0…
#> # ℹ 14 more rows
#> # ℹ 6 more variables: calibration_level <chr>, r_squared <dbl>,
#> #   extrapolated <lgl>, gap_ms <dbl>, subject <chr>, timestamp_ms <dbl>
#> 
#> $settings
#> $settings$left_col
#> [1] "pupil_left"
#> 
#> $settings$right_col
#> [1] "pupil_right"
#> 
#> $settings$time_col
#> [1] "timestamp_ms"
#> 
#> $settings$group_cols
#> [1] "subject"
#> 
#> $settings$gap_group_cols
#> [1] "subject"
#> 
#> $settings$direction
#> [1] "both"
#> 
#> $settings$mask_prop
#> [1] 0.05
#> 
#> $settings$mask_mode
#> [1] "random"
#> 
#> $settings$block_size
#> [1] 6
#> 
#> $settings$repeats
#> [1] 1
#> 
#> $settings$seed
#> [1] 1
#> 
#> $settings$min_pairs
#> [1] 20
#> 
#> $settings$min_unique
#> [1] 5
#> 
#> $settings$min_r2
#> NULL
#> 
#> $settings$max_gap_ms
#> [1] Inf
#> 
#> $settings$allow_edge_gaps
#> [1] TRUE
#> 
#> $settings$allow_extrapolation
#> [1] FALSE
#> 
#> $settings$valid_min
#> NULL
#> 
#> $settings$valid_max
#> NULL
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_validation"
#> 
#> $validations$random_0.1
#> $summary
#> # A tibble: 2 × 11
#>   direction       repeats total_requested total_predicted prediction_rate   rmse
#>   <chr>             <int>           <int>           <int>           <dbl>  <dbl>
#> 1 left_from_right       1              22              22               1 0.0890
#> 2 right_from_left       1              25              25               1 0.109 
#> # ℹ 5 more variables: mae <dbl>, bias <dbl>, median_error <dbl>,
#> #   error_mad <dbl>, correlation <dbl>
#> 
#> $metrics
#> # A tibble: 2 × 11
#>   repeat_id direction      n_requested n_predicted prediction_rate   rmse    mae
#>       <int> <chr>                <int>       <int>           <dbl>  <dbl>  <dbl>
#> 1         1 left_from_rig…          22          22               1 0.0890 0.0729
#> 2         1 right_from_le…          25          25               1 0.109  0.0830
#> # ℹ 4 more variables: bias <dbl>, median_error <dbl>, error_mad <dbl>,
#> #   correlation <dbl>
#> 
#> $predictions
#> # A tibble: 47 × 14
#>    repeat_id row_id direction       observed predicted   error status   model_id
#>        <int>  <int> <chr>              <dbl>     <dbl>   <dbl> <chr>    <chr>   
#>  1         1     17 left_from_right     3.32      3.42  0.104  left_re… binoc_0…
#>  2         1     26 left_from_right     3.24      3.42  0.176  left_re… binoc_0…
#>  3         1     36 left_from_right     3.36      3.42  0.0588 left_re… binoc_0…
#>  4         1     41 left_from_right     3.46      3.40 -0.0546 left_re… binoc_0…
#>  5         1     69 left_from_right     3.39      3.37 -0.0255 left_re… binoc_0…
#>  6         1    108 left_from_right     3.33      3.44  0.109  left_re… binoc_0…
#>  7         1    149 left_from_right     4.16      4.13 -0.0318 left_re… binoc_0…
#>  8         1    154 left_from_right     4.16      4.13 -0.0347 left_re… binoc_0…
#>  9         1    159 left_from_right     4.16      4.12 -0.0342 left_re… binoc_0…
#> 10         1    197 left_from_right     4.29      4.19 -0.0957 left_re… binoc_0…
#> # ℹ 37 more rows
#> # ℹ 6 more variables: calibration_level <chr>, r_squared <dbl>,
#> #   extrapolated <lgl>, gap_ms <dbl>, subject <chr>, timestamp_ms <dbl>
#> 
#> $settings
#> $settings$left_col
#> [1] "pupil_left"
#> 
#> $settings$right_col
#> [1] "pupil_right"
#> 
#> $settings$time_col
#> [1] "timestamp_ms"
#> 
#> $settings$group_cols
#> [1] "subject"
#> 
#> $settings$gap_group_cols
#> [1] "subject"
#> 
#> $settings$direction
#> [1] "both"
#> 
#> $settings$mask_prop
#> [1] 0.1
#> 
#> $settings$mask_mode
#> [1] "random"
#> 
#> $settings$block_size
#> [1] 6
#> 
#> $settings$repeats
#> [1] 1
#> 
#> $settings$seed
#> [1] 2
#> 
#> $settings$min_pairs
#> [1] 20
#> 
#> $settings$min_unique
#> [1] 5
#> 
#> $settings$min_r2
#> NULL
#> 
#> $settings$max_gap_ms
#> [1] Inf
#> 
#> $settings$allow_edge_gaps
#> [1] TRUE
#> 
#> $settings$allow_extrapolation
#> [1] FALSE
#> 
#> $settings$valid_min
#> NULL
#> 
#> $settings$valid_max
#> NULL
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_validation"
#> 
#> 
#> $settings
#> $settings$missingness
#> [1] 0.05 0.10
#> 
#> $settings$mask_modes
#> [1] "random"
#> 
#> $settings$block_size
#> [1] 6
#> 
#> $settings$repeats
#> [1] 1
#> 
#> $settings$seed
#> [1] 1
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_stress_test"
```
