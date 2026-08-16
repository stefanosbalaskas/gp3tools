# Compare pupil-construction policies as a sensitivity analysis

Constructs the same pupil stream under multiple declared policies and
compares data retention, descriptive distribution summaries, condition
means/contrasts, and pairwise series correlations. It deliberately
avoids converting policy disagreement into an automatic inferential
verdict.

## Usage

``` r
analyse_gazepoint_binocular_sensitivity(
  data,
  left_col,
  right_col,
  policies = c("complete_case", "available_eye", "reconstructed_mean", "left_only",
    "right_only"),
  prefix = "gp3_binocular",
  group_cols = NULL,
  condition_col = NULL,
  valid_min = NULL,
  valid_max = NULL
)
```

## Arguments

- data:

  Raw data or reconstruction output. `reconstructed_mean` requires
  reconstruction columns.

- left_col, right_col:

  Original pupil channels.

- policies:

  Any of `"complete_case"`, `"available_eye"`, `"reconstructed_mean"`,
  `"left_only"`, and `"right_only"`.

- prefix:

  Reconstruction prefix.

- group_cols:

  Optional grouping columns for descriptive summaries.

- condition_col:

  Optional condition column. When supplied, simple descriptive mean
  contrasts relative to the first observed condition are returned; no
  p-values are calculated.

- valid_min, valid_max:

  Optional raw-channel bounds.

## Value

A `gp3_binocular_sensitivity` object containing policy summaries,
pairwise correlations, optional condition summaries/contrasts, and the
constructed long series.

## See also

[`audit_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_binocular_reconstruction.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 24)
dat$pupil_right[20:24] <- NA_real_
rec <- reconstruct_gazepoint_binocular_pupil(
  dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
)
analyse_gazepoint_binocular_sensitivity(
  rec, "pupil_left", "pupil_right", condition_col = "condition"
)
#> $summary
#> # A tibble: 5 × 9
#>   policy    group_key n_total n_usable missing_fraction  mean    sd median   mad
#>   <chr>     <chr>       <int>    <int>            <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1 complete… __pooled…     480      461           0.0396  3.54 0.158   3.54 0.140
#> 2 availabl… __pooled…     480      466           0.0292  3.53 0.158   3.54 0.139
#> 3 reconstr… __pooled…     480      466           0.0292  3.53 0.158   3.54 0.139
#> 4 left_only __pooled…     480      466           0.0292  3.53 0.167   3.52 0.141
#> 5 right_on… __pooled…     480      461           0.0396  3.54 0.168   3.54 0.142
#> 
#> $correlations
#> # A tibble: 10 × 6
#>    policy_1           policy_2           n_complete correlation mean_difference
#>    <chr>              <chr>                   <int>       <dbl>           <dbl>
#>  1 complete_case      available_eye             461       1            0       
#>  2 complete_case      reconstructed_mean        461       1            0       
#>  3 complete_case      left_only                 461       0.942        0.000849
#>  4 complete_case      right_only                461       0.943       -0.000849
#>  5 available_eye      reconstructed_mean        466       1.000       -0.000215
#>  6 available_eye      left_only                 466       0.943        0.000840
#>  7 available_eye      right_only                461       0.943       -0.000849
#>  8 reconstructed_mean left_only                 466       0.943        0.00105 
#>  9 reconstructed_mean right_only                461       0.943       -0.000849
#> 10 left_only          right_only                461       0.776       -0.00170 
#> # ℹ 1 more variable: mean_absolute_difference <dbl>
#> 
#> $condition_summary
#> # A tibble: 10 × 10
#>    policy      condition group_key n_total n_usable missing_fraction  mean    sd
#>    <chr>       <chr>     <chr>       <int>    <int>            <dbl> <dbl> <dbl>
#>  1 complete_c… control   conditio…     240      227           0.0542  3.49 0.148
#>  2 complete_c… treatment conditio…     240      234           0.025   3.58 0.154
#>  3 available_… control   conditio…     240      232           0.0333  3.49 0.148
#>  4 available_… treatment conditio…     240      234           0.025   3.58 0.154
#>  5 reconstruc… control   conditio…     240      232           0.0333  3.49 0.147
#>  6 reconstruc… treatment conditio…     240      234           0.025   3.58 0.154
#>  7 left_only   control   conditio…     240      232           0.0333  3.48 0.158
#>  8 left_only   treatment conditio…     240      234           0.025   3.58 0.162
#>  9 right_only  control   conditio…     240      227           0.0542  3.49 0.155
#> 10 right_only  treatment conditio…     240      234           0.025   3.58 0.169
#> # ℹ 2 more variables: median <dbl>, mad <dbl>
#> 
#> $condition_contrasts
#> # A tibble: 5 × 4
#>   policy             reference comparison mean_difference
#>   <chr>              <chr>     <chr>                <dbl>
#> 1 complete_case      control   treatment           0.0920
#> 2 available_eye      control   treatment           0.0952
#> 3 reconstructed_mean control   treatment           0.0947
#> 4 left_only          control   treatment           0.0960
#> 5 right_only         control   treatment           0.0912
#> 
#> $series
#> # A tibble: 2,400 × 5
#>    row_id policy        pupil source             condition
#>     <int> <chr>         <dbl> <chr>              <chr>    
#>  1      1 complete_case  3.40 bilateral_observed control  
#>  2      2 complete_case  3.39 bilateral_observed control  
#>  3      3 complete_case  3.40 bilateral_observed control  
#>  4      4 complete_case  3.28 bilateral_observed control  
#>  5      5 complete_case  3.37 bilateral_observed control  
#>  6      6 complete_case  3.32 bilateral_observed control  
#>  7      7 complete_case  3.24 bilateral_observed control  
#>  8      8 complete_case  3.39 bilateral_observed control  
#>  9      9 complete_case  3.39 bilateral_observed control  
#> 10     10 complete_case  3.34 bilateral_observed control  
#> # ℹ 2,390 more rows
#> 
#> $settings
#> $settings$policies
#> [1] "complete_case"      "available_eye"      "reconstructed_mean"
#> [4] "left_only"          "right_only"        
#> 
#> $settings$prefix
#> [1] "gp3_binocular"
#> 
#> $settings$group_cols
#> character(0)
#> 
#> $settings$condition_col
#> [1] "condition"
#> 
#> $settings$valid_min
#> NULL
#> 
#> $settings$valid_max
#> NULL
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_sensitivity"
```
