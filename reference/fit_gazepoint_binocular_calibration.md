# Fit audited cross-eye pupil calibration models

Fits separate left-from-right and right-from-left linear calibration
models using only bilateral observations. Optional fallback levels
permit transparent participant/session-to-pooled fallback without
silently mixing calibration scopes.

## Usage

``` r
fit_gazepoint_binocular_calibration(
  data,
  left_col,
  right_col,
  group_cols = NULL,
  fallback_group_cols = NULL,
  valid_min = NULL,
  valid_max = NULL,
  min_pairs = 30L,
  min_unique = 5L,
  min_r2 = NULL,
  allow_negative_slope = FALSE,
  max_abs_slope = NULL
)
```

## Arguments

- data:

  A data frame containing pupil channels.

- left_col, right_col:

  Numeric pupil columns.

- group_cols:

  Primary calibration grouping columns.

- fallback_group_cols:

  Optional list of fallback grouping specifications. By default a pooled
  fallback is added when `group_cols` is non-empty. Use
  [`list()`](https://rdrr.io/r/base/list.html) to disable fallback.

- valid_min, valid_max:

  Optional measurement bounds.

- min_pairs:

  Minimum bilateral training observations per model.

- min_unique:

  Minimum unique predictor and outcome values.

- min_r2:

  Optional minimum in-sample R-squared. `NULL` reports R-squared without
  using it as a gate.

- allow_negative_slope:

  Whether a non-positive cross-eye slope can be eligible. The
  conservative default is `FALSE`.

- max_abs_slope:

  Optional absolute slope ceiling; use `NULL` for none.

## Value

An object of class `gp3_binocular_calibration` with a flat `models`
table, per-level model tables, and settings.

## Details

R-squared is a diagnostic, not evidence that reconstructed values are
measured observations. Calibration eligibility is explicit and can be
reviewed before reconstruction.

## References

Ong J, He W, Maglanque P, Jiang X, Gillman LM, Vergis A, Hardy K (2025).
A Preprocessing Pipeline for Pupillometry Signal from Multimodal iMotion
Data. *Sensors*, 25(15), 4737.
[doi:10.3390/s25154737](https://doi.org/10.3390/s25154737)

## See also

[`validate_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_binocular_reconstruction.md),
[`reconstruct_gazepoint_binocular_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/reconstruct_gazepoint_binocular_pupil.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 12)
fit_gazepoint_binocular_calibration(
  dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
)
#> $models
#> # A tibble: 10 × 23
#>    model_id      direction calibration_level group_key subject n_pairs intercept
#>    <chr>         <chr>     <chr>             <chr>     <chr>     <int>     <dbl>
#>  1 binoc_001_le… left_fro… subject           subject=… S001        117     2.00 
#>  2 binoc_002_ri… right_fr… subject           subject=… S001        117     1.77 
#>  3 binoc_003_le… left_fro… subject           subject=… S002        117     2.18 
#>  4 binoc_004_ri… right_fr… subject           subject=… S002        117     2.07 
#>  5 binoc_005_le… left_fro… subject           subject=… S003        118     2.06 
#>  6 binoc_006_ri… right_fr… subject           subject=… S003        118     1.85 
#>  7 binoc_007_le… left_fro… subject           subject=… S004        119     2.47 
#>  8 binoc_008_ri… right_fr… subject           subject=… S004        119     2.45 
#>  9 binoc_009_le… left_fro… pooled            __pooled… NA          471     0.291
#> 10 binoc_010_ri… right_fr… pooled            __pooled… NA          471     0.119
#> # ℹ 16 more variables: slope <dbl>, r_squared <dbl>, adjusted_r_squared <dbl>,
#> #   rmse <dbl>, mae <dbl>, residual_sd <dbl>, residual_median <dbl>,
#> #   residual_mad <dbl>, predictor_min <dbl>, predictor_max <dbl>,
#> #   outcome_min <dbl>, outcome_max <dbl>, eligible <lgl>, status <chr>,
#> #   reason <chr>, model_index <int>
#> 
#> $levels
#> $levels[[1]]
#> $levels[[1]]$group_cols
#> [1] "subject"
#> 
#> $levels[[1]]$calibration_level
#> [1] "subject"
#> 
#> $levels[[1]]$models
#>                    model_id       direction calibration_level    group_key
#> 1 binoc_001_left_from_right left_from_right           subject subject=S001
#> 2 binoc_002_right_from_left right_from_left           subject subject=S001
#> 3 binoc_003_left_from_right left_from_right           subject subject=S002
#> 4 binoc_004_right_from_left right_from_left           subject subject=S002
#> 5 binoc_005_left_from_right left_from_right           subject subject=S003
#> 6 binoc_006_right_from_left right_from_left           subject subject=S003
#> 7 binoc_007_left_from_right left_from_right           subject subject=S004
#> 8 binoc_008_right_from_left right_from_left           subject subject=S004
#>   subject n_pairs intercept     slope r_squared adjusted_r_squared       rmse
#> 1    S001     117  2.001514 0.3725398 0.1626599         0.15537867 0.08464463
#> 2    S001     117  1.774759 0.4366241 0.1626599         0.15537867 0.09163616
#> 3    S002     117  2.183236 0.4448511 0.2115359         0.20467967 0.08930178
#> 4    S002     117  2.071458 0.4755206 0.2115359         0.20467967 0.09232885
#> 5    S003     118  2.063869 0.3757985 0.1651391         0.15794205 0.08448250
#> 6    S003     118  1.851364 0.4394353 0.1651391         0.15794205 0.09135592
#> 7    S004     119  2.465963 0.2576785 0.0673498         0.05937843 0.09218440
#> 8    S004     119  2.449276 0.2613715 0.0673498         0.05937843 0.09284264
#>          mae residual_sd residual_median residual_mad predictor_min
#> 1 0.06532267  0.08500869    0.0040052888   0.05273690      2.933003
#> 2 0.06931170  0.09203030    0.0050225297   0.05560719      2.955297
#> 3 0.07214268  0.08968587    0.0065146752   0.06530157      3.746641
#> 4 0.07432371  0.09272597    0.0042714279   0.06863269      3.732032
#> 5 0.06719721  0.08484276    0.0051635294   0.05879319      3.042834
#> 6 0.07006244  0.09174550   -0.0029159035   0.05488545      3.034559
#> 7 0.07439054  0.09257419    0.0041462449   0.06131905      3.018587
#> 8 0.07177167  0.09323521   -0.0009564984   0.05513519      3.141336
#>   predictor_max outcome_min outcome_max eligible   status reason
#> 1      3.447186    2.955297    3.427283     TRUE eligible   <NA>
#> 2      3.427283    2.933003    3.447186     TRUE eligible   <NA>
#> 3      4.270105    3.732032    4.161962     TRUE eligible   <NA>
#> 4      4.161962    3.746641    4.270105     TRUE eligible   <NA>
#> 5      3.583341    3.034559    3.530136     TRUE eligible   <NA>
#> 6      3.530136    3.042834    3.583341     TRUE eligible   <NA>
#> 7      3.549814    3.141336    3.548403     TRUE eligible   <NA>
#> 8      3.548403    3.018587    3.549814     TRUE eligible   <NA>
#> 
#> 
#> $levels[[2]]
#> $levels[[2]]$group_cols
#> character(0)
#> 
#> $levels[[2]]$calibration_level
#> [1] "pooled"
#> 
#> $levels[[2]]$models
#>                    model_id       direction calibration_level  group_key
#> 1 binoc_009_left_from_right left_from_right            pooled __pooled__
#> 2 binoc_010_right_from_left right_from_left            pooled __pooled__
#>   n_pairs intercept     slope r_squared adjusted_r_squared      rmse        mae
#> 1     471 0.2909944 0.9163287 0.8834643          0.8832158 0.1055043 0.08308950
#> 2     471 0.1193111 0.9641346 0.8834643          0.8832158 0.1082214 0.08489755
#>   residual_sd residual_median residual_mad predictor_min predictor_max
#> 1   0.1056164     0.003452670   0.06741157      2.933003      4.270105
#> 2   0.1083365    -0.003310834   0.06843109      2.955297      4.161962
#>   outcome_min outcome_max eligible   status reason
#> 1    2.955297    4.161962     TRUE eligible   <NA>
#> 2    2.933003    4.270105     TRUE eligible   <NA>
#> 
#> 
#> 
#> $settings
#> $settings$left_col
#> [1] "pupil_left"
#> 
#> $settings$right_col
#> [1] "pupil_right"
#> 
#> $settings$group_cols
#> [1] "subject"
#> 
#> $settings$fallback_group_cols
#> $settings$fallback_group_cols[[1]]
#> character(0)
#> 
#> 
#> $settings$valid_min
#> NULL
#> 
#> $settings$valid_max
#> NULL
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
#> $settings$allow_negative_slope
#> [1] FALSE
#> 
#> $settings$max_abs_slope
#> NULL
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_calibration"
```
