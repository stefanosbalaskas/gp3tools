# Audit binocular reconstruction burden and imbalance

Summarises how much of the retained pupil signal depends on model-based
reconstruction, where reconstruction was blocked, whether reconstruction
is uneven across declared groups, and how much reconstruction shifts a
simple available-eye combined signal.

## Usage

``` r
audit_gazepoint_binocular_reconstruction(
  data,
  by = NULL,
  prefix = "gp3_binocular",
  max_reconstruction_prop = NULL,
  max_group_rate_difference = NULL
)
```

## Arguments

- data:

  Output from
  [`reconstruct_gazepoint_binocular_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/reconstruct_gazepoint_binocular_pupil.md).

- by:

  Optional condition, participant, stimulus, trial, or other columns
  used to assess reconstruction-rate imbalance.

- prefix:

  Reconstruction prefix.

- max_reconstruction_prop:

  Optional descriptive threshold above which overall reconstruction
  burden is flagged for review. `NULL` reports burden without imposing a
  universal cutoff.

- max_group_rate_difference:

  Optional descriptive threshold for the maximum minus minimum
  reconstruction fraction across `by` groups. `NULL` reports imbalance
  without imposing a universal cutoff.

## Value

A `gp3_binocular_audit` object with overall burden, grouped burden,
status counts, model diagnostics, and audit status.

## Details

Thresholds are governance flags, not statistical tests and not universal
validity cutoffs.

## See also

[`analyse_gazepoint_binocular_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/analyse_gazepoint_binocular_sensitivity.md),
[`summarise_gazepoint_binocular_reporting()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_binocular_reporting.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 23)
dat$pupil_left[35:38] <- NA_real_
rec <- reconstruct_gazepoint_binocular_pupil(
  dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
)
audit_gazepoint_binocular_reconstruction(rec, by = "condition")
#> $overall
#> # A tibble: 1 × 11
#>   group_key      n n_bilateral_observed n_reconstructed n_monocular_unreconstr…¹
#>   <chr>      <int>                <int>           <int>                    <int>
#> 1 __pooled__   480                  455               4                        0
#> # ℹ abbreviated name: ¹​n_monocular_unreconstructed
#> # ℹ 6 more variables: n_unavailable <int>, n_blocked <int>,
#> #   bilateral_observed_fraction <dbl>, reconstruction_fraction <dbl>,
#> #   monocular_unreconstructed_fraction <dbl>, unavailable_fraction <dbl>
#> 
#> $by_group
#> # A tibble: 2 × 12
#>   condition group_key               n n_bilateral_observed n_reconstructed
#>   <chr>     <chr>               <int>                <int>           <int>
#> 1 control   condition=control     240                  227               4
#> 2 treatment condition=treatment   240                  228               0
#> # ℹ 7 more variables: n_monocular_unreconstructed <int>, n_unavailable <int>,
#> #   n_blocked <int>, bilateral_observed_fraction <dbl>,
#> #   reconstruction_fraction <dbl>, monocular_unreconstructed_fraction <dbl>,
#> #   unavailable_fraction <dbl>
#> 
#> $status_counts
#> # A tibble: 3 × 3
#>   status                 n proportion
#>   <chr>              <int>      <dbl>
#> 1 bilateral_observed   455    0.948  
#> 2 both_unavailable      21    0.0438 
#> 3 left_reconstructed     4    0.00833
#> 
#> $reconstruction_shift
#> # A tibble: 1 × 5
#>   n_reconstructed_rows_with_shift mean_reconstruction_s…¹ median_reconstructio…²
#>                             <int>                   <dbl>                  <dbl>
#> 1                               4                 0.00365               0.000597
#> # ℹ abbreviated names: ¹​mean_reconstruction_shift, ²​median_reconstruction_shift
#> # ℹ 2 more variables: mean_absolute_reconstruction_shift <dbl>,
#> #   max_absolute_reconstruction_shift <dbl>
#> 
#> $models
#> # A tibble: 10 × 23
#>    model_id      direction calibration_level group_key subject n_pairs intercept
#>    <chr>         <chr>     <chr>             <chr>     <chr>     <int>     <dbl>
#>  1 binoc_001_le… left_fro… subject           subject=… S001        108     2.42 
#>  2 binoc_002_ri… right_fr… subject           subject=… S001        108     2.25 
#>  3 binoc_003_le… left_fro… subject           subject=… S002        117     2.57 
#>  4 binoc_004_ri… right_fr… subject           subject=… S002        117     2.52 
#>  5 binoc_005_le… left_fro… subject           subject=… S003        112     2.40 
#>  6 binoc_006_ri… right_fr… subject           subject=… S003        112     2.73 
#>  7 binoc_007_le… left_fro… subject           subject=… S004        118     2.24 
#>  8 binoc_008_ri… right_fr… subject           subject=… S004        118     2.60 
#>  9 binoc_009_le… left_fro… pooled            __pooled… NA          455     0.350
#> 10 binoc_010_ri… right_fr… pooled            __pooled… NA          455     0.523
#> # ℹ 16 more variables: slope <dbl>, r_squared <dbl>, adjusted_r_squared <dbl>,
#> #   rmse <dbl>, mae <dbl>, residual_sd <dbl>, residual_median <dbl>,
#> #   residual_mad <dbl>, predictor_min <dbl>, predictor_max <dbl>,
#> #   outcome_min <dbl>, outcome_max <dbl>, eligible <lgl>, status <chr>,
#> #   reason <chr>, model_index <int>
#> 
#> $imbalance
#> # A tibble: 1 × 3
#>   max_group_rate_difference threshold flagged
#>                       <dbl>     <dbl> <lgl>  
#> 1                    0.0167    0.0167 FALSE  
#> 
#> $audit
#> # A tibble: 1 × 5
#>   status      reconstruction_fraction reconstruction_threshold burden_flag
#>   <chr>                         <dbl>                    <dbl> <lgl>      
#> 1 descriptive                 0.00833                       NA FALSE      
#> # ℹ 1 more variable: imbalance_flag <lgl>
#> 
#> $settings
#> $settings$by
#> [1] "condition"
#> 
#> $settings$prefix
#> [1] "gp3_binocular"
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_audit"
```
