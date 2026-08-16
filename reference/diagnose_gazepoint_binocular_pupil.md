# Diagnose binocular pupil quality and agreement

Quantifies binocular availability, monocular loss, left/right agreement,
systematic offset, regression diagnostics, and temporal missingness runs
before any model-based reconstruction is attempted. Source pupil columns
are never modified.

## Usage

``` r
diagnose_gazepoint_binocular_pupil(
  data,
  left_col,
  right_col,
  time_col = NULL,
  group_cols = NULL,
  time_unit = c("auto", "milliseconds", "seconds"),
  valid_min = NULL,
  valid_max = NULL,
  min_pairs = 30L,
  min_unique = 5L,
  disagreement_mad_k = 6
)
```

## Arguments

- data:

  A data frame containing left and right pupil measurements.

- left_col, right_col:

  Names of numeric pupil columns.

- time_col:

  Optional numeric time column. When supplied, gap durations and
  time-order diagnostics are reported.

- group_cols:

  Optional grouping columns, typically participant and/or session.

- time_unit:

  Unit for `time_col`: `"auto"`, `"milliseconds"`, or `"seconds"`.

- valid_min, valid_max:

  Optional physiological or instrument-specific bounds. Values outside
  the declared bounds are treated as unavailable for diagnostics; the
  source data remain unchanged.

- min_pairs:

  Minimum bilateral observations used to label a group as
  calibration-eligible.

- min_unique:

  Minimum unique values required in each eye for eligibility.

- disagreement_mad_k:

  Robust multiplier used to describe unusually large absolute
  between-eye differences. This is a diagnostic flag, not an exclusion
  threshold.

## Value

An object of class `gp3_binocular_diagnostics` containing `summary`,
`gaps`, and `settings` components.

## Details

The Bland-Altman-style limits returned here are descriptive
mean-difference +/- 1.96 SD limits. They do not establish
interchangeability of the two measurements. `correlation` is Pearson
correlation and `rank_correlation` is Spearman correlation on bilateral
observations.

## See also

[`fit_gazepoint_binocular_calibration()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_binocular_calibration.md),
[`reconstruct_gazepoint_binocular_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/reconstruct_gazepoint_binocular_pupil.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 11)
dat$pupil_left[25:28] <- NA_real_
diagnose_gazepoint_binocular_pupil(
  dat, "pupil_left", "pupil_right",
  time_col = "timestamp_ms", group_cols = "subject", min_pairs = 20
)
#> $summary
#> # A tibble: 4 × 39
#>   subject group_key        n n_left n_right n_bilateral n_left_only n_right_only
#>   <chr>   <chr>        <int>  <int>   <int>       <int>       <int>        <int>
#> 1 S001    subject=S001   120    113     117         113           0            4
#> 2 S002    subject=S002   120    117     117         117           0            0
#> 3 S003    subject=S003   120    117     117         117           0            0
#> 4 S004    subject=S004   120    113     113         113           0            0
#> # ℹ 31 more variables: n_both_missing <int>, prop_bilateral <dbl>,
#> #   prop_left_only <dbl>, prop_right_only <dbl>, prop_both_missing <dbl>,
#> #   left_mean <dbl>, right_mean <dbl>, left_sd <dbl>, right_sd <dbl>,
#> #   left_median <dbl>, right_median <dbl>, left_mad <dbl>, right_mad <dbl>,
#> #   mean_difference <dbl>, median_difference <dbl>, correlation <dbl>,
#> #   rank_correlation <dbl>, rmse_between_eyes <dbl>, mae_between_eyes <dbl>,
#> #   disagreement_threshold <dbl>, disagreement_fraction <dbl>, …
#> 
#> $gaps
#> # A tibble: 36 × 8
#>    gap_id group_key    n_samples gap_ms edge_gap start_row end_row eye  
#>     <int> <chr>            <int>  <dbl> <lgl>        <int>   <int> <chr>
#>  1      1 subject=S001         1   16.7 FALSE           64      64 left 
#>  2      2 subject=S001         1   16.7 FALSE           19      19 left 
#>  3      3 subject=S001         1   16.7 FALSE           25      25 left 
#>  4      4 subject=S001         1   16.7 FALSE           26      26 left 
#>  5      5 subject=S001         1   16.7 FALSE           27      27 left 
#>  6      6 subject=S001         1   16.7 FALSE           28      28 left 
#>  7      7 subject=S001         1   16.7 FALSE           45      45 left 
#>  8      8 subject=S002         1   16.7 FALSE          182     182 left 
#>  9      9 subject=S002         1   16.7 FALSE          150     150 left 
#> 10     10 subject=S002         1   16.7 FALSE          160     160 left 
#> # ℹ 26 more rows
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
#> $settings$time_unit
#> [1] "auto"
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
#> $settings$disagreement_mad_k
#> [1] 6
#> 
#> 
#> attr(,"class")
#> [1] "gp3_binocular_diagnostics"
```
