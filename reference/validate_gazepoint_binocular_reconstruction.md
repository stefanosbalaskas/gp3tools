# Validate binocular reconstruction using artificial monocular loss

Uses bilateral observations as known reference values, temporarily masks
one eye, refits cross-eye calibration without the masked target values,
reconstructs the hidden observations, and compares predictions with the
held-out values. This provides dataset-specific empirical reconstruction
diagnostics rather than assuming that a cross-eye regression is
adequate.

## Usage

``` r
validate_gazepoint_binocular_reconstruction(
  data,
  left_col,
  right_col,
  time_col = NULL,
  group_cols = NULL,
  gap_group_cols = NULL,
  fallback_group_cols = NULL,
  direction = c("both", "left_from_right", "right_from_left"),
  mask_prop = 0.2,
  mask_mode = c("random", "contiguous"),
  block_size = 6L,
  repeats = 5L,
  seed = 1L,
  min_pairs = 30L,
  min_unique = 5L,
  min_r2 = NULL,
  time_unit = c("auto", "milliseconds", "seconds"),
  max_gap_ms = Inf,
  allow_edge_gaps = TRUE,
  allow_extrapolation = FALSE,
  valid_min = NULL,
  valid_max = NULL
)
```

## Arguments

- data:

  A data frame containing binocular pupil measurements.

- left_col, right_col:

  Numeric pupil columns.

- time_col:

  Optional time column; required for meaningful contiguous-gap
  validation and finite `max_gap_ms`.

- group_cols:

  Primary calibration and masking groups.

- gap_group_cols:

  Optional groups used to define temporal missing-eye runs during
  reconstruction; defaults to `group_cols`.

- fallback_group_cols:

  Optional calibration fallback groups.

- direction:

  `"both"`, `"left_from_right"`, or `"right_from_left"`.

- mask_prop:

  Proportion of bilateral observations masked in each repeat.

- mask_mode:

  `"random"` masks individual observations; `"contiguous"` masks short
  ordered runs.

- block_size:

  Number of samples per attempted contiguous block.

- repeats:

  Number of repeated artificial-missingness evaluations.

- seed:

  Random seed. The caller's RNG state is restored on exit.

- min_pairs, min_unique, min_r2:

  Calibration gates.

- time_unit:

  Time unit used by reconstruction.

- max_gap_ms, allow_edge_gaps, allow_extrapolation:

  Reconstruction gates.

- valid_min, valid_max:

  Optional bounds.

## Value

A `gp3_binocular_validation` object containing repeat-level `metrics`,
row-level `predictions`, aggregated `summary`, and settings.

## Details

Artificial masking evaluates prediction error where the hidden target is
actually known. It does not prove that naturally missing observations
are missing at random or that their unobserved values would follow the
same error distribution.

## See also

[`fit_gazepoint_binocular_calibration()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_binocular_calibration.md),
[`stress_test_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/stress_test_gazepoint_binocular_reconstruction.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 5, n_trials = 2, seed = 21)
val <- validate_gazepoint_binocular_reconstruction(
  dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
  group_cols = "subject", mask_prop = 0.1, repeats = 2,
  min_pairs = 20, seed = 9
)
val$summary
#> # A tibble: 2 × 11
#>   direction       repeats total_requested total_predicted prediction_rate   rmse
#>   <chr>             <int>           <int>           <int>           <dbl>  <dbl>
#> 1 left_from_right       2              65              64           0.985 0.0952
#> 2 right_from_left       2              55              54           0.982 0.0890
#> # ℹ 5 more variables: mae <dbl>, bias <dbl>, median_error <dbl>,
#> #   error_mad <dbl>, correlation <dbl>
```
