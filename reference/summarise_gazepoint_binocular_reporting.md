# Create manuscript-ready binocular reconstruction reporting

Produces structured reporting fields plus conservative prose describing
the declared calibration, reconstruction burden, and optional
pseudo-missing validation. The text never describes predicted values as
measured or as the biological truth.

## Usage

``` r
summarise_gazepoint_binocular_reporting(
  data,
  audit = NULL,
  validation = NULL,
  by = NULL,
  prefix = "gp3_binocular"
)
```

## Arguments

- data:

  Output from
  [`reconstruct_gazepoint_binocular_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/reconstruct_gazepoint_binocular_pupil.md).

- audit:

  Optional result from
  [`audit_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_binocular_reconstruction.md).

- validation:

  Optional result from
  [`validate_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_binocular_reconstruction.md).

- by:

  Optional audit grouping used when `audit` is not supplied.

- prefix:

  Reconstruction prefix.

## Value

A `gp3_binocular_reporting` object with `summary`, `models`,
`validation`, `text`, and `limitations`.

## See also

[`audit_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_binocular_reconstruction.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 25)
dat$pupil_left[30:33] <- NA_real_
rec <- reconstruct_gazepoint_binocular_pupil(
  dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
)
summarise_gazepoint_binocular_reporting(rec)$text
#> [1] "Binocular pupil handling used the declared `linear_regression` policy. Of 480 rows, 4 (0.8%) contained model-based cross-eye reconstruction; 96.7% were retained as directly observed bilateral samples and 0.0% remained monocular without reconstruction. Eligible cross-eye calibration models used a median of 116 paired observations; the median in-sample R-squared was 0.161.  Reconstructed values were retained as predicted values with explicit row-level provenance; they were not treated as independently measured pupil observations."
```
