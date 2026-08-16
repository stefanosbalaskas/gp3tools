# Reconstruct temporarily unavailable binocular pupil channels

Applies a declared reconstruction policy without overwriting the
original eye channels. Linear-regression reconstruction uses separately
fitted left-from-right and right-from-left models, explicit eligibility
gates, gap-duration restrictions, extrapolation controls, physiological
bounds, and row-level provenance.

## Usage

``` r
reconstruct_gazepoint_binocular_pupil(
  data,
  left_col,
  right_col,
  time_col = NULL,
  group_cols = NULL,
  gap_group_cols = NULL,
  method = c("linear_regression", "available_eye", "none"),
  calibration = NULL,
  fallback_group_cols = NULL,
  min_pairs = 30L,
  min_unique = 5L,
  min_r2 = NULL,
  time_unit = c("auto", "milliseconds", "seconds"),
  max_gap_ms = Inf,
  allow_edge_gaps = TRUE,
  allow_extrapolation = FALSE,
  valid_min = NULL,
  valid_max = NULL,
  exclude_flag_cols = NULL,
  prefix = "gp3_binocular",
  overwrite = FALSE
)
```

## Arguments

- data:

  A data frame containing pupil channels.

- left_col, right_col:

  Numeric pupil columns.

- time_col:

  Optional numeric time column. Required when `max_gap_ms` is finite.

- group_cols:

  Primary calibration grouping columns.

- gap_group_cols:

  Optional grouping columns used only to define temporal missing-eye
  runs. When `NULL`, `group_cols` are used. This permits, for example,
  participant-level calibration with participant-by-trial gap gates.

- method:

  Reconstruction policy: `"linear_regression"`, `"available_eye"`, or
  `"none"`. The latter two do not synthesize a missing eye; they retain
  explicit monocular provenance for downstream construction.

- calibration:

  Optional result from
  [`fit_gazepoint_binocular_calibration()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_binocular_calibration.md).
  When omitted and `method = "linear_regression"`, calibration is fitted
  from `data`.

- fallback_group_cols, min_pairs, min_unique, min_r2:

  Calibration settings used only when `calibration` is not supplied.

- time_unit:

  Unit for `time_col`.

- max_gap_ms:

  Maximum contiguous missing-eye run eligible for model-based
  reconstruction. `Inf` disables the duration gate. No study-specific
  cutoff is imposed by default.

- allow_edge_gaps:

  Whether missing runs touching a group boundary may be reconstructed
  from the simultaneously observed contralateral eye.

- allow_extrapolation:

  Whether predictions outside the calibration predictor range are
  allowed.

- valid_min, valid_max:

  Optional bounds applied both to observed values used by this workflow
  and to predictions.

- exclude_flag_cols:

  Optional logical/numeric flag columns. Rows flagged in any supplied
  column are not reconstructed.

- prefix:

  Prefix for added provenance and reconstructed-channel columns.

- overwrite:

  Whether existing output columns with this prefix may be replaced.

## Value

The input data with additional observed, final-channel, reconstruction,
model, gap, and status columns. Original pupil columns are untouched. A
`gp3_binocular_reconstruction` metadata attribute records the declared
policy and calibration object.

## Details

Reconstructed values are predictions from the contralateral eye; they
are never labelled as measurements. Temporal interpolation and cross-eye
reconstruction solve different missing-data problems and are
deliberately kept separate.

## References

Ong J, He W, Maglanque P, Jiang X, Gillman LM, Vergis A, Hardy K (2025).
A Preprocessing Pipeline for Pupillometry Signal from Multimodal iMotion
Data. *Sensors*, 25(15), 4737.
[doi:10.3390/s25154737](https://doi.org/10.3390/s25154737)

## See also

[`construct_gazepoint_combined_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/construct_gazepoint_combined_pupil.md),
[`validate_gazepoint_binocular_reconstruction()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_binocular_reconstruction.md)

## Examples

``` r
dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 13)
dat$pupil_left[40:43] <- NA_real_
rec <- reconstruct_gazepoint_binocular_pupil(
  dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
  group_cols = "subject", min_pairs = 20
)
table(rec$gp3_binocular_status)
#> 
#>                   bilateral_observed                     both_unavailable 
#>                                  459                                   17 
#>                   left_reconstructed reconstruction_blocked_extrapolation 
#>                                    3                                    1 
```
