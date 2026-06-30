# Validate a Gazepoint master sample table

Performs formal validation checks on a Gazepoint master sample-level
table created by
[`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
or
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md).
This function is intended as a gate between master-table construction
and more advanced steps such as pupil preprocessing, AOI modelling, or
statistical analysis.

## Usage

``` r
validate_gazepoint_master(
  master,
  min_valid_sample_pct = 75,
  max_missing_gaze_pct = 25,
  max_missing_pupil_pct = 50,
  max_offscreen_gaze_pct = 25,
  require_pupil = FALSE,
  require_aoi = FALSE,
  fail_on_error = FALSE
)
```

## Arguments

- master:

  A Gazepoint master sample-level table.

- min_valid_sample_pct:

  Minimum acceptable percentage of valid gaze samples. Defaults to `75`.

- max_missing_gaze_pct:

  Maximum acceptable percentage of missing gaze samples. Defaults to
  `25`.

- max_missing_pupil_pct:

  Maximum acceptable percentage of missing pupil samples. Defaults to
  `50`.

- max_offscreen_gaze_pct:

  Maximum acceptable percentage of off-screen gaze samples. Defaults to
  `25`.

- require_pupil:

  Logical. If `TRUE`, the validation fails when no usable pupil column
  is present. Defaults to `FALSE`.

- require_aoi:

  Logical. If `TRUE`, the validation fails when no real AOI samples are
  present. Defaults to `FALSE`.

- fail_on_error:

  Logical. If `TRUE`, the function aborts when one or more validation
  checks fail. Defaults to `FALSE`.

## Value

A named list with:

- summary:

  One-row validation summary.

- checks:

  A tibble containing all validation checks.

- failed_checks:

  Validation checks with status `"fail"`.

- warning_checks:

  Validation checks with status `"warning"`.

- column_map:

  Detected column mapping used for validation.

## Examples

``` r
if (FALSE) { # \dontrun{
master <- create_gazepoint_master(
  gaze_data = results$all_gaze,
  screen_width_px = 1920,
  screen_height_px = 1080
)

validation <- validate_gazepoint_master(master)

validation$summary
validation$checks
} # }
```
