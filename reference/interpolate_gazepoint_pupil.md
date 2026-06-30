# Interpolate short missing gaps in Gazepoint pupil data

Performs linear interpolation over short internal gaps in Gazepoint
pupil data. This function is intended to be used after
[`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md)
or
[`flag_gazepoint_pupil_artifacts()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_artifacts.md).
When available, `pupil_clean` is used as the preferred default input
column, followed by `pupil_for_preprocessing`. Leading gaps, trailing
gaps, long gaps, non-finite time values, and groups with too few valid
pupil samples are not interpolated.

## Usage

``` r
interpolate_gazepoint_pupil(
  data,
  pupil_col = NULL,
  time_col = NULL,
  group_cols = c("subject", "media_id"),
  max_gap_ms = 150,
  max_gap_samples = Inf,
  min_valid_points = 2
)
```

## Arguments

- data:

  A Gazepoint master table, preferably after
  [`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md)
  or
  [`flag_gazepoint_pupil_artifacts()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_artifacts.md).

- pupil_col:

  Optional name of the pupil column to interpolate. If `NULL`, the
  function detects one of `pupil_clean`, `pupil_for_preprocessing`,
  `mean_pupil`, `pupil`, `pupil_raw`, `left_pupil`, or `right_pupil`.

- time_col:

  Optional name of the time column. If `NULL`, the function detects one
  of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.

- group_cols:

  Character vector of grouping columns used to keep interpolation within
  independent time series. Defaults to `c("subject", "media_id")` using
  internally standardised names. Use `character(0)` for global
  interpolation.

- max_gap_ms:

  Maximum duration, in milliseconds, of a gap that may be interpolated.
  The duration is measured between the valid samples immediately before
  and after the gap. Defaults to `150`.

- max_gap_samples:

  Maximum number of consecutive missing samples that may be
  interpolated. Defaults to `Inf`.

- min_valid_points:

  Minimum number of valid samples required within a group before
  interpolation is attempted. Defaults to `2`.

## Value

A tibble containing the original data plus interpolation columns.

## Examples

``` r
if (FALSE) { # \dontrun{
flagged <- flag_gazepoint_pupil(master)

interpolated <- interpolate_gazepoint_pupil(flagged)

dplyr::count(interpolated, pupil_interpolation_status)

artifact_flagged <- flag_gazepoint_pupil_artifacts(master)

artifact_interpolated <- interpolate_gazepoint_pupil(artifact_flagged)

dplyr::count(artifact_interpolated, pupil_interpolation_status)
} # }
```
