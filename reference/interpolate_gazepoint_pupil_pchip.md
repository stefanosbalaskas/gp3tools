# Interpolate Gazepoint pupil data using PCHIP

Apply shape-preserving piecewise cubic Hermite interpolation to short
internal gaps in Gazepoint pupil time series. This helper is intended as
a sensitivity branch alongside the default linear interpolation
workflow. It does not overwrite the original pupil column.

## Usage

``` r
interpolate_gazepoint_pupil_pchip(
  data,
  pupil_col = NULL,
  time_col = NULL,
  grouping_cols = NULL,
  max_gap_ms = 500,
  max_gap_samples = NULL,
  min_valid_points = 3,
  output_col = "pupil_interpolated_pchip",
  flag_col = "interpolated_pupil_pchip",
  status_col = "pchip_interpolation_status"
)
```

## Arguments

- data:

  A data frame containing pupil time-series data.

- pupil_col:

  Name of the pupil column to interpolate. If `NULL`, common processed
  pupil columns are detected automatically.

- time_col:

  Name of the time column. If `NULL`, common time columns are detected
  automatically.

- grouping_cols:

  Optional character vector of grouping columns. If `NULL`, common
  participant/trial/media grouping columns are detected automatically.
  Use `character(0)` to interpolate the full data as one sequence.

- max_gap_ms:

  Maximum internal gap duration, in milliseconds, eligible for
  interpolation. If both `max_gap_ms` and `max_gap_samples` are
  supplied, both criteria must be satisfied.

- max_gap_samples:

  Maximum number of consecutive missing samples eligible for
  interpolation.

- min_valid_points:

  Minimum number of valid non-missing points required within a group
  before PCHIP interpolation is attempted.

- output_col:

  Name of the interpolated pupil output column.

- flag_col:

  Name of the logical flag column indicating samples filled by PCHIP
  interpolation.

- status_col:

  Name of the interpolation-status column.

## Value

A tibble with PCHIP interpolation columns added.
