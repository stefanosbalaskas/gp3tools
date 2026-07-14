# Interpolate pupil values across detected blink intervals

Masks samples inside blink intervals and interpolates only bounded
internal gaps. Long or edge gaps remain missing.

## Usage

``` r
interpolate_gazepoint_blinks(
  master_df,
  blink_df,
  pupil_cols = NULL,
  id_col = "USER_ID",
  group_cols = NULL,
  ts_col = "TIME",
  start_col = "start_time",
  end_col = "end_time",
  method = c("linear", "spline"),
  max_gap_ms = 500,
  suffix = "_blink_interp",
  keep_mask = TRUE,
  time_unit = c("auto", "seconds", "milliseconds")
)
```

## Arguments

- master_df:

  A sample-level data frame.

- blink_df:

  Blink intervals returned by
  [`detect_gazepoint_blinks()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_blinks.md).

- pupil_cols:

  Pupil columns to interpolate. When `NULL`, common pupil columns are
  detected automatically.

- id_col:

  Participant identifier shared by both inputs.

- group_cols:

  Optional additional grouping columns shared by both inputs.

- ts_col:

  Timestamp column in `master_df`.

- start_col, end_col:

  Blink interval boundaries.

- method:

  Interpolation method.

- max_gap_ms:

  Maximum blink duration eligible for interpolation.

- suffix:

  Suffix used for interpolated columns.

- keep_mask:

  Add `blink_interpolated` and `blink_masked` columns.

- time_unit:

  Timestamp unit.

## Value

The sample table with interpolated pupil columns.

## Examples

``` r
pupil <- data.frame(
  USER_ID = "P01",
  TIME = seq(0, 0.09, by = 0.01),
  mean_pupil = c(3, 3.1, 3.2, NA, NA, 3.3, 3.4, 3.5, 3.6, 3.7)
)
blinks <- detect_gazepoint_blinks(
  pupil,
  min_duration = 10
)
interpolate_gazepoint_blinks(pupil, blinks)
#>    USER_ID TIME mean_pupil blink_masked blink_interpolated
#> 1      P01 0.00        3.0        FALSE              FALSE
#> 2      P01 0.01        3.1        FALSE              FALSE
#> 3      P01 0.02        3.2        FALSE              FALSE
#> 4      P01 0.03         NA         TRUE               TRUE
#> 5      P01 0.04         NA         TRUE               TRUE
#> 6      P01 0.05        3.3        FALSE              FALSE
#> 7      P01 0.06        3.4        FALSE              FALSE
#> 8      P01 0.07        3.5        FALSE              FALSE
#> 9      P01 0.08        3.6        FALSE              FALSE
#> 10     P01 0.09        3.7        FALSE              FALSE
#>    mean_pupil_blink_interp
#> 1                 3.000000
#> 2                 3.100000
#> 3                 3.200000
#> 4                 3.233333
#> 5                 3.266667
#> 6                 3.300000
#> 7                 3.400000
#> 8                 3.500000
#> 9                 3.600000
#> 10                3.700000
```
