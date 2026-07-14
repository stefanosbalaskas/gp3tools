# Smooth gaze coordinates within independent sequences

Applies a centred rolling median or moving average to gaze coordinates.

## Usage

``` r
smooth_gazepoint_coordinate(
  all_gaze,
  method = c("median", "mean"),
  window = 5,
  x_col = "FPOGX",
  y_col = "FPOGY",
  id_col = "USER_ID",
  group_cols = NULL,
  suffix = "_smooth",
  min_valid = 1,
  preserve_missing = TRUE
)
```

## Arguments

- all_gaze:

  A sample-level gaze data frame.

- method:

  Smoothing method.

- window:

  Positive integer rolling-window width.

- x_col, y_col:

  Coordinate columns.

- id_col:

  Participant identifier.

- group_cols:

  Additional independent-sequence columns.

- suffix:

  Suffix for generated columns.

- min_valid:

  Minimum finite samples required in a rolling window.

- preserve_missing:

  Keep smoothed values missing where the original coordinate is missing.

## Value

The input data with smoothed coordinate columns.

## Examples

``` r
gaze <- data.frame(
  USER_ID = "P01",
  FPOGX = c(0.1, 0.11, 0.5, 0.12, 0.13),
  FPOGY = c(0.2, 0.21, 0.6, 0.22, 0.23)
)
smooth_gazepoint_coordinate(gaze, window = 3)
#>   USER_ID FPOGX FPOGY FPOGX_smooth FPOGY_smooth
#> 1     P01  0.10  0.20        0.105        0.205
#> 2     P01  0.11  0.21        0.110        0.210
#> 3     P01  0.50  0.60        0.120        0.220
#> 4     P01  0.12  0.22        0.130        0.230
#> 5     P01  0.13  0.23        0.125        0.225
```
