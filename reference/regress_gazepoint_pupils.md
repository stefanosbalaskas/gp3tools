# Fuse binocular pupil traces using cross-eye regression

Fits cross-eye regressions within independent sequences and creates a
regression-smoothed binocular pupil trace. The function is diagnostic
and preprocessing-oriented; it does not imply that one eye causally
predicts the other.

## Usage

``` r
regress_gazepoint_pupils(
  master_df,
  lp_col = "LPupil",
  rp_col = "RPupil",
  id_col = "USER_ID",
  group_cols = NULL,
  direction = c("bidirectional", "right_on_left", "left_on_right"),
  output_col = "pupil_regressed",
  residual_col = "pupil_regression_residual",
  min_complete = 10
)
```

## Arguments

- master_df:

  A sample-level pupil data frame.

- lp_col, rp_col:

  Left- and right-pupil columns.

- id_col:

  Participant identifier.

- group_cols:

  Optional additional independent-sequence columns.

- direction:

  Regression direction. `"bidirectional"` fits both right-on-left and
  left-on-right models.

- output_col:

  Name of the fused pupil column.

- residual_col:

  Name of the right-on-left residual column.

- min_complete:

  Minimum complete binocular samples required for regression. Groups
  below this threshold use the binocular mean.

## Value

The input data with regression-smoothed pupil columns.

## Examples

``` r
pupil <- data.frame(
  USER_ID = rep("P01", 20),
  LPupil = seq(3, 4, length.out = 20),
  RPupil = seq(3.1, 4.1, length.out = 20)
)
regress_gazepoint_pupils(pupil)
#>    USER_ID   LPupil   RPupil pupil_regressed pupil_regression_residual
#> 1      P01 3.000000 3.100000        3.050000             -4.440892e-16
#> 2      P01 3.052632 3.152632        3.102632             -4.440892e-16
#> 3      P01 3.105263 3.205263        3.155263             -4.440892e-16
#> 4      P01 3.157895 3.257895        3.207895             -4.440892e-16
#> 5      P01 3.210526 3.310526        3.260526             -4.440892e-16
#> 6      P01 3.263158 3.363158        3.313158             -4.440892e-16
#> 7      P01 3.315789 3.415789        3.365789             -8.881784e-16
#> 8      P01 3.368421 3.468421        3.418421             -4.440892e-16
#> 9      P01 3.421053 3.521053        3.471053             -4.440892e-16
#> 10     P01 3.473684 3.573684        3.523684             -8.881784e-16
#> 11     P01 3.526316 3.626316        3.576316              0.000000e+00
#> 12     P01 3.578947 3.678947        3.628947             -4.440892e-16
#> 13     P01 3.631579 3.731579        3.681579             -4.440892e-16
#> 14     P01 3.684211 3.784211        3.734211             -4.440892e-16
#> 15     P01 3.736842 3.836842        3.786842             -4.440892e-16
#> 16     P01 3.789474 3.889474        3.839474             -4.440892e-16
#> 17     P01 3.842105 3.942105        3.892105             -4.440892e-16
#> 18     P01 3.894737 3.994737        3.944737              0.000000e+00
#> 19     P01 3.947368 4.047368        3.997368              0.000000e+00
#> 20     P01 4.000000 4.100000        4.050000             -8.881784e-16
#>    pupil_regression_n pupil_regression_method
#> 1                  20           bidirectional
#> 2                  20           bidirectional
#> 3                  20           bidirectional
#> 4                  20           bidirectional
#> 5                  20           bidirectional
#> 6                  20           bidirectional
#> 7                  20           bidirectional
#> 8                  20           bidirectional
#> 9                  20           bidirectional
#> 10                 20           bidirectional
#> 11                 20           bidirectional
#> 12                 20           bidirectional
#> 13                 20           bidirectional
#> 14                 20           bidirectional
#> 15                 20           bidirectional
#> 16                 20           bidirectional
#> 17                 20           bidirectional
#> 18                 20           bidirectional
#> 19                 20           bidirectional
#> 20                 20           bidirectional
```
