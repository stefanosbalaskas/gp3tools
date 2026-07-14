# Calculate mean binocular pupil size

Calculate mean binocular pupil size

## Usage

``` r
mean_gazepoint_pupil(
  master_df,
  lp_col = "LPupil",
  rp_col = "RPupil",
  output_col = "mean_pupil",
  min_eyes = 1
)
```

## Arguments

- master_df:

  A sample-level pupil data frame.

- lp_col, rp_col:

  Left- and right-pupil columns.

- output_col:

  Name of the generated column.

- min_eyes:

  Minimum number of finite eye measurements required.

## Value

The input data with a binocular mean-pupil column.

## Examples

``` r
pupil <- data.frame(
  LPupil = c(3, NA, 4),
  RPupil = c(3.2, 3.5, NA)
)
mean_gazepoint_pupil(pupil)
#>   LPupil RPupil mean_pupil
#> 1      3    3.2        3.1
#> 2     NA    3.5        3.5
#> 3      4     NA        4.0
```
