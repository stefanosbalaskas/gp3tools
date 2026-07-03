# Combine left and right Gazepoint eye channels

Combines two numeric eye-specific columns into a single analysis column.
The helper is intentionally simple and transparent: it can average
available left/right values, prefer one eye with fallback to the other,
or select the globally less-missing eye as a pragmatic "best eye" rule.

## Usage

``` r
combine_gazepoint_eyes(
  data,
  left_col,
  right_col,
  output_col = "combined_eye",
  method = c("mean", "left", "right", "prefer_left", "prefer_right", "best"),
  valid_min = NULL,
  valid_max = NULL
)
```

## Arguments

- data:

  A data frame.

- left_col, right_col:

  Character names of the left- and right-eye columns.

- output_col:

  Character name of the combined output column.

- method:

  Combination rule. One of `"mean"`, `"left"`, `"right"`,
  `"prefer_left"`, `"prefer_right"`, or `"best"`.

- valid_min, valid_max:

  Optional numeric bounds. Values outside these bounds are treated as
  missing before combination.

## Value

A copy of `data` with `output_col` added.

## Examples

``` r
x <- data.frame(left_pupil = c(3.1, NA, 3.4), right_pupil = c(3.3, 3.2, NA))
combine_gazepoint_eyes(x, "left_pupil", "right_pupil", "pupil")
#>   left_pupil right_pupil pupil
#> 1        3.1         3.3   3.2
#> 2         NA         3.2   3.2
#> 3        3.4          NA   3.4
```
