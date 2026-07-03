# Flag or filter Gazepoint data by trackloss

Computes trackloss rates globally or within user-specified grouping
columns, then flags or removes groups exceeding a transparent threshold.
Trackloss can be supplied directly through a validity/tracking column or
inferred from missing/out-of-range gaze coordinates.

## Usage

``` r
clean_gazepoint_by_trackloss(
  data,
  group_cols = NULL,
  tracking_col = NULL,
  x_col = NULL,
  y_col = NULL,
  max_trackloss = 0.25,
  action = c("flag", "filter"),
  treat_zero_zero_as_loss = TRUE,
  rate_col = ".gp3_trackloss_rate",
  exclude_col = ".gp3_trackloss_exclude"
)
```

## Arguments

- data:

  A data frame.

- group_cols:

  Optional character vector of grouping columns, for example participant
  and trial identifiers.

- tracking_col:

  Optional tracking/validity column. Logical, numeric, and character
  encodings are supported.

- x_col, y_col:

  Optional gaze coordinate columns used when `tracking_col` is not
  supplied.

- max_trackloss:

  Maximum allowed trackloss proportion per group.

- action:

  Either `"flag"` to retain all rows and add diagnostic columns, or
  `"filter"` to remove groups above the threshold.

- treat_zero_zero_as_loss:

  If `TRUE`, `(0, 0)` gaze coordinates are treated as trackloss when
  using `x_col` and `y_col`.

- rate_col, exclude_col:

  Names of the added diagnostic columns.

## Value

A data frame with diagnostic columns. If `action = "filter"`, rows from
excluded groups are removed. A compact summary is stored in the
`"gp3_trackloss_summary"` attribute.

## Examples

``` r
x <- data.frame(
  participant = c("P1", "P1", "P2", "P2"),
  trial = c(1, 1, 1, 1),
  valid = c(1, 0, 1, 1)
)
clean_gazepoint_by_trackloss(
  x,
  group_cols = c("participant", "trial"),
  tracking_col = "valid",
  max_trackloss = 0.25
)
#>   participant trial valid .gp3_trackloss_rate .gp3_trackloss_exclude
#> 1          P1     1     1                 0.5                   TRUE
#> 2          P1     1     0                 0.5                   TRUE
#> 3          P2     1     1                 0.0                  FALSE
#> 4          P2     1     1                 0.0                  FALSE
```
