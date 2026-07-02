# Prepare time-course data for Gazepoint cluster-permutation testing

`prepare_gazepoint_timecourse_test_data()` converts a long-format
subject-by-condition-by-time data frame into the internal column
contract used by
[`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md).
It is intended for conservative two-condition, within-subject,
one-dimensional time-course workflows.

## Usage

``` r
prepare_gazepoint_timecourse_test_data(
  data,
  subject_col,
  condition_col,
  time_col,
  outcome_col,
  condition_order = NULL,
  aggregate_fun = mean,
  complete_only = TRUE
)
```

## Arguments

- data:

  A long-format data frame.

- subject_col:

  Column identifying participants or paired units.

- condition_col:

  Column identifying the two within-subject conditions.

- time_col:

  Numeric time or time-bin column.

- outcome_col:

  Numeric outcome column.

- condition_order:

  Optional character vector of length two giving the condition order
  used for contrasts.

- aggregate_fun:

  Function used when duplicate subject-condition-time rows are present.
  Defaults to `mean`.

- complete_only:

  If `TRUE`, keep only subject-by-time cells with both conditions
  present.

## Value

A data frame with internal cluster columns: `.gp3_cluster_subject`,
`.gp3_cluster_condition`, `.gp3_cluster_time_bin`, and
`.gp3_cluster_outcome`.

## Examples

``` r
d <- data.frame(
  subject = rep(1:4, each = 6),
  condition = rep(rep(c("A", "B"), each = 3), 4),
  time = rep(1:3, 8),
  value = rnorm(24)
)

prepare_gazepoint_timecourse_test_data(
  d,
  subject_col = "subject",
  condition_col = "condition",
  time_col = "time",
  outcome_col = "value"
)
#>    .gp3_cluster_subject .gp3_cluster_condition .gp3_cluster_time_bin
#> 1                     1                      A                     1
#> 2                     1                      B                     1
#> 3                     1                      A                     2
#> 4                     1                      B                     2
#> 5                     1                      A                     3
#> 6                     1                      B                     3
#> 7                     2                      A                     1
#> 8                     2                      B                     1
#> 9                     2                      A                     2
#> 10                    2                      B                     2
#> 11                    2                      A                     3
#> 12                    2                      B                     3
#> 13                    3                      A                     1
#> 14                    3                      B                     1
#> 15                    3                      A                     2
#> 16                    3                      B                     2
#> 17                    3                      A                     3
#> 18                    3                      B                     3
#> 19                    4                      A                     1
#> 20                    4                      B                     1
#> 21                    4                      A                     2
#> 22                    4                      B                     2
#> 23                    4                      A                     3
#> 24                    4                      B                     3
#>    .gp3_cluster_outcome .gp3_cluster_status
#> 1          -1.400043517                  ok
#> 2          -0.005571287                  ok
#> 3           0.255317055                  ok
#> 4           0.621552721                  ok
#> 5          -2.437263611                  ok
#> 6           1.148411606                  ok
#> 7          -1.821817661                  ok
#> 8          -0.282705449                  ok
#> 9          -0.247325302                  ok
#> 10         -0.553699384                  ok
#> 11         -0.244199607                  ok
#> 12          0.628982042                  ok
#> 13          2.065024895                  ok
#> 14         -1.863011492                  ok
#> 15         -1.630989402                  ok
#> 16         -0.522012515                  ok
#> 17          0.512426950                  ok
#> 18         -0.052601910                  ok
#> 19          0.542996343                  ok
#> 20          0.362951256                  ok
#> 21         -0.914074827                  ok
#> 22         -1.304543545                  ok
#> 23          0.468154420                  ok
#> 24          0.737776321                  ok
```
