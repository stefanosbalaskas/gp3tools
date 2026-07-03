# Summarize missingness in Gazepoint-style data

Computes missingness and observed-data rates for selected columns,
optionally within grouping variables such as participant, trial,
condition, or AOI. The helper is intended for transparent data-coverage
reporting and does not make exclusion decisions.

## Usage

``` r
summarize_gazepoint_missingness(
  data,
  cols = NULL,
  group_cols = NULL,
  include_group_cols = FALSE
)

summarise_gazepoint_missingness(
  data,
  cols = NULL,
  group_cols = NULL,
  include_group_cols = FALSE
)
```

## Arguments

- data:

  A data frame.

- cols:

  Optional character vector of columns to summarize. If `NULL`, all
  non-grouping columns are summarized.

- group_cols:

  Optional character vector of grouping columns.

- include_group_cols:

  If `TRUE`, grouping columns can also be summarized when `cols = NULL`.

## Value

A data frame with one row per variable and group.

## Examples

``` r
x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
summarize_gazepoint_missingness(
  x,
  cols = c("pupil_left", "pupil_right", "pupil"),
  group_cols = "condition"
)
#>    group_id    variable n_rows n_missing n_observed missing_rate observed_rate
#> 1   control  pupil_left     10         0         10            0             1
#> 2   control pupil_right     10         0         10            0             1
#> 3   control       pupil     10         0         10            0             1
#> 4 treatment  pupil_left     10         0         10            0             1
#> 5 treatment pupil_right     10         0         10            0             1
#> 6 treatment       pupil     10         0         10            0             1
```
