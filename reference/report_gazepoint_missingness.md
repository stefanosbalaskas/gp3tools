# Report Gazepoint missingness

Produces a compact, cautious text summary of missingness rates. The
report is intended for transparent methods/results documentation and
does not recommend exclusions.

## Usage

``` r
report_gazepoint_missingness(
  data,
  cols = NULL,
  group_cols = NULL,
  digits = 1,
  max_variables = 5
)
```

## Arguments

- data:

  A data frame or a missingness summary produced by
  [`summarize_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md).

- cols:

  Optional columns to summarize when `data` is raw data.

- group_cols:

  Optional grouping columns when `data` is raw data.

- digits:

  Number of decimal places for percentages.

- max_variables:

  Maximum number of highest-missingness variables to name in the report
  text.

## Value

A list with `summary`, `overall`, and `report_text`.

## Examples

``` r
x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
report_gazepoint_missingness(
  x,
  cols = c("pupil_left", "pupil_right", "pupil")
)
#> $summary
#>   group_id    variable n_rows n_missing n_observed missing_rate observed_rate
#> 1      all  pupil_left     20         0         20            0             1
#> 2      all pupil_right     20         0         20            0             1
#> 3      all       pupil     20         0         20            0             1
#> 
#> $overall
#>   n_variables n_groups total_cells total_missing overall_missing_rate
#> 1           3        1          60             0                    0
#> 
#> $variable_summary
#>      variable n_rows n_missing missing_rate
#> 1       pupil     20         0            0
#> 2  pupil_left     20         0            0
#> 3 pupil_right     20         0            0
#> 
#> $report_text
#> [1] "Missingness was summarized across 3 variable(s). The overall cell-level missingness rate was 0%. The highest missingness variable(s) were: pupil (0%), pupil_left (0%), pupil_right (0%). These values are descriptive data-coverage diagnostics and do not by themselves define exclusion decisions."
#> 
```
