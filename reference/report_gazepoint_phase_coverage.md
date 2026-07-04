# Report task-phase coverage

Produces compact, cautious text describing task-phase data coverage. The
report is intended for methods/results documentation and does not define
exclusion rules.

## Usage

``` r
report_gazepoint_phase_coverage(
  data,
  phase_col = "task_phase",
  group_cols = NULL,
  time_col = NULL,
  value_cols = NULL,
  digits = 1
)
```

## Arguments

- data:

  A raw data frame or a summary produced by
  [`summarize_gazepoint_phase_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_phase_coverage.md).

- phase_col, group_cols, time_col, value_cols:

  Arguments used when `data` is raw data.

- digits:

  Number of decimal places used in report percentages.

## Value

A list with `summary`, `overall`, and `report_text`.

## Examples

``` r
x <- data.frame(time_ms = c(0, 250, 750, 1250), value = c(1, NA, 3, 4))
windows <- data.frame(phase = c("baseline", "stimulus"), start = c(0, 500), end = c(500, 1500))
segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)
report_gazepoint_phase_coverage(segmented, phase_col = "task_phase", time_col = "time_ms")
#> $summary
#>   group_id    phase n_rows n_finite_time min_time max_time time_span
#> 1      all baseline      2             2        0      250       250
#> 2      all stimulus      2             2      750     1250       500
#>   n_complete_value_rows complete_value_rate n_any_value_missing
#> 1                    NA                  NA                  NA
#> 2                    NA                  NA                  NA
#>   any_value_missing_rate
#> 1                     NA
#> 2                     NA
#> 
#> $overall
#>   n_phases n_groups total_rows least_represented_phase
#> 1        2        1          4                baseline
#>   least_represented_phase_rows weighted_complete_value_rate
#> 1                            2                          NaN
#> 
#> $phase_totals
#>      phase n_rows
#> 1 baseline      2
#> 2 stimulus      2
#> 
#> $report_text
#> [1] "Task-phase coverage was summarized across 2 phase(s) and 1 group(s), representing 4 row(s). The least represented phase was 'baseline' with 2 row(s). These values are descriptive data-coverage diagnostics and do not by themselves define exclusion decisions."
#> 
```
