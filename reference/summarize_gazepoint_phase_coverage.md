# Summarize task-phase coverage

Summarizes row counts, optional time coverage, and optional value
completeness by task phase and optional grouping variables. The helper
is intended for documenting task-stage data availability, not for
inferential modelling.

## Usage

``` r
summarize_gazepoint_phase_coverage(
  data,
  phase_col = "task_phase",
  group_cols = NULL,
  time_col = NULL,
  value_cols = NULL
)

summarise_gazepoint_phase_coverage(
  data,
  phase_col = "task_phase",
  group_cols = NULL,
  time_col = NULL,
  value_cols = NULL
)
```

## Arguments

- data:

  A data frame containing a phase column.

- phase_col:

  Character name of the phase column.

- group_cols:

  Optional grouping columns, such as subject, trial, or condition.

- time_col:

  Optional time column used to summarize phase timing.

- value_cols:

  Optional value columns used to summarize complete-value coverage
  within phases.

## Value

A data frame with one row per group-phase combination.

## Examples

``` r
x <- data.frame(time_ms = c(0, 250, 750, 1250), value = c(1, NA, 3, 4))
windows <- data.frame(phase = c("baseline", "stimulus"), start = c(0, 500), end = c(500, 1500))
segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)
summarize_gazepoint_phase_coverage(segmented, phase_col = "task_phase", time_col = "time_ms")
#>   group_id    phase n_rows n_finite_time min_time max_time time_span
#> 1      all baseline      2             2        0      250       250
#> 2      all stimulus      2             2      750     1250       500
#>   n_complete_value_rows complete_value_rate n_any_value_missing
#> 1                    NA                  NA                  NA
#> 2                    NA                  NA                  NA
#>   any_value_missing_rate
#> 1                     NA
#> 2                     NA
```
