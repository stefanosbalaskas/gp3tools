# Audit condition-level quality imbalance

Create a publication-level audit of whether gaze, pupil, retention, or
other quality metrics differ across experimental conditions.

## Usage

``` r
audit_gazepoint_condition_quality_imbalance(
  data,
  condition_col = "condition",
  quality_cols = NULL,
  subject_col = NULL,
  min_units_per_condition = 1L,
  max_mean_difference = 0.1,
  max_condition_ratio = 2,
  lower_is_better = c("missing_gaze_prop", "offscreen_prop", "excluded_prop",
    "failure_prop", "artifact_prop")
)
```

## Arguments

- data:

  A data frame containing condition-level, unit-level, or
  subject-condition-level quality metrics.

- condition_col:

  Condition column.

- quality_cols:

  Numeric quality-metric columns. If `NULL`, common quality columns are
  detected automatically.

- subject_col:

  Optional subject column.

- min_units_per_condition:

  Minimum number of rows/units expected per condition.

- max_mean_difference:

  Maximum acceptable absolute difference between condition means for
  each quality metric.

- max_condition_ratio:

  Maximum acceptable ratio between the largest and smallest non-zero
  condition mean for each quality metric.

- lower_is_better:

  Optional character vector naming metrics where lower values indicate
  better quality, such as missing-gaze or exclusion metrics.

## Value

A list with class `gp3_condition_quality_imbalance_audit` containing
overview, condition_summary, metric_summary, flagged_metrics, and
settings tables.
