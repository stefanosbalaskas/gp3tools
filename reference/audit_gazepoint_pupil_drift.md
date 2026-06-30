# Audit Gazepoint pupil drift

Summarise tonic pupil/time-on-task drift in processed Gazepoint pupil
data.

## Usage

``` r
audit_gazepoint_pupil_drift(
  data,
  group_cols = "subject",
  pupil_col = NULL,
  time_col = "time",
  order_col = "trial",
  condition_col = "condition",
  exclude_col = "excluded_trial",
  include_excluded = FALSE,
  min_valid_samples = 3,
  max_abs_slope_per_min = 1,
  max_condition_time_mean_diff_ms = 1000,
  max_condition_order_mean_diff = 1
)
```

## Arguments

- data:

  A data frame from a Gazepoint pupil preprocessing pipeline.

- group_cols:

  Character vector of grouping columns for the main drift audit. The
  default is `"subject"`.

- pupil_col:

  Name of the pupil column to analyse. If `NULL`, the function
  automatically tries `pupil_smoothed`, `pupil_baseline_corrected`,
  `pupil_interpolated`, `pupil_clean`, and `pupil`.

- time_col:

  Name of the within-trial or sample-time column.

- order_col:

  Optional trial/order column used to assess time-on-task imbalance. If
  `NULL`, order-based summaries are skipped.

- condition_col:

  Optional condition column used to summarise condition drift and
  time-on-task imbalance. If `NULL`, condition summaries are skipped.

- exclude_col:

  Optional logical exclusion column. If present and
  `include_excluded = FALSE`, excluded rows are removed before analysis.

- include_excluded:

  Logical. If `FALSE`, rows marked by `exclude_col` are excluded when
  that column exists.

- min_valid_samples:

  Minimum valid pupil samples required to estimate a drift slope.

- max_abs_slope_per_min:

  Maximum acceptable absolute pupil slope per minute before a drift
  warning is raised.

- max_condition_time_mean_diff_ms:

  Maximum acceptable difference in mean sample time across conditions.

- max_condition_order_mean_diff:

  Maximum acceptable difference in mean trial/order value across
  conditions.

## Value

A named list containing `by_group`, `by_subject`, `by_condition`,
`condition_balance`, and `summary` tibbles.

## Details

The function estimates simple linear pupil trends over time within
selected grouping variables, usually subjects. It also reports
subject-level drift, condition-level drift, and possible condition
imbalance in time-on-task.
