# Audit AOI window denominators before binomial modelling

Audit AOI-window sample denominators before confirmatory binomial or
logistic mixed-effects modelling. The function is designed for output
from
[`summarise_gazepoint_aoi_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_windows.md).

## Usage

``` r
audit_gazepoint_aoi_window_denominators(
  data,
  window_col = "window_label",
  window_start_col = "window_start_ms",
  window_end_col = "window_end_ms",
  denominator_col = "n_valid_denominator_samples",
  total_col = "n_window_samples",
  target_col = "n_target_samples",
  condition_col = "condition",
  group_cols = NULL,
  min_denominator_samples = 5,
  min_valid_denominator_prop = 0.7,
  max_denominator_cv = 0.25,
  max_condition_ratio = 2
)
```

## Arguments

- data:

  AOI-window summary data.

- window_col:

  Name of the AOI-window label column.

- window_start_col:

  Optional window-start column.

- window_end_col:

  Optional window-end column.

- denominator_col:

  Name of the denominator column to audit.

- total_col:

  Name of the total window-sample column.

- target_col:

  Name of the target-success count column.

- condition_col:

  Optional condition column.

- group_cols:

  Optional grouping columns for row-level audit context.

- min_denominator_samples:

  Minimum acceptable denominator count.

- min_valid_denominator_prop:

  Minimum acceptable valid-denominator proportion relative to total
  window samples.

- max_denominator_cv:

  Maximum acceptable denominator coefficient of variation within each
  window.

- max_condition_ratio:

  Maximum acceptable ratio between the largest and smallest mean
  denominator across conditions within a window.

## Value

A named list containing overview, row audit, window summary,
condition-window summary, denominator-imbalance summary, flagged rows,
and settings.
