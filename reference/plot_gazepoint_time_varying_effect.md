# Plot a time-varying effect curve

Plot a time-varying effect, difference curve, or model-prediction
contrast from a tidy data frame. This helper is intentionally
lightweight: it does not refit a model, but visualises already computed
estimates and optional interval bounds from GAMM, GCA, cluster,
bootstrap, or prediction workflows.

## Usage

``` r
plot_gazepoint_time_varying_effect(
  data,
  time_col,
  estimate_col,
  lower_col = NULL,
  upper_col = NULL,
  group_col = NULL,
  zero_line = TRUE,
  title = NULL,
  x_label = NULL,
  y_label = NULL
)
```

## Arguments

- data:

  A data frame containing time-varying estimates.

- time_col:

  Name of the time column.

- estimate_col:

  Name of the estimate/effect column.

- lower_col:

  Optional lower interval column.

- upper_col:

  Optional upper interval column.

- group_col:

  Optional grouping/contrast column.

- zero_line:

  Should a horizontal zero reference line be shown?

- title:

  Optional plot title.

- x_label:

  Optional x-axis label.

- y_label:

  Optional y-axis label.

## Value

A ggplot object.
