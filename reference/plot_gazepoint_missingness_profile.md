# Plot a Gazepoint missingness profile

Creates a descriptive plot of missingness rates from raw data or from
the output of
[`summarize_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md).
The plot is intended for quality-control review and reporting.

## Usage

``` r
plot_gazepoint_missingness_profile(
  data,
  cols = NULL,
  group_cols = NULL,
  plot_type = c("bar", "tile"),
  title = NULL,
  y_label = "Missingness rate"
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

- plot_type:

  Either `"bar"` or `"tile"`.

- title:

  Optional plot title.

- y_label:

  Optional y-axis label for bar plots.

## Value

A ggplot object.

## Examples

``` r
x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
plot_gazepoint_missingness_profile(
  x,
  cols = c("pupil_left", "pupil_right", "pupil"),
  group_cols = "condition"
)
```
