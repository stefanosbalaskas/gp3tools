# Plot AOI time-course GAMM results

Plot observed AOI target-looking proportions and fitted GAMM
trajectories from a model returned by
[`fit_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_gamm.md).

## Usage

``` r
plot_gazepoint_aoi_gamm(
  fit,
  n_time_points = 100,
  include_observed = TRUE,
  include_fitted = TRUE,
  show_ci = TRUE,
  ci_level = 0.95,
  exclude_random_effects = TRUE,
  observed_summary = c("pooled"),
  point_size = 1.8,
  point_alpha = 0.65,
  line_width = 0.8,
  ribbon_alpha = 0.15,
  title = NULL,
  subtitle = NULL,
  x_label = "Time (ms)",
  y_label = "Target AOI looking probability",
  y_limits = c(0, 1)
)
```

## Arguments

- fit:

  A result object returned by
  [`fit_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_gamm.md).

- n_time_points:

  Number of time points used for the fitted prediction grid. If `NULL`,
  the observed time bins are used.

- include_observed:

  Logical. If `TRUE`, plot observed binned proportions.

- include_fitted:

  Logical. If `TRUE`, plot fitted GAMM trajectories.

- show_ci:

  Logical. If `TRUE`, plot fitted confidence intervals.

- ci_level:

  Confidence level for fitted intervals.

- exclude_random_effects:

  Logical. If `TRUE`, exclude subject random-effect smooths from fitted
  predictions.

- observed_summary:

  Character. Currently `"pooled"` pools successes and denominators by
  condition and time.

- point_size:

  Size of observed points.

- point_alpha:

  Transparency for observed points.

- line_width:

  Width of fitted trajectory lines.

- ribbon_alpha:

  Transparency for fitted confidence ribbons.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- x_label:

  X-axis label.

- y_label:

  Y-axis label.

- y_limits:

  Optional numeric vector of length 2 for y-axis limits.

## Value

A `ggplot` object with prediction and observed data stored as
attributes.

## Details

The plot supports single-condition fallback models and multi-condition
AOI time-course GAMMs. By default, fitted trajectories are
population-level predictions with subject random-effect smooths
excluded.
