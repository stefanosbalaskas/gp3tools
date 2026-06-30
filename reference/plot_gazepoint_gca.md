# Plot observed and fitted Growth Curve Analysis trajectories

Plot observed and fitted pupil trajectories from a `gp3_gca_model`
object. The function aggregates observed and fitted values by condition
and time, and returns a `ggplot2` object.

## Usage

``` r
plot_gazepoint_gca(
  model,
  data = NULL,
  time_col = "gca_time",
  observed_col = "gca_pupil",
  fitted_col = "gca_fitted",
  condition_col = "condition",
  subject_col = "subject",
  summarise = TRUE,
  show_observed = TRUE,
  show_fitted = TRUE,
  show_subjects = FALSE,
  interval = TRUE,
  title = NULL,
  point_size = 1.6,
  line_width = 0.8,
  alpha = 0.75
)
```

## Arguments

- model:

  A fitted object returned by
  [`fit_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_gca.md),
  or a data frame containing observed and fitted values.

- data:

  Optional data frame. If `NULL` and `model` is a `gp3_gca_model`, the
  model data are used.

- time_col:

  Name of the time column.

- observed_col:

  Name of the observed outcome column.

- fitted_col:

  Name of the fitted-value column. If unavailable and `model` is a
  `gp3_gca_model`, fitted values are computed from the model.

- condition_col:

  Name of the condition column.

- subject_col:

  Optional subject column, used only when `show_subjects = TRUE`.

- summarise:

  Logical. If `TRUE`, plot mean trajectories by condition and time. If
  `FALSE`, plot row-level values.

- show_observed:

  Logical. If `TRUE`, include observed trajectory.

- show_fitted:

  Logical. If `TRUE`, include fitted trajectory.

- show_subjects:

  Logical. If `TRUE`, add faint subject-level observed trajectories when
  a subject column is available.

- interval:

  Logical. If `TRUE`, add a standard-error ribbon around the observed
  mean trajectory when `summarise = TRUE`.

- title:

  Optional plot title.

- point_size:

  Point size for observed means.

- line_width:

  Line width for trajectories.

- alpha:

  Alpha value for observed points/lines.

## Value

A `ggplot2` object.
