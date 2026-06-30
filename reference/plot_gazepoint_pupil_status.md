# Plot Gazepoint pupil preprocessing status

Visualise observed, interpolated, missing, artifact, and other
pupil-sample statuses over time or as grouped percentages.

## Usage

``` r
plot_gazepoint_pupil_status(
  data,
  time_col = "time",
  pupil_col = NULL,
  status_col = "pupil_interpolation_status",
  interpolated_col = "pupil_was_interpolated",
  artifact_col = NULL,
  artifact_reason_col = NULL,
  group_cols = c("subject", "trial_global"),
  facet_cols = NULL,
  plot_type = c("timeline", "summary"),
  point_size = 0.7,
  alpha = 0.8,
  max_points = 50000
)
```

## Arguments

- data:

  A Gazepoint pupil data frame.

- time_col:

  Name of the time column.

- pupil_col:

  Optional pupil column used to detect remaining missing samples. If
  `NULL`, the function tries `pupil_smoothed`,
  `pupil_baseline_corrected`, `pupil_interpolated`, `pupil_clean`, and
  `pupil`.

- status_col:

  Optional interpolation-status column.

- interpolated_col:

  Optional logical interpolation flag column.

- artifact_col:

  Optional artifact flag column. If `NULL`, the function tries
  `pupil_artifact_flag`, `pupil_flag_invalid`, and `artifact_flag`.

- artifact_reason_col:

  Optional artifact-reason column. If `NULL`, the function tries
  `pupil_artifact_reason`, `pupil_flag_reason`, and `artifact_reason`.

- group_cols:

  Character vector used to define timeline rows or summary groups.

- facet_cols:

  Optional character vector of columns used for faceting.

- plot_type:

  Either `"timeline"` or `"summary"`.

- point_size:

  Point size for timeline plots.

- alpha:

  Point/column transparency.

- max_points:

  Maximum number of rows to plot in timeline mode. If the input has more
  rows, rows are evenly thinned for plotting only.

## Value

A `ggplot2` plot object.
