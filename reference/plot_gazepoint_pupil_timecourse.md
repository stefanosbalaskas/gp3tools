# Plot Gazepoint pupil time course

Plot a binned pupil time course with a mean line and confidence band.
The function can plot one overall time course or condition-wise time
courses, with optional faceting by variables such as condition, media,
AOI, subject, or trial.

## Usage

``` r
plot_gazepoint_pupil_timecourse(
  data,
  pupil_col = NULL,
  time_col = "time",
  condition_col = "condition",
  facet_cols = NULL,
  bin_width_ms = 100,
  ci_level = 0.95,
  min_samples = 1,
  band_alpha = 0.2,
  line_width = 0.8
)
```

## Arguments

- data:

  A Gazepoint pupil data frame.

- pupil_col:

  Name of the pupil column to plot. If `NULL`, the function tries
  `pupil_smoothed`, `pupil_baseline_corrected`,
  `pupil_baseline_percent_change`, `pupil_interpolated`, `pupil_clean`,
  and `pupil`.

- time_col:

  Name of the time column.

- condition_col:

  Optional condition column used for separate lines. If the column is
  missing or contains only missing values, the function plots a single
  `"all_data"` time course.

- facet_cols:

  Optional character vector of columns used for faceting.

- bin_width_ms:

  Width of time bins in milliseconds.

- ci_level:

  Confidence level for the band.

- min_samples:

  Minimum number of valid pupil samples required per time bin.

- band_alpha:

  Transparency of the confidence band.

- line_width:

  Width of the mean time-course line.

## Value

A `ggplot2` plot object.
