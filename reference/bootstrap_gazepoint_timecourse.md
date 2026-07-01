# Bootstrap time-course summaries

Compute simple nonparametric bootstrap confidence intervals for a
time-varying gaze or pupil measure. The function is intentionally
lightweight and returns a tidy data frame that can be plotted or used as
a descriptive robustness check alongside model-based analyses.

## Usage

``` r
bootstrap_gazepoint_timecourse(
  data,
  time_col,
  value_col,
  group_col = NULL,
  subject_col = NULL,
  n_boot = 1000,
  ci = 0.95,
  statistic = c("mean", "median"),
  difference_groups = NULL,
  seed = NULL
)
```

## Arguments

- data:

  A data frame.

- time_col:

  Name of the time column.

- value_col:

  Name of the numeric outcome column.

- group_col:

  Optional grouping column, for example condition.

- subject_col:

  Optional subject/participant column. If supplied, bootstrap resampling
  is performed over subjects within each time and group cell; otherwise
  rows are resampled.

- n_boot:

  Number of bootstrap draws.

- ci:

  Confidence level for the interval.

- statistic:

  Summary statistic, either `mean` or `median`.

- difference_groups:

  Optional character vector of length two. If supplied with `group_col`,
  the function also returns a bootstrap difference curve for group 1
  minus group 2.

- seed:

  Optional random seed.

## Value

A data frame with time, group/contrast, estimate, lower and upper
bootstrap interval limits, sample size, and status columns.
