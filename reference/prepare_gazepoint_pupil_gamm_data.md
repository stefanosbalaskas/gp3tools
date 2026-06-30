# Prepare Gazepoint pupil GAMM data

Prepare binned pupil time-course data for GAMM modelling with
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html). The function
aggregates processed sample-level pupil data into subject-by-
condition-by-time-bin rows and creates an `AR.start` indicator for
autoregressive GAMM models.

## Usage

``` r
prepare_gazepoint_pupil_gamm_data(
  data,
  pupil_col = NULL,
  time_col = "time",
  subject_col = "subject",
  condition_col = "condition",
  x_col = NULL,
  y_col = NULL,
  group_cols = c("subject", "condition"),
  bin_width_ms = 50,
  time_window = NULL,
  min_valid_samples = 1,
  missing_condition_label = "all_data"
)
```

## Arguments

- data:

  A Gazepoint sample-level data frame, usually after pupil
  preprocessing, interpolation, baseline correction, and optional
  smoothing.

- pupil_col:

  Name of the pupil column to aggregate. If `NULL`, the function tries
  common processed pupil columns such as `pupil_smoothed`,
  `pupil_baseline_corrected`, `pupil_interpolated`, `pupil_clean`, and
  `pupil_for_preprocessing`.

- time_col:

  Name of the time column in milliseconds. If the requested column is
  not available, the function tries common alternatives.

- subject_col:

  Name of the subject column. If unavailable, the function tries common
  participant identifiers.

- condition_col:

  Name of the condition column. If unavailable or entirely missing, a
  single condition label is used.

- x_col:

  Optional gaze x-coordinate column. If `NULL`, common x-coordinate
  columns are auto-detected when available.

- y_col:

  Optional gaze y-coordinate column. If `NULL`, common y-coordinate
  columns are auto-detected when available.

- group_cols:

  Columns defining independent time series before binning. Defaults to
  `c("subject", "condition")`.

- bin_width_ms:

  Width of time bins in milliseconds.

- time_window:

  Optional numeric vector of length 2 giving the time window to retain
  before binning.

- min_valid_samples:

  Minimum number of valid pupil samples required for a bin to be
  retained.

- missing_condition_label:

  Label used when condition values are missing or when no usable
  condition column is available.

## Value

A tibble with binned pupil time-course data for GAMM modelling.
