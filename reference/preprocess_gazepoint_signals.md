# Run an integrated Gazepoint signal-preprocessing workflow

Orchestrate transparent blink, pupil, coordinate, downsampling, and
velocity-based fixation-processing steps while preserving the original
input columns. Every requested operation is recorded in a decision log.

## Usage

``` r
preprocess_gazepoint_signals(
  data,
  id_col = "USER_ID",
  group_cols = NULL,
  time_col = "TIME",
  x_col = "FPOGX",
  y_col = "FPOGY",
  left_pupil_col = NULL,
  right_pupil_col = NULL,
  pupil_col = NULL,
  pupil_mode = c("mean", "regression", "none"),
  detect_blinks = TRUE,
  interpolate_blinks = TRUE,
  smooth_pupil = TRUE,
  smooth_coordinates = TRUE,
  downsample_factor = 1L,
  detect_fixations = TRUE,
  blink_args = list(),
  interpolation_args = list(),
  pupil_args = list(),
  pupil_smoothing_args = list(),
  coordinate_smoothing_args = list(),
  downsampling_args = list(),
  fixation_args = list()
)
```

## Arguments

- data:

  A sample-level Gazepoint or gp3tools data frame.

- id_col:

  Participant identifier column.

- group_cols:

  Optional additional columns defining independent time series, such as
  trial or stimulus.

- time_col:

  Timestamp column.

- x_col, y_col:

  Gaze-coordinate columns.

- left_pupil_col, right_pupil_col:

  Optional binocular pupil columns. When `NULL`, common Gazepoint and
  gp3tools names are detected.

- pupil_col:

  Optional existing monocular or fused pupil column used when
  `pupil_mode = "none"`.

- pupil_mode:

  Binocular fusion mode: `"mean"`, `"regression"`, or `"none"`.

- detect_blinks:

  Should
  [`detect_gazepoint_blinks()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_blinks.md)
  be run?

- interpolate_blinks:

  Should detected blink intervals be interpolated?

- smooth_pupil:

  Should
  [`smooth_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_pupil.md)
  be run?

- smooth_coordinates:

  Should
  [`smooth_gazepoint_coordinate()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_coordinate.md)
  be run?

- downsample_factor:

  Positive integer downsampling factor. Use `1` to retain the original
  sample count.

- detect_fixations:

  Should
  [`detect_gazepoint_fixations_velocity()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_fixations_velocity.md)
  be run on the final full-resolution coordinates?

- blink_args:

  Named list overriding blink-detection defaults.

- interpolation_args:

  Named list overriding blink-interpolation defaults.

- pupil_args:

  Named list overriding binocular fusion defaults.

- pupil_smoothing_args:

  Named list overriding pupil-smoothing defaults.

- coordinate_smoothing_args:

  Named list overriding coordinate-smoothing defaults.

- downsampling_args:

  Named list overriding downsampling defaults.

- fixation_args:

  Named list overriding fixation-detection defaults.

## Value

An object of class `"gp3_signal_preprocessing_result"` containing
processed `data`, detected `blinks`, detected `fixations`, diagnostic
tables, a `decision_log`, and resolved `settings`.

## Examples

``` r
pupil <- data.frame(
  USER_ID = rep("P01", 30),
  trial = rep("T01", 30),
  TIME = seq(0, 0.29, by = 0.01),
  FPOGX = c(rep(0.25, 15), rep(0.75, 15)),
  FPOGY = 0.50,
  LPupil = c(rep(3.2, 10), NA, NA, rep(3.2, 18)),
  RPupil = c(rep(3.1, 10), NA, NA, rep(3.1, 18))
)

result <- preprocess_gazepoint_signals(
  pupil,
  group_cols = "trial",
  downsample_factor = 2
)

result$decision_log
#>   step                   operation requested  status input_rows output_rows
#> 1    1        binocular_pupil_mean      TRUE applied         30          30
#> 2    2             blink_detection      TRUE applied         30          30
#> 3    3         blink_interpolation      TRUE applied         30          30
#> 4    4             pupil_smoothing      TRUE applied         30          30
#> 5    5        coordinate_smoothing      TRUE applied         30          30
#> 6    6 velocity_fixation_detection      TRUE applied         30          30
#> 7    7                downsampling      TRUE applied         30          15
#>                                             details
#> 1                                   LPupil + RPupil
#> 2                               0 blink interval(s)
#> 3 Output pupil column: gp3_pupil_fused_blink_interp
#> 4               Output pupil column: pupil_smoothed
#> 5                        FPOGX_smooth, FPOGY_smooth
#> 6                               2 fixation event(s)
#> 7                             Aggregation factor: 2
```
