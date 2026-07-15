# Compare Gazepoint event-detection workflows

Run native velocity-threshold detection, the lightweight gp3tools HMM
classifier, and an optional eyetools branch on the same sample-level
gaze data. Detector failures are recorded without invalidating
successful branches.

## Usage

``` r
compare_gazepoint_event_detectors(
  data,
  id_col = "USER_ID",
  trial_col = NULL,
  group_cols = NULL,
  x_col = "FPOGX",
  y_col = "FPOGY",
  time_col = "TIME",
  methods = c("velocity", "hmm", "eyetools"),
  velocity_thresholds = c(5, 10, 20),
  min_duration = 50,
  hmm_states = 3L,
  eyetools_method = c("vti", "dispersion"),
  run_optional_eyetools = FALSE,
  min_overlap = 0.5,
  velocity_args = list(),
  hmm_args = list(),
  eyetools_args = list()
)
```

## Arguments

- data:

  A sample-level gaze data frame.

- id_col:

  Participant identifier column.

- trial_col:

  Optional trial identifier column.

- group_cols:

  Optional additional sequence columns.

- x_col, y_col:

  Gaze-coordinate columns.

- time_col:

  Timestamp column.

- methods:

  Detector branches to run: `"velocity"`, `"hmm"`, and `"eyetools"`.

- velocity_thresholds:

  One or more positive velocity thresholds.

- min_duration:

  Minimum fixation duration in milliseconds for the native velocity
  branch.

- hmm_states:

  Number of HMM states.

- eyetools_method:

  eyetools detector method.

- run_optional_eyetools:

  Should the optional eyetools branch be run?

- min_overlap:

  Minimum intersection-over-union used to classify overlapping events as
  matched.

- velocity_args:

  Named list overriding native velocity-detector defaults.

- hmm_args:

  Named list overriding HMM-classifier defaults.

- eyetools_args:

  Named list overriding eyetools-wrapper defaults.

## Value

An object of class `"gp3_event_detector_comparison"` containing
standardized event tables, detector-run status, detector summaries,
pairwise agreement, unmatched events, raw detector outputs, and
settings.

## Examples

``` r
gaze <- data.frame(
  USER_ID = rep("P01", 80),
  trial = rep("T01", 80),
  TIME = seq(0, by = 0.01, length.out = 80),
  FPOGX = c(rep(0.2, 30), seq(0.2, 0.8, length.out = 10), rep(0.8, 40)),
  FPOGY = 0.5
)

comparison <- compare_gazepoint_event_detectors(
  gaze,
  trial_col = "trial",
  methods = "velocity",
  velocity_thresholds = c(5, 10)
)

comparison$detector_summary
#>      detector   family threshold n_fixations mean_duration_ms
#> 1  velocity_5 velocity         5           2              355
#> 2 velocity_10 velocity        10           1              800
#>   median_duration_ms total_duration_ms
#> 1                355               710
#> 2                800               800
```
