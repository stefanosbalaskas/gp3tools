# Run a tested gp3tools-to-gpbiometrics integration workflow

Aligns gp3tools gaze output with an already imported biometric data
frame, computes AOI/event-contingent signal summaries, and returns a
combined audit and cautious report text. A user-supplied adapter can
replace the native nearest-timestamp join without making gpbiometrics a
mandatory dependency.

## Usage

``` r
run_gazepoint_gpbiometrics_workflow(
  gaze_data,
  biometrics_data,
  gaze_args = list(),
  biometric_participant_col = NULL,
  biometric_trial_col = NULL,
  biometric_time_col = NULL,
  biometric_time_unit = c("auto", "seconds", "milliseconds"),
  signal_cols = NULL,
  event_col = NULL,
  tolerance_s = NULL,
  adapter = NULL,
  include_unmatched = TRUE
)
```

## Arguments

- gaze_data:

  Raw gp3tools gaze/master data or a prepared bridge.

- biometrics_data:

  Biometric samples, including participant, trial, time, and one or more
  numeric signal columns.

- gaze_args:

  Named list forwarded to
  [`prepare_gazepoint_gpbiometrics_bridge()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gpbiometrics_bridge.md).

- biometric_participant_col, biometric_trial_col, biometric_time_col:

  Explicit biometric source columns.

- biometric_time_unit:

  Unit for the biometric time column.

- signal_cols:

  Numeric biometric signals to summarise.

- event_col:

  Optional biometric event column.

- tolerance_s:

  Maximum absolute time difference for a match. When `NULL`, it is
  estimated conservatively from biometric sampling intervals.

- adapter:

  Optional function with arguments `gaze`, `biometrics`, and
  `tolerance_s`; it must return a synchronized data frame.

- include_unmatched:

  Retain unmatched gaze rows.

## Value

A `"gazepoint_cross_package_workflow"` object.
