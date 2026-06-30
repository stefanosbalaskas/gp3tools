# Run optional eyetools fixation and saccade detection

Prepare Gazepoint sample-level gaze data for the optional `eyetools`
package and, when `eyetools` is installed, run fixation and/or saccade
detection using
[`eyetools::fixation_dispersion()`](https://tombeesley.github.io/eyetools/reference/fixation_dispersion.html),
[`eyetools::fixation_VTI()`](https://tombeesley.github.io/eyetools/reference/fixation_VTI.html),
and/or
[`eyetools::saccade_VTI()`](https://tombeesley.github.io/eyetools/reference/saccade_VTI.html).

## Usage

``` r
run_gazepoint_eyetools_fixation_detection(
  data,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  x_col = NULL,
  y_col = NULL,
  condition_col = NULL,
  stimulus_col = NULL,
  method = c("dispersion", "vti", "saccade", "all"),
  sample_rate = NULL,
  threshold = 100,
  min_dur = 150,
  min_dur_sac = 20,
  disp_tol = 100,
  NA_tol = 0.25,
  smooth = FALSE,
  drop_missing = TRUE,
  progress = FALSE,
  name = "gazepoint_eyetools_fixation_detection"
)
```

## Arguments

- data:

  A data frame containing sample-level gaze data.

- participant_col:

  Participant/subject column. If `NULL`, common names are detected.

- trial_col:

  Trial column. If `NULL`, common names are detected.

- time_col:

  Time column. If `NULL`, common names are detected.

- x_col:

  Horizontal gaze coordinate column. If `NULL`, common names are
  detected.

- y_col:

  Vertical gaze coordinate column. If `NULL`, common names are detected.

- condition_col:

  Optional condition/group column.

- stimulus_col:

  Optional stimulus/media column.

- method:

  Detector branch. Options are `"dispersion"`, `"vti"`, `"saccade"`, and
  `"all"`.

- sample_rate:

  Optional sample rate passed to velocity-threshold functions.

- threshold:

  Velocity threshold passed to velocity-threshold functions.

- min_dur:

  Minimum fixation duration in milliseconds.

- min_dur_sac:

  Minimum saccade duration in milliseconds.

- disp_tol:

  Dispersion tolerance in pixels.

- NA_tol:

  Missing-data tolerance passed to `fixation_dispersion()`.

- smooth:

  Logical; passed to `fixation_VTI()`.

- drop_missing:

  Logical. If `TRUE`, rows with non-finite time, x, or y are removed
  before detector execution.

- progress:

  Logical. Passed to eyetools detector functions.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_eyetools_fixation_detection`.

## Details

This helper is an optional external-detector branch. It does not replace
the main `gp3tools` summaries or AOI/transition workflows.
