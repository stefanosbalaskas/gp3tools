# Run an optional gazeR pupil-preprocessing cross-check

Prepare Gazepoint pupil data for the optional `gazer` package and, when
`gazer` is installed, run a conservative pupil-preprocessing cross-check
using gazeR-style blink extension, smoothing/interpolation, optional
baseline correction, and optional downsampling.

## Usage

``` r
run_gazepoint_gazer_crosscheck(
  data,
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  pupil_col = NULL,
  condition_col = NULL,
  message_col = NULL,
  blink_col = NULL,
  hz = 60,
  fillback = 100,
  fillforward = 100,
  smooth_n = 5,
  step_first = c("smooth", "interpolate"),
  interpolation_type = "linear",
  maxgap = Inf,
  baseline_window = NULL,
  baseline_event = NULL,
  baseline_dur = 100,
  baseline_method = "sub",
  bin_length = NULL,
  name = "gazepoint_gazer_crosscheck"
)
```

## Arguments

- data:

  A data frame containing Gazepoint or gp3tools pupil time-series data.

- participant_col:

  Participant/subject column. If `NULL`, common names are detected.

- trial_col:

  Trial column. If `NULL`, common names are detected.

- time_col:

  Time column. If `NULL`, common names are detected.

- pupil_col:

  Pupil column. If `NULL`, common processed/raw pupil columns are
  detected.

- condition_col:

  Optional condition column. If `NULL`, common names are detected;
  otherwise `"all_data"` is used.

- message_col:

  Optional message/event column.

- blink_col:

  Optional blink/trackloss column.

- hz:

  Sampling rate passed to gazeR functions.

- fillback:

  Blink-extension window before missing/blink samples, in ms.

- fillforward:

  Blink-extension window after missing/blink samples, in ms.

- smooth_n:

  Smoothing window parameter passed to
  [`gazer::smooth_interpolate_pupil()`](https://rdrr.io/pkg/gazer/man/smooth_interpolate_pupil.html).

- step_first:

  Processing order passed to
  [`gazer::smooth_interpolate_pupil()`](https://rdrr.io/pkg/gazer/man/smooth_interpolate_pupil.html).

- interpolation_type:

  Interpolation type passed to
  [`gazer::smooth_interpolate_pupil()`](https://rdrr.io/pkg/gazer/man/smooth_interpolate_pupil.html).

- maxgap:

  Maximum gap passed to
  [`gazer::smooth_interpolate_pupil()`](https://rdrr.io/pkg/gazer/man/smooth_interpolate_pupil.html).

- baseline_window:

  Optional numeric vector of length 2 passed to
  [`gazer::baseline_correction_pupil()`](https://rdrr.io/pkg/gazer/man/baseline_correction_pupil.html).

- baseline_event:

  Optional event label passed to
  [`gazer::baseline_correction_pupil_msg()`](https://rdrr.io/pkg/gazer/man/baseline_correction_pupil_msg.html).

- baseline_dur:

  Baseline duration used with `baseline_event`.

- baseline_method:

  Baseline method used with `baseline_event`.

- bin_length:

  Optional bin length passed to
  [`gazer::downsample_gaze()`](https://rdrr.io/pkg/gazer/man/downsample_gaze.html).

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_gazer_crosscheck`.

## Details

This helper is a cross-check branch. It is not intended to replace the
main `gp3tools` pupil preprocessing pipeline.
