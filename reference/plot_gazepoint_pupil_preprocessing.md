# Plot Gazepoint pupil preprocessing for one trial

Create a visual audit plot for one selected subject, media item, trial,
or trial-global identifier. The plot can show raw pupil, cleaned pupil,
interpolated pupil, baseline-corrected pupil, smoothed pupil, and
artifact flags.

## Usage

``` r
plot_gazepoint_pupil_preprocessing(
  data,
  subject = NULL,
  media_id = NULL,
  trial = NULL,
  trial_global = NULL,
  condition = NULL,
  subject_col = "subject",
  media_col = "MEDIA_ID",
  trial_col = "trial",
  trial_global_col = "trial_global",
  condition_col = "condition",
  time_col = "time",
  raw_pupil_col = "pupil",
  clean_pupil_col = "pupil_clean",
  interpolated_pupil_col = "pupil_interpolated",
  baseline_pupil_col = "pupil_baseline_corrected",
  smoothed_pupil_col = "pupil_smoothed",
  artifact_col = NULL,
  artifact_reason_col = NULL,
  status_col = "pupil_interpolation_status",
  plot_style = c("faceted", "overlaid"),
  bin_width_ms = 50,
  max_event_marks = 150,
  point_size = 0.8,
  line_width = 0.35,
  alpha = 0.95
)
```

## Arguments

- data:

  A Gazepoint pupil data frame.

- subject:

  Optional subject value to filter.

- media_id:

  Optional media identifier value to filter.

- trial:

  Optional trial value to filter.

- trial_global:

  Optional global trial identifier value to filter.

- condition:

  Optional condition value to filter.

- subject_col:

  Name of the subject column.

- media_col:

  Name of the media identifier column.

- trial_col:

  Name of the trial column.

- trial_global_col:

  Name of the global trial identifier column.

- condition_col:

  Name of the condition column.

- time_col:

  Name of the time column.

- raw_pupil_col:

  Optional raw pupil column.

- clean_pupil_col:

  Optional cleaned pupil column.

- interpolated_pupil_col:

  Optional interpolated pupil column.

- baseline_pupil_col:

  Optional baseline-corrected pupil column.

- smoothed_pupil_col:

  Optional smoothed pupil column.

- artifact_col:

  Optional artifact flag column. If `NULL`, the function tries
  `pupil_artifact_flag`, `pupil_flag_invalid`, and `artifact_flag`.

- artifact_reason_col:

  Optional artifact-reason column. If `NULL`, the function tries
  `pupil_artifact_reason`, `pupil_flag_reason`, and `artifact_reason`.

- status_col:

  Optional interpolation-status column.

- plot_style:

  Either `"faceted"` or `"overlaid"`.

- bin_width_ms:

  Width of time bins in milliseconds. This is used only for visual
  smoothing of dense sample-level traces.

- max_event_marks:

  Maximum number of artifact/interpolation rug marks to draw. Event
  marks are evenly thinned if there are more events.

- point_size:

  Size control for artifact/interpolation rug marks.

- line_width:

  Line width for pupil series.

- alpha:

  Line and marker transparency.

## Value

A `ggplot2` plot object.
