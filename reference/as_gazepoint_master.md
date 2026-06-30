# Convert Gazepoint all-gaze data to a master sample table

Converts a Gazepoint all-gaze export into a standard sample-level table
with one row per gaze sample. The returned table keeps Gazepoint
identifiers but also adds analysis-friendly columns such as `subject`,
`media_id`, `time_ms`, `x`, `y`, `left_pupil`, `right_pupil`,
`mean_pupil`, `valid_sample`, `missing_gaze`, `missing_pupil`,
`trackloss`, `blink`, `aoi_current`, `message`, and `event_type`.

## Usage

``` r
as_gazepoint_master(
  data,
  screen_width_px = NULL,
  screen_height_px = NULL,
  source_col = "USER_FILE",
  media_col = "MEDIA_ID",
  media_name_col = "MEDIA_NAME",
  time_col = "TIME",
  coordinate_unit = c("auto", "normalised", "pixels"),
  event_latency_offset_ms = 0
)
```

## Arguments

- data:

  A Gazepoint all-gaze data frame, usually `results$all_gaze`.

- screen_width_px:

  Optional screen width in pixels. If supplied and gaze coordinates are
  detected as normalised 0-1 coordinates, x coordinates are converted to
  pixels.

- screen_height_px:

  Optional screen height in pixels. If supplied and gaze coordinates are
  detected as normalised 0-1 coordinates, y coordinates are converted to
  pixels.

- source_col:

  Column identifying the source/user file.

- media_col:

  Column identifying the Gazepoint media/stimulus.

- media_name_col:

  Column identifying the Gazepoint media/stimulus name.

- time_col:

  Gazepoint time column, usually `TIME`.

- coordinate_unit:

  One of `"auto"`, `"normalised"`, or `"pixels"`. `"auto"` detects
  normalised coordinates when coordinate values are mostly between 0 and
  1.

- event_latency_offset_ms:

  Optional timing correction in milliseconds. Positive values shift
  event/sample time forward.

## Value

A tibble with one row per sample and standardised sample-level
eye-tracking columns.

## Details

This function is intended as a bridge between raw Gazepoint exports and
more advanced eye-tracking workflows. It does not require an external
trial log. Later, experiment-level information such as condition, trial
ID, response, accuracy, or reaction time can be joined to the returned
table.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_gazepoint_workflow(
  export_dir = "C:/Users/YourName/Desktop/gp3_test_exports",
  output_dir = "C:/Users/YourName/Desktop/gp3_outputs"
)

master <- as_gazepoint_master(
  results$all_gaze,
  screen_width_px = 1920,
  screen_height_px = 1080
)

dplyr::glimpse(master)
} # }
```
