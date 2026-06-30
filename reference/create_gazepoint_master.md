# Create a master long-format dataset from Gazepoint all-gaze data

Converts Gazepoint all-gaze exports imported with
[`read_gazepoint()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint.md)
or
[`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_folder.md)
into a master sample-level structure suitable for quality checks, AOI
summaries, pupil preprocessing, time-course analyses, and publication
reporting.

## Usage

``` r
create_gazepoint_master(
  gaze_data,
  screen_width_px = NULL,
  screen_height_px = NULL,
  screen_width_cm = NULL,
  screen_height_cm = NULL,
  viewing_distance_cm = NULL,
  time_unit = c("seconds", "milliseconds"),
  user_col = "USER_FILE",
  media_col = "MEDIA_ID",
  media_name_col = "MEDIA_NAME",
  tracker_model = "Gazepoint",
  tracker_sampling_rate = 60,
  event_latency_offset_ms = 0,
  baseline_window = NULL,
  analysis_window = NULL
)
```

## Arguments

- gaze_data:

  Gazepoint all-gaze data frame.

- screen_width_px:

  Optional screen width in pixels. If provided and gaze coordinates are
  normalised, x-coordinates are converted to pixels.

- screen_height_px:

  Optional screen height in pixels. If provided and gaze coordinates are
  normalised, y-coordinates are converted to pixels.

- screen_width_cm:

  Optional physical screen width in centimetres.

- screen_height_cm:

  Optional physical screen height in centimetres.

- viewing_distance_cm:

  Optional viewing distance in centimetres.

- time_unit:

  Unit of the Gazepoint `TIME` column. Usually `"seconds"`.

- user_col:

  Column identifying the source/user file.

- media_col:

  Column identifying the stimulus/media.

- media_name_col:

  Column identifying the media/stimulus name.

- tracker_model:

  Tracker label stored in the master data.

- tracker_sampling_rate:

  Expected tracker sampling rate.

- event_latency_offset_ms:

  Optional event-latency correction in milliseconds.

- baseline_window:

  Optional numeric vector of length 2 giving baseline start and end in
  milliseconds.

- analysis_window:

  Optional numeric vector of length 2 giving analysis start and end in
  milliseconds.

## Value

A tibble with one row per Gazepoint sample and publication-oriented
master columns.
