# Flag low-quality Gazepoint recordings

Combines tracking-quality and sampling-rate summaries and flags rows
with low gaze validity, low pupil validity, abnormal sampling rate, or
short duration.

## Usage

``` r
flag_tracking_quality(
  quality,
  sampling,
  by = c("USER_FILE", "MEDIA_ID"),
  min_gaze_valid_pct = 70,
  min_pupil_valid_pct = 70,
  expected_hz = 60,
  hz_tolerance = 5,
  min_duration_sec = NULL
)
```

## Arguments

- quality:

  Tracking-quality table from
  [`summarise_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_tracking_quality.md).

- sampling:

  Sampling-rate table from
  [`check_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_sampling_rate.md).

- by:

  Columns used to join `quality` and `sampling`.

- min_gaze_valid_pct:

  Minimum acceptable FPOGV validity percentage.

- min_pupil_valid_pct:

  Minimum acceptable pupil validity percentage.

- expected_hz:

  Expected sampling rate.

- hz_tolerance:

  Allowed deviation from the expected sampling rate.

- min_duration_sec:

  Minimum acceptable recording duration in seconds.

## Value

A tibble with quality, sampling, flag columns, and an overall review
flag.
