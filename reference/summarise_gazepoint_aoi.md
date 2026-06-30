# Summarise Gazepoint AOI metrics from gaze and fixation exports

Combines sample-level AOI viewing information from all-gaze data with
fixation-level AOI metrics from fixation data.

## Usage

``` r
summarise_gazepoint_aoi(
  gaze_data,
  fixation_data,
  user_col = "USER_FILE",
  sample_rate = 60
)
```

## Arguments

- gaze_data:

  A Gazepoint all-gaze data frame imported with
  [`read_gazepoint()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint.md).

- fixation_data:

  A Gazepoint fixation data frame imported with
  [`read_gazepoint()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint.md).

- user_col:

  Name of the column identifying the user file. Default is
  `"USER_FILE"`.

- sample_rate:

  Assumed sampling rate used to approximate viewed time from sample
  counts.

## Value

A tibble with one row per user, media, and AOI.
