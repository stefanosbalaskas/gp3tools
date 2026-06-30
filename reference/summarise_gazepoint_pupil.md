# Summarise Gazepoint pupil data

Creates compact pupil-quality and pupil-distribution summaries from a
Gazepoint master sample-level table created by
[`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
or
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md).
This function is intended as the first pupil preprocessing gate before
interpolation, filtering, baseline correction, or pupil-based modelling.

## Usage

``` r
summarise_gazepoint_pupil(
  master,
  group_cols = c("subject", "media_id"),
  pupil_col = NULL,
  time_col = NULL,
  missing_pupil_col = NULL,
  min_pupil = 0,
  max_pupil = Inf,
  outlier_k = 1.5
)
```

## Arguments

- master:

  A Gazepoint master sample-level table.

- group_cols:

  Character vector of grouping columns. Defaults to
  `c("subject", "media_id")` using internally standardised names. Use
  `character(0)` for an overall summary.

- pupil_col:

  Optional name of the pupil column to summarise. If `NULL`, the
  function detects one of `mean_pupil`, `pupil`, `pupil_raw`,
  `left_pupil`, or `right_pupil`.

- time_col:

  Optional name of the time column. If `NULL`, the function detects one
  of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.

- missing_pupil_col:

  Optional name of the missing-pupil flag column. If `NULL`, the
  function uses `missing_pupil` when available.

- min_pupil:

  Minimum plausible pupil value. Defaults to `0`.

- max_pupil:

  Maximum plausible pupil value. Defaults to `Inf`. Use narrower values,
  such as `1` and `9`, only when the pupil column is known to be
  measured in millimetres.

- outlier_k:

  Multiplier for IQR-based outlier detection. Defaults to `1.5`.

## Value

A tibble with pupil-quality and pupil-distribution summaries.

## Examples

``` r
if (FALSE) { # \dontrun{
master <- create_gazepoint_master(
  gaze_data = results$all_gaze,
  screen_width_px = 1920,
  screen_height_px = 1080
)

summarise_gazepoint_pupil(master)
summarise_gazepoint_pupil(master, group_cols = "subject")
summarise_gazepoint_pupil(master, group_cols = character(0))
} # }
```
