# Flag invalid, missing, implausible, and outlying Gazepoint pupil samples

Adds pupil-quality flags to a Gazepoint master sample-level table
created by
[`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
or
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md).
This function is intended as a preprocessing step before interpolation,
filtering, baseline correction, or pupil-based modelling.

## Usage

``` r
flag_gazepoint_pupil(
  master,
  pupil_col = NULL,
  time_col = NULL,
  missing_pupil_col = NULL,
  group_cols = c("subject", "media_id"),
  min_pupil = 0,
  max_pupil = Inf,
  outlier_k = 1.5,
  flag_iqr_outliers = TRUE
)
```

## Arguments

- master:

  A Gazepoint master sample-level table.

- pupil_col:

  Optional name of the pupil column to flag. If `NULL`, the function
  detects one of `mean_pupil`, `pupil`, `pupil_raw`, `left_pupil`, or
  `right_pupil`.

- time_col:

  Optional name of the time column. If `NULL`, the function detects one
  of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.

- missing_pupil_col:

  Optional name of the missing-pupil flag column. If `NULL`, the
  function uses `missing_pupil` when available.

- group_cols:

  Character vector of grouping columns used for IQR-based outlier
  detection. Defaults to `c("subject", "media_id")` using internally
  standardised names. Use `character(0)` for global outlier detection.

- min_pupil:

  Minimum plausible pupil value. Defaults to `0`.

- max_pupil:

  Maximum plausible pupil value. Defaults to `Inf`. Use narrower values,
  such as `1` and `9`, only when the pupil column is known to be
  measured in millimetres.

- outlier_k:

  Multiplier for IQR-based outlier detection. Defaults to `1.5`.

- flag_iqr_outliers:

  Logical. If `TRUE`, IQR-based outliers are flagged. Defaults to
  `TRUE`.

## Value

A tibble containing the original master table plus pupil-flagging
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
flagged <- flag_gazepoint_pupil(master)

dplyr::count(flagged, pupil_flag_reason)
} # }
```
