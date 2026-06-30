# Summarise sample-level AOI viewing

Computes transparent AOI metrics from sample-level rows. These may not
exactly reproduce Gazepoint Analysis summary metrics; use
[`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_summary.md)
when official Gazepoint summary values are available.

## Usage

``` r
summarise_aoi_samples(
  data,
  group_cols = "MEDIA_ID",
  aoi_col = "AOI",
  time_col = "TIME"
)
```

## Arguments

- data:

  A Gazepoint all-gaze data frame.

- group_cols:

  Grouping columns.

- aoi_col:

  AOI column name.

- time_col:

  Time column name.

## Value

A tibble with AOI sample count, TTFF, and approximate dwell time.
