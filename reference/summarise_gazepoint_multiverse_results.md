# Summarise Gazepoint preprocessing multiverse results

Summarise pupil and/or AOI preprocessing multiverse result objects
created by
[`run_gazepoint_pupil_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_pupil_multiverse.md)
and
[`run_gazepoint_aoi_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_aoi_multiverse.md).

## Usage

``` r
summarise_gazepoint_multiverse_results(..., results = NULL)
```

## Arguments

- ...:

  One or more multiverse result objects.

- results:

  Optional named list of multiverse result objects.

## Value

A list with class `gp3_multiverse_summary_results` containing overview,
branch summary, failure summary, and settings tables.
