# Write standard Gazepoint analysis outputs

Convenience wrapper for exporting standard `gp3tools` outputs such as
sampling checks, tracking quality summaries, flagged quality rows, and
AOI tables.

## Usage

``` r
write_gazepoint_outputs(
  sampling = NULL,
  quality = NULL,
  flagged_quality = NULL,
  aoi_table = NULL,
  output_dir,
  prefix = "gazepoint",
  overwrite = TRUE
)
```

## Arguments

- sampling:

  Sampling-rate table, usually from
  [`check_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_sampling_rate.md).

- quality:

  Tracking-quality table, usually from
  [`summarise_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_tracking_quality.md).

- flagged_quality:

  Flagged quality table, usually from
  [`flag_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_tracking_quality.md).

- aoi_table:

  AOI summary table, usually from
  [`summarise_gazepoint_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi.md).

- output_dir:

  Folder where CSV files should be written.

- prefix:

  Optional filename prefix.

- overwrite:

  Logical. If `FALSE`, stop when files already exist.

## Value

A tibble with table names and written file paths.
