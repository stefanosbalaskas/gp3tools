# Save standard Gazepoint diagnostic plots

Saves standard diagnostic plots produced from `gp3tools` workflow
outputs.

## Usage

``` r
save_gazepoint_plots(
  flagged_quality = NULL,
  sampling = NULL,
  output_dir,
  prefix = "gazepoint",
  overwrite = TRUE,
  width = 9,
  height_quality = 6,
  height_sampling = 5,
  dpi = 300
)
```

## Arguments

- flagged_quality:

  Flagged tracking-quality table, usually from
  [`flag_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_tracking_quality.md)
  or
  [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md).

- sampling:

  Sampling-rate table, usually from
  [`check_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_sampling_rate.md)
  or
  [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md).

- output_dir:

  Folder where plot files should be saved.

- prefix:

  Filename prefix used for saved plot files.

- overwrite:

  Logical. If `FALSE`, stop when output plot files already exist.

- width:

  Plot width in inches.

- height_quality:

  Tracking-quality plot height in inches.

- height_sampling:

  Sampling-rate plot height in inches.

- dpi:

  Plot resolution.

## Value

A tibble with plot names and written file paths.
