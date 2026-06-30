# Create a Gazepoint HTML diagnostic report

Creates a lightweight HTML report from a `gp3tools` workflow result
object. The report includes dataset dimensions, sampling-rate checks,
flagged recordings, AOI summaries, and standard diagnostic plots.

## Usage

``` r
create_gazepoint_report(
  results,
  output_file,
  title = "Gazepoint diagnostic report",
  overwrite = TRUE,
  max_rows = 30,
  save_plots = TRUE,
  plot_dir = NULL
)
```

## Arguments

- results:

  A named list returned by
  [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md).

- output_file:

  Path to the HTML report file to create.

- title:

  Report title.

- overwrite:

  Logical. If `FALSE`, stop when the report file already exists.

- max_rows:

  Maximum number of rows to show in preview tables.

- save_plots:

  Logical. If `TRUE`, save and include diagnostic plots.

- plot_dir:

  Optional folder where report plot files should be saved. If `NULL`, a
  folder next to the report is created.

## Value

A tibble with the written report path and plot folder.
