# Export manuscript-ready model tables

Export model-summary tables and optional estimated marginal means tables
to CSV files. The function is designed for objects returned by
[`tidy_gazepoint_model_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/tidy_gazepoint_model_summary.md)
and
[`summarise_gazepoint_emmeans()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_emmeans.md).

## Usage

``` r
export_gazepoint_model_tables(
  model_summary = NULL,
  emmeans_summary = NULL,
  output_dir,
  prefix = "gazepoint_model",
  overwrite = TRUE,
  include_diagnostics = TRUE
)
```

## Arguments

- model_summary:

  Optional object returned by
  [`tidy_gazepoint_model_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/tidy_gazepoint_model_summary.md).

- emmeans_summary:

  Optional object returned by
  [`summarise_gazepoint_emmeans()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_emmeans.md).

- output_dir:

  Output directory.

- prefix:

  File-name prefix.

- overwrite:

  Logical. If `FALSE`, existing output files cause an error.

- include_diagnostics:

  Logical. If `TRUE`, export available diagnostic component tables from
  `model_summary`.

## Value

A tibble indexing the written files.
