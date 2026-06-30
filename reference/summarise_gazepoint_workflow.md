# Summarise a Gazepoint workflow result

Creates a compact one-row summary from a result object returned by
[`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md).
This is useful for quickly checking how many rows, file pairs, flagged
recordings, exported tables, exported plots, and reports were produced
by the workflow.

## Usage

``` r
summarise_gazepoint_workflow(results)
```

## Arguments

- results:

  A named list returned by
  [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md).

## Value

A tibble with one row containing workflow-level summary counts.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_gazepoint_workflow(
  export_dir = "C:/Users/YourName/Desktop/gp3_test_exports",
  output_dir = "C:/Users/YourName/Desktop/gp3_outputs",
  prefix = "study1",
  save_plots = TRUE,
  create_report = TRUE
)

summarise_gazepoint_workflow(results)
} # }
```
