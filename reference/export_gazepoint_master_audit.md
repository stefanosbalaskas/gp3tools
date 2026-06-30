# Export a Gazepoint master table, audit tables, and validation tables

Exports a Gazepoint master sample-level table together with the output
of
[`audit_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_master.md)
and, optionally,
[`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md).
This function is useful after creating a master table with
[`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
or
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md).

## Usage

``` r
export_gazepoint_master_audit(
  master,
  audit = NULL,
  validation = NULL,
  output_dir = ".",
  prefix = "gazepoint",
  export_master = TRUE,
  export_audit = TRUE,
  export_validation = TRUE,
  overwrite = TRUE,
  na = ""
)
```

## Arguments

- master:

  A Gazepoint master sample-level table.

- audit:

  Optional audit list returned by
  [`audit_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_master.md).
  If `NULL`, the audit is created automatically.

- validation:

  Optional validation list returned by
  [`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md).
  If `NULL` and `export_validation = TRUE`, validation is created
  automatically.

- output_dir:

  Directory where CSV files should be written.

- prefix:

  File prefix used for all exported CSV files.

- export_master:

  Logical. If `TRUE`, exports the full master table.

- export_audit:

  Logical. If `TRUE`, exports audit tables.

- export_validation:

  Logical. If `TRUE`, exports validation tables.

- overwrite:

  Logical. If `FALSE`, the function aborts when any target file already
  exists.

- na:

  String used for missing values in CSV output.

## Value

A tibble listing the exported files, table names, and dimensions.

## Examples

``` r
if (FALSE) { # \dontrun{
master <- create_gazepoint_master(
  gaze_data = results$all_gaze,
  screen_width_px = 1920,
  screen_height_px = 1080
)

audit <- audit_gazepoint_master(master)
validation <- validate_gazepoint_master(master)

exported <- export_gazepoint_master_audit(
  master = master,
  audit = audit,
  validation = validation,
  output_dir = "gazepoint_outputs",
  prefix = "study1"
)

exported
} # }
```
