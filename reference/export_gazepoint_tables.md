# Export Gazepoint analysis tables to CSV files

Writes a named list of analysis tables to CSV files in an output folder.

## Usage

``` r
export_gazepoint_tables(
  tables,
  output_dir,
  prefix = NULL,
  overwrite = TRUE,
  na = ""
)
```

## Arguments

- tables:

  A named list of data frames or tibbles.

- output_dir:

  Folder where CSV files should be written.

- prefix:

  Optional filename prefix.

- overwrite:

  Logical. If `FALSE`, the function stops when a target file already
  exists.

- na:

  Value used for missing values in the exported CSV files.

## Value

A tibble with table names and written file paths.
