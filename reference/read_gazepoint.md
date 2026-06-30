# Read a Gazepoint all-gaze or fixation CSV export

Reads Gazepoint all-gaze and fixation CSV exports, standardises
timestamped column names, and removes empty trailing columns produced by
Gazepoint exports.

## Usage

``` r
read_gazepoint(path, standardise_names = TRUE, drop_empty_cols = TRUE)
```

## Arguments

- path:

  Path to a Gazepoint CSV export.

- standardise_names:

  Logical. If `TRUE`, standardise `TIME(...)` and `TIMETICK(...)`
  headers.

- drop_empty_cols:

  Logical. If `TRUE`, remove empty trailing or unnamed columns created
  by Gazepoint export formatting.

## Value

A tibble with attributes `gp3_file_type` and `gp3_source_file`.
