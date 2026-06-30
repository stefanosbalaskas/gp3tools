# Read a Gazepoint Analysis Data Summary export

Parses the multi-section `Data_Summary_export_*.csv` file into metadata,
`aoi_summary`, and `aoi_by_user` tables.

## Usage

``` r
read_gazepoint_summary(path)
```

## Arguments

- path:

  Path to `Data_Summary_export_*.csv`.

## Value

A list with `metadata`, `aoi_summary`, and `aoi_by_user`.
