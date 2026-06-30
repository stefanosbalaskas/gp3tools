# Standardise Gazepoint column names

Converts timestamped Gazepoint headers such as
`TIME(2026/02/20 00:53:57.275)` to `TIME`, converts
`TIMETICK(f=10000000)` to `TIMETICK`, trims whitespace, and removes
empty columns created by trailing commas in Gazepoint exports.

## Usage

``` r
standardise_gazepoint_names(x)
```

## Arguments

- x:

  A data frame or character vector of column names.

## Value

If `x` is a data frame, the same data frame with standardised names and
empty Gazepoint columns removed. If `x` is a character vector, a
character vector.
