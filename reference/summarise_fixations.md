# Summarise fixation-level AOI metrics

Summarise fixation-level AOI metrics

## Usage

``` r
summarise_fixations(data, group_cols = "MEDIA_ID", aoi_col = "AOI")
```

## Arguments

- data:

  A Gazepoint fixation data frame.

- group_cols:

  Grouping columns.

- aoi_col:

  AOI column name.

## Value

A tibble with fixation counts and summed fixation duration by AOI.
