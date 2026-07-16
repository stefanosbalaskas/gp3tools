# Audit time-varying AOI coverage

Summarise whether gaze samples could be matched to dynamic AOI
definitions, how large the definition-time gaps were, and how often
samples fell inside versus outside defined AOIs.

## Usage

``` r
audit_gazepoint_dynamic_aoi_coverage(
  data,
  label_col = "aoi_current",
  definition_time_col = "aoi_definition_time",
  time_gap_col = "aoi_time_gap",
  group_cols = NULL,
  outside_label = "outside",
  max_time_gap = Inf,
  x_col = NULL,
  y_col = NULL
)
```

## Arguments

- data:

  Output from
  [`add_gazepoint_dynamic_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_dynamic_aoi.md).

- label_col:

  AOI-label column.

- definition_time_col:

  Matched-definition timestamp column.

- time_gap_col:

  Definition-time-gap column.

- group_cols:

  Optional summary grouping columns.

- outside_label:

  Outside-AOI label.

- max_time_gap:

  Optional audit threshold for definition-time gaps.

- x_col, y_col:

  Optional coordinate columns used to flag missing gaze.

## Value

An object of class `"gp3_dynamic_aoi_coverage_audit"` containing
overview, group, AOI, and flagged-row tables plus settings.
