# Audit Gazepoint exclusion and retention flow

Create a publication-level audit of retained and excluded analysis
units.

## Usage

``` r
audit_gazepoint_exclusion_flow(
  data,
  subject_col = "subject",
  condition_col = NULL,
  unit_cols = c("media_id", "trial_global"),
  include_col = NULL,
  exclude_col = NULL,
  status_col = NULL,
  reason_col = NULL,
  included_values = c("included", "include", "kept", "keep", "retained", "ok", "ready",
    "complete", "completed"),
  excluded_values = c("excluded", "exclude", "drop", "dropped", "removed", "fail",
    "failed", "not_ready", "review", "invalid"),
  min_retained_prop = 0.7,
  max_condition_exclusion_ratio = 2
)
```

## Arguments

- data:

  A data frame containing row-, trial-, window-, or unit-level data.

- subject_col:

  Subject/participant identifier column.

- condition_col:

  Optional condition column.

- unit_cols:

  Optional columns defining the analysis unit, such as media, trial,
  block, or window.

- include_col:

  Optional logical/numeric/character column indicating rows or units
  retained for analysis.

- exclude_col:

  Optional logical/numeric/character column indicating rows or units
  excluded from analysis.

- status_col:

  Optional status column used to infer inclusion or exclusion.

- reason_col:

  Optional exclusion-reason column.

- included_values:

  Character values in `status_col` treated as retained.

- excluded_values:

  Character values in `status_col` treated as excluded.

- min_retained_prop:

  Minimum acceptable retained-unit proportion.

- max_condition_exclusion_ratio:

  Maximum allowed ratio between condition exclusion proportions.

## Value

A list with class `gp3_exclusion_flow_audit` containing overview,
unit_flow, reason_summary, condition_summary, subject_summary,
flagged_units, and settings tables.
