# Audit Gazepoint experimental design balance

Create a publication-level audit of observed design balance across
subjects, conditions, and optional stimulus/trial identifiers.

## Usage

``` r
audit_gazepoint_design_balance(
  data,
  subject_col = "subject",
  condition_col = "condition",
  unit_cols = c("media_id", "trial_global"),
  expected_conditions = NULL,
  min_units_per_condition = 1L,
  max_condition_ratio = 2,
  require_all_conditions_per_subject = TRUE
)
```

## Arguments

- data:

  A data frame containing trial-level, window-level, or sample-level
  Gazepoint-derived data.

- subject_col:

  Subject/participant identifier column.

- condition_col:

  Experimental condition column.

- unit_cols:

  Optional columns defining the repeated unit to count within each
  subject and condition, such as media, trial, block, or window.

- expected_conditions:

  Optional character vector of expected condition labels.

- min_units_per_condition:

  Minimum number of observed units expected per subject-condition cell.

- max_condition_ratio:

  Maximum allowed ratio between a subject's largest and smallest
  non-zero condition counts.

- require_all_conditions_per_subject:

  Logical. If `TRUE`, flag subjects who do not have all expected or
  observed conditions.

## Value

A list with class `gp3_design_balance_audit` containing overview,
subject_summary, condition_summary, cell_summary, imbalance_summary,
flagged_cells, and settings tables.
