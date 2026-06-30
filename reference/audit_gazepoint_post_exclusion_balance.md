# Audit post-exclusion condition balance

Create a publication-level audit of whether the retained analysis sample
remains balanced across subjects and experimental conditions after
exclusions.

## Usage

``` r
audit_gazepoint_post_exclusion_balance(
  data,
  subject_col = "subject",
  condition_col = "condition",
  unit_cols = c("media_id", "trial_global"),
  retained_col = NULL,
  include_col = NULL,
  exclude_col = NULL,
  status_col = NULL,
  expected_conditions = NULL,
  included_values = c("included", "include", "kept", "keep", "retained", "ok", "ready",
    "complete", "completed"),
  excluded_values = c("excluded", "exclude", "drop", "dropped", "removed", "fail",
    "failed", "not_ready", "review", "invalid"),
  min_retained_units_per_condition = 1L,
  min_retained_units_per_subject_condition = 1L,
  max_condition_count_ratio = 2,
  max_subject_condition_ratio = 2,
  require_all_conditions_per_subject = TRUE
)
```

## Arguments

- data:

  A data frame containing row-, sample-, trial-, or unit-level data.

- subject_col:

  Subject/participant identifier column.

- condition_col:

  Experimental condition column.

- unit_cols:

  Optional columns defining the analysis unit, such as media, trial,
  block, or window.

- retained_col:

  Optional logical/numeric/character column indicating retained units.

- include_col:

  Optional logical/numeric/character inclusion column.

- exclude_col:

  Optional logical/numeric/character exclusion column.

- status_col:

  Optional status column used to infer retained/excluded units.

- expected_conditions:

  Optional character vector of expected conditions.

- included_values:

  Character values in `status_col` treated as retained.

- excluded_values:

  Character values in `status_col` treated as excluded.

- min_retained_units_per_condition:

  Minimum retained units required per condition.

- min_retained_units_per_subject_condition:

  Minimum retained units required per subject-condition cell.

- max_condition_count_ratio:

  Maximum allowed ratio between condition-level retained counts.

- max_subject_condition_ratio:

  Maximum allowed within-subject retained condition-count ratio.

- require_all_conditions_per_subject:

  Logical. If `TRUE`, flag subjects missing retained units in one or
  more expected conditions.

## Value

A list with class `gp3_post_exclusion_balance_audit` containing
overview, unit_flow, cell_summary, condition_summary, subject_summary,
flagged_cells, flagged_subjects, and settings tables.
