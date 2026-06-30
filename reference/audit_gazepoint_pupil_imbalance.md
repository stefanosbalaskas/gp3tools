# Audit Gazepoint pupil preprocessing imbalance

Check whether pupil preprocessing loss differs across experimental
conditions or other grouping variables.

## Usage

``` r
audit_gazepoint_pupil_imbalance(
  data,
  group_cols = "condition",
  pupil_col = "pupil_interpolated",
  interpolated_col = "pupil_was_interpolated",
  interpolation_status_col = "pupil_interpolation_status",
  artifact_col = NULL,
  artifact_reason_col = NULL,
  min_group_n = 1,
  max_valid_pct_diff = 10,
  max_artifact_pct_diff = 10,
  max_missing_pct_diff = 10,
  max_interpolated_pct_diff = 10
)
```

## Arguments

- data:

  A data frame from a pupil preprocessing pipeline.

- group_cols:

  Character vector of grouping columns. By default, summaries are
  produced by `condition`.

- pupil_col:

  Name of the post-preprocessing pupil column used to define remaining
  valid and missing samples.

- interpolated_col:

  Name of the logical interpolation flag column.

- interpolation_status_col:

  Name of the interpolation-status column.

- artifact_col:

  Optional artifact flag column. If `NULL`, the function tries to detect
  `pupil_artifact_flag`, `pupil_flag_invalid`, or `artifact_flag`.

- artifact_reason_col:

  Optional artifact-reason column. If `NULL`, the function tries to
  detect `pupil_artifact_reason`, `pupil_flag_reason`, or
  `artifact_reason`.

- min_group_n:

  Minimum group size below which a group is flagged.

- max_valid_pct_diff:

  Maximum acceptable range in valid-sample percentage across groups.

- max_artifact_pct_diff:

  Maximum acceptable range in artifact percentage across groups.

- max_missing_pct_diff:

  Maximum acceptable range in remaining-missing percentage across
  groups.

- max_interpolated_pct_diff:

  Maximum acceptable range in interpolated percentage across groups.

## Value

A tibble with one row per group and imbalance-warning columns.

## Details

The function summarises valid pupil samples, interpolated samples,
artifact-flagged samples, and remaining missing samples. It also adds
simple imbalance flags based on differences between groups.
