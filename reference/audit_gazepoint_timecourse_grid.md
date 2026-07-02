# Audit a Gazepoint time-course grid for cluster-permutation readiness

Check whether a prepared or raw long-format time-course data set has the
subject-by-condition-by-time structure expected by the conservative
two-condition cluster-permutation workflow.

## Usage

``` r
audit_gazepoint_timecourse_grid(
  data,
  subject_col = ".gp3_cluster_subject",
  condition_col = ".gp3_cluster_condition",
  time_col = ".gp3_cluster_time_bin",
  outcome_col = ".gp3_cluster_outcome"
)
```

## Arguments

- data:

  A data frame.

- subject_col:

  Subject column. Defaults to the internal prepared column.

- condition_col:

  Condition column. Defaults to the internal prepared column.

- time_col:

  Time-bin column. Defaults to the internal prepared column.

- outcome_col:

  Outcome column. Defaults to the internal prepared column.

## Value

A list containing grid counts, missing cells, duplicate cells, and
readiness flags.
