# Diagnose the design assumptions of a Gazepoint cluster-permutation workflow

Provide a compact diagnostic summary for the conservative two-condition,
within-subject, one-dimensional cluster-permutation workflow.

## Usage

``` r
diagnose_gazepoint_cluster_design(
  data,
  subject_col = ".gp3_cluster_subject",
  condition_col = ".gp3_cluster_condition",
  time_col = ".gp3_cluster_time_bin",
  outcome_col = ".gp3_cluster_outcome"
)
```

## Arguments

- data:

  A prepared or raw long-format time-course data frame.

- subject_col:

  Subject column.

- condition_col:

  Condition column.

- time_col:

  Time-bin column.

- outcome_col:

  Outcome column.

## Value

A data frame of design checks and cautious interpretations.
