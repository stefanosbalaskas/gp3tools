# Export Gazepoint time-course data for permutes-style cluster workflows

Write a conservative long-format CSV file and README for continuation in
an external R/permutes workflow. This helper prepares data only; it does
not run permutes or validate the external model specification.

## Usage

``` r
export_gazepoint_permutes_cluster_input(
  data,
  outdir,
  subject_col = ".gp3_cluster_subject",
  condition_col = ".gp3_cluster_condition",
  time_col = ".gp3_cluster_time_bin",
  outcome_col = ".gp3_cluster_outcome",
  overwrite = FALSE
)
```

## Arguments

- data:

  A prepared or raw long-format time-course data frame.

- outdir:

  Output directory.

- subject_col:

  Subject column.

- condition_col:

  Condition column.

- time_col:

  Time-bin column.

- outcome_col:

  Outcome column.

- overwrite:

  Should an existing directory be reused?

## Value

A data frame listing written files.
