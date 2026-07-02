# Export Gazepoint time-course data for MNE-style cluster workflows

Write a conservative long-format CSV file and README for continuation in
an external Python/MNE workflow. This helper prepares data only; it does
not run MNE, construct adjacency matrices, validate exchangeability, or
validate the external analysis.

## Usage

``` r
export_gazepoint_mne_cluster_input(
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
