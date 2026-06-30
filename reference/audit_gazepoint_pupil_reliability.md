# Audit split-half reliability for Gazepoint pupil outcomes

Create a split-half reliability audit for trial-level or window-level
pupil outcomes. The helper is intended for publication-readiness checks
when pupil features are interpreted as stable participant-level outcomes
or individual difference measures.

## Usage

``` r
audit_gazepoint_pupil_reliability(
  data,
  outcome_cols = NULL,
  participant_col = NULL,
  trial_col = NULL,
  split_col = NULL,
  by_cols = NULL,
  split_method = c("odd_even", "first_second"),
  aggregate_function = c("mean", "median"),
  correlation_method = c("pearson", "spearman"),
  min_trials_per_split = 2,
  name = "gazepoint_pupil_reliability"
)
```

## Arguments

- data:

  A data frame containing trial-level or window-level pupil outcomes.

- outcome_cols:

  Character vector of pupil outcome columns. If `NULL`, common pupil
  outcome columns are detected automatically.

- participant_col:

  Participant/subject column. If `NULL`, common participant columns are
  detected automatically.

- trial_col:

  Trial/order column. If `NULL`, common trial columns are detected
  automatically when available. If no trial column is available, row
  order within participant is used.

- split_col:

  Optional pre-existing split column. If supplied, it must have exactly
  two non-missing levels.

- by_cols:

  Optional grouping columns for separate reliability audits, such as
  `"condition"` or `"window"`.

- split_method:

  Split method used when `split_col = NULL`. Options are `"odd_even"`
  and `"first_second"`.

- aggregate_function:

  Function used to aggregate trial-level values within participant and
  split. Options are `"mean"` and `"median"`.

- correlation_method:

  Correlation method for split-half association. Options are `"pearson"`
  and `"spearman"`.

- min_trials_per_split:

  Minimum number of non-missing outcome values required in each split
  for a participant to contribute to the reliability estimate.

- name:

  Character label stored in the audit object.

## Value

A list with class `gp3_pupil_reliability_audit`.
