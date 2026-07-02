# Audit split-half reliability of fixation or AOI metrics

Computes odd-even or random split-half reliability for common fixation
and AOI-derived metrics. The audit returns the split-half correlation
and the Spearman-Brown corrected reliability estimate.

## Usage

``` r
audit_gazepoint_fixation_reliability(
  data,
  subject_col,
  trial_col,
  metric = c("fixation_count", "mean_fixation_duration", "total_fixation_duration",
    "aoi_dwell_prop", "transition_count", "entropy_score"),
  duration_col = NULL,
  aoi_col = NULL,
  target_aoi = NULL,
  time_col = NULL,
  group_cols = NULL,
  min_trials = 4,
  split_method = c("odd_even", "random"),
  seed = NULL,
  correlation_method = c("pearson", "spearman")
)
```

## Arguments

- data:

  A fixation-level or AOI-event-level data frame.

- subject_col:

  Character scalar. Subject identifier column.

- trial_col:

  Character scalar. Trial identifier column.

- metric:

  Metric to audit. Supported values are `"fixation_count"`,
  `"mean_fixation_duration"`, `"total_fixation_duration"`,
  `"aoi_dwell_prop"`, `"transition_count"`, and `"entropy_score"`.

- duration_col:

  Optional duration column. Required for duration metrics. Optional for
  `"aoi_dwell_prop"`; if omitted, row proportions are used.

- aoi_col:

  Optional AOI column. Required for AOI, transition, and entropy
  metrics.

- target_aoi:

  Optional AOI label required when `metric = "aoi_dwell_prop"`.

- time_col:

  Optional time column used to order AOI sequences.

- group_cols:

  Optional grouping columns. Reliability is computed separately within
  each group.

- min_trials:

  Minimum number of trials per subject required for inclusion.

- split_method:

  `"odd_even"` or `"random"`.

- seed:

  Optional random seed used when `split_method = "random"`.

- correlation_method:

  Correlation method passed to
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html).

## Value

A data frame containing split-half reliability diagnostics.

## Examples

``` r
dat <- expand.grid(
  subject = paste0("S", 1:6),
  trial = paste0("T", 1:4),
  KEEP.OUT.ATTRS = FALSE
)
dat$duration <- rep(seq_len(6), each = 4) + rep(c(0, 0.1, 0, 0.1), 6)

audit_gazepoint_fixation_reliability(
  dat,
  subject_col = "subject",
  trial_col = "trial",
  metric = "total_fixation_duration",
  duration_col = "duration"
)
#> Warning: internal error 1 in R_decompress1 with libdeflate
#> Error: lazy-load database 'C:/Users/Stefanos-PC/AppData/Local/R/win-library/4.6/gp3tools/R/gp3tools.rdb' is corrupt
```
