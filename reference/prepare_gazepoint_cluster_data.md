# Prepare time-course data for cluster-based permutation tests

Prepare sample-level or already binned Gazepoint time-course data for
cluster-based permutation testing. The function standardises subject,
condition, time-bin, outcome, sample-count, trial-count, and status
columns. It can be used for AOI proportions, pupil time-course outcomes,
or other continuous time-varying measures.

## Usage

``` r
prepare_gazepoint_cluster_data(
  data,
  outcome_col,
  subject_col = "subject",
  condition_col = "condition",
  time_col = "time",
  trial_col = NULL,
  time_bin_col = NULL,
  conditions = NULL,
  time_window = NULL,
  bin_size_ms = 50,
  aggregation = c("mean", "proportion", "sum", "median"),
  min_samples_per_bin = 1,
  paired = TRUE,
  drop_invalid = TRUE,
  missing_condition_label = "all_data",
  outcome_label = "outcome"
)
```

## Arguments

- data:

  A data frame containing sample-level or binned time-course data.

- outcome_col:

  Column containing the outcome to test. For AOI analyses this is often
  a 0/1 or logical AOI column. For pupil analyses this is often a
  processed pupil column.

- subject_col:

  Subject/participant column.

- condition_col:

  Optional condition column.

- time_col:

  Time column in milliseconds.

- trial_col:

  Optional trial identifier column.

- time_bin_col:

  Optional existing time-bin column. If `NULL`, time bins are created
  from `time_col` and `bin_size_ms`.

- conditions:

  Optional character vector of condition levels to keep. Cluster tests
  are usually pairwise, so this is typically length 2.

- time_window:

  Optional numeric vector of length 2 giving the time range to retain,
  in milliseconds.

- bin_size_ms:

  Bin size in milliseconds when `time_bin_col = NULL`.

- aggregation:

  How to aggregate samples within subject-condition-time bins. Supported
  values are `"mean"`, `"proportion"`, `"sum"`, and `"median"`.
  `"proportion"` is equivalent to the mean of a numeric/logical 0/1
  outcome.

- min_samples_per_bin:

  Minimum number of samples required per subject-condition-time bin.

- paired:

  Logical. If `TRUE`, retain only subjects with all retained condition
  levels.

- drop_invalid:

  Logical. If `TRUE`, rows and bins that are not suitable for cluster
  testing are removed.

- missing_condition_label:

  Label used when condition is missing or `condition_col` is
  unavailable.

- outcome_label:

  Label stored in the output to identify the outcome.

## Value

A tibble with standardised cluster-test preparation columns.

## Details

Cluster-based permutation tests are intended for time-course inference.
They should not be used to discover a time window and then test that
same window again as a confirmatory analysis.
