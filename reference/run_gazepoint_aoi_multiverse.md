# Run a Gazepoint AOI preprocessing multiverse

Run all AOI preprocessing branches defined by
[`create_gazepoint_preprocessing_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_multiverse.md).
Each branch creates AOI-window summaries and then prepares binomial AOI
GLMM data using the branch-specific denominator and minimum denominator
threshold.

## Usage

``` r
run_gazepoint_aoi_multiverse(
  data,
  multiverse,
  branch_ids = NULL,
  windows,
  time_col = "time",
  aoi_col = "aoi_current",
  subject_col = "subject",
  condition_col = NULL,
  group_cols = NULL,
  target_aoi_values,
  distractor_aoi_values = NULL,
  success_col = "n_target_samples",
  outcome_label = "target",
  keep_outputs = TRUE,
  stop_on_error = FALSE
)
```

## Arguments

- data:

  A Gazepoint master table or sample-level AOI table.

- multiverse:

  A `gp3_preprocessing_multiverse` object returned by
  [`create_gazepoint_preprocessing_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_multiverse.md).

- branch_ids:

  Optional character vector of AOI branch IDs to run.

- windows:

  Numeric vector or labelled window table passed to
  [`summarise_gazepoint_aoi_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_windows.md).

- time_col:

  Time column.

- aoi_col:

  AOI-state column.

- subject_col:

  Subject column.

- condition_col:

  Optional condition column.

- group_cols:

  Optional grouping columns for AOI-window summaries.

- target_aoi_values:

  Target AOI values.

- distractor_aoi_values:

  Optional distractor AOI values.

- success_col:

  Success-count column passed to
  [`prepare_gazepoint_aoi_glmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_glmm_data.md).

- outcome_label:

  Outcome label passed to AOI helpers.

- keep_outputs:

  Logical. If `TRUE`, keep branch outputs in `branch_outputs`.

- stop_on_error:

  Logical. If `TRUE`, stop when a branch fails. If `FALSE`, record the
  branch error and continue.

## Value

A list with class `gp3_aoi_multiverse_results` containing overview,
branch results, optional branch outputs, and settings.
