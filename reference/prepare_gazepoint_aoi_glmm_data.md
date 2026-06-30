# Prepare AOI-window data for binomial GLMMs

Prepare AOI-window summaries for confirmatory binomial mixed-effects
modelling. The function creates success, failure, denominator,
proportion, subject, condition, and window columns from output produced
by
[`summarise_gazepoint_aoi_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_windows.md).

## Usage

``` r
prepare_gazepoint_aoi_glmm_data(
  data,
  success_col = "n_target_samples",
  denominator = c("valid", "all", "aoi", "custom"),
  denominator_col = NULL,
  valid_denominator_col = "n_valid_denominator_samples",
  all_denominator_col = "n_window_samples",
  aoi_denominator_col = "n_aoi_samples",
  subject_col = "subject",
  condition_col = "condition",
  window_col = "window_label",
  window_start_col = "window_start_ms",
  window_end_col = "window_end_ms",
  group_cols = NULL,
  min_denominator_samples = 1,
  drop_invalid = TRUE,
  missing_condition_label = "all_data",
  outcome_label = "target"
)
```

## Arguments

- data:

  AOI-window summary data.

- success_col:

  Column containing the success count. For target-looking models this is
  usually `n_target_samples`.

- denominator:

  Denominator definition. Use `"valid"` for valid AOI-window denominator
  samples, `"all"` for all window samples, `"aoi"` for AOI-only samples,
  or `"custom"` with `denominator_col`.

- denominator_col:

  Custom denominator column when `denominator = "custom"`.

- valid_denominator_col:

  Column used when `denominator = "valid"`.

- all_denominator_col:

  Column used when `denominator = "all"`.

- aoi_denominator_col:

  Column used when `denominator = "aoi"`.

- subject_col:

  Subject/participant column.

- condition_col:

  Optional condition column.

- window_col:

  AOI-window label column.

- window_start_col:

  Optional window-start column.

- window_end_col:

  Optional window-end column.

- group_cols:

  Optional extra grouping columns to keep/check.

- min_denominator_samples:

  Minimum acceptable denominator.

- drop_invalid:

  Logical. If `TRUE`, rows with invalid binomial counts or too-small
  denominators are removed.

- missing_condition_label:

  Label used when condition is missing.

- outcome_label:

  Label stored in the output to identify the modelled AOI outcome.

## Value

A tibble of GLMM-ready AOI-window rows.
