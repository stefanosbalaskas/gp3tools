# Prepare AOI time-course data for GAMM analysis

Prepare sample-level or binned Gazepoint AOI data for AOI time-course
GAMM analysis. The function creates binned subject-by-condition-by-time
summaries with binomial success/failure counts for target-AOI looking
over time.

## Usage

``` r
prepare_gazepoint_aoi_gamm_data(
  data,
  aoi_col = "aoi_current",
  target_aoi_values = NULL,
  outcome_col = NULL,
  subject_col = "subject",
  condition_col = "condition",
  time_col = "time",
  trial_col = NULL,
  time_bin_col = NULL,
  conditions = NULL,
  time_window = NULL,
  bin_size_ms = 50,
  denominator = c("valid", "all", "aoi_only"),
  valid_aoi_values = NULL,
  non_aoi_values = c("non_aoi"),
  missing_aoi_values = c("missing", ""),
  min_denominator_samples = 1,
  drop_invalid = TRUE,
  missing_condition_label = "all_data",
  outcome_label = "target_aoi"
)
```

## Arguments

- data:

  A data frame containing sample-level or binned AOI data.

- aoi_col:

  Name of the AOI-state column. Used when `outcome_col = NULL`.

- target_aoi_values:

  Character vector identifying target AOI values. Required when
  `outcome_col = NULL`.

- outcome_col:

  Optional logical or 0/1 numeric column indicating target AOI looking
  at the sample level. If supplied, this takes priority over `aoi_col`
  and `target_aoi_values`.

- subject_col:

  Name of the subject/participant column.

- condition_col:

  Name of the condition column. If unavailable, a single fallback
  condition is created.

- time_col:

  Name of the time column, in milliseconds.

- trial_col:

  Optional trial identifier column.

- time_bin_col:

  Optional existing time-bin column. If `NULL`, time bins are created
  from `time_col` using `bin_size_ms`.

- conditions:

  Optional character vector of condition levels to retain and order.

- time_window:

  Optional numeric vector of length 2 defining the retained time window
  in milliseconds.

- bin_size_ms:

  Time-bin width in milliseconds when `time_bin_col = NULL`.

- denominator:

  Denominator definition. `"valid"` uses non-missing AOI states, `"all"`
  uses all retained rows, and `"aoi_only"` uses only explicit AOI
  states.

- valid_aoi_values:

  Optional character vector defining explicit AOI values for
  `"aoi_only"` denominators. If `NULL`, values beginning with `"AOI"`
  are treated as explicit AOIs, excluding `non_aoi_values`.

- non_aoi_values:

  Character vector identifying non-AOI/background states.

- missing_aoi_values:

  Character vector identifying missing AOI-state labels.

- min_denominator_samples:

  Minimum number of denominator samples required per
  subject-condition-time bin.

- drop_invalid:

  Logical. If `TRUE`, bins with zero or low denominators are removed
  from the returned data.

- missing_condition_label:

  Fallback condition label when no usable condition column is available.

- outcome_label:

  Descriptive label for the AOI-GAMM outcome.

## Value

A tibble with standardised AOI-GAMM columns.

## Details

This helper is intended for modelling AOI time-course trajectories, such
as target-AOI looking probability over time. It is separate from
confirmatory AOI-window GLMMs and from cluster-based permutation tests.
