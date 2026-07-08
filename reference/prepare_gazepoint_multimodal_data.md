# Prepare multimodal Gazepoint and external face-window data

Joins facial-behaviour window summaries with optional Gazepoint-derived
summaries, response variables, or covariates. The helper is
intentionally conservative: it prepares transparent analysis tables and
optional scaled predictors, but it does not infer emotional states or
causal effects.

## Usage

``` r
prepare_gazepoint_multimodal_data(
  face_windows,
  gaze_data = NULL,
  response_data = NULL,
  by = NULL,
  gaze_by = NULL,
  response_by = NULL,
  predictor_cols = NULL,
  outcome_cols = NULL,
  covariate_cols = NULL,
  scale_predictors = TRUE,
  scaled_suffix = "_z",
  drop_missing_outcomes = FALSE,
  keep_all = TRUE
)
```

## Arguments

- face_windows:

  A data frame, usually returned by
  [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md)
  or
  [`summarize_gazepoint_face_reactivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_reactivity.md).

- gaze_data:

  Optional Gazepoint-derived data frame to join.

- response_data:

  Optional response/outcome data frame to join.

- by:

  Character vector of join columns shared across tables. If `NULL`,
  common identifier-like columns are detected.

- gaze_by:

  Optional named join mapping passed to
  [`merge()`](https://rdrr.io/r/base/merge.html) for `gaze_data`. If
  `NULL`, `by` is used.

- response_by:

  Optional named join mapping passed to
  [`merge()`](https://rdrr.io/r/base/merge.html) for `response_data`. If
  `NULL`, `by` is used.

- predictor_cols:

  Optional predictor columns to mark for modelling. If `NULL`, numeric
  non-identifier columns from the joined table are used.

- outcome_cols:

  Optional outcome columns to mark for modelling.

- covariate_cols:

  Optional covariate columns to mark for modelling.

- scale_predictors:

  Should numeric predictor columns be z-scaled?

- scaled_suffix:

  Suffix for scaled predictor columns.

- drop_missing_outcomes:

  Should rows with missing values in any `outcome_cols` be dropped?

- keep_all:

  Should all rows from `face_windows` be retained during joins?

## Value

A tibble with class `gp3_multimodal_data`. Attributes contain join
settings, selected predictors, outcomes, covariates, and scaling
metadata.

## Examples

``` r
face_windows <- data.frame(
  participant_id = c("P001", "P002"),
  trial_id = c(1, 1),
  AU12_r_mean = c(0.2, 0.3),
  face_confidence_mean = c(0.95, 0.94)
)

responses <- data.frame(
  participant_id = c("P001", "P002"),
  trial_id = c(1, 1),
  rating = c(4, 5)
)

prepare_gazepoint_multimodal_data(
  face_windows,
  response_data = responses,
  by = c("participant_id", "trial_id"),
  outcome_cols = "rating",
  predictor_cols = "AU12_r_mean"
)
#> # A tibble: 2 × 6
#>   participant_id trial_id AU12_r_mean face_confidence_mean rating AU12_r_mean_z
#>   <chr>             <dbl>       <dbl>                <dbl>  <dbl>         <dbl>
#> 1 P001                  1         0.2                 0.95      4        -0.707
#> 2 P002                  1         0.3                 0.94      5         0.707
```
