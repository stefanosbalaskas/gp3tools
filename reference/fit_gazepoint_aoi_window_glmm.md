# Fit an AOI-window binomial GLMM

Fit a confirmatory AOI-window mixed-effects logistic regression from
data prepared by
[`prepare_gazepoint_aoi_glmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_glmm_data.md).

## Usage

``` r
fit_gazepoint_aoi_window_glmm(
  data,
  success_col = "aoi_glmm_success",
  failure_col = "aoi_glmm_failure",
  subject_col = "aoi_glmm_subject",
  condition_col = "aoi_glmm_condition",
  window_col = "aoi_glmm_window",
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE,
  random_intercept = TRUE,
  random_window_slopes = FALSE,
  fallback_on_singular = TRUE,
  optimizer = "bobyqa",
  maxfun = 2e+05,
  nAGQ = 0,
  drop_missing = TRUE
)
```

## Arguments

- data:

  AOI GLMM data returned by
  [`prepare_gazepoint_aoi_glmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_glmm_data.md).

- success_col:

  Success-count column.

- failure_col:

  Failure-count column.

- subject_col:

  Subject factor/column.

- condition_col:

  Condition factor/column.

- window_col:

  AOI-window factor/column.

- include_condition:

  Logical. Include condition fixed effects when at least two conditions
  are available.

- include_window:

  Logical. Include window fixed effects when at least two windows are
  available.

- include_interaction:

  Logical. Include condition-by-window interaction when both condition
  and window fixed effects are included.

- random_intercept:

  Logical. Include subject random intercept.

- random_window_slopes:

  Logical. Attempt subject-level random slopes for AOI window.

- fallback_on_singular:

  Logical. If `TRUE`, fall back to a simpler random intercept model when
  a random-slope model is singular or fails.

- optimizer:

  Optimizer passed to
  [`lme4::glmerControl()`](https://rdrr.io/pkg/lme4/man/lmerControl.html).

- maxfun:

  Maximum optimizer evaluations.

- nAGQ:

  Number of adaptive Gauss-Hermite quadrature points.

- drop_missing:

  Logical. Drop rows with missing model variables before fitting.

## Value

A list with fitted model, attempted model, formulas, comparison table,
settings, status fields, and model data.
