# Fit AOI-window model-family sensitivity checks

Fit a compact set of sensitivity models for AOI-window outcomes. The
main model is a binomial GLMM. Additional checks can include an
empirical-logit LMM, a weighted proportion LMM, and a fixed-effects
quasibinomial GLM.

## Usage

``` r
fit_gazepoint_aoi_model_sensitivity(
  data,
  success_col = "aoi_glmm_success",
  failure_col = "aoi_glmm_failure",
  denominator_col = "aoi_glmm_denominator",
  proportion_col = "aoi_glmm_prop",
  subject_col = "aoi_glmm_subject",
  condition_col = "aoi_glmm_condition",
  window_col = "aoi_glmm_window",
  model_types = c("binomial_glmm", "empirical_logit_lmm", "proportion_lmm",
    "quasibinomial_glm"),
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE,
  random_intercept = TRUE,
  optimizer = "bobyqa",
  maxfun = 2e+05,
  nAGQ = 0,
  empirical_logit_correction = 0.5,
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

- denominator_col:

  Denominator column.

- proportion_col:

  Proportion column.

- subject_col:

  Subject column.

- condition_col:

  Condition column.

- window_col:

  Window column.

- model_types:

  Character vector of model types. Supported values are
  `"binomial_glmm"`, `"empirical_logit_lmm"`, `"proportion_lmm"`, and
  `"quasibinomial_glm"`.

- include_condition:

  Logical. Include condition fixed effect when possible.

- include_window:

  Logical. Include window fixed effect when possible.

- include_interaction:

  Logical. Include condition-by-window interaction when both condition
  and window are included.

- random_intercept:

  Logical. Include subject random intercept in mixed sensitivity models.

- optimizer:

  Optimizer for `lme4` mixed models.

- maxfun:

  Maximum optimizer evaluations.

- nAGQ:

  Number of adaptive Gauss-Hermite quadrature points for the binomial
  GLMM.

- empirical_logit_correction:

  Small correction added to success and failure counts for
  empirical-logit models.

- drop_missing:

  Logical. Drop rows with missing model variables.

## Value

A list containing fitted models, formulas, comparison table, fixed
effects table, settings, and status information.
