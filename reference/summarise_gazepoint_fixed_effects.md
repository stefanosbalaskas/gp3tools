# Summarise fixed effects from fitted models

Create a compact manuscript-ready fixed-effect summary table from common
models used in `gp3tools` workflows.

## Usage

``` r
summarise_gazepoint_fixed_effects(
  model,
  model_name = NULL,
  conf_level = 0.95,
  exponentiate = FALSE,
  drop_intercept = FALSE
)
```

## Arguments

- model:

  A fitted model object, or a `gp3tools` fit object containing a
  `$model` element.

- model_name:

  Optional model label used in the returned table.

- conf_level:

  Confidence level for Wald confidence intervals.

- exponentiate:

  Logical. If `TRUE`, exponentiate estimates and confidence intervals.
  This is useful for logistic models when reporting odds ratios.

- drop_intercept:

  Logical. If `TRUE`, remove the intercept row.

## Value

A tibble with fixed-effect estimates, standard errors, test statistics,
p-values when available, confidence intervals, significance stars, and
status fields.

## Details

The function supports `lm`, `glm`, `lme4` mixed models, and `mgcv`
GAM/BAM objects. It can also accept a `gp3tools` fit object containing a
`$model` element. Confidence intervals are computed using a Wald
approximation from the estimate and standard error so that the function
remains lightweight and fast for mixed models.
