# Check model overdispersion

Compute a Pearson-residual overdispersion diagnostic for models where
this is meaningful, especially binomial and count models.

## Usage

``` r
check_gazepoint_model_overdispersion(
  model,
  ratio_threshold = 1.2,
  model_name = NULL
)
```

## Arguments

- model:

  A fitted model object, or a `gp3tools` fit object containing a
  `$model` element.

- ratio_threshold:

  Numeric threshold above which the model is flagged as overdispersed.

- model_name:

  Optional model label used in the returned table.

## Value

A tibble with Pearson chi-square, residual degrees of freedom,
dispersion ratio, overdispersion flag, diagnostic status, and message.

## Details

This helper supports `glm`, `lme4` GLMMs, and `mgcv` GAM/GAMM objects
when their family is binomial, quasibinomial, Poisson, quasipoisson, or
negative-binomial-like. Gaussian `lm`, `lmer`, and Gaussian GAM models
return a structured `not_applicable` diagnostic row.
