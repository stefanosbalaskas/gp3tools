# Check model singularity

Check whether a fitted mixed model has a singular random-effects
structure.

## Usage

``` r
check_gazepoint_model_singularity(model, tolerance = 1e-04, model_name = NULL)
```

## Arguments

- model:

  A fitted model object, or a `gp3tools` fit object containing a
  `$model` element.

- tolerance:

  Numeric tolerance passed to
  [`lme4::isSingular()`](https://rdrr.io/pkg/lme4/man/isSingular.html).

- model_name:

  Optional model label used in the returned table.

## Value

A tibble with model class, singular-fit status, diagnostic status, and
message.

## Details

This helper is primarily intended for `lme4` mixed models. For model
classes where singularity is not meaningful, such as `lm`, `glm`, and
`mgcv` GAM objects, it returns a structured `not_applicable` diagnostic
row.
