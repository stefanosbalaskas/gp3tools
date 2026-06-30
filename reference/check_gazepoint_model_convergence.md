# Check model convergence

Check convergence status for fitted models used in `gp3tools` workflows.

## Usage

``` r
check_gazepoint_model_convergence(model, model_name = NULL)
```

## Arguments

- model:

  A fitted model object, or a `gp3tools` fit object containing a
  `$model` element.

- model_name:

  Optional model label used in the returned table.

## Value

A tibble with model class, convergence status, diagnostic status, and
message.

## Details

This helper supports `lme4` mixed models, `mgcv` GAM/GAMM objects, `glm`
objects, and `lm` objects where convergence is meaningful. It returns a
compact diagnostic table instead of printing model-specific messages.
