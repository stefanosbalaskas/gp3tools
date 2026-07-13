# Fit an optional brms model for Gazepoint-derived data

Fits a Bayesian model using brms when brms is installed. brms is treated
as an optional external backend; gp3tools does not import or require it.

## Usage

``` r
fit_gazepoint_brms_model(
  data,
  formula,
  family = "gaussian",
  prior = NULL,
  chains = 4,
  iter = 2000,
  warmup = floor(iter/2),
  cores = 1,
  backend = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- formula:

  A model formula.

- family:

  brms family specification as a character string or brms family object.

- prior:

  Optional brms prior specification.

- chains:

  Number of MCMC chains.

- iter:

  Total iterations per chain.

- warmup:

  Warmup iterations per chain.

- cores:

  Number of cores.

- backend:

  Optional brms backend, e.g. `"cmdstanr"`.

- ...:

  Additional arguments passed to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## Value

A fitted brms model.
