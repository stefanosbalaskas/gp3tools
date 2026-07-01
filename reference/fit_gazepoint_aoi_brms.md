# Fit an optional Bayesian AOI model with brms

Prepare and optionally fit a Bayesian AOI model using brms. The
dependency is optional: the function checks for brms only when
`dry_run = FALSE`. Tests and lightweight workflows can use
`dry_run = TRUE` to inspect the formula and data without running Stan.

## Usage

``` r
fit_gazepoint_aoi_brms(
  data,
  response,
  predictors,
  subject_col = NULL,
  family = "bernoulli",
  prior = NULL,
  dry_run = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- response:

  Response column.

- predictors:

  Character vector of fixed-effect predictors.

- subject_col:

  Optional grouping column for a random intercept.

- family:

  brms family specification as a character string.

- prior:

  Optional brms prior object.

- dry_run:

  If TRUE, return the prepared call components without fitting.

- ...:

  Additional arguments passed to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) when
  fitting.

## Value

A dry-run specification list or a `brmsfit` object.
