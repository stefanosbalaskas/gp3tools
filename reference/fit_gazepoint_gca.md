# Fit a Gazepoint Growth Curve Analysis mixed model

Fit a Growth Curve Analysis (GCA) mixed model to prepared pupil
time-course data. The function first attempts a random-intercept plus
random-time-slopes model and, if the model fails or is singular, falls
back to a random-intercept model.

## Usage

``` r
fit_gazepoint_gca(
  data,
  outcome_col = "gca_pupil",
  subject_col = "subject",
  condition_col = "condition",
  time_terms = NULL,
  degree = NULL,
  weights_col = "gca_weight",
  use_weights = TRUE,
  random_slopes = TRUE,
  fallback_on_singular = TRUE,
  REML = FALSE,
  optimizer = "bobyqa",
  maxfun = 2e+05,
  drop_missing = TRUE
)
```

## Arguments

- data:

  A data frame created by
  [`prepare_gazepoint_gca_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gca_data.md).

- outcome_col:

  Name of the GCA outcome column.

- subject_col:

  Name of the subject column.

- condition_col:

  Name of the condition column.

- time_terms:

  Optional character vector of polynomial time-term columns. If `NULL`,
  terms named `time_poly_1`, `time_poly_2`, ... are detected.

- degree:

  Optional number of polynomial terms to use. If supplied and
  `time_terms = NULL`, the function uses `time_poly_1` through
  `time_poly_degree`.

- weights_col:

  Optional weights column. Use `NULL` for unweighted models.

- use_weights:

  Logical. If `TRUE`, uses `weights_col` when available.

- random_slopes:

  Logical. If `TRUE`, first attempts random slopes for all polynomial
  time terms.

- fallback_on_singular:

  Logical. If `TRUE`, falls back to a random-intercept model when the
  random-slope model is singular.

- REML:

  Logical passed to
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html).

- optimizer:

  Optimizer passed to
  [`lme4::lmerControl()`](https://rdrr.io/pkg/lme4/man/lmerControl.html).

- maxfun:

  Maximum optimizer function evaluations.

- drop_missing:

  Logical. If `TRUE`, rows with missing model variables are removed
  before fitting.

## Value

A list of class `gp3_gca_model` containing the fitted model, attempted
and final formulas, model comparison information, settings, and status.
