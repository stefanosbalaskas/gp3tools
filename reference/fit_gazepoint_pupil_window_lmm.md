# Fit confirmatory pupil-window linear mixed models

Fit the main confirmatory trial/window-level pupil model from data
prepared with
[`prepare_gazepoint_pupil_window_model_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_window_model_data.md).
The default model is a linear mixed model with pupil outcome as the
continuous dependent variable, condition and/or window fixed effects
when available, and a subject random intercept when feasible.

## Usage

``` r
fit_gazepoint_pupil_window_lmm(
  data,
  formula = NULL,
  outcome_col = "pupil_model_outcome",
  subject_col = "pupil_model_subject",
  condition_col = "pupil_model_condition",
  window_col = "pupil_model_window",
  weights_col = "pupil_model_weight",
  use_weights = FALSE,
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE,
  random_intercept = TRUE,
  random_window_slopes = FALSE,
  fallback_on_singular = TRUE,
  REML = FALSE,
  optimizer = "bobyqa",
  maxfun = 2e+05,
  drop_missing = TRUE,
  ...
)
```

## Arguments

- data:

  Pupil-window model data, usually produced by
  [`prepare_gazepoint_pupil_window_model_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_window_model_data.md).

- formula:

  Optional model formula. If `NULL`, a formula is constructed
  automatically.

- outcome_col:

  Outcome column.

- subject_col:

  Subject column.

- condition_col:

  Condition column.

- window_col:

  Window column.

- weights_col:

  Optional weights column.

- use_weights:

  Logical. If `TRUE`, use `weights_col` as model weights.

- include_condition:

  Logical. Include condition fixed effects when more than one condition
  level is available.

- include_window:

  Logical. Include window fixed effects when more than one window level
  is available.

- include_interaction:

  Logical. Include the condition-by-window interaction when both
  condition and window are used.

- random_intercept:

  Logical. Include a subject random intercept when feasible.

- random_window_slopes:

  Logical. Attempt subject-level random window slopes when feasible.

- fallback_on_singular:

  Logical. If `TRUE`, fall back from a random-slope model to a
  random-intercept model when the attempted model is singular or fails.

- REML:

  Logical. Passed to
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html).

- optimizer:

  Optimizer passed to
  [`lme4::lmerControl()`](https://rdrr.io/pkg/lme4/man/lmerControl.html).

- maxfun:

  Maximum optimizer iterations passed to
  [`lme4::lmerControl()`](https://rdrr.io/pkg/lme4/man/lmerControl.html).

- drop_missing:

  Logical. If `TRUE`, rows with missing or non-finite model inputs are
  removed before fitting.

- ...:

  Additional arguments passed to
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

## Value

A list containing the fitted model, formula, attempted model, fallback
information, fixed effects, comparison table, settings, and model
diagnostics.
