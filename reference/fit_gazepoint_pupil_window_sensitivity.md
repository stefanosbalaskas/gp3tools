# Run sensitivity models for confirmatory pupil-window analyses

Run a compact set of sensitivity models for confirmatory pupil-window
analyses. Supported model families are the main linear mixed model, a
weighted linear mixed model, a fixed-effects linear model, and a
weighted fixed-effects linear model. Weighted models use the prepared
valid-sample count column as weights by default.

## Usage

``` r
fit_gazepoint_pupil_window_sensitivity(
  data,
  outcome_col = "pupil_model_outcome",
  subject_col = "pupil_model_subject",
  condition_col = "pupil_model_condition",
  window_col = "pupil_model_window",
  weights_col = "pupil_model_weight",
  model_types = c("lmm", "weighted_lmm", "lm", "weighted_lm"),
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

- model_types:

  Character vector of model types to fit. Supported values are `"lmm"`,
  `"weighted_lmm"`, `"lm"`, and `"weighted_lm"`.

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

  Logical. Include a subject random intercept for LMM model types when
  feasible.

- random_window_slopes:

  Logical. Attempt subject-level random window slopes for LMM model
  types when feasible.

- fallback_on_singular:

  Logical. If `TRUE`, LMM model types may fall back from
  random-window-slope models to random-intercept models when needed.

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
  [`fit_gazepoint_pupil_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_window_lmm.md).

## Value

A list containing fitted models, formulas, fixed effects, a comparison
table, settings, and model-status information.
