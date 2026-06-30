# Create a tidy model summary for manuscript tables

Create a compact model-summary object from common fitted models used in
`gp3tools` workflows. The function combines model metadata, fixed-effect
summaries, and optional model diagnostics into one structured object.

## Usage

``` r
tidy_gazepoint_model_summary(
  model,
  model_name = NULL,
  conf_level = 0.95,
  exponentiate = FALSE,
  drop_intercept = FALSE,
  include_diagnostics = TRUE,
  use_dharma = FALSE,
  dharma_simulations = 250,
  seed = 123
)
```

## Arguments

- model:

  A fitted model object, or a `gp3tools` fit object containing a
  `$model` element.

- model_name:

  Optional model label used in returned tables.

- conf_level:

  Confidence level for Wald confidence intervals.

- exponentiate:

  Logical. If `TRUE`, exponentiate fixed-effect estimates and confidence
  intervals.

- drop_intercept:

  Logical. If `TRUE`, remove the intercept from the fixed-effect table.

- include_diagnostics:

  Logical. If `TRUE`, include model diagnostics when supported.

- use_dharma:

  Logical. If `TRUE`, request optional DHARMa diagnostics.

- dharma_simulations:

  Number of DHARMa simulations.

- seed:

  Random seed used before DHARMa simulation.

## Value

A list with overview, model_info, fixed_effects, diagnostics, and
settings. The returned object has class `gp3_model_summary`.
