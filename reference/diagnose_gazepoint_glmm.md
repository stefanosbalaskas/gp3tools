# Diagnose GLMM, LMM, and GLM models

Run a compact diagnostics bundle for model objects used in `gp3tools`
workflows. The function combines convergence, singularity,
overdispersion, and optional DHARMa simulation-based residual
diagnostics.

## Usage

``` r
diagnose_gazepoint_glmm(
  model,
  model_name = NULL,
  check_convergence = TRUE,
  check_singularity = TRUE,
  check_overdispersion = TRUE,
  use_dharma = TRUE,
  dharma_simulations = 250,
  seed = 123
)
```

## Arguments

- model:

  A fitted model object, a `gp3tools` fit object containing `$model`, or
  a named list of fitted model objects.

- model_name:

  Optional model label used in returned tables.

- check_convergence:

  Logical. If `TRUE`, run convergence diagnostics.

- check_singularity:

  Logical. If `TRUE`, run singularity diagnostics.

- check_overdispersion:

  Logical. If `TRUE`, run overdispersion diagnostics.

- use_dharma:

  Logical. If `TRUE`, try to run optional DHARMa diagnostics.

- dharma_simulations:

  Number of DHARMa simulations.

- seed:

  Random seed used before DHARMa simulation.

## Value

A list with overview, convergence, singularity, overdispersion, DHARMa
diagnostics, and settings.

## Details

The function accepts raw fitted models, `gp3tools` fit objects
containing a `$model` element, or a named list of fitted models. DHARMa
diagnostics are optional and are skipped cleanly when DHARMa is not
installed.
