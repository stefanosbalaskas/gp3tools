# Diagnose GAM and BAM models

Run a compact diagnostics bundle for `mgcv` GAM/BAM models used in
`gp3tools` workflows. The function combines convergence, basis-dimension
checks, overdispersion checks, and optional DHARMa simulation-based
residual diagnostics.

## Usage

``` r
diagnose_gazepoint_gamm(
  model,
  model_name = NULL,
  check_convergence = TRUE,
  check_basis = TRUE,
  check_overdispersion = TRUE,
  use_dharma = FALSE,
  dharma_simulations = 250,
  seed = 123
)
```

## Arguments

- model:

  A fitted GAM/BAM object, a `gp3tools` fit object containing `$model`,
  or a named list of fitted model objects.

- model_name:

  Optional model label used in returned tables.

- check_convergence:

  Logical. If `TRUE`, run convergence diagnostics.

- check_basis:

  Logical. If `TRUE`, run
  [`mgcv::k.check()`](https://rdrr.io/pkg/mgcv/man/k.check.html)
  basis-dimension diagnostics when available.

- check_overdispersion:

  Logical. If `TRUE`, run overdispersion diagnostics when meaningful for
  the model family.

- use_dharma:

  Logical. If `TRUE`, try to run optional DHARMa diagnostics.

- dharma_simulations:

  Number of DHARMa simulations.

- seed:

  Random seed used before DHARMa simulation.

## Value

A list with overview, convergence, basis, overdispersion, DHARMa
diagnostics, and settings.

## Details

The function accepts raw
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) /
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) model objects,
`gp3tools` fit objects containing a `$model` element, or a named list of
fitted model objects.
