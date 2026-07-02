# Guardrail for cluster-permutation ANOVA

This function intentionally fails safely. Cluster-permutation ANOVA is
not implemented as an active inferential engine in gp3tools because it
requires additional design, exchangeability, and error-term choices that
are outside the currently validated two-condition workflow.

## Usage

``` r
run_gazepoint_cluster_permutation_anova(...)
```

## Arguments

- ...:

  Arguments reserved for a future implementation.

## Value

This function always stops with an explanatory error.
