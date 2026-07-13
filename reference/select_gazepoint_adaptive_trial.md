# Select the next adaptive trial from candidate stimuli

Provides a lightweight Bayesian-optimization-style acquisition helper
for adaptive testing. It assumes candidate-level posterior means and
standard deviations are already available or supplied by the user.

## Usage

``` r
select_gazepoint_adaptive_trial(
  candidates,
  mean,
  sd,
  acquisition = c("ucb", "uncertainty", "expected_improvement"),
  kappa = 2,
  best_observed = NULL,
  maximize = TRUE
)
```

## Arguments

- candidates:

  A data frame of candidate stimuli/trials.

- mean:

  Column containing posterior mean utility or expected information.

- sd:

  Column containing posterior uncertainty.

- acquisition:

  Acquisition rule: `"ucb"`, `"uncertainty"`, or
  `"expected_improvement"`.

- kappa:

  Exploration weight for UCB.

- best_observed:

  Best observed value for expected improvement.

- maximize:

  Logical; select maximum acquisition value if `TRUE`.

## Value

One-row data frame corresponding to the selected candidate, with an
added acquisition score.
