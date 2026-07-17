# Check gp3tools performance results against regression limits

Applies explicit absolute limits and, optionally, compares matched rows
with a saved baseline. Scaling exponents are estimated from median
elapsed times across row-count levels.

## Usage

``` r
check_gazepoint_performance_regression(
  x,
  limits = gp3tools_performance_limits(),
  baseline = NULL,
  elapsed_ratio_limit = 1.5,
  memory_ratio_limit = 1.5
)
```

## Arguments

- x:

  A benchmark object or summary data frame.

- limits:

  Explicit operation-level limits.

- baseline:

  Optional prior benchmark object or summary data frame.

- elapsed_ratio_limit:

  Maximum allowed elapsed-time ratio to baseline.

- memory_ratio_limit:

  Maximum allowed heap-growth ratio to baseline.

## Value

A `"gazepoint_performance_regression"` object.
