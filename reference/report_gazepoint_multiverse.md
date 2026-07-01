# Report multiverse-analysis results

Create compact branch-, status-, and term-level summaries from a
multiverse result object. This helper is intentionally generic and can
summarise the package's multiverse output after it has been tidied to
data frames.

## Usage

``` r
report_gazepoint_multiverse(
  multiverse_results,
  branch_col = NULL,
  term_col = NULL,
  estimate_col = NULL,
  p_col = NULL,
  status_col = NULL,
  alpha = 0.05
)
```

## Arguments

- multiverse_results:

  A data frame, or a list containing data frames.

- branch_col:

  Optional branch/specification column.

- term_col:

  Optional model term column.

- estimate_col:

  Optional estimate/effect column.

- p_col:

  Optional p-value column.

- status_col:

  Optional status column.

- alpha:

  Significance threshold used for descriptive counts.

## Value

A list with branch, status, and term summaries.
