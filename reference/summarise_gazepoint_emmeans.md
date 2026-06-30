# Summarise estimated marginal means and contrasts

Create manuscript-ready estimated marginal means and pairwise contrast
tables from fitted models used in `gp3tools` workflows.

## Usage

``` r
summarise_gazepoint_emmeans(
  model,
  specs,
  by = NULL,
  model_name = NULL,
  type = "response",
  contrast_method = "pairwise",
  adjust = "tukey",
  conf_level = 0.95,
  include_contrasts = TRUE
)
```

## Arguments

- model:

  A fitted model object, or a `gp3tools` fit object containing a
  `$model` element.

- specs:

  Character vector or formula passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

- by:

  Optional character vector of grouping variables passed to
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html).

- model_name:

  Optional model label used in returned tables.

- type:

  Scale passed to `emmeans` summaries. Common values are `"link"` and
  `"response"`.

- contrast_method:

  Contrast method passed to
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- adjust:

  Multiplicity adjustment for contrasts.

- conf_level:

  Confidence level.

- include_contrasts:

  Logical. If `TRUE`, compute contrasts.

## Value

A list with overview, emmeans, contrasts, and settings. The returned
object has class `gp3_emmeans_summary`.

## Details

The function uses the optional `emmeans` package. If `emmeans` is not
installed, the function returns structured skipped tables rather than
failing. This keeps `emmeans` as an optional suggested dependency.
