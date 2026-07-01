# Plot model residual diagnostics

Create a compact residual diagnostic plot from either a fitted model
object with [`residuals()`](https://rdrr.io/r/stats/residuals.html) and
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) methods, or a
data frame that already contains fitted values and residuals.

## Usage

``` r
plot_gazepoint_model_residuals(
  model = NULL,
  data = NULL,
  fitted_col = NULL,
  residual_col = NULL,
  type = c("residuals_fitted", "qq"),
  title = NULL
)
```

## Arguments

- model:

  Optional fitted model object.

- data:

  Optional data frame containing fitted and residual columns.

- fitted_col:

  Fitted-value column when `data` is supplied.

- residual_col:

  Residual column when `data` is supplied.

- type:

  Diagnostic plot type: residuals-versus-fitted or QQ plot.

- title:

  Optional plot title.

## Value

A ggplot object.
