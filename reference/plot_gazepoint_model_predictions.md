# Plot observed summaries and model-implied predictions

Create a reporting plot that overlays observed outcome summaries with
fitted/model-predicted trajectories. The helper is intentionally generic
and can be used with linear models, GLMs, mixed models, GAMMs, GCA-style
models, AOI GLMMs, and pupil LMMs when the fitted object supports
[`predict()`](https://rdrr.io/r/stats/predict.html).

## Usage

``` r
plot_gazepoint_model_predictions(
  data,
  model = NULL,
  x_col,
  outcome_col,
  condition_col = NULL,
  group_cols = NULL,
  facet_cols = NULL,
  newdata = NULL,
  prediction_type = c("response", "link"),
  include_random_effects = FALSE,
  observed_summary_function = c("mean", "median"),
  ci = 0.95,
  show_observed = TRUE,
  show_observed_ci = TRUE,
  show_predictions = TRUE,
  show_prediction_ci = TRUE,
  point_alpha = 0.55,
  line_width = 1,
  name = "gazepoint_model_predictions"
)
```

## Arguments

- data:

  A data frame containing the observed data.

- model:

  Optional fitted model object. If supplied, predictions are computed
  using [`predict()`](https://rdrr.io/r/stats/predict.html).

- x_col:

  Column used on the x-axis, usually time or time bin.

- outcome_col:

  Observed outcome column.

- condition_col:

  Optional condition column used for colour/grouping.

- group_cols:

  Optional additional grouping columns for observed and predicted
  trajectories.

- facet_cols:

  Optional columns used for faceting.

- newdata:

  Optional prediction grid. If `NULL`, predictions are computed on
  `data` and then summarised by x/group/facet.

- prediction_type:

  Prediction scale passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html). Common values are
  `"response"` and `"link"`.

- include_random_effects:

  Logical. For `lme4` mixed models, `FALSE` requests population-level
  predictions via `re.form = NA`; `TRUE` includes conditional random
  effects where possible.

- observed_summary_function:

  Summary for observed outcomes. Options are `"mean"` and `"median"`.

- ci:

  Confidence level for observed and prediction intervals when standard
  errors are available.

- show_observed:

  Logical. Plot observed summaries.

- show_observed_ci:

  Logical. Plot observed normal-approximation intervals.

- show_predictions:

  Logical. Plot model predictions when `model` is supplied.

- show_prediction_ci:

  Logical. Plot prediction intervals when standard errors are available
  from [`predict()`](https://rdrr.io/r/stats/predict.html).

- point_alpha:

  Alpha value for observed points.

- line_width:

  Line width for prediction trajectories.

- name:

  Character label stored in plot attributes.

## Value

A `ggplot` object with attributes containing the observed summary,
prediction summary, overview, and settings.
