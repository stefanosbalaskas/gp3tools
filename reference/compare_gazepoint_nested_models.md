# Compare nested Gazepoint models

Compare a sequence of nested models, such as null, main-effect, time,
condition, and interaction models. The helper returns model-level fit
indices, likelihood-ratio comparisons, ranking information, and
extraction statuses.

## Usage

``` r
compare_gazepoint_nested_models(
  models,
  model_names = NULL,
  comparison = c("sequential", "against_first"),
  name = "gazepoint_nested_model_comparison"
)
```

## Arguments

- models:

  A list of fitted model objects.

- model_names:

  Optional character vector of model names. If `NULL`, names are taken
  from `models` or generated as `model_1`, `model_2`, etc.

- comparison:

  Comparison strategy. `"sequential"` compares each model with the
  previous model. `"against_first"` compares each model with the first
  model.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_nested_model_comparison`.

## Details

This helper is useful for GCA, confirmatory LMM/GLMM workflows, AOI
GLMMs, pupil LMMs, and other fitted model workflows where reviewers
expect explicit model-comparison evidence.
