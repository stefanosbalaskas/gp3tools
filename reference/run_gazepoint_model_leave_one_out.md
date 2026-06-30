# Run leave-one-unit model sensitivity analysis

Refit the same model repeatedly while removing one participant, item,
stimulus, trial, or other analysis unit at a time. The helper compares
leave-one-out estimates with the full-data model to assess whether a key
effect is driven by a single unit.

## Usage

``` r
run_gazepoint_model_leave_one_out(
  data,
  unit_col,
  fit_function,
  extract_function = NULL,
  effect_terms = NULL,
  min_rows = 2L,
  keep_models = FALSE,
  name = "gazepoint_model_leave_one_out"
)
```

## Arguments

- data:

  A data frame used for model fitting.

- unit_col:

  Column identifying the unit to leave out, for example subject,
  participant, item, stimulus, or trial.

- fit_function:

  Function that takes one data frame argument and returns a fitted
  model.

- extract_function:

  Optional function that takes a fitted model and returns a data frame
  of effects. If `NULL`, a default coefficient extractor is used for
  common model objects.

- effect_terms:

  Optional character vector of terms/effects to retain in the
  sensitivity summary.

- min_rows:

  Minimum number of rows required after leaving one unit out.

- keep_models:

  Logical. If `TRUE`, keep the full model and refitted models.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_model_leave_one_out_sensitivity`.

## Details

This is a generic robustness wrapper. It can be used with linear models,
GLMs, mixed models, GAMMs, GCA models, AOI GLMMs, pupil LMMs, or any
custom model as long as a fitting function is supplied.
