# Fit a multimodal response model

Fits an explicit response model using multimodal predictors. Linear
models, generalised linear models, and mixed-effects models are
supported depending on `family` and `random_effects`. The helper
prepares and fits the model only; it does not make causal, diagnostic,
or emotion-inference claims.

## Usage

``` r
fit_gazepoint_multimodal_response_model(
  data,
  outcome,
  predictors,
  covariates = NULL,
  random_effects = NULL,
  family = NULL,
  na_action = c("na.omit", "na.exclude"),
  REML = FALSE,
  ...
)
```

## Arguments

- data:

  A multimodal analysis data frame, usually returned by
  [`prepare_gazepoint_multimodal_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_multimodal_data.md).

- outcome:

  Outcome column name.

- predictors:

  Character vector of fixed-effect predictor columns.

- covariates:

  Optional character vector of covariate columns.

- random_effects:

  Optional random-effects formula component, for example
  `"(1 | participant_id)"`.

- family:

  Optional model family. If `NULL`,
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) is used. If
  supplied, [`stats::glm()`](https://rdrr.io/r/stats/glm.html) or
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) is used.

- na_action:

  Missing-data handling. One of `"na.omit"` or `"na.exclude"`.

- REML:

  Passed to [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)
  when a linear mixed model is fitted.

- ...:

  Additional arguments passed to the model-fitting function.

## Value

A list with model, formula, data, and settings. The object has class
`gp3_multimodal_response_model`.

## Examples

``` r
dat <- data.frame(
  participant_id = c("P001", "P002", "P003"),
  AU12_r_mean = c(0.1, 0.2, 0.3),
  dwell_time = c(1.2, 1.4, 1.1),
  rating = c(3, 4, 5)
)

fit_gazepoint_multimodal_response_model(
  dat,
  outcome = "rating",
  predictors = c("AU12_r_mean", "dwell_time")
)
#> $model
#> 
#> Call:
#> stats::lm(formula = form, data = analysis_data, na.action = na_fun)
#> 
#> Coefficients:
#> (Intercept)  AU12_r_mean   dwell_time  
#>   2.000e+00    1.000e+01   -1.088e-15  
#> 
#> 
#> $formula
#> rating ~ AU12_r_mean + dwell_time
#> <environment: 0x55b5fb212078>
#> 
#> $data
#> # A tibble: 3 × 4
#>   participant_id AU12_r_mean dwell_time rating
#>   <chr>                <dbl>      <dbl>  <dbl>
#> 1 P001                   0.1        1.2      3
#> 2 P002                   0.2        1.4      4
#> 3 P003                   0.3        1.1      5
#> 
#> $settings
#> $settings$model_label
#> [1] "multimodal_response_model"
#> 
#> $settings$outcome
#> [1] "rating"
#> 
#> $settings$predictors
#> [1] "AU12_r_mean" "dwell_time" 
#> 
#> $settings$covariates
#> NULL
#> 
#> $settings$random_effects
#> NULL
#> 
#> $settings$family
#> NULL
#> 
#> $settings$na_action
#> [1] "na.omit"
#> 
#> $settings$REML
#> [1] FALSE
#> 
#> $settings$n_rows_input
#> [1] 3
#> 
#> $settings$n_rows_model
#> [1] 3
#> 
#> 
#> attr(,"class")
#> [1] "gp3_multimodal_response_model" "gp3_multimodal_model"         
#> [3] "list"                         
```
