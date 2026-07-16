# Fit a facial-behaviour window mixed or linear model

Fits an explicit model to a face-window or multimodal analysis table. If
`random_effects` is supplied, the model is fitted with
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html); otherwise
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) is used. The helper is
a modelling convenience wrapper and does not interpret facial-behaviour
variables as emotions.

## Usage

``` r
fit_gazepoint_face_window_lmm(
  data,
  outcome,
  predictors,
  covariates = NULL,
  random_effects = NULL,
  na_action = c("na.omit", "na.exclude"),
  REML = FALSE
)
```

## Arguments

- data:

  A face-window or multimodal data frame.

- outcome:

  Outcome column name.

- predictors:

  Character vector of fixed-effect predictor columns.

- covariates:

  Optional character vector of covariate columns.

- random_effects:

  Optional random-effects formula component, for example
  `"(1 | participant_id)"`.

- na_action:

  Missing-data handling. One of `"na.omit"` or `"na.exclude"`.

- REML:

  Passed to [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)
  when a mixed model is fitted.

## Value

A list with model, formula, data, and settings. The object has class
`gp3_face_window_lmm`.

## Examples

``` r
dat <- data.frame(
  participant_id = c("P001", "P002", "P003"),
  AU12_r_mean = c(0.1, 0.2, 0.3),
  rating = c(3, 4, 5)
)

fit_gazepoint_face_window_lmm(
  dat,
  outcome = "rating",
  predictors = "AU12_r_mean"
)
#> $model
#> 
#> Call:
#> stats::lm(formula = form, data = analysis_data, na.action = na_fun)
#> 
#> Coefficients:
#> (Intercept)  AU12_r_mean  
#>           2           10  
#> 
#> 
#> $formula
#> rating ~ AU12_r_mean
#> <environment: 0x55a3829932e8>
#> 
#> $data
#> # A tibble: 3 × 3
#>   participant_id AU12_r_mean rating
#>   <chr>                <dbl>  <dbl>
#> 1 P001                   0.1      3
#> 2 P002                   0.2      4
#> 3 P003                   0.3      5
#> 
#> $settings
#> $settings$model_label
#> [1] "face_window_lmm"
#> 
#> $settings$outcome
#> [1] "rating"
#> 
#> $settings$predictors
#> [1] "AU12_r_mean"
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
#> [1] "gp3_face_window_lmm"  "gp3_multimodal_model" "list"                
```
