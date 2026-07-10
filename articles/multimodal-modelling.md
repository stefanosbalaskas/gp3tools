# Multimodal modelling with external face-window summaries

This article demonstrates how to prepare and model multimodal tables
that combine external facial-behaviour window summaries with optional
Gazepoint-derived gaze, pupil, physiological, or response variables.

These helpers do **not** infer facial expressions from Gazepoint CSV
files. They also do **not** interpret facial behaviour as emotion. Their
purpose is narrower: to prepare transparent multimodal analysis tables
and fit explicit user-specified models.

The current scope is:

1.  join facial window summaries with optional gaze or response tables;
2.  mark predictor, outcome, and covariate columns;
3.  optionally create scaled predictor columns;
4.  fit explicit linear, generalised linear, or mixed-effects models;
5.  return the fitted model together with the prepared data and
    modelling settings.

The helpers do not make causal, diagnostic, or emotion-inference claims.

## Example face-window summaries

``` r

face_windows <- data.frame(
  participant_id = c("P001", "P002", "P003", "P004", "P005", "P006"),
  trial_id = c(1, 1, 1, 1, 1, 1),
  face_window_label = "response",
  AU04_r_mean = c(0.05, 0.08, 0.10, 0.12, 0.14, 0.15),
  AU12_r_mean = c(0.20, 0.22, 0.25, 0.28, 0.30, 0.32),
  face_confidence_mean = c(0.95, 0.94, 0.95, 0.93, 0.94, 0.92),
  stringsAsFactors = FALSE
)
```

## Optional Gazepoint-derived summaries

The gaze table may contain AOI dwell time, fixation counts, pupil
summaries, GSR summaries, HR/IBI summaries, or other already-computed
Gazepoint-derived variables.

``` r

gaze_summary <- data.frame(
  participant_id = c("P001", "P002", "P003", "P004", "P005", "P006"),
  trial_id = c(1, 1, 1, 1, 1, 1),
  claim_dwell_time = c(1.10, 1.25, 1.30, 1.45, 1.50, 1.65),
  pupil_mean = c(3.10, 3.15, 3.20, 3.25, 3.30, 3.35),
  stringsAsFactors = FALSE
)
```

## Optional response table

``` r

responses <- data.frame(
  participant_id = c("P001", "P002", "P003", "P004", "P005", "P006"),
  trial_id = c(1, 1, 1, 1, 1, 1),
  rating = c(3, 3.5, 4, 4.5, 5, 5.5),
  choice = c(0, 0, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)
```

## Prepare multimodal data

[`prepare_gazepoint_multimodal_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_multimodal_data.md)
joins the supplied tables and optionally creates scaled predictor
columns.

``` r

multimodal <- prepare_gazepoint_multimodal_data(
  face_windows = face_windows,
  gaze_data = gaze_summary,
  response_data = responses,
  by = c("participant_id", "trial_id"),
  predictor_cols = c(
    "AU04_r_mean",
    "AU12_r_mean",
    "claim_dwell_time",
    "pupil_mean"
  ),
  outcome_cols = c("rating", "choice"),
  scale_predictors = TRUE
)

multimodal
#> # A tibble: 6 × 14
#>   participant_id trial_id face_window_label AU04_r_mean AU12_r_mean
#>   <chr>             <dbl> <chr>                   <dbl>       <dbl>
#> 1 P001                  1 response                 0.05        0.2 
#> 2 P002                  1 response                 0.08        0.22
#> 3 P003                  1 response                 0.1         0.25
#> 4 P004                  1 response                 0.12        0.28
#> 5 P005                  1 response                 0.14        0.3 
#> 6 P006                  1 response                 0.15        0.32
#> # ℹ 9 more variables: face_confidence_mean <dbl>, claim_dwell_time <dbl>,
#> #   pupil_mean <dbl>, rating <dbl>, choice <dbl>, AU04_r_mean_z <dbl>,
#> #   AU12_r_mean_z <dbl>, claim_dwell_time_z <dbl>, pupil_mean_z <dbl>
```

The scaling metadata are stored as an attribute.

``` r

attr(multimodal, "gp3_multimodal_scaling")
#> # A tibble: 4 × 4
#>   predictor        scaled_column      center  scale
#>   <chr>            <chr>               <dbl>  <dbl>
#> 1 AU04_r_mean      AU04_r_mean_z       0.107 0.0378
#> 2 AU12_r_mean      AU12_r_mean_z       0.262 0.0467
#> 3 claim_dwell_time claim_dwell_time_z  1.38  0.197 
#> 4 pupil_mean       pupil_mean_z        3.22  0.0935
```

The modelling settings are also retained.

``` r

attr(multimodal, "gp3_multimodal_settings")
#> $by
#> [1] "participant_id" "trial_id"      
#> 
#> $gaze_by
#> NULL
#> 
#> $response_by
#> NULL
#> 
#> $predictor_cols
#> [1] "AU04_r_mean"      "AU12_r_mean"      "claim_dwell_time" "pupil_mean"      
#> 
#> $outcome_cols
#> [1] "rating" "choice"
#> 
#> $covariate_cols
#> NULL
#> 
#> $scale_predictors
#> [1] TRUE
#> 
#> $scaled_suffix
#> [1] "_z"
#> 
#> $drop_missing_outcomes
#> [1] FALSE
#> 
#> $keep_all
#> [1] TRUE
```

## Fit a face-window model

[`fit_gazepoint_face_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_face_window_lmm.md)
fits an explicit model to a face-window or multimodal table. If
`random_effects` is omitted, the helper uses
[`stats::lm()`](https://rdrr.io/r/stats/lm.html).

``` r

face_fit <- fit_gazepoint_face_window_lmm(
  data = multimodal,
  outcome = "rating",
  predictors = c("AU04_r_mean_z", "AU12_r_mean_z")
)

face_fit$formula
#> rating ~ AU04_r_mean_z + AU12_r_mean_z
#> <environment: 0x560bef2305b0>
```

``` r

summary(face_fit$model)
#> 
#> Call:
#> stats::lm(formula = form, data = analysis_data, na.action = na_fun)
#> 
#> Residuals:
#>         1         2         3         4         5         6 
#>  0.009169  0.057457 -0.026895 -0.111247 -0.025672  0.097188 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    4.25000    0.03843 110.579 1.63e-06 ***
#> AU04_r_mean_z  0.14083    0.31771   0.443   0.6876    
#> AU12_r_mean_z  0.79279    0.31771   2.495   0.0881 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.09414 on 3 degrees of freedom
#> Multiple R-squared:  0.9939, Adjusted R-squared:  0.9899 
#> F-statistic: 245.3 on 2 and 3 DF,  p-value: 0.0004738
```

When `random_effects` is supplied, the helper uses
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) if `lme4` is
installed. This article uses a simple linear model so that the example
remains lightweight.

## Fit a multimodal response model

[`fit_gazepoint_multimodal_response_model()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_multimodal_response_model.md)
can include face-window variables together with gaze, pupil,
physiological, or response predictors.

``` r

multi_fit <- fit_gazepoint_multimodal_response_model(
  data = multimodal,
  outcome = "rating",
  predictors = c(
    "AU04_r_mean_z",
    "AU12_r_mean_z",
    "claim_dwell_time_z",
    "pupil_mean_z"
  )
)

multi_fit$formula
#> rating ~ AU04_r_mean_z + AU12_r_mean_z + claim_dwell_time_z + 
#>     pupil_mean_z
#> <environment: 0x560bf00b3468>
```

``` r

summary(multi_fit$model)
#> 
#> Call:
#> stats::lm(formula = form, data = analysis_data, na.action = na_fun)
#> 
#> Residuals:
#>          1          2          3          4          5          6 
#>  5.776e-16  5.776e-16 -2.310e-15 -4.981e-29  1.733e-15 -5.776e-16 
#> 
#> Coefficients:
#>                      Estimate Std. Error    t value Pr(>|t|)    
#> (Intercept)         4.250e+00  1.248e-15  3.406e+15  < 2e-16 ***
#> AU04_r_mean_z      -1.292e-15  1.069e-14 -1.210e-01    0.923    
#> AU12_r_mean_z       1.048e-15  1.867e-14  5.600e-02    0.964    
#> claim_dwell_time_z -2.083e-15  1.060e-14 -1.960e-01    0.877    
#> pupil_mean_z        9.354e-01  2.256e-14  4.147e+13 1.54e-14 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.056e-15 on 1 degrees of freedom
#> Multiple R-squared:      1,  Adjusted R-squared:      1 
#> F-statistic: 1.171e+29 on 4 and 1 DF,  p-value: 2.192e-15
```

## Generalised response model

When a `family` is supplied, the helper uses a generalised linear model
unless random effects are requested.

``` r

choice_fit <- fit_gazepoint_multimodal_response_model(
  data = multimodal,
  outcome = "choice",
  predictors = c("AU12_r_mean_z", "claim_dwell_time_z"),
  family = stats::binomial()
)

choice_fit$formula
#> choice ~ AU12_r_mean_z + claim_dwell_time_z
#> <environment: 0x560bf09c3240>
```

``` r

summary(choice_fit$model)
#> 
#> Call:
#> stats::glm(formula = form, family = family, data = analysis_data, 
#>     na.action = na_fun)
#> 
#> Coefficients:
#>                      Estimate Std. Error z value Pr(>|z|)
#> (Intercept)         9.892e+00  2.022e+05       0        1
#> AU12_r_mean_z       2.057e+02  1.569e+06       0        1
#> claim_dwell_time_z -1.705e+02  1.601e+06       0        1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 7.6382e+00  on 5  degrees of freedom
#> Residual deviance: 2.4495e-10  on 3  degrees of freedom
#> AIC: 6
#> 
#> Number of Fisher Scoring iterations: 24
```

## Recommended interpretation

The fitted models are statistical summaries of associations among
observed or derived variables. They should be interpreted in relation to
the study design, measurement quality, synchronisation quality, and
preregistered or theoretically justified hypotheses.

Prefer cautious language such as:

- facial-behaviour window predictor;
- multimodal association;
- response model;
- adjusted association;
- model-estimated relationship;
- exploratory multimodal feature.

Avoid unsupported language such as:

- true emotion detection;
- hidden affect;
- causal proof from observational predictors;
- psychological diagnosis;
- emotional state inferred directly from an algorithmic label;
- definitive mechanism without experimental or longitudinal support.

## Suggested workflow position

A transparent workflow is:

1.  import external face-analysis CSVs with
    [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md);
2.  standardise face-analysis columns with
    [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md);
3.  audit face-data quality with
    [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md);
4.  synchronise face data with Gazepoint rows using
    [`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md);
5.  audit synchronisation quality with
    [`audit_gazepoint_face_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_sync.md);
6.  summarise facial-behaviour variables within analysis windows with
    [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md);
7.  compute descriptive baseline-to-response changes with
    [`summarize_gazepoint_face_reactivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_reactivity.md);
8.  prepare multimodal analysis tables with
    [`prepare_gazepoint_multimodal_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_multimodal_data.md);
9.  fit explicit models with
    [`fit_gazepoint_face_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_face_window_lmm.md)
    or
    [`fit_gazepoint_multimodal_response_model()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_multimodal_response_model.md);
10. report model results together with quality, synchronisation, and
    window-summary diagnostics.
