# Gazepoint worked example: evidence verification latency

## Purpose

This article shows the thin `gp3tools` route for **time to first
source/evidence inspection**. It does not implement a second survival
engine: all event/censor construction, model fitting, diagnostics, and
reporting are delegated to `eyeprocess`.

## Build the synthetic verification dataset

``` r

raw <- eyeprocess::simulate_gaze_survival_inputs(
  "verification",
  seed = 20260918,
  n_participants = 36,
  trials_per_participant = 3
)
```

## Run the explicit adapter workflow

``` r

verification <- gp3tools::run_gazepoint_latency_analysis(
  raw$trials,
  raw$events,
  target_aoi = "source_evidence",
  formula = "condition",
  participant_col = "participant_id",
  cox_structure = "cluster_robust",
  aft_distribution = "weibull",
  km_group = "condition",
  event_type = "first_aoi_entry",
  condition_col = "condition_id",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic source/evidence AOI",
  quality_rules = list(valid_fraction_min = .90)
)
```

    ## Warning: Information criteria are not directly comparable across Cox partial
    ## likelihood, coxme penalized frailty likelihood, and AFT full likelihood, or
    ## across different analysis-row counts. Use diagnostics and estimand-specific
    ## interpretation instead of ranking by AIC/BIC.

``` r

verification$censoring
```

    ##   n_trials n_analyzable n_observed_events n_censored n_review_required
    ## 1      108          108               102          6                 0
    ##   censoring_fraction
    ## 1         0.05555556

``` r

verification$report$effects
```

    ##                term estimate_log_scale std_error hazard_ratio  conf_low
    ## 1 conditionstandard          -1.087844 0.2157481    0.3369423 0.2207549
    ##   conf_high statistic      p_value effect_measure
    ## 1 0.5142812 -5.042194 4.602255e-07   hazard_ratio

``` r

verification$ph_diagnostics
```

    ##        term rho    chisq    p_value alpha ph_flag
    ## 1 condition  NA 6.369666 0.01160874  0.05    TRUE
    ## 2    GLOBAL  NA 6.369666 0.01160874  0.05    TRUE

`cox_structure` and `aft_distribution` are required deliberately.
`gp3tools` never chooses a repeated-participant estimator or AFT family
for the analyst.

## Visualize time to evidence inspection

``` r

eyeprocess::plot_gaze_survival_curve(
  verification$data,
  group = "condition"
)
```

![Kaplan-Meier curves for time to first source/evidence AOI
entry.](gaze-survival-verification-example_files/figure-html/plot-1.png)

Kaplan-Meier curves for time to first source/evidence AOI entry.

A lower survival curve indicates that a larger share of trials has
already entered the source/evidence AOI by that time.

## Complementary verification plots

``` r

eyeprocess::plot_gaze_cumulative_incidence(
  verification$data,
  group = "condition"
)
```

![Single-event 1-KM view for evidence
inspection.](gaze-survival-verification-example_files/figure-html/incidence-1.png)

Single-event 1-KM view for evidence inspection.

``` r

eyeprocess::plot_gaze_hazard(
  verification$data,
  group = "condition"
)
```

![Empirical event/risk increments for evidence
inspection.](gaze-survival-verification-example_files/figure-html/hazard-1.png)

Empirical event/risk increments for evidence inspection.

Use the 1-KM plot only for the single target event represented here. It
is not a competing-risks cumulative-incidence estimator when mutually
exclusive event types compete. The hazard panel is an empirical
event/risk diagnostic, not a smooth latent hazard estimate.

## Interpretation

The clustered Cox hazard ratio describes the relative instantaneous
evidence-inspection rate among trials still at risk. It is not a
mean-latency ratio. The delegated Weibull AFT output instead expresses a
multiplicative event-time contrast under the named distribution.

## Troubleshooting clinic

Do not silently convert any of the following into ordinary right
censoring:

- an incomplete trial observation window;
- missing or ambiguous trial/event identity;
- gaze quality below the declared rule;
- event time after the censoring limit.

If event counts are sparse or censoring is extreme, report that
limitation even if the backend converges. If the scientific model
requires latent participant frailty, request `cox_structure = "frailty"`
explicitly and report the corresponding marginal Cox PH diagnostic
separately.

## Reporting example

> Gazepoint evidence-inspection latency was analysed through the
> vendor-neutral `eyeprocess` survival engine using `gp3tools`. Complete
> usable trials without a source/evidence entry were retained as
> right-censored observations; unresolved or poor-quality trials were
> not converted to censoring. We fitted a participant-clustered Cox
> model and a pre-specified Weibull AFT sensitivity model, reporting
> effect estimates with 95% confidence intervals together with censoring
> summaries and proportional-hazards diagnostics.

Report both package versions, the target event/AOI, time origin,
observation-window rule, quality criterion, Cox structure, AFT family,
event/censor counts, diagnostics, and sensitivity branches.

## API links

- [`gp3tools::prepare_gazepoint_survival_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/gaze-survival-adapter.md)
  — thin preparation adapter.
- [`gp3tools::run_gazepoint_latency_analysis()`](https://stefanosbalaskas.github.io/gp3tools/reference/gaze-survival-adapter.md)
  — thin end-to-end adapter.
- [`eyeprocess::plot_gaze_survival_curve()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.html)
  — vendor-neutral survival visualization.
- [`eyeprocess::report_gaze_survival_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.html)
  — vendor-neutral reporting bundle.
