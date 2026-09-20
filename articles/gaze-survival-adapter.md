# Gazepoint gaze-latency survival analysis

## Why this is an adapter, not another survival engine

gp3tools does not duplicate survival-analysis science.
gp3tools::prepare_gazepoint_survival_data() and
gp3tools::run_gazepoint_latency_analysis() translate a
Gazepoint-oriented workflow into the vendor-neutral eyeprocess survival
API. Event/censor construction, validation, Cox/AFT fitting,
diagnostics, provenance, and reporting remain in eyeprocess.

Use this route only after Gazepoint exports have been represented as
explicit trial observation windows and fixation/AOI-visit events. A
missing Gazepoint field is not evidence of censoring. A target that is
never inspected during a complete usable trial can be right-censored;
incomplete or unusable gaze remains a review state.

## When to use it

Use censored latency when the scientific outcome is time to first target
fixation, first AOI entry, first evidence inspection, first revisit,
first transition into a target AOI, or a pre-defined disengagement
event, and when some valid trials end before that event occurs.

Do not use this workflow for ordinary fixation-duration summaries, for
trials whose observation window cannot be established, or to turn
missing/unusable gaze into an artificial never-inspected outcome.

## Censoring decision table

| Gazepoint trial state                             | Survival treatment      |
|:--------------------------------------------------|:------------------------|
| Target event observed in a complete usable window | Event                   |
| Target absent when a complete usable window ends  | Right censored          |
| Missing/incomplete observation window             | Review required         |
| Gaze quality below the declared rule              | Review/exclusion branch |
| Ambiguous trial/event identity                    | Stop with an error      |

The adapter never guesses a censoring state from a missing value alone.

## Explicit estimator choices

The convenience workflow intentionally has no default model family:

``` r

gp3tools::run_gazepoint_latency_analysis(
  trials,
  events,
  target_aoi = "disclosure",
  formula = "condition",
  cox_structure = "cluster_robust",  # or "frailty"
  aft_distribution = "weibull"      # or "lognormal"
)
```

cox_structure = “cluster_robust” asks eyeprocess for a marginal Cox
model with participant-clustered uncertainty. cox_structure = “frailty”
asks for the distinct Gaussian participant-frailty model implemented by
the specialist coxme backend. The wrapper never substitutes one for the
other.

## Fully runnable synthetic example

``` r

raw <- eyeprocess::simulate_gaze_survival_inputs(
  "disclosure",
  seed = 20260918,
  n_participants = 36,
  trials_per_participant = 3
)

result <- gp3tools::run_gazepoint_latency_analysis(
  raw$trials,
  raw$events,
  target_aoi = "disclosure",
  formula = "condition",
  participant_col = "participant_id",
  cox_structure = "cluster_robust",
  aft_distribution = "weibull",
  km_group = "condition",
  event_type = "first_fixation",
  condition_col = "condition_id",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic disclosure AOI",
  quality_rules = list(valid_fraction_min = .90)
)
```

    ## Warning: Information criteria are not directly comparable across Cox partial
    ## likelihood, coxme penalized frailty likelihood, and AFT full likelihood, or
    ## across different analysis-row counts. Use diagnostics and estimand-specific
    ## interpretation instead of ranking by AIC/BIC.

``` r

result$censoring
```

    ##   n_trials n_analyzable n_observed_events n_censored n_review_required
    ## 1      108          108                92         16                 0
    ##   censoring_fraction
    ## 1          0.1481481

``` r

result$report$effects
```

    ##                           term estimate_log_scale std_error hazard_ratio
    ## 1 conditiondetailed_disclosure           1.377500 0.2344998     3.964979
    ## 2  conditionminimal_disclosure           1.231458 0.2426759     3.426220
    ##   conf_low conf_high statistic      p_value effect_measure
    ## 1 2.504000  6.278378  5.874207 4.248714e-09   hazard_ratio
    ## 2 2.129361  5.512916  5.074495 3.885256e-07   hazard_ratio

``` r

result$ph_diagnostics
```

    ##        term rho    chisq   p_value alpha ph_flag
    ## 1 condition  NA 2.636062 0.2676618  0.05   FALSE
    ## 2    GLOBAL  NA 2.636062 0.2676618  0.05   FALSE

## Visual check

A Kaplan–Meier curve keeps every usable trial in the risk set until the
target event occurs or the trial ends. The line style identifies the
condition without relying on colour alone.

``` r

eyeprocess::plot_gaze_survival_curve(result$data, group = "condition")
```

![Kaplan–Meier curves for time to first disclosure fixation in the
synthetic Gazepoint adapter
example.](gaze-survival-adapter_files/figure-html/survival-plot-1.png)

Kaplan–Meier curves for time to first disclosure fixation in the
synthetic Gazepoint adapter example.

## Complementary survival views

``` r

eyeprocess::plot_gaze_cumulative_incidence(
  result$data,
  group = "condition"
)
```

![Single-event 1-KM view of the synthetic disclosure-inspection
process.](gaze-survival-adapter_files/figure-html/survival-incidence-plot-1.png)

Single-event 1-KM view of the synthetic disclosure-inspection process.

``` r

eyeprocess::plot_gaze_hazard(
  result$data,
  group = "condition"
)
```

![Empirical event/risk increments in the synthetic disclosure-inspection
process.](gaze-survival-adapter_files/figure-html/survival-hazard-plot-1.png)

Empirical event/risk increments in the synthetic disclosure-inspection
process.

The first plot is simply `1 - KM` for one target event; it is not a
competing-risks cumulative-incidence estimator. The second is a
descriptive event/risk increment view rather than a smoothed continuous
hazard estimate.

The output retains never-inspected usable trials instead of deleting
them.
result$`specification records the requested repeated-Cox structure and AFT family; result`$data
retains the eyeprocess provenance fields describing detector, AOI,
preprocessing, quality rules, and software version.

## Evidence-verification variant

The same adapter can analyse time to first source/evidence inspection
without introducing any Gazepoint-specific estimator logic:

``` r

verify_raw <- eyeprocess::simulate_gaze_survival_inputs(
  "verification",
  seed = 20260918,
  n_participants = 24,
  trials_per_participant = 3
)

verification <- gp3tools::run_gazepoint_latency_analysis(
  verify_raw$trials,
  verify_raw$events,
  target_aoi = "source_evidence",
  formula = "condition",
  participant_col = "participant_id",
  cox_structure = "cluster_robust",
  aft_distribution = "weibull",
  km_group = "condition",
  event_type = "first_aoi_entry",
  condition_col = "condition_id",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic source/evidence AOI"
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
    ## 1       72           72                68          4                 0
    ##   censoring_fraction
    ## 1         0.05555556

The scientific interpretation remains a censored time-to-event analysis:
complete trials without an evidence entry remain in the risk set until
their observation window ends.

## Interpretation

A Cox hazard ratio describes the instantaneous target-event rate among
trials still at risk. It is not a ratio of mean TTFF. An AFT time ratio
describes multiplicative event time under the named parametric family.
Cox partial-likelihood and AFT full-likelihood AIC/BIC values are not
automatically comparable; the eyeprocess comparison helper flags this
boundary.

For frailty Cox models, survival::cox.zph() is not applied to the coxme
object. The wrapper therefore returns an explicit diagnostic-status row
and directs the analyst to report the corresponding marginal Cox PH
diagnostic separately.

## Model-choice quick guide

| Question | Adapter choice | Interpretation |
|:---|:---|:---|
| Describe when Gazepoint trials first inspect the target | Kaplan-Meier output | Remaining uninspected probability |
| Estimate a marginal condition effect across repeated trials | `cox_structure = "cluster_robust"` | Marginal hazard ratio |
| Estimate latent participant heterogeneity in R | `cox_structure = "frailty"` | Conditional hazard ratio with participant frailty |
| Express multiplicative event-time differences | `aft_distribution = "weibull"` or `"lognormal"` | Time ratio |

The adapter does not select among these estimands. The analyst must name
the repeated-Cox structure and AFT family explicitly.

## Sensitivity guidance

Rebuild the survival table and rerun the model under pre-specified
alternatives that matter scientifically: AOI boundaries, fixation/event
detector, trial start marker, minimum fixation duration, quality
threshold, clustered versus frailty Cox, Weibull versus log-normal AFT,
and transparent exclusion/review rules. Do not merely relabel the same
prepared table as a new specification.

## Reporting checklist

Report participants and trials, observed events and censored trials,
censoring percentage, review-required rows, target event and AOI, time
origin and observation-window rule, event detector and fixation-duration
rule, gaze-quality criterion, Cox repeated-observation structure, AFT
family, effect estimate with 95% CI, PH diagnostic status, sensitivity
branches, and the eyeprocess/gp3tools versions.

### Copyable reporting template

> Gazepoint gaze latency was analysed using the vendor-neutral
> `eyeprocess` survival engine through `gp3tools`. The target event was
> \[EVENT\] within \[TARGET AOI\], measured from \[TIME ORIGIN\].
> Complete usable trials in which the event did not occur were retained
> as right-censored observations; unresolved or low-quality trials were
> not converted to censoring. We fitted \[CLUSTER-ROBUST/FRAILTY\] Cox
> and \[WEIBULL/LOG-NORMAL\] AFT specifications, reporting \[HAZARD
> RATIOS/TIME RATIOS\] with 95% confidence intervals. We documented
> censoring by condition, proportional-hazards diagnostics where
> applicable, and pre-specified sensitivity analyses across AOI,
> detector, quality, and model definitions.

Report the `eyeprocess` and `gp3tools` versions together so the
scientific engine and vendor-adapter layer are both reproducible.

## Failure cases

The adapter should stop or preserve a review flag rather than silently
proceed when trial identity is ambiguous, event time exceeds the
observation window, the window is missing, gaze quality is unusable,
estimator choice is omitted, or the required eyeprocess survival API is
unavailable.

## API links

- gp3tools::prepare_gazepoint_survival_data() — thin preparation
  adapter.
- gp3tools::run_gazepoint_latency_analysis() — thin end-to-end adapter
  requiring explicit model choices.
- See the eyeprocess Survival Analysis for Gaze Latency article for the
  full vendor-neutral methodology and model APIs.
