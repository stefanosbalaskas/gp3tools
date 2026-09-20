# Gazepoint survival-analysis reproducibility checklist

## Purpose

This checklist documents what must be frozen when `gp3tools` is used as
the Gazepoint-specific adapter to the vendor-neutral `eyeprocess`
survival engine. It does not introduce any new estimator or censoring
logic.

## Gazepoint input contract

Record the Gazepoint export type, trial-window construction,
participant/trial identifiers, coordinate transformation, event/fixation
source, target AOI geometry, time origin, and gaze-quality rule. A
missing target event is not sufficient evidence for censoring unless the
observation window is complete and usable.

## Adapter choices

Record the explicit `cox_structure` and `aft_distribution` supplied to
[`run_gazepoint_latency_analysis()`](https://stefanosbalaskas.github.io/gp3tools/reference/gaze-survival-adapter.md).
Distinguish participant-clustered Cox from latent participant frailty
and report the corresponding hazard-ratio interpretation. Record the AFT
family and report time ratios when AFT results are presented.

## Quality and censoring audit

Archive observed-event, right-censored, and review-required counts;
censoring percentage by condition; ambiguous join failures;
event-after-window failures; and gaze-quality exclusions/review states.
Do not convert low-quality or incomplete Gazepoint trials into ordinary
censoring.

## Sensitivity plan

Pre-specify AOI geometry, detector/fixation rule, time origin, minimum
fixation duration, quality threshold, Cox structure where scientifically
relevant, AFT family, and review/exclusion alternatives. Rebuild the
delegated `eyeprocess` survival table whenever the event or AOI
definition changes.

## Software provenance

Report both `gp3tools` and `eyeprocess` versions, plus the specialist
model backends (`survival` and `coxme`) where relevant. Preserve the
`eyeprocess` provenance columns returned by the adapter.

## Reporting checklist

Report the target event/AOI, time origin, observation-window rule,
gaze-quality criterion, event/censor/review counts, Cox structure, AFT
family, effect estimate with 95% CI, diagnostics, sensitivity branches,
and both package versions.

## API links

- [`prepare_gazepoint_survival_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/gaze-survival-adapter.md)
  — Gazepoint preparation adapter.
- [`run_gazepoint_latency_analysis()`](https://stefanosbalaskas.github.io/gp3tools/reference/gaze-survival-adapter.md)
  — explicit end-to-end adapter.
- [`eyeprocess::summarise_gaze_censoring()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.html)
  — vendor-neutral censoring audit.
- [`eyeprocess::report_gaze_survival_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.html)
  — vendor-neutral reporting bundle.
