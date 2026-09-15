# Pupil preprocessing and baseline audit

This article focuses on pupil preprocessing, baseline-readiness checks,
and the additional sampling/estimator checks needed when pupil-response
latency is reported.

## Recommended order

1.  inspect raw pupil coverage;
2.  flag missingness and implausible values;
3.  interpolate short gaps only when justified;
4.  audit baseline coverage;
5.  create baseline-corrected or change-score variables;
6.  if latency is a target outcome, audit estimator sensitivity and
    sampling resolution;
7.  document preprocessing and timing choices.

## Example preprocessing workflow

``` r

gap_qc <- audit_gazepoint_pupil_gaps(all_gaze)

pupil_flagged <- flag_gazepoint_pupil_hampel(
  all_gaze,
  pupil_col = "pupil_diameter"
)

pupil_interp <- interpolate_gazepoint_pupil_pchip(
  pupil_flagged,
  pupil_col = "pupil_diameter"
)

baseline_qc <- audit_gazepoint_pupil_baseline(
  pupil_interp,
  baseline_start = -0.500,
  baseline_end = 0
)

reliability_qc <- audit_gazepoint_pupil_reliability(pupil_interp)
```

## When latency is a target outcome

A latency estimate is partly a property of the sampling grid and the
estimator, not only of the underlying physiological response.
[`audit_gp3_pupil_latency_resolution()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gp3_pupil_latency_resolution.md)
therefore compares two onset estimators and reports their spread
alongside the observed sampling rate, the implied temporal resolution,
robust baseline noise, signal-to-noise ratio, and a compact
latency-resolvability label.

Use the diagnostic on a scientifically justified analysis trace after
time alignment and preprocessing. Do not pool raw observations across
independent participants or trials and treat the resulting latency as
inferential evidence. The function is intended for QC and transparent
reporting.

## Deterministic synthetic example

The example below creates a 60 Hz trace with a known constriction
beginning after the event. The audit is then run on the resulting pupil
series.

``` r

set.seed(42)

time <- seq(-0.5, 2, by = 1 / 60)
true_onset <- 0.45
response <- ifelse(
  time > true_onset,
  -0.35 * (1 - exp(-(time - true_onset) / 0.12)),
  0
)
pupil <- 4.2 + response + rnorm(length(time), sd = 0.006)

latency_qc <- audit_gp3_pupil_latency_resolution(
  time = time,
  pupil = pupil,
  event_time = 0,
  baseline_window = c(-0.5, 0),
  search_window = c(0, 2),
  direction = "constriction",
  threshold_sigma = 3,
  sustain_ms = 50
)

latency_qc$estimates_s
#> sustained_threshold   max_slope_tangent 
#>           0.4666667           0.4502978
round(latency_qc$estimator_spread_ms, 1)
#> [1] 16.4
round(latency_qc$sampling_hz, 1)
#> [1] 60
round(latency_qc$sampling_resolution_ms, 1)
#> [1] 16.7
latency_qc$latency_resolvability
#> [1] "high"
```

The two latency estimates should be read together. A small estimator
spread is reassuring, but it does not remove the sampling-resolution
limit or make the estimate hardware- and algorithm-independent.

``` r

plot(
  time,
  pupil,
  type = "l",
  xlab = "Time from event (s)",
  ylab = "Pupil diameter (a.u.)",
  main = "Synthetic pupil trace with latency estimates"
)
abline(v = 0, lty = 2)
latency_lines <- latency_qc$estimates_s[is.finite(latency_qc$estimates_s)]
if (length(latency_lines)) {
  abline(v = latency_lines, lty = 3)
}
legend(
  "bottomleft",
  legend = c("Pupil trace", "Event time", "Latency estimates"),
  lty = c(1, 2, 3),
  bty = "n"
)
```

![Synthetic pupil trace with event time and two latency
estimates](pupil-preprocessing-baseline-audit_files/figure-html/latency-plot-1.png)

## Interpreting the audit

- `estimates_s` contains the sustained-threshold and maximum-slope
  tangent estimates.
- `estimator_spread_ms` quantifies disagreement between finite
  estimators.
- `sampling_hz` and `sampling_resolution_ms` make the temporal sampling
  limit explicit.
- `noise_mad_sigma` and `signal_to_noise` describe baseline noise and
  response strength used by the diagnostic.
- `latency_resolvability` is a package QC heuristic (`high`, `moderate`,
  or `low`), not a physiological certainty grade.

When one estimator is unavailable, report that fact rather than silently
substituting the remaining estimate as if both methods agreed.

## Sensitivity checks

Threshold and sustain settings should be chosen from methodological
considerations rather than tuned post hoc to obtain a preferred onset.
When timing is substantively important, rerun the audit under defensible
alternatives and report whether the scientific conclusion changes.

``` r

latency_qc_lower_threshold <- audit_gp3_pupil_latency_resolution(
  time,
  pupil,
  threshold_sigma = 2.5,
  sustain_ms = 50
)

latency_qc_longer_sustain <- audit_gp3_pupil_latency_resolution(
  time,
  pupil,
  threshold_sigma = 3,
  sustain_ms = 100
)
```

## Reporting note

Baseline correction should be reported with the baseline window, minimum
coverage rule, interpolation rule, excluded cases, and sensitivity
checks. If latency is reported, also state the sampling rate, estimator
definition, threshold/sustain settings, estimator spread, and any cases
in which latency was not resolvable. Treat latency as method-dependent
timing evidence rather than an instrument-independent ground truth.
