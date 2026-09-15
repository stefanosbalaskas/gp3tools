# Audit GP3 pupil-latency estimator sensitivity

Compares a sustained-threshold onset and maximum-slope tangent onset
while reporting estimator spread, sampling rate, robust baseline noise,
and a latency-resolvability label. This is a QC/reporting diagnostic
rather than a claim that any one estimator is universally correct.

## Usage

``` r
audit_gp3_pupil_latency_resolution(
  time,
  pupil,
  event_time = 0,
  baseline_window = c(-0.5, 0),
  search_window = c(0, 2),
  direction = c("constriction", "dilation"),
  threshold_sigma = 3,
  sustain_ms = 50
)
```

## Arguments

- time:

  Numeric sample times in seconds.

- pupil:

  Numeric pupil series.

- event_time:

  Nominal event time in seconds.

- baseline_window, search_window:

  Two-element windows relative to the event.

- direction:

  `"constriction"` or `"dilation"`.

- threshold_sigma:

  Multiples of robust baseline noise.

- sustain_ms:

  Required sustained threshold duration in milliseconds.

## Value

A structured pupil-latency QC record suitable for reports.
