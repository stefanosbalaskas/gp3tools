# Plot event-detector benchmark diagnostics

Plot event-detector benchmark diagnostics

## Usage

``` r
plot_gazepoint_event_detector_benchmark(
  x,
  plot = c("f1", "precision_recall", "overlap", "timing_error", "counts"),
  main = NULL,
  ylab = NULL,
  las = 2
)
```

## Arguments

- x:

  An object returned by
  [`benchmark_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/benchmark_gazepoint_event_detectors.md).

- plot:

  Plot type: `"f1"`, `"precision_recall"`, `"overlap"`,
  `"timing_error"`, or `"counts"`.

- main:

  Optional plot title.

- ylab:

  Optional vertical-axis label.

- las:

  Axis-label orientation passed to base graphics.

## Value

Invisibly returns the detector-level data used by the plot.
