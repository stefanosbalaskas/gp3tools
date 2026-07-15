# Plot event-detector comparison diagnostics

Plot event-detector comparison diagnostics

## Usage

``` r
plot_gazepoint_event_detector_agreement(
  x,
  plot = c("counts", "durations", "agreement"),
  main = NULL,
  ylab = NULL,
  las = 2
)
```

## Arguments

- x:

  An object returned by
  [`compare_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_event_detectors.md).

- plot:

  Plot type: `"counts"`, `"durations"`, or `"agreement"`.

- main:

  Optional plot title.

- ylab:

  Optional vertical-axis label.

- las:

  Axis-label orientation passed to base graphics.

## Value

Invisibly returns the plotted data.
