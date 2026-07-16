# Summarise an event-detector benchmark

Extract detector-level, sequence-level, matched-event, or
unmatched-event tables from an event-detector benchmark.

## Usage

``` r
summarise_gazepoint_event_detector_benchmark(
  x,
  level = c("detector", "sequence", "matches", "errors"),
  sort = TRUE
)
```

## Arguments

- x:

  An object returned by
  [`benchmark_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/benchmark_gazepoint_event_detectors.md).

- level:

  Summary level to return.

- sort:

  Should detector summaries be ordered by decreasing F1 score?

## Value

A data frame.
