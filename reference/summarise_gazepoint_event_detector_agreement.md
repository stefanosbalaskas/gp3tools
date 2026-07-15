# Summarise agreement between event detectors

Compare standardized fixation intervals using event-level
intersection-over-union. Agreement is methodological and does not
identify a uniquely correct detector.

## Usage

``` r
summarise_gazepoint_event_detector_agreement(x, min_overlap = 0.5)
```

## Arguments

- x:

  An object returned by
  [`compare_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_event_detectors.md)
  or a standardized event data frame containing detector, sequence,
  start, end, and duration columns.

- min_overlap:

  Minimum interval intersection-over-union required for a match.

## Value

A list containing detector summaries, pairwise agreement, unmatched
events, and settings.
