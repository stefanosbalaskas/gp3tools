# Summarize pupil response features by subject and trial

Extracts common pupil-response features from sample-level pupil data.

## Usage

``` r
summarize_gazepoint_pupil_response_features(
  data,
  pupil,
  time,
  subject,
  trial,
  baseline_window,
  response_window,
  condition = NULL,
  interpolated = NULL
)
```

## Arguments

- data:

  A sample-level data frame.

- pupil:

  Pupil column.

- time:

  Time column.

- subject:

  Subject column.

- trial:

  Trial column.

- baseline_window:

  Numeric vector of length two.

- response_window:

  Numeric vector of length two.

- condition:

  Optional condition column to carry forward.

- interpolated:

  Optional logical/numeric interpolation flag column.

## Value

A trial-level data frame of pupil features.
