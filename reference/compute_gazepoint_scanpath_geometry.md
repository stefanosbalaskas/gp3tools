# Compute scanpath geometry features by subject and trial

Computes scanpath length, straight-line distance, efficiency,
convex-hull area, and spatial dispersion from sequential gaze/fixation
coordinates.

## Usage

``` r
compute_gazepoint_scanpath_geometry(
  data,
  x,
  y,
  subject,
  trial,
  time = NULL,
  condition = NULL
)
```

## Arguments

- data:

  A data frame.

- x:

  X-coordinate column.

- y:

  Y-coordinate column.

- subject:

  Subject column.

- trial:

  Trial column.

- time:

  Optional time column used for ordering.

- condition:

  Optional condition column to carry forward.

## Value

A trial-level data frame of scanpath geometry features.
