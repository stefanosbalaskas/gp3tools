# Export Gazepoint data to a lightweight BIDS-style folder

Write a conservative BIDS-style eye-tracking folder from a
Gazepoint-style data frame. This helper creates standard-looking TSV and
JSON sidecar files for sharing and inspection, but it does not claim
full validation against a specific evolving BIDS eye-tracking validator.

## Usage

``` r
export_gazepoint_to_bids(
  data,
  outdir,
  subject_col,
  task = "gazepoint",
  session = NULL,
  time_col,
  x_col,
  y_col,
  pupil_col = NULL,
  trial_col = NULL,
  aoi_col = NULL,
  overwrite = FALSE
)
```

## Arguments

- data:

  A gaze-sample data frame.

- outdir:

  Output directory.

- subject_col:

  Subject column.

- task:

  Task label used in file names.

- session:

  Optional session label.

- time_col:

  Time column.

- x_col:

  Horizontal gaze coordinate column.

- y_col:

  Vertical gaze coordinate column.

- pupil_col:

  Optional pupil column.

- trial_col:

  Optional trial column.

- aoi_col:

  Optional AOI column.

- overwrite:

  Should an existing output directory be reused?

## Value

A data frame listing written files.
