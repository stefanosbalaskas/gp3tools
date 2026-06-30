# Create a Gazepoint preprocessing multiverse

Create a preprocessing multiverse specification for Gazepoint pupil and
AOI workflows. The returned object defines alternative preprocessing
decisions that can later be passed to pupil or AOI multiverse runners.

## Usage

``` r
create_gazepoint_preprocessing_multiverse(
  pupil_max_gap_ms = c(75, 150, 250),
  pupil_smoothing_window_samples = c(3L, 5L, 7L),
  pupil_baseline_windows = list(c(0, 200), c(-200, 0)),
  pupil_artifact_padding_ms = c(0, 50),
  aoi_denominators = c("valid", "all", "aoi_only"),
  aoi_min_denominator_samples = c(1L, 5L, 10L),
  include_pupil = TRUE,
  include_aoi = TRUE,
  label_prefix = "mv"
)
```

## Arguments

- pupil_max_gap_ms:

  Numeric vector of maximum pupil-interpolation gap durations in
  milliseconds.

- pupil_smoothing_window_samples:

  Integer vector of pupil smoothing window sizes in samples.

- pupil_baseline_windows:

  List of numeric vectors of length 2 defining pupil baseline windows in
  milliseconds.

- pupil_artifact_padding_ms:

  Numeric vector of artifact-padding values in milliseconds.

- aoi_denominators:

  Character vector of AOI denominator definitions. Typical values are
  `"valid"`, `"all"`, and `"aoi_only"`.

- aoi_min_denominator_samples:

  Integer vector of minimum denominator sample thresholds for AOI-window
  modelling.

- include_pupil:

  Logical. If `TRUE`, create pupil preprocessing branches.

- include_aoi:

  Logical. If `TRUE`, create AOI preprocessing branches.

- label_prefix:

  Character prefix used for branch identifiers.

## Value

A list with class `gp3_preprocessing_multiverse` containing overview,
pupil grid, AOI grid, combined grid, and settings tables.
