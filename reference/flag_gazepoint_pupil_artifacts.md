# Flag Gazepoint pupil artifacts before interpolation

Adds pupil-specific artifact flags for blink/trackloss contamination,
physiological implausibility when pupil units are millimetres,
pupil-speed outliers, left-right binocular pupil disagreement, and
temporal padding around bad samples. The function preserves raw pupil
columns and creates `pupil_clean`, which can be used as input for
interpolation.

## Usage

``` r
flag_gazepoint_pupil_artifacts(
  data,
  pupil_col = NULL,
  left_pupil_col = NULL,
  right_pupil_col = NULL,
  time_col = NULL,
  blink_col = NULL,
  trackloss_col = NULL,
  missing_pupil_col = NULL,
  pupil_unit_col = NULL,
  group_cols = c("subject", "media_id"),
  registry = NULL,
  blink_padding_pre_ms = NULL,
  blink_padding_post_ms = NULL,
  pupil_min_mm = NULL,
  pupil_max_mm = NULL,
  pupil_speed_mad_k = NULL,
  binocular_mad_k = NULL,
  max_physio_outlier_prop = 0.8,
  flag_speed_outliers = TRUE,
  flag_binocular_disagreement = TRUE,
  flag_physiological_outliers = TRUE
)
```

## Arguments

- data:

  A Gazepoint master table or pupil-processing table.

- pupil_col:

  Optional name of the pupil column to clean. If `NULL`, the function
  detects one of `mean_pupil`, `pupil_raw`, `pupil`, `left_pupil`, or
  `right_pupil`.

- left_pupil_col:

  Optional left-pupil column. If `NULL`, `left_pupil` is used when
  available.

- right_pupil_col:

  Optional right-pupil column. If `NULL`, `right_pupil` is used when
  available.

- time_col:

  Optional time column. If `NULL`, the function detects one of
  `time_ms`, `time`, `time_orig`, or `time_orig_ms`.

- blink_col:

  Optional blink column. If `NULL`, `blink` is used when available.

- trackloss_col:

  Optional trackloss column. If `NULL`, one of `trackloss` or
  `Trackloss` is used when available.

- missing_pupil_col:

  Optional missing-pupil column. If `NULL`, `missing_pupil` is used when
  available.

- pupil_unit_col:

  Optional pupil-unit column. If `NULL`, `pupil_unit` is used when
  available.

- group_cols:

  Character vector of grouping columns used for speed outlier detection
  and artifact-padding windows. Defaults to `c("subject", "media_id")`.
  Use `character(0)` for global processing.

- registry:

  Optional preprocessing registry created by
  [`create_gazepoint_preprocessing_registry()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_registry.md).

- blink_padding_pre_ms:

  Padding before bad samples, in milliseconds. If `NULL`, taken from
  `registry` or defaults to `100`.

- blink_padding_post_ms:

  Padding after bad samples, in milliseconds. If `NULL`, taken from
  `registry` or defaults to `100`.

- pupil_min_mm:

  Minimum plausible pupil value when units are millimetres. If `NULL`,
  taken from `registry` or defaults to `1`.

- pupil_max_mm:

  Maximum plausible pupil value when units are millimetres. If `NULL`,
  taken from `registry` or defaults to `9`.

- pupil_speed_mad_k:

  MAD multiplier for pupil-speed outlier detection. If `NULL`, taken
  from `registry` or defaults to `6`.

- binocular_mad_k:

  MAD multiplier for binocular-disagreement detection. If `NULL`, taken
  from `registry` or defaults to `6`.

- max_physio_outlier_prop:

  Maximum allowed proportion of non-missing millimetre-labelled pupil
  samples that may be rejected by the physiological rule before the rule
  is automatically suppressed. Defaults to `0.80`. This prevents
  raw-unit Gazepoint exports from being silently erased when the unit
  label suggests millimetres but the numeric scale is not compatible
  with ordinary 1–9 mm thresholds.

- flag_speed_outliers:

  Logical. If `TRUE`, pupil-speed outliers are flagged. Defaults to
  `TRUE`.

- flag_binocular_disagreement:

  Logical. If `TRUE`, left-right pupil disagreement is flagged when both
  eyes are available. Defaults to `TRUE`.

- flag_physiological_outliers:

  Logical. If `TRUE`, millimetre-based physiological thresholds are
  applied only when the pupil unit is identified as millimetres.
  Defaults to `TRUE`.

## Value

A tibble containing the original data plus pupil-artifact columns.

## Examples

``` r
if (FALSE) { # \dontrun{
registry <- create_gazepoint_preprocessing_registry()

artifact_pupil <- flag_gazepoint_pupil_artifacts(
  master,
  registry = registry
)

dplyr::count(artifact_pupil, pupil_artifact_reason)
} # }
```
