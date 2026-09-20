# Gazepoint adapters for standardized eye-tracking data quality

Thin Gazepoint-facing convenience wrappers for the vendor-neutral
spatial and sampling quality implementation in the suggested eyeprocess
package. gp3tools resolves source columns and available
geometry/metadata but does not duplicate accuracy, precision, BCEA,
sampling, jitter, or data-loss formulas.

## Usage

``` r
summarise_gazepoint_spatial_quality(
  data, x_col = NULL, y_col = NULL, time_col = NULL,
  target_x_col = NULL, target_y_col = NULL,
  coordinate_unit = c("auto", "normalized", "pixels", "degrees"),
  output_unit = NULL, geometry = NULL,
  time_unit = c("auto", "ms", "s", "us", "ns"),
  level = c("dataset", "sample", "participant", "session", "trial", "file"),
  by = NULL, probability = 0.68, max_gap_ms = NULL
)

create_gazepoint_quality_report(
  data, x_col = NULL, y_col = NULL, time_col = NULL,
  target_x_col = NULL, target_y_col = NULL, valid_col = NULL,
  missing_reason_col = NULL, participant_col = NULL,
  session_col = NULL, trial_col = NULL, file_col = NULL,
  coordinate_unit = c("auto", "normalized", "pixels", "degrees"),
  output_unit = NULL, geometry = NULL,
  time_unit = c("auto", "ms", "s", "us", "ns"),
  level = c("dataset", "sample", "participant", "session", "trial", "file"),
  by = NULL, nominal_sampling_hz = 60, bcea_probability = 0.68,
  max_gap_ms = NULL, thresholds = NULL, preprocessing_spec = NULL,
  event_detector = NULL, aoi_specification = NULL,
  quality_rules = NULL, model_specification = NULL
)

plot_gazepoint_quality_dashboard(x, ...)
report_gazepoint_quality(x, digits = 3L, ...)
```

## Arguments

- data:

  A non-empty sample-level Gazepoint data frame.

- x_col, y_col, time_col:

  Optional explicit source gaze/timestamp columns.

- target_x_col, target_y_col:

  Known validation-target coordinate columns.

- valid_col:

  Optional gaze-validity column.

- missing_reason_col:

  Optional reason-specific loss column.

- participant_col, session_col, trial_col, file_col:

  Optional source identifier columns.

- coordinate_unit:

  Input coordinate unit. Auto inference is restricted to explicit
  metadata and recognized vendor-native coordinate columns.

- output_unit:

  Optional explicit output unit.

- geometry:

  Optional screen/viewing geometry.

- time_unit:

  Timestamp unit.

- level:

  Requested aggregation level when by is not supplied.

- by:

  Optional explicit grouping columns.

- probability, bcea_probability:

  Explicit BCEA probability level.

- max_gap_ms:

  Optional maximum interval for adjacent RMS-S2S pairs.

- nominal_sampling_hz:

  Optional nominal hardware frequency.

- thresholds:

  Study-specific review thresholds; never automatic exclusions.

- preprocessing_spec, event_detector, aoi_specification, quality_rules,
  model_specification:

  Optional provenance fields forwarded to eyeprocess.

- x:

  A gaze_quality_report or raw Gazepoint data.

- digits:

  Digits in compact reporting text.

- ...:

  Arguments forwarded to create_gazepoint_quality_report() when raw data
  are supplied.

## Details

Gazepoint BPOG/FPOG/LPOG/RPOG coordinates are recognized as normalized
coordinates. TIME is recognized as seconds and MSTIMER as milliseconds.
Generic coordinate columns do not trigger range-based unit guessing:
users must declare the unit explicitly unless an unambiguous
coordinate_unit column is available.

When complete screen pixel dimensions, physical dimensions, and viewing
distance are available, they can be forwarded to eyeprocess for an
explicitly requested coordinate conversion. Incomplete geometry is never
silently completed.

For sample-level grouping, gp3tools creates a deterministic source-row
identifier. Single-sample rows can be useful for availability or
target-error auditing, but single-sample SD/BCEA and successive-sample
precision should not be interpreted as stable-target precision evidence.

The wrappers require a compatible development version of eyeprocess. The
scientific formulas remain implemented only in that vendor-neutral core.

## Value

The summary and report wrappers return the corresponding eyeprocess data
frames with a gazepoint_quality_adapter provenance attribute. The
plotting wrapper returns the eyeprocess dashboard object. The reporting
wrapper returns character text.

## Examples

``` r
if (FALSE) { # \dontrun{
q <- create_gazepoint_quality_report(
  gazepoint_samples,
  target_x_col = "target_x",
  target_y_col = "target_y",
  level = "trial",
  nominal_sampling_hz = 60
)
report_gazepoint_quality(q)
} # }
```
