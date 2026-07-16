# Benchmark Gazepoint event detectors against reviewed events

Compare standardized fixation intervals from one or more detectors with
a manually reviewed or synthetic reference-event table. Matching is
one-to-one within each participant/trial sequence and is based on
interval intersection-over-union. Results quantify methodological
agreement with the supplied reference annotations; they do not establish
a universally correct detector.

## Usage

``` r
benchmark_gazepoint_event_detectors(
  x,
  reviewed_events,
  sequence_cols = NULL,
  reviewed_start_col = "start_time",
  reviewed_end_col = "end_time",
  reviewed_id_col = "review_event_id",
  review_status_col = "review_status",
  accepted_status = c("accepted", "include", "reviewed", "confirmed"),
  min_overlap = 0.5,
  time_unit = c("seconds", "milliseconds")
)
```

## Arguments

- x:

  An object returned by
  [`compare_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_event_detectors.md)
  or a standardized detector-event data frame.

- reviewed_events:

  A data frame containing reviewed reference intervals.

- sequence_cols:

  Sequence identifier columns. When `x` is a comparison object, its
  stored sequence columns are used by default.

- reviewed_start_col, reviewed_end_col:

  Start and end columns in `reviewed_events`.

- reviewed_id_col:

  Optional reviewed-event identifier column.

- review_status_col:

  Optional review-status column. When present, only rows whose status is
  included in `accepted_status` are used.

- accepted_status:

  Character values treated as accepted reviews.

- min_overlap:

  Minimum interval intersection-over-union required for a true-positive
  match.

- time_unit:

  Unit used by event start/end values. This controls conversion of
  timing errors to milliseconds.

## Value

An object of class `"gp3_event_detector_benchmark"` containing
detector-level metrics, sequence-level metrics, one-to-one matches,
unmatched-event diagnostics, accepted reviewed events, detector events,
detector-run information, and settings.

## Examples

``` r
reviewed <- data.frame(
  USER_ID = "P01",
  trial = "T01",
  review_event_id = 1:2,
  start_time = c(0, 2),
  end_time = c(1, 3)
)

detected <- data.frame(
  USER_ID = "P01",
  trial = "T01",
  detector = "velocity_10",
  family = "velocity",
  threshold = 10,
  event_id = 1:2,
  start_time = c(0.05, 2.05),
  end_time = c(1.05, 3.05),
  duration_ms = 1000
)

benchmark <- benchmark_gazepoint_event_detectors(
  detected,
  reviewed,
  sequence_cols = c("USER_ID", "trial")
)

benchmark$detector_metrics
#>      detector   family threshold n_sequences n_reviewed n_detected
#> 1 velocity_10 velocity        10           1          2          2
#>   true_positive false_positive false_negative precision recall f1  mean_iou
#> 1             2              0              0         1      1  1 0.9047619
#>   median_iou mean_onset_error_ms mean_abs_onset_error_ms mean_offset_error_ms
#> 1  0.9047619                  50                      50                   50
#>   mean_abs_offset_error_ms mean_duration_error_ms mean_abs_duration_error_ms
#> 1                       50                      0                          0
#>   detection_count_bias
#> 1                    0
```
