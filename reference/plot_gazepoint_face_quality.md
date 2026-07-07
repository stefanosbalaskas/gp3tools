# Plot external facial-behaviour data quality

Creates descriptive quality-control plots from
[`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md)
output. The plots summarise face-data validity, confidence, status, or
timing gaps. They do not infer facial expressions or emotional states.

## Usage

``` r
plot_gazepoint_face_quality(
  data,
  plot_type = c("status", "validity", "confidence", "time_gaps"),
  group_col = NULL,
  title = NULL,
  ...
)
```

## Arguments

- data:

  A `gp3_face_quality_audit` object, a standardised face data frame, an
  unstandardised face data frame, or a CSV path.

- plot_type:

  One of `"status"`, `"validity"`, `"confidence"`, or `"time_gaps"`.

- group_col:

  Optional grouping column to use on the y-axis for group-level plots.
  Defaults to `face_quality_group`.

- title:

  Optional plot title.

- ...:

  Additional arguments passed to
  [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md)
  when `data` is not already an audit object.

## Value

A ggplot object.

## Examples

``` r
face <- data.frame(
  frame = 1:3,
  timestamp = c(0, 0.033, 0.066),
  confidence = c(0.95, 0.90, 0.40),
  success = c(1, 1, 1)
)

plot_gazepoint_face_quality(face)
```
