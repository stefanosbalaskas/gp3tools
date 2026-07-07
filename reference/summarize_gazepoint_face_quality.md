# Summarise external facial-behaviour data quality

Returns the overview table from
[`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md).
The helper accepts either an existing face-quality audit object or data
that can be audited directly.

## Usage

``` r
summarize_gazepoint_face_quality(data, ...)

summarise_gazepoint_face_quality(data, ...)
```

## Arguments

- data:

  A `gp3_face_quality_audit` object, a standardised face data frame, an
  unstandardised face data frame, or a CSV path.

- ...:

  Additional arguments passed to
  [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md)
  when `data` is not already an audit object.

## Value

A tibble with class `gp3_face_quality_summary`.

## Examples

``` r
face <- data.frame(
  frame = 1:2,
  timestamp = c(0, 0.033),
  confidence = c(0.95, 0.90),
  success = c(1, 1)
)

summarize_gazepoint_face_quality(face)
#> # A tibble: 1 × 25
#>   n_groups n_rows n_valid valid_percent n_invalid invalid_percent
#>      <int>  <int>   <int>         <dbl>     <int>           <dbl>
#> 1        1      2       2           100         0               0
#> # ℹ 19 more variables: n_unknown_validity <int>,
#> #   unknown_validity_percent <dbl>, n_missing_confidence <int>,
#> #   confidence_missing_percent <dbl>, mean_confidence <dbl>,
#> #   median_confidence <dbl>, min_confidence <dbl>, max_confidence <dbl>,
#> #   n_success <int>, success_percent <dbl>, n_duplicate_frames <int>,
#> #   duplicate_frame_percent <dbl>, n_missing_time <int>,
#> #   n_nonpositive_time_steps <int>, max_time_gap_sec <dbl>, …
```
