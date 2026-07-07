# Standardise external facial-analysis columns

Adds common gp3tools-friendly facial-analysis columns to external
face-analysis outputs. The function keeps the original columns by
default and prepends standard columns such as `face_frame`,
`face_time_sec`, `face_confidence`, `face_success`, and `face_valid`.

## Usage

``` r
standardize_gazepoint_face_columns(
  data,
  source = c("auto", "openface", "pyfeat", "mediapipe", "facereader", "generic"),
  participant_id_col = NULL,
  frame_col = NULL,
  time_col = NULL,
  confidence_col = NULL,
  success_col = NULL,
  face_id_col = NULL,
  file_col = NULL,
  confidence_threshold = 0.8,
  keep_original_columns = TRUE
)
```

## Arguments

- data:

  A data frame returned by
  [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md),
  a plain data frame, or a CSV path readable by
  [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md).

- source:

  Facial-analysis source. Use `"auto"` to infer a likely source.

- participant_id_col:

  Optional participant identifier column.

- frame_col:

  Optional frame-index column.

- time_col:

  Optional time column, preferably in seconds.

- confidence_col:

  Optional face-detection confidence column.

- success_col:

  Optional face-detection success column.

- face_id_col:

  Optional face identifier column.

- file_col:

  Optional file/source column.

- confidence_threshold:

  Minimum confidence required for `face_valid` when a confidence column
  is available.

- keep_original_columns:

  Should original facial-analysis columns be kept?

## Value

A tibble with standardised facial-analysis columns. The returned object
has class `gp3_face_data`.

## Examples

``` r
face <- data.frame(
  frame = 1:2,
  timestamp = c(0, 0.033),
  confidence = c(0.98, 0.40),
  success = c(1, 1),
  AU12_r = c(0.1, 0.2)
)

standardize_gazepoint_face_columns(face)
#> # A tibble: 2 × 15
#>   face_source face_file participant_id face_id face_frame face_time_sec
#>   <chr>       <chr>     <chr>          <chr>        <int>         <dbl>
#> 1 openface    NA        NA             NA               1         0    
#> 2 openface    NA        NA             NA               2         0.033
#> # ℹ 9 more variables: face_time_ms <dbl>, face_confidence <dbl>,
#> #   face_success <lgl>, face_valid <lgl>, frame <int>, timestamp <dbl>,
#> #   confidence <dbl>, success <dbl>, AU12_r <dbl>
```
