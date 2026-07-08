# Synchronise external facial-behaviour data with Gazepoint data

Aligns external facial-behaviour data to Gazepoint rows using either
nearest time matching or exact frame matching. The helper is designed
for facial data previously imported with
[`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md)
and standardised with
[`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md).
It does not infer facial expressions from Gazepoint exports and does not
interpret facial behaviour as emotion.

## Usage

``` r
sync_gazepoint_face_data(
  gazepoint_data,
  face_data,
  method = c("nearest_time", "frame_exact"),
  by = NULL,
  gaze_time_col = NULL,
  face_time_col = NULL,
  gaze_frame_col = NULL,
  face_frame_col = NULL,
  tolerance_sec = 0.05,
  prefix = "face_",
  keep_unmatched = TRUE,
  standardize_face = TRUE
)
```

## Arguments

- gazepoint_data:

  Gazepoint data frame, typically a master table, trial table, or
  sample-level table.

- face_data:

  External facial-behaviour data frame, preferably returned by
  [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md).

- method:

  Synchronisation method. `"nearest_time"` matches each Gazepoint row to
  the nearest facial-data row within group. `"frame_exact"` matches by
  exact frame index within group.

- by:

  Optional named character vector mapping Gazepoint grouping columns to
  facial-data grouping columns. For example,
  `c(subject_id = "participant_id")`. Use `NULL` for no grouping.

- gaze_time_col:

  Gazepoint time column. Required for `method = "nearest_time"` unless
  auto-detected.

- face_time_col:

  Facial-data time column. Defaults to `face_time_sec` when available.

- gaze_frame_col:

  Gazepoint frame column. Required for `method = "frame_exact"` unless
  auto-detected.

- face_frame_col:

  Facial-data frame column. Defaults to `face_frame` when available.

- tolerance_sec:

  Maximum allowed absolute time difference in seconds for nearest-time
  matching.

- prefix:

  Prefix added to non-standard facial-data columns before joining.

- keep_unmatched:

  Should Gazepoint rows without a valid face-data match be retained?

- standardize_face:

  Should `face_data` be passed through
  [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md)
  before synchronisation when needed?

## Value

A tibble with Gazepoint columns plus matched facial-behaviour columns
and synchronisation metadata. The returned object has class
`gp3_face_sync`.

## Examples

``` r
gaze <- data.frame(
  subject_id = "P001",
  time_sec = c(0.00, 0.03, 0.07),
  AOI = c("A", "A", "B")
)

face <- data.frame(
  participant_id = "P001",
  frame = 1:3,
  timestamp = c(0.00, 0.033, 0.066),
  confidence = c(0.95, 0.94, 0.93),
  success = c(1, 1, 1),
  AU12_r = c(0.1, 0.2, 0.3)
)

sync_gazepoint_face_data(
  gaze,
  face,
  by = c(subject_id = "participant_id"),
  gaze_time_col = "time_sec"
)
#> # A tibble: 3 × 26
#>   subject_id time_sec AOI   .gp3_face_sync_gaze_row face_source face_file
#>   <chr>         <dbl> <chr>                   <int> <chr>       <chr>    
#> 1 P001           0    A                           1 openface    NA       
#> 2 P001           0.03 A                           2 openface    NA       
#> 3 P001           0.07 B                           3 openface    NA       
#> # ℹ 20 more variables: face_participant_id <chr>, face_id <chr>,
#> #   face_frame <int>, face_time_sec <dbl>, face_time_ms <dbl>,
#> #   face_confidence <dbl>, face_success <lgl>, face_valid <lgl>,
#> #   face_frame_1 <int>, face_timestamp <dbl>, face_confidence_1 <dbl>,
#> #   face_success_1 <dbl>, face_AU12_r <dbl>, .gp3_face_sync_face_row <int>,
#> #   face_sync_method <chr>, face_sync_status <chr>, face_sync_diff_sec <dbl>,
#> #   face_sync_abs_diff_sec <dbl>, face_sync_within_tolerance <lgl>, …
```
