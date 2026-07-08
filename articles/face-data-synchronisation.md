# External face-data synchronisation

This article demonstrates how to align externally generated
facial-behaviour data with Gazepoint rows.

The helpers shown here do **not** infer facial expressions from
Gazepoint CSV files. They also do **not** interpret facial behaviour as
emotion. Their purpose is narrower: to align already imported and
standardised external face-analysis data with Gazepoint timing or frame
indices, and to audit the quality of that alignment.

The current scope is:

1.  nearest-time synchronisation;
2.  exact-frame synchronisation;
3.  synchronisation-status columns;
4.  matched/unmatched/outside-tolerance summaries;
5.  absolute time-difference summaries for nearest-time matching.

Window summaries, trial-level aggregation, multimodal modelling, and
emotion interpretation are later workflow stages.

## Example Gazepoint and face-analysis data

The Gazepoint table may be a sample-level table, trial-level table, or
another table with timing or frame information.

``` r

gaze <- data.frame(
  subject_id = c("P001", "P001", "P001", "P002", "P002"),
  trial_id = c(1, 1, 1, 1, 1),
  time_sec = c(0.000, 0.033, 0.066, 0.000, 0.050),
  VID_FRAME = c(1, 2, 3, 1, 2),
  AOI = c("claim", "claim", "logo", "claim", "evidence"),
  stringsAsFactors = FALSE
)
```

The facial-behaviour table is assumed to come from an external
face-analysis pipeline. It can be standardised before synchronisation,
or standardised automatically by
[`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md).

``` r

face <- data.frame(
  participant_id = c("P001", "P001", "P001", "P002", "P002"),
  frame = c(1, 2, 3, 1, 2),
  timestamp = c(0.000, 0.033, 0.066, 0.000, 0.060),
  confidence = c(0.98, 0.96, 0.94, 0.95, 0.93),
  success = c(1, 1, 1, 1, 1),
  AU04_r = c(0.05, 0.06, 0.05, 0.10, 0.12),
  AU12_r = c(0.20, 0.22, 0.25, 0.10, 0.08),
  stringsAsFactors = FALSE
)
```

## Nearest-time synchronisation

[`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md)
can match each Gazepoint row to the nearest face-analysis row within a
grouping structure. The `by` argument maps Gazepoint grouping columns to
facial-data grouping columns.

``` r

synced_time <- sync_gazepoint_face_data(
  gazepoint_data = gaze,
  face_data = face,
  method = "nearest_time",
  by = c(subject_id = "participant_id"),
  gaze_time_col = "time_sec",
  tolerance_sec = 0.050
)

synced_time
#> # A tibble: 5 × 29
#>   subject_id trial_id time_sec VID_FRAME AOI      .gp3_face_sync_gaze_row
#>   <chr>         <dbl>    <dbl>     <dbl> <chr>                      <int>
#> 1 P001              1    0             1 claim                          1
#> 2 P001              1    0.033         2 claim                          2
#> 3 P001              1    0.066         3 logo                           3
#> 4 P002              1    0             1 claim                          4
#> 5 P002              1    0.05          2 evidence                       5
#> # ℹ 23 more variables: face_source <chr>, face_file <chr>,
#> #   face_participant_id <chr>, face_id <chr>, face_frame <int>,
#> #   face_time_sec <dbl>, face_time_ms <dbl>, face_confidence <dbl>,
#> #   face_success <lgl>, face_valid <lgl>, face_frame_1 <dbl>,
#> #   face_timestamp <dbl>, face_confidence_1 <dbl>, face_success_1 <dbl>,
#> #   face_AU04_r <dbl>, face_AU12_r <dbl>, .gp3_face_sync_face_row <int>,
#> #   face_sync_method <chr>, face_sync_status <chr>, face_sync_diff_sec <dbl>, …
```

The output keeps the Gazepoint columns and appends matched face-analysis
columns. It also adds synchronisation metadata:

``` r

synced_time[, c(
  "subject_id",
  "time_sec",
  "face_time_sec",
  "face_confidence",
  "face_valid",
  "face_AU04_r",
  "face_AU12_r",
  "face_sync_status",
  "face_sync_diff_sec",
  "face_sync_abs_diff_sec",
  "face_sync_within_tolerance"
)]
#> # A tibble: 5 × 11
#>   subject_id time_sec face_time_sec face_confidence face_valid face_AU04_r
#>   <chr>         <dbl>         <dbl>           <dbl> <lgl>            <dbl>
#> 1 P001          0             0                0.98 TRUE              0.05
#> 2 P001          0.033         0.033            0.96 TRUE              0.06
#> 3 P001          0.066         0.066            0.94 TRUE              0.05
#> 4 P002          0             0                0.95 TRUE              0.1 
#> 5 P002          0.05          0.06             0.93 TRUE              0.12
#> # ℹ 5 more variables: face_AU12_r <dbl>, face_sync_status <chr>,
#> #   face_sync_diff_sec <dbl>, face_sync_abs_diff_sec <dbl>,
#> #   face_sync_within_tolerance <lgl>
```

The main status values are:

- `"matched"`: nearest facial row was within the tolerance;
- `"outside_tolerance"`: nearest facial row was found but exceeded the
  tolerance;
- `"unmatched"`: no facial row was available in the group;
- `"missing_gaze_time"`: the Gazepoint row had no usable time value.

## Exact-frame synchronisation

When Gazepoint and external face-analysis tables share frame indices,
exact-frame matching can be used.

``` r

synced_frame <- sync_gazepoint_face_data(
  gazepoint_data = gaze,
  face_data = face,
  method = "frame_exact",
  by = c(subject_id = "participant_id"),
  gaze_frame_col = "VID_FRAME"
)

synced_frame[, c(
  "subject_id",
  "VID_FRAME",
  "face_frame",
  "face_confidence",
  "face_AU04_r",
  "face_AU12_r",
  "face_sync_status",
  "face_sync_within_tolerance"
)]
#> # A tibble: 5 × 8
#>   subject_id VID_FRAME face_frame face_confidence face_AU04_r face_AU12_r
#>   <chr>          <dbl>      <int>           <dbl>       <dbl>       <dbl>
#> 1 P001               1          1            0.98        0.05        0.2 
#> 2 P001               2          2            0.96        0.06        0.22
#> 3 P001               3          3            0.94        0.05        0.25
#> 4 P002               1          1            0.95        0.1         0.1 
#> 5 P002               2          2            0.93        0.12        0.08
#> # ℹ 2 more variables: face_sync_status <chr>, face_sync_within_tolerance <lgl>
```

Exact-frame matching does not estimate time differences. It is most
appropriate when both data sources are known to share the same
video-frame basis.

## Audit synchronisation quality

[`audit_gazepoint_face_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_sync.md)
summarises synchronisation quality across all rows or within
user-defined groups.

``` r

sync_audit <- audit_gazepoint_face_sync(
  synced_time,
  group_cols = "subject_id",
  min_matched_percent = 70,
  warning_matched_percent = 85,
  max_abs_diff_sec = 0.050
)

sync_audit$overview
#> # A tibble: 1 × 20
#>   n_groups n_rows n_matched matched_percent n_unmatched unmatched_percent
#>      <int>  <int>     <int>           <dbl>       <int>             <dbl>
#> 1        2      5         5             100           0                 0
#> # ℹ 14 more variables: n_outside_tolerance <int>,
#> #   outside_tolerance_percent <dbl>, n_missing_gaze_time <int>,
#> #   n_missing_gaze_frame <int>, n_unknown_status <int>,
#> #   n_within_tolerance <int>, within_tolerance_percent <dbl>,
#> #   mean_abs_diff_sec <dbl>, median_abs_diff_sec <dbl>, p95_abs_diff_sec <dbl>,
#> #   max_abs_diff_sec <dbl>, n_abs_diff_above_limit <int>,
#> #   face_sync_audit_status <chr>, message <chr>
```

``` r

sync_audit$group_summary
#> # A tibble: 2 × 21
#>   face_sync_group subject_id n_rows n_matched matched_percent n_unmatched
#>   <chr>           <chr>       <int>     <int>           <dbl>       <int>
#> 1 subject_id=P001 P001            3         3             100           0
#> 2 subject_id=P002 P002            2         2             100           0
#> # ℹ 15 more variables: unmatched_percent <dbl>, n_outside_tolerance <int>,
#> #   outside_tolerance_percent <dbl>, n_missing_gaze_time <int>,
#> #   n_missing_gaze_frame <int>, n_unknown_status <int>,
#> #   n_within_tolerance <int>, within_tolerance_percent <dbl>,
#> #   mean_abs_diff_sec <dbl>, median_abs_diff_sec <dbl>, p95_abs_diff_sec <dbl>,
#> #   max_abs_diff_sec <dbl>, n_abs_diff_above_limit <int>,
#> #   face_sync_audit_status <chr>, message <chr>
```

``` r

sync_audit$issue_summary
#> # A tibble: 7 × 5
#>   issue                         n_groups_affected n_groups threshold status
#>   <chr>                                     <int>    <int>     <dbl> <chr> 
#> 1 matched_percent_below_minimum                 0        2     70    ok    
#> 2 matched_percent_below_warning                 0        2     85    ok    
#> 3 unmatched_rows                                0        2     NA    ok    
#> 4 outside_tolerance_rows                        0        2     NA    ok    
#> 5 missing_gaze_time_rows                        0        2     NA    ok    
#> 6 missing_gaze_frame_rows                       0        2     NA    ok    
#> 7 large_time_differences                        0        2      0.05 ok
```

The audit reports matched rows, unmatched rows, rows outside tolerance,
missing timing or frame indicators, and absolute time-difference
summaries when available.

## Example with an outside-tolerance row

The following example uses a stricter tolerance to illustrate how rows
can be flagged for review.

``` r

synced_strict <- sync_gazepoint_face_data(
  gazepoint_data = gaze,
  face_data = face,
  method = "nearest_time",
  by = c(subject_id = "participant_id"),
  gaze_time_col = "time_sec",
  tolerance_sec = 0.005
)

synced_strict[, c(
  "subject_id",
  "time_sec",
  "face_time_sec",
  "face_sync_status",
  "face_sync_abs_diff_sec",
  "face_sync_within_tolerance"
)]
#> # A tibble: 5 × 6
#>   subject_id time_sec face_time_sec face_sync_status  face_sync_abs_diff_sec
#>   <chr>         <dbl>         <dbl> <chr>                              <dbl>
#> 1 P001          0             0     matched                           0     
#> 2 P001          0.033         0.033 matched                           0     
#> 3 P001          0.066         0.066 matched                           0     
#> 4 P002          0             0     matched                           0     
#> 5 P002          0.05          0.06  outside_tolerance                 0.0100
#> # ℹ 1 more variable: face_sync_within_tolerance <lgl>
```

``` r

audit_gazepoint_face_sync(
  synced_strict,
  group_cols = "subject_id",
  min_matched_percent = 70,
  warning_matched_percent = 85,
  max_abs_diff_sec = 0.005
)$overview
#> # A tibble: 1 × 20
#>   n_groups n_rows n_matched matched_percent n_unmatched unmatched_percent
#>      <int>  <int>     <int>           <dbl>       <int>             <dbl>
#> 1        2      5         4              80           0                 0
#> # ℹ 14 more variables: n_outside_tolerance <int>,
#> #   outside_tolerance_percent <dbl>, n_missing_gaze_time <int>,
#> #   n_missing_gaze_frame <int>, n_unknown_status <int>,
#> #   n_within_tolerance <int>, within_tolerance_percent <dbl>,
#> #   mean_abs_diff_sec <dbl>, median_abs_diff_sec <dbl>, p95_abs_diff_sec <dbl>,
#> #   max_abs_diff_sec <dbl>, n_abs_diff_above_limit <int>,
#> #   face_sync_audit_status <chr>, message <chr>
```

## Recommended interpretation

Synchronisation status is a technical alignment diagnostic. It should
not be interpreted as evidence of facial expression validity or
emotional state.

Prefer cautious language such as:

- face-data alignment;
- matched facial-behaviour rows;
- synchronisation tolerance;
- timing difference;
- frame-level match;
- synchronisation coverage;
- rows requiring alignment review.

Avoid unsupported language such as:

- emotion detection from Gazepoint;
- hidden affect;
- psychological diagnosis;
- real-time emotional response inferred directly from classifier labels;
- micro-expression evidence.

## Suggested workflow position

A transparent workflow is:

1.  import external face-analysis CSVs with
    [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md);
2.  standardise face-analysis columns with
    [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md);
3.  audit face-data quality with
    [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md);
4.  synchronise face data with Gazepoint rows using
    [`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md);
5.  audit synchronisation quality with
    [`audit_gazepoint_face_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_sync.md);
6.  only then proceed to trial-window summaries, AOI-window summaries,
    or multimodal modelling.
