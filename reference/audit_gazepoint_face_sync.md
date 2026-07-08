# Audit synchronisation between Gazepoint and external facial-behaviour data

Summarises the output of
[`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md)
by reporting matched, unmatched, outside-tolerance, and
missing-timing/frame rows. For nearest-time synchronisation, it also
reports absolute time-difference summaries. The helper audits alignment
quality only; it does not infer facial expressions or emotional states.

## Usage

``` r
audit_gazepoint_face_sync(
  data,
  group_cols = NULL,
  min_matched_percent = 70,
  warning_matched_percent = 85,
  max_abs_diff_sec = NULL
)
```

## Arguments

- data:

  A data frame returned by
  [`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md).

- group_cols:

  Optional character vector of grouping columns. Columns not present in
  `data` are ignored. Use `NULL` for an overall-only audit.

- min_matched_percent:

  Minimum percentage of rows that must have
  `face_sync_status == "matched"` for a group to pass.

- warning_matched_percent:

  Percentage below which a group is marked as `"warn"` when still above
  `min_matched_percent`.

- max_abs_diff_sec:

  Optional maximum allowed absolute synchronisation difference in
  seconds. Only evaluated when `face_sync_abs_diff_sec` is available.

## Value

A list with `overview`, `group_summary`, `issue_summary`, `data`, and
`settings`. The returned object has class `gp3_face_sync_audit`.

## Examples

``` r
gaze <- data.frame(
  subject_id = "P001",
  time_sec = c(0.00, 0.03, 0.07)
)

face <- data.frame(
  participant_id = "P001",
  frame = 1:3,
  timestamp = c(0.00, 0.033, 0.066),
  confidence = c(0.95, 0.94, 0.93),
  success = c(1, 1, 1)
)

synced <- sync_gazepoint_face_data(
  gaze,
  face,
  by = c(subject_id = "participant_id"),
  gaze_time_col = "time_sec"
)

audit_gazepoint_face_sync(synced)
#> $overview
#> # A tibble: 1 × 20
#>   n_groups n_rows n_matched matched_percent n_unmatched unmatched_percent
#>      <int>  <int>     <int>           <dbl>       <int>             <dbl>
#> 1        1      3         3             100           0                 0
#> # ℹ 14 more variables: n_outside_tolerance <int>,
#> #   outside_tolerance_percent <dbl>, n_missing_gaze_time <int>,
#> #   n_missing_gaze_frame <int>, n_unknown_status <int>,
#> #   n_within_tolerance <int>, within_tolerance_percent <dbl>,
#> #   mean_abs_diff_sec <dbl>, median_abs_diff_sec <dbl>, p95_abs_diff_sec <dbl>,
#> #   max_abs_diff_sec <dbl>, n_abs_diff_above_limit <int>,
#> #   face_sync_audit_status <chr>, message <chr>
#> 
#> $group_summary
#> # A tibble: 1 × 20
#>   face_sync_group n_rows n_matched matched_percent n_unmatched unmatched_percent
#>   <chr>            <int>     <int>           <dbl>       <int>             <dbl>
#> 1 overall              3         3             100           0                 0
#> # ℹ 14 more variables: n_outside_tolerance <int>,
#> #   outside_tolerance_percent <dbl>, n_missing_gaze_time <int>,
#> #   n_missing_gaze_frame <int>, n_unknown_status <int>,
#> #   n_within_tolerance <int>, within_tolerance_percent <dbl>,
#> #   mean_abs_diff_sec <dbl>, median_abs_diff_sec <dbl>, p95_abs_diff_sec <dbl>,
#> #   max_abs_diff_sec <dbl>, n_abs_diff_above_limit <int>,
#> #   face_sync_audit_status <chr>, message <chr>
#> 
#> $issue_summary
#> # A tibble: 7 × 5
#>   issue                         n_groups_affected n_groups threshold status     
#>   <chr>                                     <int>    <int>     <dbl> <chr>      
#> 1 matched_percent_below_minimum                 0        1        70 ok         
#> 2 matched_percent_below_warning                 0        1        85 ok         
#> 3 unmatched_rows                                0        1        NA ok         
#> 4 outside_tolerance_rows                        0        1        NA ok         
#> 5 missing_gaze_time_rows                        0        1        NA ok         
#> 6 missing_gaze_frame_rows                       0        1        NA ok         
#> 7 large_time_differences                       NA        1        NA not_checked
#> 
#> $data
#> # A tibble: 3 × 24
#>   subject_id time_sec .gp3_face_sync_gaze_row face_source face_file
#>   <chr>         <dbl>                   <int> <chr>       <chr>    
#> 1 P001           0                          1 generic     NA       
#> 2 P001           0.03                       2 generic     NA       
#> 3 P001           0.07                       3 generic     NA       
#> # ℹ 19 more variables: face_participant_id <chr>, face_id <chr>,
#> #   face_frame <int>, face_time_sec <dbl>, face_time_ms <dbl>,
#> #   face_confidence <dbl>, face_success <lgl>, face_valid <lgl>,
#> #   face_frame_1 <int>, face_timestamp <dbl>, face_confidence_1 <dbl>,
#> #   face_success_1 <dbl>, .gp3_face_sync_face_row <int>,
#> #   face_sync_method <chr>, face_sync_status <chr>, face_sync_diff_sec <dbl>,
#> #   face_sync_abs_diff_sec <dbl>, face_sync_within_tolerance <lgl>, …
#> 
#> $settings
#> $settings$group_cols
#> NULL
#> 
#> $settings$min_matched_percent
#> [1] 70
#> 
#> $settings$warning_matched_percent
#> [1] 85
#> 
#> $settings$max_abs_diff_sec
#> NULL
#> 
#> 
#> attr(,"class")
#> [1] "gp3_face_sync_audit" "list"               
```
