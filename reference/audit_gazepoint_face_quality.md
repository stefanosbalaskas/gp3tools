# Audit external facial-behaviour data quality

Audits the quality of standardised external facial-behaviour data
imported with
[`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md)
and standardised with
[`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md).
The helper checks face-detection validity, confidence, success,
duplicate frame indices, and basic timing continuity. It does not infer
facial expressions or emotional states.

## Usage

``` r
audit_gazepoint_face_quality(
  data,
  group_cols = c("participant_id", "face_file"),
  confidence_threshold = 0.8,
  min_valid_percent = 70,
  warning_valid_percent = 85,
  max_time_gap_sec = NULL,
  max_duplicate_frame_percent = 1,
  standardize = TRUE
)
```

## Arguments

- data:

  A data frame returned by
  [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md),
  a data frame that can be standardised by that function, or a path to a
  CSV file readable by
  [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md).

- group_cols:

  Character vector of grouping columns for quality summaries. Columns
  not present in `data` are ignored. Use `NULL` for an overall-only
  audit.

- confidence_threshold:

  Minimum face-detection confidence used when standardising
  unstandardised data.

- min_valid_percent:

  Minimum valid-row percentage below which a group is marked as
  `"fail"`.

- warning_valid_percent:

  Valid-row percentage below which a group is marked as `"warn"` when it
  is still above `min_valid_percent`.

- max_time_gap_sec:

  Optional maximum allowed time gap in seconds. If supplied, groups with
  larger observed positive gaps are marked as `"warn"`.

- max_duplicate_frame_percent:

  Maximum tolerated percentage of duplicate non-missing frame indices
  before a group is marked as `"warn"`.

- standardize:

  Should unstandardised data be passed through
  [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md)
  before auditing?

## Value

A list with `overview`, `group_summary`, `issue_summary`, `data`, and
`settings`. The returned object has class `gp3_face_quality_audit`.

## Examples

``` r
face <- data.frame(
  frame = 1:3,
  timestamp = c(0, 0.033, 0.066),
  confidence = c(0.95, 0.90, 0.40),
  success = c(1, 1, 1),
  AU12_r = c(0.1, 0.2, 0.3)
)

audit_gazepoint_face_quality(face)
#> $overview
#> # A tibble: 1 × 25
#>   n_groups n_rows n_valid valid_percent n_invalid invalid_percent
#>      <int>  <int>   <int>         <dbl>     <int>           <dbl>
#> 1        1      3       2          66.7         1            33.3
#> # ℹ 19 more variables: n_unknown_validity <int>,
#> #   unknown_validity_percent <dbl>, n_missing_confidence <int>,
#> #   confidence_missing_percent <dbl>, mean_confidence <dbl>,
#> #   median_confidence <dbl>, min_confidence <dbl>, max_confidence <dbl>,
#> #   n_success <int>, success_percent <dbl>, n_duplicate_frames <int>,
#> #   duplicate_frame_percent <dbl>, n_missing_time <int>,
#> #   n_nonpositive_time_steps <int>, max_time_gap_sec <dbl>, …
#> 
#> $group_summary
#> # A tibble: 1 × 27
#>   face_quality_group       participant_id face_file n_rows n_valid valid_percent
#>   <chr>                    <chr>          <chr>      <int>   <int>         <dbl>
#> 1 participant_id=missing … missing        missing        3       2          66.7
#> # ℹ 21 more variables: n_invalid <int>, invalid_percent <dbl>,
#> #   n_unknown_validity <int>, unknown_validity_percent <dbl>,
#> #   n_missing_confidence <int>, confidence_missing_percent <dbl>,
#> #   mean_confidence <dbl>, median_confidence <dbl>, min_confidence <dbl>,
#> #   max_confidence <dbl>, n_success <int>, success_percent <dbl>,
#> #   n_duplicate_frames <int>, duplicate_frame_percent <dbl>,
#> #   n_missing_time <int>, n_nonpositive_time_steps <int>, …
#> 
#> $issue_summary
#> # A tibble: 6 × 5
#>   issue                       n_groups_affected n_groups threshold status     
#>   <chr>                                   <int>    <int>     <dbl> <chr>      
#> 1 valid_percent_below_minimum                 1        1        70 review     
#> 2 valid_percent_below_warning                 1        1        85 review     
#> 3 unknown_validity                            0        1        NA ok         
#> 4 duplicate_frames                            0        1         1 ok         
#> 5 large_time_gaps                            NA        1        NA not_checked
#> 6 missing_confidence                          0        1        NA ok         
#> 
#> $data
#> # A tibble: 3 × 15
#>   face_source face_file participant_id face_id face_frame face_time_sec
#>   <chr>       <chr>     <chr>          <chr>        <int>         <dbl>
#> 1 openface    NA        NA             NA               1         0    
#> 2 openface    NA        NA             NA               2         0.033
#> 3 openface    NA        NA             NA               3         0.066
#> # ℹ 9 more variables: face_time_ms <dbl>, face_confidence <dbl>,
#> #   face_success <lgl>, face_valid <lgl>, frame <int>, timestamp <dbl>,
#> #   confidence <dbl>, success <dbl>, AU12_r <dbl>
#> 
#> $settings
#> $settings$group_cols
#> [1] "participant_id" "face_file"     
#> 
#> $settings$confidence_threshold
#> [1] 0.8
#> 
#> $settings$min_valid_percent
#> [1] 70
#> 
#> $settings$warning_valid_percent
#> [1] 85
#> 
#> $settings$max_time_gap_sec
#> NULL
#> 
#> $settings$max_duplicate_frame_percent
#> [1] 1
#> 
#> $settings$standardize
#> [1] TRUE
#> 
#> 
#> attr(,"class")
#> [1] "gp3_face_quality_audit" "list"                  
```
