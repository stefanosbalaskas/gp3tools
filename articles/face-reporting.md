# External face-data reporting

This article demonstrates reporting helpers for external
facial-behaviour workflows used alongside Gazepoint data.

These helpers do **not** infer facial expressions from Gazepoint CSV
files. They also do **not** interpret facial behaviour as emotion. Their
purpose is narrower: to create reviewer-facing reporting checklists and
compact QC summaries for externally generated face-analysis data.

The current scope is:

1.  document whether face-analysis input data are available;
2.  check whether standardised face-analysis columns are present;
3.  summarise face-data quality-audit readiness;
4.  summarise synchronisation-audit readiness;
5.  summarise window-summary and reactivity readiness;
6.  summarise model-reporting readiness when models are used;
7.  preserve explicit interpretation cautions.

## Example external face-analysis data

``` r

face <- data.frame(
  participant_id = "P001",
  frame = 1:4,
  timestamp = c(0.000, 0.033, 0.066, 0.099),
  confidence = c(0.98, 0.96, 0.94, 0.92),
  success = c(1, 1, 1, 1),
  AU04_r = c(0.05, 0.06, 0.08, 0.09),
  AU12_r = c(0.20, 0.22, 0.25, 0.27),
  stringsAsFactors = FALSE
)

face_std <- standardize_gazepoint_face_columns(face)
```

## Quality audit

``` r

quality_audit <- audit_gazepoint_face_quality(face_std)

quality_audit$overview
#> # A tibble: 1 × 25
#>   n_groups n_rows n_valid valid_percent n_invalid invalid_percent
#>      <int>  <int>   <int>         <dbl>     <int>           <dbl>
#> 1        1      4       4           100         0               0
#> # ℹ 19 more variables: n_unknown_validity <int>,
#> #   unknown_validity_percent <dbl>, n_missing_confidence <int>,
#> #   confidence_missing_percent <dbl>, mean_confidence <dbl>,
#> #   median_confidence <dbl>, min_confidence <dbl>, max_confidence <dbl>,
#> #   n_success <int>, success_percent <dbl>, n_duplicate_frames <int>,
#> #   duplicate_frame_percent <dbl>, n_missing_time <int>,
#> #   n_nonpositive_time_steps <int>, max_time_gap_sec <dbl>, …
```

## Synchronisation audit

``` r

gaze <- data.frame(
  participant_id = "P001",
  time_sec = c(0.000, 0.033, 0.066, 0.099),
  AOI = c("claim", "claim", "logo", "evidence"),
  stringsAsFactors = FALSE
)

synced <- sync_gazepoint_face_data(
  gazepoint_data = gaze,
  face_data = face,
  by = c(participant_id = "participant_id"),
  gaze_time_col = "time_sec",
  tolerance_sec = 0.050
)

sync_audit <- audit_gazepoint_face_sync(synced)

sync_audit$overview
#> # A tibble: 1 × 20
#>   n_groups n_rows n_matched matched_percent n_unmatched unmatched_percent
#>      <int>  <int>     <int>           <dbl>       <int>             <dbl>
#> 1        1      4         4             100           0                 0
#> # ℹ 14 more variables: n_outside_tolerance <int>,
#> #   outside_tolerance_percent <dbl>, n_missing_gaze_time <int>,
#> #   n_missing_gaze_frame <int>, n_unknown_status <int>,
#> #   n_within_tolerance <int>, within_tolerance_percent <dbl>,
#> #   mean_abs_diff_sec <dbl>, median_abs_diff_sec <dbl>, p95_abs_diff_sec <dbl>,
#> #   max_abs_diff_sec <dbl>, n_abs_diff_above_limit <int>,
#> #   face_sync_audit_status <chr>, message <chr>
```

## Window summaries

``` r

windows <- data.frame(
  participant_id = "P001",
  window = c("baseline", "response"),
  window_start_sec = c(0.000, 0.066),
  window_end_sec = c(0.033, 0.120),
  stringsAsFactors = FALSE
)

face_windows <- summarize_gazepoint_face_windows(
  face_std,
  windows = windows,
  group_cols = "participant_id",
  window_label_col = "window",
  measure_cols = c("AU04_r", "AU12_r")
)

face_windows
#> # A tibble: 2 × 24
#>   participant_id face_window_id face_window_label window_start_sec
#>   <chr>                   <int> <chr>                        <dbl>
#> 1 P001                        1 baseline                     0    
#> 2 P001                        2 response                     0.066
#> # ℹ 20 more variables: window_end_sec <dbl>, n_rows <int>, n_used <int>,
#> #   n_valid <int>, n_invalid <int>, valid_percent <dbl>,
#> #   face_confidence_mean <dbl>, face_confidence_median <dbl>, AU04_r_n <int>,
#> #   AU04_r_mean <dbl>, AU04_r_median <dbl>, AU04_r_sd <dbl>, AU04_r_min <dbl>,
#> #   AU04_r_max <dbl>, AU12_r_n <int>, AU12_r_mean <dbl>, AU12_r_median <dbl>,
#> #   AU12_r_sd <dbl>, AU12_r_min <dbl>, AU12_r_max <dbl>
```

## Reactivity summary

``` r

face_reactivity <- summarize_gazepoint_face_reactivity(
  face_windows,
  baseline_window = "baseline",
  response_window = "response",
  group_cols = "participant_id",
  measure_cols = c("AU04_r", "AU12_r")
)

face_reactivity
#> # A tibble: 2 × 12
#>   participant_id measure statistic baseline_window response_window
#>   <chr>          <chr>   <chr>     <chr>           <chr>          
#> 1 P001           AU04_r  mean      baseline        response       
#> 2 P001           AU12_r  mean      baseline        response       
#> # ℹ 7 more variables: baseline_value <dbl>, response_value <dbl>,
#> #   reactivity <dbl>, absolute_reactivity <dbl>, percent_reactivity <dbl>,
#> #   n_baseline_windows <int>, n_response_windows <int>
```

## Optional model object

``` r

model_data <- data.frame(
  AU12_r_mean = c(0.10, 0.20, 0.30, 0.40),
  rating = c(3, 4, 5, 6)
)

face_model <- fit_gazepoint_face_window_lmm(
  model_data,
  outcome = "rating",
  predictors = "AU12_r_mean"
)

face_model$formula
#> rating ~ AU12_r_mean
#> <environment: 0x5638a44895d0>
```

## Reporting checklist

[`create_gazepoint_face_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_face_reporting_checklist.md)
creates a compact checklist for methods, supplements, or internal
review.

``` r

checklist <- create_gazepoint_face_reporting_checklist(
  face_data = face_std,
  quality_audit = quality_audit,
  sync_audit = sync_audit,
  window_summary = face_windows,
  reactivity_summary = face_reactivity,
  multimodal_model = face_model
)

checklist
#> # A tibble: 13 × 5
#>    section              item                      status evidence recommendation
#>    <chr>                <chr>                     <chr>  <chr>    <chr>         
#>  1 Input and provenance External face-analysis d… pass   4 row(s… Report the ex…
#>  2 Input and provenance Standardised face column… pass   Present… Report standa…
#>  3 Quality control      Face-data quality audit … pass   Class: … Use audit_gaz…
#>  4 Quality control      Face-data quality status… pass   n_rows=… Report valid-…
#>  5 Quality control      Quality issues are docum… pass   NA=NA    Document grou…
#>  6 Synchronisation      Face-data synchronisatio… pass   Class: … Use audit_gaz…
#>  7 Synchronisation      Synchronisation status i… pass   n_rows=… Report matchi…
#>  8 Window summaries     Face-window summary is a… pass   2 row(s… Report window…
#>  9 Window summaries     Window-summary coverage … pass   2 windo… Report n_rows…
#> 10 Reactivity summaries Baseline-to-response rea… pass   2 react… Define baseli…
#> 11 Modelling            Multimodal or face-windo… pass   Outcome… Report formul…
#> 12 Interpretation       Facial-behaviour variabl… review Manual … Use cautious …
#> 13 Interpretation       Unsupported claims are a… review Manual … Avoid claims …
```

The checklist records whether key workflow objects are present, what
evidence is available, and what should be reported.

## Markdown QC report

[`report_gazepoint_face_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_face_qc.md)
returns a compact markdown report by default.

``` r

face_report <- report_gazepoint_face_qc(
  face_data = face_std,
  quality_audit = quality_audit,
  sync_audit = sync_audit,
  window_summary = face_windows,
  reactivity_summary = face_reactivity,
  multimodal_model = face_model
)

cat(paste(face_report, collapse = "\n"))
#> # External facial-behaviour QC report
#> 
#> This report summarises technical reporting readiness for external facial-behaviour data used with Gazepoint workflows. It does not infer facial expressions or emotional states.
#> 
#> ## Reporting checklist
#> 
#> | section | item | status | evidence | recommendation |
#> | --- | --- | --- | --- | --- |
#> | Input and provenance | External face-analysis data are available | pass | 4 row(s), 16 column(s). | Report the external face-analysis tool, version, input files, and exported columns. |
#> | Input and provenance | Standardised face columns are available | pass | Present: face_frame, face_time_sec, face_confidence, face_success, face_valid. Missing: none. | Report standardised timing, frame, confidence, success, and validity fields where available. |
#> | Quality control | Face-data quality audit is available | pass | Class: gp3_face_quality_audit, list (expected class present). | Use audit_gazepoint_face_quality() before reporting facial-behaviour summaries. |
#> | Quality control | Face-data quality status is acceptable | pass | n_rows=4; valid_percent=100; face_quality_status=pass; max_time_gap_sec=0.033 | Report valid-row percentage, confidence coverage, duplicate-frame checks, and timing-gap checks. |
#> | Quality control | Quality issues are documented | pass | NA=NA | Document groups requiring review and explain any exclusions or sensitivity analyses. |
#> | Synchronisation | Face-data synchronisation audit is available | pass | Class: gp3_face_sync_audit, list (expected class present). | Use audit_gazepoint_face_sync() when face data are aligned to Gazepoint rows. |
#> | Synchronisation | Synchronisation status is acceptable | pass | n_rows=4; matched_percent=100; face_sync_audit_status=pass; max_abs_diff_sec=0 | Report matching method, tolerance, matched percentage, unmatched rows, and timing differences. |
#> | Window summaries | Face-window summary is available | pass | 2 row(s), 24 column(s). | Report window definitions, grouping variables, validity filtering, and summarised facial-behaviour measures. |
#> | Window summaries | Window-summary coverage is documented | pass | 2 window-summary row(s). n_used range: 2-2. | Report n_rows, n_used, valid_percent, confidence summaries, and measure summaries for each window. |
#> | Reactivity summaries | Baseline-to-response reactivity is available when used | pass | 2 reactivity row(s); measure(s): AU04_r, AU12_r. | Define baseline and response windows and report reactivity as response minus baseline. |
#> | Modelling | Multimodal or face-window model object is available when models are reported | pass | Outcome: rating; model rows: 4; class: gp3_face_window_lmm, gp3_multimodal_model, list. | Report formula, predictors, covariates, random effects, family, missing-data handling, and model sample size. |
#> | Interpretation | Facial-behaviour variables are not interpreted as direct emotion measures | review | Manual manuscript/reporting review required. | Use cautious language such as facial-behaviour measure, action-unit intensity, confidence, synchronisation coverage, or window-level feature. |
#> | Interpretation | Unsupported claims are avoided | review | Manual manuscript/reporting review required. | Avoid claims of true emotion detection, hidden affect, micro-expression evidence, diagnosis, or causal mechanism without design support. |
#> 
#> ## Face-data quality overview
#> 
#> | n_groups | n_rows | n_valid | valid_percent | n_invalid | invalid_percent | n_unknown_validity | unknown_validity_percent | n_missing_confidence | confidence_missing_percent | mean_confidence | median_confidence | min_confidence | max_confidence | n_success | success_percent | n_duplicate_frames | duplicate_frame_percent | n_missing_time | n_nonpositive_time_steps | max_time_gap_sec | median_time_step_sec | estimated_sampling_rate_hz | face_quality_status | message |
#> | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
#> | 1 | 4 | 4 | 100 | 0 | 0 | 0 | 0 | 0 | 0 | 0.95 | 0.95 | 0.92 | 0.98 | 4 | 100 | 0 | 0 | 0 | 0 | 0.033 | 0.033 | 30.3030303030303 | pass | Face-data quality passed the configured validity checks. |
#> 
#> ## Face-data quality issues
#> 
#> | issue | n_groups_affected | n_groups | threshold | status |
#> | --- | --- | --- | --- | --- |
#> | valid_percent_below_minimum | 0 | 1 | 70 | ok |
#> | valid_percent_below_warning | 0 | 1 | 85 | ok |
#> | unknown_validity | 0 | 1 |  | ok |
#> | duplicate_frames | 0 | 1 | 1 | ok |
#> | large_time_gaps |  | 1 |  | not_checked |
#> | missing_confidence | 0 | 1 |  | ok |
#> 
#> ## Synchronisation overview
#> 
#> | n_groups | n_rows | n_matched | matched_percent | n_unmatched | unmatched_percent | n_outside_tolerance | outside_tolerance_percent | n_missing_gaze_time | n_missing_gaze_frame | n_unknown_status | n_within_tolerance | within_tolerance_percent | mean_abs_diff_sec | median_abs_diff_sec | p95_abs_diff_sec | max_abs_diff_sec | n_abs_diff_above_limit | face_sync_audit_status | message |
#> | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
#> | 1 | 4 | 4 | 100 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 4 | 100 | 0 | 0 | 0 | 0 |  | pass | Face-data synchronisation passed the configured checks. |
#> 
#> ## Synchronisation issues
#> 
#> | issue | n_groups_affected | n_groups | threshold | status |
#> | --- | --- | --- | --- | --- |
#> | matched_percent_below_minimum | 0 | 1 | 70 | ok |
#> | matched_percent_below_warning | 0 | 1 | 85 | ok |
#> | unmatched_rows | 0 | 1 |  | ok |
#> | outside_tolerance_rows | 0 | 1 |  | ok |
#> | missing_gaze_time_rows | 0 | 1 |  | ok |
#> | missing_gaze_frame_rows | 0 | 1 |  | ok |
#> | large_time_differences |  | 1 |  | not_checked |
#> 
#> ## Window-summary overview
#> 
#> | participant_id | face_window_label | n_rows | n_used | valid_percent | face_confidence_mean |
#> | --- | --- | --- | --- | --- | --- |
#> | P001 | baseline | 2 | 2 | 100 | 0.97 |
#> | P001 | response | 2 | 2 | 100 | 0.93 |
#> 
#> ## Reactivity overview
#> 
#> | participant_id | measure | statistic | baseline_window | response_window | baseline_value | response_value | reactivity | absolute_reactivity |
#> | --- | --- | --- | --- | --- | --- | --- | --- | --- |
#> | P001 | AU04_r | mean | baseline | response | 0.055 | 0.085 | 0.03 | 0.03 |
#> | P001 | AU12_r | mean | baseline | response | 0.21 | 0.26 | 0.05 | 0.05 |
#> 
#> ## Model summary
#> 
#> | model_class | outcome | predictors | covariates | random_effects | n_rows_input | n_rows_model |
#> | --- | --- | --- | --- | --- | --- | --- |
#> | gp3_face_window_lmm, gp3_multimodal_model, list | rating | AU12_r_mean |  |  | 4 | 4 |
#> 
#> ## Interpretation cautions
#> 
#> - External facial-behaviour outputs should be reported as algorithmic or tool-derived measurements, not as direct evidence of emotional states.
#> - Report face-data quality, confidence, validity, synchronisation, and window coverage before interpreting model estimates.
#> - Avoid claims of true emotion detection, hidden affect, psychological diagnosis, micro-expression evidence, or causal mechanism unless the study design and validation evidence support them.
```

## Structured report list

The same information can be returned as a structured list.

``` r

face_report_list <- report_gazepoint_face_qc(
  face_data = face_std,
  quality_audit = quality_audit,
  sync_audit = sync_audit,
  window_summary = face_windows,
  reactivity_summary = face_reactivity,
  multimodal_model = face_model,
  output = "list"
)

names(face_report_list)
#> [1] "checklist"               "quality_overview"       
#> [3] "quality_issues"          "sync_overview"          
#> [5] "sync_issues"             "window_summary_overview"
#> [7] "reactivity_overview"     "model_summary"          
#> [9] "cautions"
```

``` r

face_report_list$model_summary
#> # A tibble: 1 × 7
#>   model_class          outcome predictors covariates random_effects n_rows_input
#>   <chr>                <chr>   <chr>      <chr>      <chr>                 <int>
#> 1 gp3_face_window_lmm… rating  AU12_r_me… ""         NA                        4
#> # ℹ 1 more variable: n_rows_model <int>
```

## Recommended reporting language

Prefer cautious language such as:

- external facial-behaviour data;
- face-analysis confidence;
- valid face-analysis rows;
- synchronisation coverage;
- timing difference;
- window-level facial-behaviour summary;
- baseline-to-response facial-behaviour change;
- multimodal association.

Avoid unsupported language such as:

- true emotion detection;
- hidden affect;
- psychological diagnosis;
- micro-expression evidence;
- emotional state inferred directly from an algorithmic label;
- causal mechanism without appropriate design support.

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
6.  summarise facial-behaviour variables within analysis windows with
    [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md);
7.  compute descriptive baseline-to-response changes with
    [`summarize_gazepoint_face_reactivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_reactivity.md);
8.  prepare multimodal analysis tables with
    [`prepare_gazepoint_multimodal_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_multimodal_data.md);
9.  fit explicit models where appropriate;
10. create checklist/reporting summaries with
    [`create_gazepoint_face_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_face_reporting_checklist.md)
    and
    [`report_gazepoint_face_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_face_qc.md).
