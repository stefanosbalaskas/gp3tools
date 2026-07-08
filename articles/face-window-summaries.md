# External face-data window summaries

This article demonstrates how to summarise externally generated
facial-behaviour data inside analysis windows.

These helpers do **not** infer facial expressions from Gazepoint CSV
files. They also do **not** interpret facial behaviour as emotion. Their
purpose is narrower: to summarise already imported, quality-audited, and
optionally synchronised external face-analysis variables within
transparent time windows.

The current scope is:

1.  summarise numeric facial-behaviour variables within windows;
2.  support separate window tables or already-labelled rows;
3.  optionally use only valid face-analysis rows;
4.  report row coverage, validity, confidence, and measure summaries;
5.  compute baseline-to-response reactivity as response minus baseline.

Modelling, trial-level inference, and emotion interpretation are later
workflow stages.

## Example face-analysis data

``` r

face <- data.frame(
  participant_id = c("P001", "P001", "P001", "P001", "P002", "P002", "P002", "P002"),
  trial_id = c(1, 1, 1, 1, 1, 1, 1, 1),
  face_time_sec = c(0.00, 0.05, 0.10, 0.15, 0.00, 0.05, 0.10, 0.15),
  face_confidence = c(0.95, 0.94, 0.93, 0.92, 0.96, 0.95, 0.94, 0.93),
  face_valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  AU04_r = c(0.05, 0.06, 0.12, 0.14, 0.04, 0.05, 0.10, 0.11),
  AU12_r = c(0.20, 0.21, 0.30, 0.34, 0.18, 0.20, 0.25, 0.28),
  stringsAsFactors = FALSE
)
```

## Define analysis windows

A separate window table can define baseline and response periods. The
grouping columns should be shared with the face-analysis table.

``` r

windows <- data.frame(
  participant_id = rep(c("P001", "P002"), each = 2),
  trial_id = rep(1, 4),
  window = rep(c("baseline", "response"), times = 2),
  window_start_sec = c(0.00, 0.10, 0.00, 0.10),
  window_end_sec = c(0.05, 0.20, 0.05, 0.20),
  stringsAsFactors = FALSE
)
```

## Summarise face data within windows

``` r

face_windows <- summarize_gazepoint_face_windows(
  data = face,
  windows = windows,
  group_cols = c("participant_id", "trial_id"),
  window_label_col = "window",
  measure_cols = c("AU04_r", "AU12_r"),
  require_valid = TRUE
)

face_windows
#> # A tibble: 4 × 25
#>   participant_id trial_id face_window_id face_window_label window_start_sec
#>   <chr>          <chr>             <int> <chr>                        <dbl>
#> 1 P001           1                     1 baseline                       0  
#> 2 P001           1                     2 response                       0.1
#> 3 P002           1                     3 baseline                       0  
#> 4 P002           1                     4 response                       0.1
#> # ℹ 20 more variables: window_end_sec <dbl>, n_rows <int>, n_used <int>,
#> #   n_valid <int>, n_invalid <int>, valid_percent <dbl>,
#> #   face_confidence_mean <dbl>, face_confidence_median <dbl>, AU04_r_n <int>,
#> #   AU04_r_mean <dbl>, AU04_r_median <dbl>, AU04_r_sd <dbl>, AU04_r_min <dbl>,
#> #   AU04_r_max <dbl>, AU12_r_n <int>, AU12_r_mean <dbl>, AU12_r_median <dbl>,
#> #   AU12_r_sd <dbl>, AU12_r_min <dbl>, AU12_r_max <dbl>
```

The output includes window identifiers, row counts, validity coverage,
confidence summaries, and measure-level summaries.

``` r

face_windows[, c(
  "participant_id",
  "trial_id",
  "face_window_label",
  "n_rows",
  "n_used",
  "valid_percent",
  "face_confidence_mean",
  "AU04_r_mean",
  "AU12_r_mean"
)]
#> # A tibble: 4 × 9
#>   participant_id trial_id face_window_label n_rows n_used valid_percent
#>   <chr>          <chr>    <chr>              <int>  <int>         <dbl>
#> 1 P001           1        baseline               2      2           100
#> 2 P001           1        response               2      2           100
#> 3 P002           1        baseline               2      2           100
#> 4 P002           1        response               2      2           100
#> # ℹ 3 more variables: face_confidence_mean <dbl>, AU04_r_mean <dbl>,
#> #   AU12_r_mean <dbl>
```

## Summarise already-labelled data

If the data already contain a window or phase column, the helper can
summarise those labels directly without a separate window table.

``` r

labelled_face <- data.frame(
  participant_id = c("P001", "P001", "P001", "P001"),
  window = c("baseline", "baseline", "response", "response"),
  face_time_sec = c(0.00, 0.05, 0.10, 0.15),
  face_valid = c(TRUE, TRUE, TRUE, TRUE),
  AU12_r = c(0.10, 0.20, 0.30, 0.40),
  stringsAsFactors = FALSE
)

summarize_gazepoint_face_windows(
  labelled_face,
  group_cols = "participant_id",
  window_label_col = "window",
  measure_cols = "AU12_r"
)
#> # A tibble: 2 × 18
#>   participant_id face_window_id face_window_label window_start_sec
#>   <chr>                   <int> <chr>                        <dbl>
#> 1 P001                        1 baseline                       0  
#> 2 P001                        2 response                       0.1
#> # ℹ 14 more variables: window_end_sec <dbl>, n_rows <int>, n_used <int>,
#> #   n_valid <int>, n_invalid <int>, valid_percent <dbl>,
#> #   face_confidence_mean <dbl>, face_confidence_median <dbl>, AU12_r_n <int>,
#> #   AU12_r_mean <dbl>, AU12_r_median <dbl>, AU12_r_sd <dbl>, AU12_r_min <dbl>,
#> #   AU12_r_max <dbl>
```

## Baseline-to-response reactivity

[`summarize_gazepoint_face_reactivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_reactivity.md)
computes response minus baseline from a window-summary table.

``` r

face_reactivity <- summarize_gazepoint_face_reactivity(
  face_windows,
  baseline_window = "baseline",
  response_window = "response",
  group_cols = c("participant_id", "trial_id"),
  measure_cols = c("AU04_r", "AU12_r"),
  statistic = "mean"
)

face_reactivity
#> # A tibble: 4 × 13
#>   participant_id trial_id measure statistic baseline_window response_window
#>   <chr>          <chr>    <chr>   <chr>     <chr>           <chr>          
#> 1 P001           1        AU04_r  mean      baseline        response       
#> 2 P001           1        AU12_r  mean      baseline        response       
#> 3 P002           1        AU04_r  mean      baseline        response       
#> 4 P002           1        AU12_r  mean      baseline        response       
#> # ℹ 7 more variables: baseline_value <dbl>, response_value <dbl>,
#> #   reactivity <dbl>, absolute_reactivity <dbl>, percent_reactivity <dbl>,
#> #   n_baseline_windows <int>, n_response_windows <int>
```

The resulting `reactivity` value is a descriptive difference score. It
should be interpreted as a change in a facial-behaviour measure, not as
evidence of an emotional state.

## Recommended interpretation

Prefer cautious language such as:

- facial-behaviour window summary;
- action-unit intensity within a window;
- valid face-analysis rows;
- baseline-to-response change;
- response-minus-baseline reactivity;
- window-level facial-behaviour feature.

Avoid unsupported language such as:

- true emotion detection;
- hidden affect;
- psychological diagnosis;
- micro-expression evidence;
- emotional state inferred directly from an algorithmic label.

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
8.  only then proceed to modelling or reporting.
