# Integrated signal-preprocessing workflow

## Scope

[`preprocess_gazepoint_signals()`](https://stefanosbalaskas.github.io/gp3tools/reference/preprocess_gazepoint_signals.md)
coordinates the lightweight signal-processing helpers in a fixed,
reviewable order. The workflow preserves original columns, adds
processed columns, records each operation, and returns blink and
fixation event tables separately.

The function is intended for transparent preprocessing and
methodological sensitivity analysis. It does not imply that one detector
or preprocessing configuration recovers a uniquely true sequence of eye
events.

``` r

library(gp3tools)
```

## Synthetic sample-level data

The example is synthetic and contains no participant records.

``` r

set.seed(1)

n <- 180L
time <- seq(0, by = 1 / 60, length.out = n)

signal <- data.frame(
  USER_ID = rep("P01", n),
  trial = rep(c("T01", "T02"), each = n / 2),
  TIME = time,
  FPOGX = c(
    rep(0.25, 55),
    seq(0.25, 0.75, length.out = 10),
    rep(0.75, 55),
    seq(0.75, 0.35, length.out = 10),
    rep(0.35, 50)
  ),
  FPOGY = 0.50 + stats::rnorm(n, 0, 0.002),
  LPupil = 3.2 + stats::rnorm(n, 0, 0.03),
  RPupil = 3.1 + stats::rnorm(n, 0, 0.03),
  stringsAsFactors = FALSE
)

signal$LPupil[35:40] <- NA_real_
signal$RPupil[35:40] <- NA_real_
signal$LPupil[130:134] <- NA_real_
signal$RPupil[130:134] <- NA_real_

utils::head(signal)
#>   USER_ID trial       TIME FPOGX     FPOGY   LPupil   RPupil
#> 1     P01   T01 0.00000000  0.25 0.4987471 3.163060 3.022230
#> 2     P01   T01 0.01666667  0.25 0.5003673 3.229517 3.139420
#> 3     P01   T01 0.03333333  0.25 0.4983287 3.206598 3.080934
#> 4     P01   T01 0.05000000  0.25 0.5031906 3.155982 3.087101
#> 5     P01   T01 0.06666667  0.25 0.5006590 3.215631 3.094920
#> 6     P01   T01 0.08333333  0.25 0.4983591 3.195237 3.118367
```

## Run the workflow

``` r

result <- preprocess_gazepoint_signals(
  data = signal,
  id_col = "USER_ID",
  group_cols = "trial",
  time_col = "TIME",
  x_col = "FPOGX",
  y_col = "FPOGY",
  left_pupil_col = "LPupil",
  right_pupil_col = "RPupil",
  pupil_mode = "mean",
  detect_blinks = TRUE,
  interpolate_blinks = TRUE,
  smooth_pupil = TRUE,
  smooth_coordinates = TRUE,
  downsample_factor = 2,
  detect_fixations = TRUE,
  blink_args = list(
    min_duration = 30,
    include_rapid_changes = FALSE
  ),
  fixation_args = list(
    vmax = 5,
    min_duration = 60
  )
)
```

## Decision log

Every requested operation is represented explicitly.

``` r

result$decision_log
#>   step                   operation requested  status input_rows output_rows
#> 1    1        binocular_pupil_mean      TRUE applied        180         180
#> 2    2             blink_detection      TRUE applied        180         180
#> 3    3         blink_interpolation      TRUE applied        180         180
#> 4    4             pupil_smoothing      TRUE applied        180         180
#> 5    5        coordinate_smoothing      TRUE applied        180         180
#> 6    6 velocity_fixation_detection      TRUE applied        180         180
#> 7    7                downsampling      TRUE applied        180          90
#>                                             details
#> 1                                   LPupil + RPupil
#> 2                               2 blink interval(s)
#> 3 Output pupil column: gp3_pupil_fused_blink_interp
#> 4               Output pupil column: pupil_smoothed
#> 5                        FPOGX_smooth, FPOGY_smooth
#> 6                               2 fixation event(s)
#> 7                             Aggregation factor: 2
```

The log records whether an operation was applied or skipped, its input
and output row counts, and a compact description of the resolved action.

## Processed data

``` r

names(result$data)
#>  [1] "USER_ID"                          "trial"                           
#>  [3] "TIME"                             "FPOGX"                           
#>  [5] "FPOGY"                            "LPupil"                          
#>  [7] "RPupil"                           "gp3_pupil_fused"                 
#>  [9] "blink_detected"                   "blink_id"                        
#> [11] "blink_reason"                     "blink_masked"                    
#> [13] "blink_interpolated"               "gp3_pupil_fused_blink_interp"    
#> [15] "pupil_smoothed"                   "pupil_smoothing_status"          
#> [17] "pupil_smoothing_window_n"         "pupil_smoothing_input_column"    
#> [19] "pupil_smoothing_time_column"      "pupil_smoothing_method"          
#> [21] "pupil_smoothing_align"            "pupil_smoothing_window_samples"  
#> [23] "pupil_smoothing_min_points"       "pupil_smoothing_preserve_missing"
#> [25] "FPOGX_smooth"                     "FPOGY_smooth"                    
#> [27] "n_samples_aggregated"             "downsample_factor"
utils::head(
  result$data[
    c(
      "USER_ID",
      "trial",
      "TIME",
      "LPupil",
      "RPupil",
      "gp3_pupil_fused",
      "gp3_pupil_fused_blink_interp",
      "pupil_smoothed",
      "FPOGX",
      "FPOGX_smooth"
    )
  ]
)
#> # A tibble: 6 × 10
#>   USER_ID trial    TIME LPupil RPupil gp3_pupil_fused gp3_pupil_fused_blink_in…¹
#>   <chr>   <chr>   <dbl>  <dbl>  <dbl>           <dbl>                      <dbl>
#> 1 P01     T01   0.00833   3.16   3.02            3.14                       3.09
#> 2 P01     T01   0.0417    3.21   3.08            3.13                       3.14
#> 3 P01     T01   0.075     3.22   3.09            3.16                       3.16
#> 4 P01     T01   0.108     3.24   3.12            3.16                       3.18
#> 5 P01     T01   0.142     3.19   3.08            3.13                       3.13
#> 6 P01     T01   0.175     3.19   3.09            3.15                       3.14
#> # ℹ abbreviated name: ¹​gp3_pupil_fused_blink_interp
#> # ℹ 3 more variables: pupil_smoothed <dbl>, FPOGX <dbl>, FPOGX_smooth <dbl>
```

Original columns remain available. Downsampling reduces rows only when
`downsample_factor` is larger than one.

## Blink intervals

``` r

result$blinks
#> # A tibble: 2 × 10
#>   USER_ID trial blink_id start_time end_time duration duration_ms n_samples
#>   <chr>   <chr>    <int>      <dbl>    <dbl>    <dbl>       <dbl>     <int>
#> 1 P01     T01          1      0.567     0.65    100         100           6
#> 2 P01     T02          1      2.15      2.22     83.3        83.3         5
#> # ℹ 2 more variables: reason <chr>, pupil_columns <chr>
result$diagnostics$blink_summary
#>    reason n_blinks mean_duration_ms max_duration_ms
#> 1 missing        2         91.66667             100
```

Blink detection is heuristic and should be reviewed against the
recording context, sampling rate, pupil scale, and missing-data pattern.

## Velocity-based fixation events

``` r

utils::head(result$fixations)
#> # A tibble: 2 × 14
#>   USER_ID trial fixation_id start_time end_time duration duration_ms n_samples
#>   <chr>   <chr>       <int>      <dbl>    <dbl>    <dbl>       <dbl>     <int>
#> 1 P01     T01             1        0       1.48     1500        1500        90
#> 2 P01     T02             1        1.5     2.98     1500        1500        90
#> # ℹ 6 more variables: mean_x <dbl>, mean_y <dbl>, median_velocity <dbl>,
#> #   max_velocity <dbl>, velocity_threshold <dbl>, algorithm <chr>
result$diagnostics$fixation_summary
#>   algorithm n_fixations mean_duration_ms median_duration_ms
#> 1      I-VT           2             1500               1500
```

The fixation table is computed from the full-resolution coordinate
series before optional downsampling.

## Diagnostic overview

``` r

result$diagnostics$overview
#>   original_rows full_resolution_processed_rows returned_rows original_columns
#> 1           180                            180            90                7
#>   returned_columns n_blinks n_fixations pupil_mode final_pupil_col
#> 1               28        2           2       mean  pupil_smoothed
#>   fixation_x_col fixation_y_col downsample_factor workflow_status
#> 1   FPOGX_smooth   FPOGY_smooth                 2              ok
result$diagnostics$signal_summary
#>                       stage n_rows finite_pupil finite_x finite_y
#> 1                  original    180           NA       NA       NA
#> 2 full_resolution_processed    180          180      180      180
#> 3                  returned     90           90       90       90
```

## Visual audit

``` r

plot(
  signal$TIME,
  signal$LPupil,
  type = "l",
  xlab = "Time",
  ylab = "Pupil",
  main = "Synthetic pupil preprocessing"
)

lines(
  result$data$TIME,
  result$data$pupil_smoothed,
  lwd = 2
)

legend(
  "topright",
  legend = c("Original left pupil", "Processed pupil"),
  lty = 1,
  lwd = c(1, 2),
  bty = "n"
)
```

![](signal-preprocessing-workflow_files/figure-html/pupil-plot-1.png)

``` r

plot(
  signal$TIME,
  signal$FPOGX,
  type = "l",
  xlab = "Time",
  ylab = "Horizontal gaze coordinate",
  main = "Synthetic coordinate smoothing"
)

lines(
  result$data$TIME,
  result$data$FPOGX_smooth,
  lwd = 2
)

legend(
  "topright",
  legend = c("Original", "Smoothed"),
  lty = 1,
  lwd = c(1, 2),
  bty = "n"
)
```

![](signal-preprocessing-workflow_files/figure-html/coordinate-plot-1.png)

## Alternative specifications

Use `pupil_mode = "regression"` for the cross-eye regression helper, or
`pupil_mode = "none"` with an explicit `pupil_col` when the input
already contains a selected pupil trace. Override lists expose the
underlying helper settings without changing workflow-managed data,
identifier, or output-column arguments.

Report the operation order, blink criteria, interpolation method and
maximum gap, smoothing windows, downsampling factor, coordinate scale,
velocity threshold, minimum fixation duration, and any sensitivity
specifications.
