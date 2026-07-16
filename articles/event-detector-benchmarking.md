# Benchmarking gaze-event detectors against reviewed intervals

## Purpose

Event detectors can disagree because they use different definitions,
thresholds, and model assumptions. `gp3tools` therefore separates two
questions:

1.  How do detector outputs agree with one another?
2.  How closely does each detector reproduce a specified set of reviewed
    or synthetic reference intervals?

The first question is addressed by
[`compare_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_event_detectors.md).
This article addresses the second with:

- [`create_gazepoint_event_review_template()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_event_review_template.md);
- [`benchmark_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/benchmark_gazepoint_event_detectors.md);
- [`summarise_gazepoint_event_detector_benchmark()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_event_detector_benchmark.md);
- [`plot_gazepoint_event_detector_benchmark()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_event_detector_benchmark.md).

The reviewed intervals are a reference standard for a particular audit,
not a claim that one event definition is universally correct.

## Synthetic trace with known fixation intervals

The example below creates three stable gaze periods separated by rapid
transitions. The reference table records the intervals used to generate
the trace.

``` r

set.seed(20260716)

sample_rate <- 100
sample_time <- seq(0, 2.39, by = 1 / sample_rate)

x <- c(
  rep(0.20, 80),
  seq(0.20, 0.50, length.out = 10),
  rep(0.50, 70),
  seq(0.50, 0.80, length.out = 10),
  rep(0.80, 70)
)

gaze <- data.frame(
  USER_ID = "P01",
  trial = "T01",
  TIME = sample_time,
  FPOGX = x + rnorm(length(x), sd = 0.0015),
  FPOGY = 0.50 + rnorm(length(x), sd = 0.0015)
)

reviewed <- data.frame(
  USER_ID = "P01",
  trial = "T01",
  review_event_id = 1:3,
  start_time = c(0.00, 0.90, 1.70),
  end_time = c(0.79, 1.59, 2.39),
  event_type = "fixation",
  review_status = "accepted"
)

head(gaze)
#>   USER_ID trial TIME     FPOGX     FPOGY
#> 1     P01   T01 0.00 0.1995298 0.4994030
#> 2     P01   T01 0.01 0.2008579 0.5018569
#> 3     P01   T01 0.02 0.1997964 0.5011622
#> 4     P01   T01 0.03 0.2031684 0.4993237
#> 5     P01   T01 0.04 0.2020606 0.4979690
#> 6     P01   T01 0.05 0.2007414 0.4983782
reviewed
#>   USER_ID trial review_event_id start_time end_time event_type review_status
#> 1     P01   T01               1        0.0     0.79   fixation      accepted
#> 2     P01   T01               2        0.9     1.59   fixation      accepted
#> 3     P01   T01               3        1.7     2.39   fixation      accepted
```

A simple trace plot helps verify that the reviewed intervals correspond
to the intended stable periods.

``` r

plot(
  gaze$TIME,
  gaze$FPOGX,
  type = "l",
  xlab = "Time (s)",
  ylab = "Horizontal gaze coordinate",
  main = "Synthetic fixation and transition structure"
)

for (i in seq_len(nrow(reviewed))) {
  rect(
    reviewed$start_time[i],
    par("usr")[3],
    reviewed$end_time[i],
    par("usr")[4],
    border = NA,
    density = 12,
    angle = 45
  )
}

lines(gaze$TIME, gaze$FPOGX)
```

![Synthetic gaze trace and reviewed fixation
intervals.](event-detector-benchmarking_files/figure-html/synthetic-trace-1.png)

Synthetic gaze trace and reviewed fixation intervals.

## Run native velocity, HMM, and optional external branches

The comparison layer runs all requested methods on the same sample-level
data. The optional external branch is recorded as disabled unless
explicitly enabled and
[eyetools](https://tombeesley.github.io/eyetools/) is installed.

``` r

comparison <- compare_gazepoint_event_detectors(
  gaze,
  trial_col = "trial",
  methods = c("velocity", "hmm", "eyetools"),
  velocity_thresholds = c(5, 10, 20),
  min_duration = 50,
  hmm_states = 3,
  run_optional_eyetools = FALSE
)

comparison$runs
#>       detector   family           status n_events
#> 1   velocity_5 velocity               ok        1
#> 2  velocity_10 velocity               ok        1
#> 3  velocity_20 velocity               ok        1
#> 4 hmm_3_states      hmm            error       NA
#> 5 eyetools_vti eyetools skipped_disabled       NA
#>                                                                     message
#> 1                                                                      <NA>
#> 2                                                                      <NA>
#> 3                                                                      <NA>
#> 4                     `HMM output` is missing required column(s): velocity.
#> 5 Set `run_optional_eyetools = TRUE` to run the optional external detector.
comparison$detector_summary
#>      detector   family threshold n_fixations mean_duration_ms
#> 1  velocity_5 velocity         5           1             2400
#> 2 velocity_10 velocity        10           1             2400
#> 3 velocity_20 velocity        20           1             2400
#>   median_duration_ms total_duration_ms
#> 1               2400              2400
#> 2               2400              2400
#> 3               2400              2400
```

Detector failures do not discard successful branches. The benchmark uses
only successful detector outputs retained in the comparison object.

## Benchmark against the synthetic reference

``` r

benchmark <- benchmark_gazepoint_event_detectors(
  comparison,
  reviewed,
  min_overlap = 0.50,
  time_unit = "seconds"
)

summarise_gazepoint_event_detector_benchmark(
  benchmark,
  level = "detector"
)
#>      detector   family threshold n_sequences n_reviewed n_detected
#> 1 velocity_10 velocity        10           1          3          1
#> 2 velocity_20 velocity        20           1          3          1
#> 3  velocity_5 velocity         5           1          3          1
#>   true_positive false_positive false_negative precision recall f1 mean_iou
#> 1             0              1              3         0      0  0       NA
#> 2             0              1              3         0      0  0       NA
#> 3             0              1              3         0      0  0       NA
#>   median_iou mean_onset_error_ms mean_abs_onset_error_ms mean_offset_error_ms
#> 1         NA                  NA                      NA                   NA
#> 2         NA                  NA                      NA                   NA
#> 3         NA                  NA                      NA                   NA
#>   mean_abs_offset_error_ms mean_duration_error_ms mean_abs_duration_error_ms
#> 1                       NA                     NA                         NA
#> 2                       NA                     NA                         NA
#> 3                       NA                     NA                         NA
#>   detection_count_bias
#> 1                   -2
#> 2                   -2
#> 3                   -2
```

The detector-level table reports:

- reviewed and detected event counts;
- true positives, false positives, and false negatives;
- precision, recall, and F1;
- matched-event intersection-over-union;
- onset, offset, and duration error in milliseconds;
- detected-minus-reviewed count bias.

Matching is one-to-one within each participant/trial sequence. Candidate
pairs are ordered by decreasing interval intersection-over-union and
retained only when neither event has already been matched.

## Inspect sequence-level and event-level diagnostics

``` r

head(
  summarise_gazepoint_event_detector_benchmark(
    benchmark,
    level = "sequence"
  )
)
#>   USER_ID trial    detector   family threshold n_reviewed n_detected
#> 1     P01   T01  velocity_5 velocity         5          3          1
#> 2     P01   T01 velocity_10 velocity        10          3          1
#> 3     P01   T01 velocity_20 velocity        20          3          1
#>   true_positive false_positive false_negative precision recall f1 mean_iou
#> 1             0              1              3         0      0  0       NA
#> 2             0              1              3         0      0  0       NA
#> 3             0              1              3         0      0  0       NA
#>   median_iou mean_onset_error_ms mean_abs_onset_error_ms mean_offset_error_ms
#> 1         NA                  NA                      NA                   NA
#> 2         NA                  NA                      NA                   NA
#> 3         NA                  NA                      NA                   NA
#>   mean_abs_offset_error_ms mean_duration_error_ms mean_abs_duration_error_ms
#> 1                       NA                     NA                         NA
#> 2                       NA                     NA                         NA
#> 3                       NA                     NA                         NA
#>   detection_count_bias min_overlap
#> 1                   -2         0.5
#> 2                   -2         0.5
#> 3                   -2         0.5

head(
  summarise_gazepoint_event_detector_benchmark(
    benchmark,
    level = "matches"
  )
)
#>  [1] USER_ID               trial                 detector             
#>  [4] family                threshold             detected_event_id    
#>  [7] review_event_id       detected_start_time   detected_end_time    
#> [10] reviewed_start_time   reviewed_end_time     iou                  
#> [13] onset_error_ms        abs_onset_error_ms    offset_error_ms      
#> [16] abs_offset_error_ms   duration_error_ms     abs_duration_error_ms
#> <0 rows> (or 0-length row.names)

summarise_gazepoint_event_detector_benchmark(
  benchmark,
  level = "errors"
)
#>    USER_ID trial    detector   family threshold     error_type
#> 1      P01   T01  velocity_5 velocity         5 false_positive
#> 2      P01   T01  velocity_5 velocity         5 false_negative
#> 3      P01   T01  velocity_5 velocity         5 false_negative
#> 4      P01   T01  velocity_5 velocity         5 false_negative
#> 5      P01   T01 velocity_10 velocity        10 false_positive
#> 6      P01   T01 velocity_10 velocity        10 false_negative
#> 7      P01   T01 velocity_10 velocity        10 false_negative
#> 8      P01   T01 velocity_10 velocity        10 false_negative
#> 9      P01   T01 velocity_20 velocity        20 false_positive
#> 10     P01   T01 velocity_20 velocity        20 false_negative
#> 11     P01   T01 velocity_20 velocity        20 false_negative
#> 12     P01   T01 velocity_20 velocity        20 false_negative
#>    detected_event_id review_event_id start_time end_time duration_ms
#> 1                  1              NA        0.0     2.39        2400
#> 2                 NA               1        0.0     0.79         790
#> 3                 NA               2        0.9     1.59         690
#> 4                 NA               3        1.7     2.39         690
#> 5                  1              NA        0.0     2.39        2400
#> 6                 NA               1        0.0     0.79         790
#> 7                 NA               2        0.9     1.59         690
#> 8                 NA               3        1.7     2.39         690
#> 9                  1              NA        0.0     2.39        2400
#> 10                NA               1        0.0     0.79         790
#> 11                NA               2        0.9     1.59         690
#> 12                NA               3        1.7     2.39         690
```

The unmatched-event table distinguishes false positives from false
negatives, which is useful when reviewing threshold sensitivity or
detector-specific failure modes.

## Plot benchmark diagnostics

``` r

plot_gazepoint_event_detector_benchmark(
  benchmark,
  plot = "f1"
)
```

![Event-level F1 scores against the reviewed
intervals.](event-detector-benchmarking_files/figure-html/benchmark-f1-1.png)

Event-level F1 scores against the reviewed intervals.

``` r

plot_gazepoint_event_detector_benchmark(
  benchmark,
  plot = "precision_recall"
)
```

![Precision and recall against the reviewed
intervals.](event-detector-benchmarking_files/figure-html/benchmark-pr-1.png)

Precision and recall against the reviewed intervals.

``` r

plot_gazepoint_event_detector_benchmark(
  benchmark,
  plot = "overlap"
)
```

![Mean interval overlap among matched
events.](event-detector-benchmarking_files/figure-html/benchmark-overlap-1.png)

Mean interval overlap among matched events.

``` r

plot_gazepoint_event_detector_benchmark(
  benchmark,
  plot = "timing_error"
)
```

![Absolute onset, offset, and duration
errors.](event-detector-benchmarking_files/figure-html/benchmark-timing-1.png)

Absolute onset, offset, and duration errors.

## Prepare manually reviewed data

Create a CSV-ready template from the sample-level data. One placeholder
row is created per sequence by default; reviewers can duplicate rows for
additional fixations.

``` r

review_template <- create_gazepoint_event_review_template(
  gaze,
  trial_col = "trial",
  rows_per_sequence = 1,
  reviewer = "reviewer_1"
)

review_template
#>   USER_ID trial review_event_id sequence_start sequence_end start_time end_time
#> 1     P01   T01               1              0         2.39         NA       NA
#>   event_type review_status   reviewer notes
#> 1   fixation       pending reviewer_1  <NA>
```

A real review workflow can keep the annotation file outside the package
repository:

``` r

write.csv(
  review_template,
  "C:/path/outside/repository/event_review.csv",
  row.names = FALSE
)

reviewed_real <- read.csv(
  "C:/path/outside/repository/event_review.csv",
  stringsAsFactors = FALSE
)

benchmark_real <- benchmark_gazepoint_event_detectors(
  comparison_real,
  reviewed_real,
  min_overlap = 0.50,
  time_unit = "seconds"
)
```

For each accepted event, fill `start_time` and `end_time`, preserve the
sequence identifiers, and set `review_status` to `"accepted"`. Pending
or rejected rows are excluded from the benchmark.

## Recommended reporting

A transparent methods supplement should report:

- how reviewed intervals were produced;
- the number of reviewers and any adjudication procedure;
- the overlap threshold used for matching;
- detector settings and sampling rate;
- precision, recall, F1, overlap, and timing error;
- the number and type of failed or skipped detector runs;
- whether conclusions changed across plausible detector definitions.

Benchmark results support sensitivity assessment. They should not be
used to claim that a detector uniquely identifies cognitive states or
exact mental-event onsets.
