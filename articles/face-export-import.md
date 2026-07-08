# External face-export import

This article demonstrates the first-stage helpers for importing external
facial-analysis exports into a `gp3tools` workflow.

These helpers do **not** infer facial expressions from Gazepoint CSV
files. They are designed for CSV outputs produced by external tools such
as OpenFace-style, py-feat-style, MediaPipe-style, FaceReader-style, or
generic frame-level facial-behaviour pipelines.

The current intended scope is:

1.  read external face-analysis CSV files;
2.  detect or record the likely source format;
3.  add file/session/participant metadata;
4.  standardise common timing, frame, confidence, and validity columns.

Synchronisation with Gazepoint timing, event windows, AOIs, pupil, GSR,
or HR/IBI should be handled in later workflow stages.

## Example external face-analysis export

Here we create a small OpenFace-style example. Real outputs may contain
many more columns, including landmarks, head pose, gaze vectors,
action-unit intensities, action-unit classifications, or
software-specific quality indicators.

``` r

face_csv <- tempfile(fileext = ".csv")

writeLines(
  c(
    "frame,timestamp,confidence,success,gaze_0_x,pose_Tx,pose_Rx,AU04_r,AU12_r,AU12_c",
    "1,0.000,0.98,1,0.10,1.5,0.10,0.05,0.20,1",
    "2,0.033,0.97,1,0.12,1.6,0.11,0.04,0.25,1",
    "3,0.066,0.45,1,0.15,1.7,0.12,0.07,0.30,1",
    "4,0.099,0.90,0,0.14,1.8,0.13,0.06,0.15,0"
  ),
  face_csv
)
```

## Read the external export

[`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md)
reads one file, several files, or a directory of CSV files. It adds
metadata columns and attempts to detect the likely source format when
`source = "auto"`.

``` r

face_raw <- read_gazepoint_face_export(
  face_csv,
  participant_id = "P001",
  session_id = "S001"
)

face_raw
#> # A tibble: 4 × 15
#>   gp3_face_file        gp3_face_path      gp3_face_source gp3_face_participant…¹
#>   <chr>                <chr>              <chr>           <chr>                 
#> 1 file36e225aac1a5.csv /tmp/Rtmp29Uo4x/f… openface        P001                  
#> 2 file36e225aac1a5.csv /tmp/Rtmp29Uo4x/f… openface        P001                  
#> 3 file36e225aac1a5.csv /tmp/Rtmp29Uo4x/f… openface        P001                  
#> 4 file36e225aac1a5.csv /tmp/Rtmp29Uo4x/f… openface        P001                  
#> # ℹ abbreviated name: ¹​gp3_face_participant_id
#> # ℹ 11 more variables: gp3_face_session_id <chr>, frame <int>, timestamp <dbl>,
#> #   confidence <dbl>, success <int>, gaze_0_x <dbl>, pose_Tx <dbl>,
#> #   pose_Rx <dbl>, AU04_r <dbl>, AU12_r <dbl>, AU12_c <int>
```

The returned object keeps the original external columns.

``` r

class(face_raw)
#> [1] "gp3_face_export" "tbl_df"          "tbl"             "data.frame"
```

``` r

attr(face_raw, "gp3_face_settings")
#> $source
#> [1] "auto"
#> 
#> $recursive
#> [1] TRUE
#> 
#> $trim_names
#> [1] TRUE
```

## Standardise common columns

[`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md)
adds common columns for later auditing and synchronisation.

``` r

face_std <- standardize_gazepoint_face_columns(face_raw)

face_std[, c(
  "face_source",
  "face_file",
  "participant_id",
  "face_frame",
  "face_time_sec",
  "face_time_ms",
  "face_confidence",
  "face_success",
  "face_valid",
  "face_pose_tx",
  "face_pose_rx",
  "AU04_r",
  "AU12_r",
  "AU12_c"
)]
#> # A tibble: 4 × 14
#>   face_source face_file     participant_id face_frame face_time_sec face_time_ms
#>   <chr>       <chr>         <chr>               <int>         <dbl>        <dbl>
#> 1 openface    file36e225aa… P001                    1         0                0
#> 2 openface    file36e225aa… P001                    2         0.033           33
#> 3 openface    file36e225aa… P001                    3         0.066           66
#> 4 openface    file36e225aa… P001                    4         0.099           99
#> # ℹ 8 more variables: face_confidence <dbl>, face_success <lgl>,
#> #   face_valid <lgl>, face_pose_tx <dbl>, face_pose_rx <dbl>, AU04_r <dbl>,
#> #   AU12_r <dbl>, AU12_c <int>
```

By default, `face_valid` uses a cautious rule based on available
confidence and success fields. If both are present, a row is valid only
when the face was successfully detected and confidence is at least the
selected threshold.

``` r

attr(face_std, "gp3_face_standardization")
#> $source
#> [1] "auto"
#> 
#> $detected_source
#> [1] "openface"
#> 
#> $participant_id_col
#> [1] "gp3_face_participant_id"
#> 
#> $frame_col
#> [1] "frame"
#> 
#> $time_col
#> [1] "timestamp"
#> 
#> $confidence_col
#> [1] "confidence"
#> 
#> $success_col
#> [1] "success"
#> 
#> $face_id_col
#> NULL
#> 
#> $file_col
#> [1] "gp3_face_file"
#> 
#> $confidence_threshold
#> [1] 0.8
```

## Generic CSVs

For non-standard exports, users can explicitly map the relevant columns.

``` r

generic_face <- data.frame(
  subject = c("P001", "P001", "P001"),
  video_frame = c(10, 11, 12),
  seconds = c(1.00, 1.03, 1.06),
  score = c(0.95, 0.70, 0.88),
  detected = c("yes", "yes", "no"),
  smile = c(0.10, 0.20, 0.05),
  brow_raise = c(0.30, 0.25, 0.10),
  stringsAsFactors = FALSE
)

generic_std <- standardize_gazepoint_face_columns(
  generic_face,
  source = "generic",
  participant_id_col = "subject",
  frame_col = "video_frame",
  time_col = "seconds",
  confidence_col = "score",
  success_col = "detected",
  confidence_threshold = 0.80
)

generic_std
#> # A tibble: 3 × 17
#>   face_source face_file participant_id face_id face_frame face_time_sec
#>   <chr>       <chr>     <chr>          <chr>        <int>         <dbl>
#> 1 generic     NA        P001           NA              10          1   
#> 2 generic     NA        P001           NA              11          1.03
#> 3 generic     NA        P001           NA              12          1.06
#> # ℹ 11 more variables: face_time_ms <dbl>, face_confidence <dbl>,
#> #   face_success <lgl>, face_valid <lgl>, subject <chr>, video_frame <dbl>,
#> #   seconds <dbl>, score <dbl>, detected <chr>, smile <dbl>, brow_raise <dbl>
```

## Recommended interpretation

The standardised table should be interpreted as a **facial-behaviour
data table**, not as direct access to internal emotional states.

Prefer cautious terms such as:

- facial action-unit intensity;
- face-detection confidence;
- head-pose estimate;
- smile-related movement;
- brow movement;
- facial-behaviour reactivity;
- algorithmic valence/arousal score, when supplied by an external tool.

Avoid unsupported claims such as:

- true emotion detection;
- micro-expression evidence;
- psychological diagnosis;
- participant emotional state inferred directly from a black-box label.

## Next workflow stages

The next stages, not covered in this article, are:

1.  quality auditing of face-detection success, confidence, occlusion,
    and missing frames;
2.  synchronisation with Gazepoint timing using participant ID,
    timestamp, frame, `CNT`, `TIMETICK`, TTL, or event logs;
3.  aggregation into stimulus, AOI, trial, or task-phase windows;
4.  cautious multimodal modelling with gaze, pupil, GSR, HR/IBI, and
    facial-behaviour features.
