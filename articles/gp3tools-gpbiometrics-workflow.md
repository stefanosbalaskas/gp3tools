# gp3tools to gpbiometrics workflow

## Workflow

The integration contract is:

``` text
gp3tools gaze import and QC
  -> standardized gaze/master output
  -> gpbiometrics biometric import and preprocessing
  -> timestamp alignment
  -> AOI/event-contingent biometric summaries
  -> combined audit and report
```

The bridge does not infer emotion, stress, preference, cognition,
comprehension, health status, or diagnosis.

## Prepare gp3tools gaze output

``` r

gaze <- data.frame(
  USER_ID = rep("P01", 6),
  MEDIA_ID = rep("T01", 6),
  MSTIMER = seq(0, 100, by = 20),
  BPOGX = c(0.2, 0.3, 0.4, 0.7, 0.8, 0.9),
  BPOGY = c(0.4, 0.4, 0.5, 0.5, 0.6, 0.6),
  BPOGV = 1,
  AOI = c("claim", "claim", "claim", "evidence", "evidence", "evidence"),
  LPD = c(3.10, 3.12, 3.11, 3.15, 3.16, 3.17)
)

gaze_bridge <- prepare_gazepoint_gpbiometrics_bridge(gaze)
gaze_bridge
#> gp3tools gaze bridge for gpbiometrics
#>   Rows: 6
#>   Time unit: milliseconds
```

In a real project, `gaze` would normally be produced by
[`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_folder.md)
and
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md)
after gp3tools QC.

## Connect biometric output

The following data frame represents the stable table returned after
gpbiometrics import and preprocessing:

``` r

biometrics <- data.frame(
  participant_id = rep("P01", 6),
  trial_id = rep("T01", 6),
  time_s = seq(0.001, 0.101, by = 0.02),
  GSR = c(1.00, 1.02, 1.03, 1.06, 1.05, 1.07),
  HR = c(70, 70, 71, 72, 72, 73),
  event = c("trial_start", "", "", "cta", "", "trial_end")
)
```

A real gpbiometrics import can be substituted directly:

``` r

biometrics <- getExportedValue(
  "gpbiometrics",
  "import_gazepoint_biometrics"
)("path/to/biometric_export.csv")
```

## Synchronize and summarise

``` r

workflow <- run_gazepoint_gpbiometrics_workflow(
  gaze_bridge,
  biometrics,
  signal_cols = c("GSR", "HR"),
  event_col = "event",
  tolerance_s = 0.01
)

workflow
#> gp3tools-gpbiometrics workflow
#>   Engine: native_nearest_time
#>   Matched rows: 6
#>   Match rate: 100.00%
workflow$audit
#>                engine gaze_rows biometric_rows synchronized_rows matched_rows
#> 1 native_nearest_time         6              6                 6            6
#>   unmatched_rows matched_rate tolerance_ms median_absolute_difference_ms
#> 1              0            1           10                             1
#>   maximum_absolute_difference_ms signal_count summary_rows gp3tools_version
#> 1                              1            2           10            2.2.0
#>   gpbiometrics_version
#> 1                 <NA>
workflow$signal_summary
#>    participant_id trial_id      aoi       event signal n_rows n_nonmissing
#> 1             P01      T01    claim                GSR      2            2
#> 2             P01      T01    claim                 HR      2            2
#> 3             P01      T01    claim trial_start    GSR      1            1
#> 4             P01      T01    claim trial_start     HR      1            1
#> 5             P01      T01 evidence                GSR      1            1
#> 6             P01      T01 evidence                 HR      1            1
#> 7             P01      T01 evidence         cta    GSR      1            1
#> 8             P01      T01 evidence         cta     HR      1            1
#> 9             P01      T01 evidence   trial_end    GSR      1            1
#> 10            P01      T01 evidence   trial_end     HR      1            1
#>      mean          sd minimum maximum
#> 1   1.025 0.007071068    1.02    1.03
#> 2  70.500 0.707106781   70.00   71.00
#> 3   1.000          NA    1.00    1.00
#> 4  70.000          NA   70.00   70.00
#> 5   1.050          NA    1.05    1.05
#> 6  72.000          NA   72.00   72.00
#> 7   1.060          NA    1.06    1.06
#> 8  72.000          NA   72.00   72.00
#> 9   1.070          NA    1.07    1.07
#> 10 73.000          NA   73.00   73.00
```

The native engine performs an explicit nearest-timestamp match within
participant and trial. A study-specific or gpbiometrics adapter can be
supplied through the `adapter` argument and is tested through the same
return contract.

## Combined report

``` r

report <- create_gazepoint_cross_package_report(workflow)
cat(report, sep = "\n")
#> # gp3tools-gpbiometrics workflow audit
#> 
#> The cross-package workflow aligned 6 of 6 retained gaze rows (100.00%) using native_nearest_time with a 10.000 ms tolerance. 2 biometric signals were summarized within the available participant, trial, AOI, and event structure. These summaries describe recorded signal values and timing; they do not directly establish psychological or clinical states.
#> 
#> ## Alignment summary
#> 
#> - Engine: `native_nearest_time`
#> - Gaze rows: 6
#> - Biometric rows: 6
#> - Matched rows: 6
#> - Unmatched rows: 0
#> - Match rate: 100.00%
#> - Median absolute timing difference: 1.000 ms
#> - Maximum absolute timing difference: 1.000 ms
#> 
#> ## Interpretation guardrail
#> 
#> The synchronized signal summaries describe measured gaze allocation and physiological signal values within the specified timing and AOI structure. They do not, by themselves, establish emotion, stress, preference, cognition, comprehension, or diagnosis.
```

The report records match coverage and timing error before any
substantive interpretation.
