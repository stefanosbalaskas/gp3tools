# Paper-grade synthetic showcase

This article outlines a paper-grade synthetic demonstration workflow.

The purpose is to support reproducible examples, reviewer inspection,
and package-paper demonstrations without exposing private participant
data.

## What a paper-grade showcase should include

- realistic participant and trial structure;
- Gazepoint-like all-gaze, fixation, and summary exports;
- planned missingness and quality-control cases;
- AOI and pupil workflow branches;
- optional external face-data and biometric branches;
- reporting outputs suitable for supplementary materials.

## Example workflow

``` r

showcase <- run_gazepoint_workflow(
  export_dir = 'inst/extdata/gazepoint_realistic_demo_exports',
  output_dir = 'paper_showcase_outputs'
)

qc_report <- create_gazepoint_reporting_checklist(showcase)

face_report <- report_gazepoint_face_qc(
  face_data = showcase$face_data,
  quality_audit = showcase$face_quality,
  sync_audit = showcase$face_sync,
  window_summary = showcase$face_windows
)
```

## Reviewer-facing outputs

A polished showcase should produce import summaries, QC tables,
exclusion-decision summaries, analysis-ready tables, plots, and concise
reporting checklists.

## Reporting note

State clearly that the showcase is synthetic. Synthetic examples are
useful for software validation and documentation, but they are not
empirical evidence for substantive hypotheses.
