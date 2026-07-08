# End-to-end Gazepoint analysis workflow

This article provides a full workflow map for using `gp3tools` with
Gazepoint exports.

The goal is to show how a complete analysis can move from raw exports to
quality control, AOI summaries, pupil preprocessing, model-ready data,
and reporting outputs.

## Workflow overview

A typical workflow is:

1.  import Gazepoint exports;
2.  validate file structure and columns;
3.  audit gaze and pupil quality;
4.  prepare AOI and fixation summaries;
5.  preprocess pupil data and audit baselines;
6.  prepare model-ready tables;
7.  run sensitivity checks;
8.  create reporting checklists and reviewer-facing summaries.

## Skeleton pipeline

``` r

exports <- read_gazepoint_exports(
  path = 'path/to/gazepoint_exports'
)

validated <- validate_gazepoint_master(exports)

pupil_qc <- audit_gazepoint_pupil_gaps(validated$all_gaze)
baseline_qc <- audit_gazepoint_pupil_baseline(validated$all_gaze)

aoi_data <- prepare_gazepoint_aoi_glmm_data(validated$all_gaze)

fixation_data <- prepare_gazepoint_fixation_aligned_data(
  fixation_data = validated$fixations,
  trial_data = validated$summary
)

report <- create_gazepoint_reporting_checklist(
  pupil_qc = pupil_qc,
  baseline_qc = baseline_qc
)
```

## Reporting principle

The package is designed to make analysis decisions explicit. Reports
should describe the files imported, rows retained, quality-control
rules, excluded trials or participants, derived variables, modelling
assumptions, and sensitivity checks.

## Related articles

- Quality-control dashboard workflow
- Pupil preprocessing and baseline audit
- AOI modelling workflow
- Fixation, transitions, and scanpaths
- Model-readiness and sensitivity analysis
