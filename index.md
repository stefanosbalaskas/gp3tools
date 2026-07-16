# gp3tools

[![R-CMD-check](https://github.com/stefanosbalaskas/gp3tools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stefanosbalaskas/gp3tools/actions/workflows/R-CMD-check.yaml)
[![GitHub
release](https://img.shields.io/github/v/release/stefanosbalaskas/gp3tools?label=GitHub%20release)](https://github.com/stefanosbalaskas/gp3tools/releases/latest)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21292384.svg)](https://doi.org/10.5281/zenodo.21292384)
[![Software
paper](https://img.shields.io/badge/Software%20paper-10.3390%2Fjemr19040076-blue.svg)](https://doi.org/10.3390/jemr19040076)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://stefanosbalaskas.github.io/gp3tools/LICENSE)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

[![R-CMD-check](https://github.com/stefanosbalaskas/gp3tools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stefanosbalaskas/gp3tools/actions/workflows/R-CMD-check.yaml)

`gp3tools` provides tools for importing, checking, summarising,
exporting, visualising, preprocessing, and modelling Gazepoint GP3 /
Gazepoint Analysis export files in R.

The package is designed for exported Gazepoint CSV files, especially:

- `User *_all_gaze.csv`
- `User *_fixations.csv`
- `Data_Summary_export_*.csv`

It supports common Gazepoint workflows, including:

- folder-level import and one-command workflow execution;
- sampling-rate checks and tracking-quality summaries;
- sample-level master-table creation, auditing, and validation;
- light and conservative pupil preprocessing;
- pupil preprocessing audits, reliability checks, interpolation
  sensitivity, and stimulus-luminance auditing;
- pupil-window confirmatory LMMs and model-family sensitivity checks;
- AOI entries, AOI windows, AOI denominators, and AOI-window GLMMs;
- AOI/fixation/transition feature extraction and time-varying transition
  matrices;
- fixation-, saccade-, and AOI-contingent alignment;
- pupil GAMMs, AOI GAMMs, gaze-position/PFE sensitivity GAMMs, and
  Growth Curve Analysis;
- cluster-based permutation tests and bootstrapped divergence-point
  estimation;
- standalone model diagnostics for GLMM/LMM/GLM and GAM/BAM models;
- nested model comparison, model-implied prediction plotting, and
  leave-one-unit model sensitivity;
- manuscript-ready fixed-effect, EMM, contrast, diagnostic, and
  model-export tables;
- real-data readiness gates, reporting checklists, and final
  analysis-decision audits;
- stimulus-layout QC, missingness reporting, task-phase coverage, and QC
  overview bundles;
- explicit trial and participant exclusion recommendations;
- offline gaze recalibration / drift correction when known target
  coordinates are available;
- diagnostic plots, lightweight HTML reports, and CSV exports;
- package-adapter exports for eyetrackingR-style, pupillometryR-style,
  gazer-style, and eyetools-style workflows;
- optional external gazeR and eyetools cross-check workflows.
- external facial-behaviour import, quality audit, synchronisation,
  window summaries, multimodal modelling, and reporting helpers for
  externally generated face-analysis outputs.

## Which workflow should I use?

Use the workflow that matches the research question and the stage of
analysis.

For a quick first pass through exported Gazepoint files, use
[`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md).
This imports a folder, checks file pairing, summarises sampling and
tracking quality, creates AOI summaries, writes CSV outputs, and
optionally saves diagnostic plots and an HTML report.

For analysis-ready sample-level data, create a master table with
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md),
audit it with
[`audit_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_master.md),
and validate it with
[`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md).

For pupil preprocessing, use the light branch when you need a
transparent minimal pipeline:

``` r

flag_gazepoint_pupil()
interpolate_gazepoint_pupil()
baseline_correct_gazepoint_pupil()
smooth_gazepoint_pupil()
summarise_gazepoint_pupil_windows()
```

Use the conservative artifact-cleaned branch when blink/trackloss
padding, pupil-speed artifacts, binocular disagreement, or stricter
preprocessing decisions are important:

``` r

create_gazepoint_preprocessing_registry()
flag_gazepoint_pupil_artifacts()
interpolate_gazepoint_pupil()
baseline_correct_gazepoint_pupil()
smooth_gazepoint_pupil()
```

Use pupil-window LMMs when the main hypothesis concerns predefined pupil
windows. Use pupil GAMMs, PFE-GAMMs, and GCA when the research question
concerns time-course shape.

Use AOI-window GLMMs when the main hypothesis concerns predefined AOI
time windows. Use AOI GAMMs when the research question concerns smooth
target-looking trajectories over time.

Use AOI-entry, fixation, and transition helpers when the analysis
concerns looking episodes, fixation summaries, AOI sequences, transition
matrices, or scanpath structure.

Use cluster-based permutation testing for time-course inference. Use
[`estimate_gazepoint_divergence_point()`](https://stefanosbalaskas.github.io/gp3tools/reference/estimate_gazepoint_divergence_point.md)
as complementary onset/sensitivity evidence, not as a replacement for
confirmatory model specification.

Use model diagnostics, model summaries, estimated marginal means, nested
model comparison, model-prediction plots, and leave-one-unit sensitivity
checks before manuscript reporting.

Use
[`check_gazepoint_real_data_readiness()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_real_data_readiness.md),
[`recommend_gazepoint_exclusions()`](https://stefanosbalaskas.github.io/gp3tools/reference/recommend_gazepoint_exclusions.md),
and
[`create_gazepoint_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_reporting_checklist.md)
before final interpretation. Use
[`summarize_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md),
[`segment_gazepoint_task_phases()`](https://stefanosbalaskas.github.io/gp3tools/reference/segment_gazepoint_task_phases.md),
[`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md),
and
[`report_gazepoint_qc_overview()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_qc_overview.md)
when preparing transparent data-coverage and QC-reporting summaries.

## Function map

`gp3tools` is organised around a complete Gazepoint analysis workflow:

| Task | Main helpers |
|----|----|
| Import Gazepoint exports | [`read_gazepoint()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint.md), [`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_folder.md), [`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_summary.md) |
| Create and validate master data | [`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md), [`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md), [`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md), [`audit_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_master.md) |
| Run a quick end-to-end workflow | [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md), [`summarise_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_workflow.md), [`create_gazepoint_report()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_report.md) |
| Check file pairing, sampling, and signal quality | [`check_gazepoint_file_pairs()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_file_pairs.md), [`check_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_sampling_rate.md), [`summarise_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_tracking_quality.md), [`audit_gazepoint_gaze_signal_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_gaze_signal_quality.md) |
| Review screen, stimulus-layout, and coordinate coverage | [`audit_gazepoint_screen_bounds()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_screen_bounds.md), [`harmonize_gazepoint_screen_coordinates()`](https://stefanosbalaskas.github.io/gp3tools/reference/harmonize_gazepoint_screen_coordinates.md), [`audit_gazepoint_aoi_screen_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_screen_coverage.md), [`summarize_gazepoint_coordinate_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_coordinate_coverage.md), [`plot_gazepoint_stimulus_layout_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_stimulus_layout_qc.md), [`add_gazepoint_polygon_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_polygon_aoi.md), [`add_gazepoint_dynamic_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_dynamic_aoi.md), [`audit_gazepoint_dynamic_aoi_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_dynamic_aoi_coverage.md) |
| Summarise missingness and task-phase coverage | [`summarize_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md), [`plot_gazepoint_missingness_profile()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_missingness_profile.md), [`report_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_missingness.md), [`segment_gazepoint_task_phases()`](https://stefanosbalaskas.github.io/gp3tools/reference/segment_gazepoint_task_phases.md), [`summarize_gazepoint_phase_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_phase_coverage.md), [`plot_gazepoint_phase_timeline()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_phase_timeline.md) |
| Collect QC outputs for reporting | [`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md), [`summarize_gazepoint_qc_status()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_qc_status.md), [`report_gazepoint_qc_overview()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_qc_overview.md), [`plot_gazepoint_qc_overview()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_qc_overview.md) |
| Preprocess pupil data | [`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md), [`flag_gazepoint_pupil_artifacts()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_artifacts.md), [`flag_gazepoint_pupil_hampel()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_hampel.md), [`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md), [`smooth_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_pupil.md), [`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md), [`preprocess_gazepoint_signals()`](https://stefanosbalaskas.github.io/gp3tools/reference/preprocess_gazepoint_signals.md) |
| Compare gaze-event detectors | [`compare_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_event_detectors.md), [`summarise_gazepoint_event_detector_agreement()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_event_detector_agreement.md), [`plot_gazepoint_event_detector_agreement()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_event_detector_agreement.md) |
| Audit pupil reliability and preprocessing choices | [`audit_gazepoint_pupil_gaps()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_gaps.md), [`audit_gazepoint_pupil_baseline()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_baseline.md), [`audit_gazepoint_pupil_drift()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_drift.md), [`audit_gazepoint_pupil_reliability()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_reliability.md), [`audit_gazepoint_pupil_overlap_risk()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_overlap_risk.md) |
| Summarise and model pupil outcomes | [`summarise_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil.md), [`summarise_gazepoint_pupil_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_windows.md), [`summarise_gazepoint_pupil_trial_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_trial_features.md), [`fit_gazepoint_pupil_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_window_lmm.md), [`fit_gazepoint_pupil_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_gamm.md) |
| Summarise AOI behaviour | [`summarise_gazepoint_aoi_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_windows.md), [`summarise_gazepoint_aoi_entries()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_entries.md), [`summarise_gazepoint_aoi_trial_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_trial_features.md), [`summarise_gazepoint_fixation_trials()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_fixation_trials.md) |
| Model AOI outcomes | [`prepare_gazepoint_aoi_glmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_glmm_data.md), [`fit_gazepoint_aoi_window_glmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_window_glmm.md), [`fit_gazepoint_aoi_model_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_model_sensitivity.md), [`prepare_gazepoint_aoi_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_gamm_data.md), [`fit_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_gamm.md) |
| Analyse sequences, transitions, and scanpaths | [`prepare_gazepoint_aoi_sequences()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_sequences.md), [`summarise_gazepoint_aoi_transitions()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_transitions.md), [`compute_gazepoint_aoi_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_transition_matrix.md), [`compute_gazepoint_time_varying_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_time_varying_transition_matrix.md), [`cluster_gazepoint_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/cluster_gazepoint_scanpaths.md), [`select_gazepoint_scanpath_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/select_gazepoint_scanpath_clusters.md), [`extract_gazepoint_representative_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/extract_gazepoint_representative_scanpaths.md), [`plot_gazepoint_scanpath_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_scanpath_clusters.md) |
| Run time-course and advanced sensitivity analyses | [`fit_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_gca.md), [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md), [`estimate_gazepoint_divergence_point()`](https://stefanosbalaskas.github.io/gp3tools/reference/estimate_gazepoint_divergence_point.md), [`run_gazepoint_model_leave_one_out()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_model_leave_one_out.md) |
| Prepare reporting and exclusion decisions | [`check_gazepoint_real_data_readiness()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_real_data_readiness.md), [`recommend_gazepoint_exclusions()`](https://stefanosbalaskas.github.io/gp3tools/reference/recommend_gazepoint_exclusions.md), [`create_gazepoint_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_reporting_checklist.md), [`create_gazepoint_analysis_decision_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_analysis_decision_audit.md) |
| Export tables and outputs | [`export_gazepoint_tables()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_tables.md), [`export_gazepoint_model_tables()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_model_tables.md), [`export_gazepoint_master_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_master_audit.md), [`save_gazepoint_plots()`](https://stefanosbalaskas.github.io/gp3tools/reference/save_gazepoint_plots.md) |
| Prepare data for other R eye-tracking packages | [`prepare_gazepoint_eyetrackingr_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_eyetrackingr_data.md), [`prepare_gazepoint_pupillometryr_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupillometryr_data.md), [`prepare_gazepoint_gazer_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gazer_data.md), [`prepare_gazepoint_eyetools_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_eyetools_data.md) |
| Work with external facial-behaviour outputs | [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md), [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md), [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md), [`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md), [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md), [`fit_gazepoint_multimodal_response_model()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_multimodal_response_model.md), [`create_gazepoint_face_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_face_reporting_checklist.md), [`report_gazepoint_face_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_face_qc.md) |

Most users should start with
[`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md)
for a quick folder-level pass, then move to the master-table, pupil,
AOI, modelling, and reporting helpers as needed.

### Statistical, sequence, simulation, and reporting extensions

Version 2.0.0 also includes an optional statistical-extension layer for
advanced, audit-friendly workflows:

- **AOI sequence structure and uncertainty:**
  [`compute_gazepoint_aoi_entropy()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_entropy.md),
  [`compute_gazepoint_aoi_sequence_metrics()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_sequence_metrics.md),
  [`compute_gazepoint_sequence_complexity()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_sequence_complexity.md),
  [`compute_gazepoint_sequence_distance()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_sequence_distance.md),
  [`compute_gazepoint_scanpath_similarity()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_scanpath_similarity.md),
  and
  [`flag_gazepoint_sequence_anomalies()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_sequence_anomalies.md).
- **Fixation, scanpath, and transition diagnostics:**
  [`audit_gazepoint_fixation_reliability()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_fixation_reliability.md),
  [`compute_gazepoint_saccade_metrics()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_saccade_metrics.md),
  [`plot_gazepoint_scanpath()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_scanpath.md),
  [`summarise_gazepoint_markovchain()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_markovchain.md),
  [`summarise_gazepoint_semimarkov()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_semimarkov.md),
  [`compute_gazepoint_sequence_recurrence()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_sequence_recurrence.md),
  and
  [`compute_gazepoint_transition_network_metrics()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_transition_network_metrics.md).
- **Time-course, modelling, and reporting helpers:**
  [`bootstrap_gazepoint_timecourse()`](https://stefanosbalaskas.github.io/gp3tools/reference/bootstrap_gazepoint_timecourse.md),
  [`plot_gazepoint_time_varying_effect()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_time_varying_effect.md),
  [`plot_gazepoint_model_residuals()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_model_residuals.md),
  and
  [`report_gazepoint_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_multiverse.md).
- **QC coverage and reporting bundles:**
  [`audit_gazepoint_aoi_screen_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_screen_coverage.md),
  [`summarize_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md),
  [`segment_gazepoint_task_phases()`](https://stefanosbalaskas.github.io/gp3tools/reference/segment_gazepoint_task_phases.md),
  [`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md),
  and
  [`report_gazepoint_qc_overview()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_qc_overview.md).
- **Simulation and optional interoperability:**
  [`simulate_gazepoint_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/simulate_gazepoint_data.md),
  [`export_gazepoint_to_bids()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_to_bids.md),
  [`fit_gazepoint_aoi_brms()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_brms.md),
  [`prepare_gazepoint_traminer_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_traminer_data.md),
  [`detect_gazepoint_fixations_ivt()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_fixations_ivt.md),
  and
  [`launch_gazepoint_qc_dashboard()`](https://stefanosbalaskas.github.io/gp3tools/reference/launch_gazepoint_qc_dashboard.md).

The rendered site includes a plot-first article for these additions:
`articles/statistical-extensions-plots.html`.

## Citation

When `gp3tools` supports a publication, cite the peer-reviewed software
paper and report the package version used in the analysis.

### Peer-reviewed software paper

> Balaskas, S. (2026). gp3tools: An R Package for Reproducible Analysis
> and Reporting of Gazepoint GP3 Eye-Tracking Exports. Journal of Eye
> Movement Research, 19(4), 76. <https://doi.org/10.3390/jemr19040076>

### R package

> Balaskas, S. (2026). `gp3tools`: Import, Inspect, Analyse, and Report
> Gazepoint GP3 Exports. R package version 2.0.1.
> <https://github.com/stefanosbalaskas/gp3tools>

The complete citation entries are available directly from R:

``` r

citation("gp3tools")
```

The previously released 2.0.0 software archive remains available at
<https://doi.org/10.5281/zenodo.21292384>. A new version-specific
archive DOI should be added after the 2.0.1 GitHub/Zenodo release is
created.

## Installation

During development, install the package locally from the package project
folder:

``` r

devtools::install(reload = FALSE)
```

Then restart RStudio and load the package:

``` r

library(gp3tools)
```

Check that the package loads correctly:

``` r

packageVersion("gp3tools")
```

## 1. Basic Gazepoint folder workflow

Use this workflow when you want a quick reproducible pass from a folder
of Gazepoint exports to quality summaries, AOI summaries, exported CSV
files, diagnostic plots, and an optional HTML report.

The workflow expects exported Gazepoint files such as:

``` r
User 1_all_gaze.csv
User 1_fixations.csv
Data_Summary_export_*.csv
```

Set the folder containing the Gazepoint exports and an output folder for
results:

``` r

export_dir <- "C:/Users/YourName/Desktop/gp3_test_exports"
output_dir <- "C:/Users/YourName/Desktop/gp3_outputs"
```

Run the complete folder workflow:

``` r

library(gp3tools)

results <- run_gazepoint_workflow(
  export_dir = export_dir,
  output_dir = output_dir,
  prefix = "study1",
  save_plots = TRUE,
  create_report = TRUE
)
```

The returned object is a named list containing imported files, quality
summaries, AOI summaries, exported file paths, saved plot paths, and
optional report path:

``` r

names(results)

results$all_gaze
results$all_fix
results$sampling
results$quality
results$flagged_quality
results$aoi_table
results$written_files
results$written_plots
results$report_file
```

Create a compact one-row summary of the workflow:

``` r

workflow_summary <- summarise_gazepoint_workflow(results)

print(workflow_summary, width = Inf)
```

The workflow summary is useful for quickly checking:

``` r

workflow_summary$n_all_gaze_rows
workflow_summary$n_all_fix_rows
workflow_summary$n_sampling_rows
workflow_summary$n_quality_rows
workflow_summary$n_aoi_rows
workflow_summary$n_flagged_recordings
workflow_summary$n_written_files
workflow_summary$n_written_plots
workflow_summary$report_created
```

When `output_dir` is provided, the workflow writes standard CSV files
such as:

``` r

study1_sampling.csv
study1_quality.csv
study1_flagged_quality.csv
study1_aoi_table.csv
```

When `save_plots = TRUE`, the workflow also saves diagnostic plots such
as:

``` r

study1_tracking_quality_plot.png
study1_sampling_rate_plot.png
```

When `create_report = TRUE`, the workflow creates a lightweight HTML
report.

### Manual version of the same workflow

The same workflow can be run step by step when you want more control.

Read all all-gaze files:

``` r

all_gaze <- read_gazepoint_folder(
  export_dir,
  pattern = "_all_gaze\\.csv$"
)

dim(all_gaze)
```

Read all fixation files:

``` r

all_fix <- read_gazepoint_folder(
  export_dir,
  pattern = "_fixations\\.csv$"
)

dim(all_fix)
```

Check that all-gaze and fixation exports are correctly paired:

``` r

file_pairs <- check_gazepoint_file_pairs(export_dir)

file_pairs
```

Estimate sampling rate by participant and stimulus:

``` r

sampling <- check_sampling_rate(
  all_gaze,
  group_cols = c("USER_FILE", "MEDIA_ID")
)

sampling
```

For Gazepoint GP3 60 Hz exports, the estimated frequency should usually
be close to 60 Hz.

Summarise tracking quality:

``` r

quality <- summarise_tracking_quality(
  all_gaze,
  group_cols = c("USER_FILE", "MEDIA_ID")
)

quality
```

Flag recordings requiring review:

``` r

flagged_quality <- flag_tracking_quality(
  quality = quality,
  sampling = sampling,
  min_gaze_valid_pct = 70,
  min_pupil_valid_pct = 70,
  expected_hz = 60,
  hz_tolerance = 5
)

flagged_quality
```

Show only recordings that require review:

``` r

dplyr::filter(flagged_quality, review_required)
```

Create a basic AOI summary from all-gaze and fixation exports:

``` r

aoi_table <- summarise_gazepoint_aoi(
  gaze_data = all_gaze,
  fixation_data = all_fix
)

aoi_table
```

The AOI table includes one row per user, media stimulus, and AOI, with
metrics such as:

- sample-level time to first view;
- sample count;
- approximate sample-based viewed time;
- fixation count;
- total fixation duration;
- mean fixation duration;
- fixation-based time to first fixation.

Write standard output tables:

``` r

written_files <- write_gazepoint_outputs(
  sampling = sampling,
  quality = quality,
  flagged_quality = flagged_quality,
  aoi_table = aoi_table,
  output_dir = output_dir,
  prefix = "gazepoint_test"
)

written_files
```

Create standard diagnostic plots:

``` r

plot_tracking_quality(flagged_quality)

plot_sampling_rate(sampling)
```

Save standard diagnostic plots:

``` r

written_plots <- save_gazepoint_plots(
  flagged_quality = flagged_quality,
  sampling = sampling,
  output_dir = output_dir,
  prefix = "gazepoint_test"
)

written_plots
```

The basic workflow is useful for first-pass inspection. For advanced
preprocessing and modelling, continue by creating a master table.

## 2. Master-table creation and validation

For advanced preprocessing, AOI modelling, pupil modelling, time-course
analysis, and reporting, first convert the imported Gazepoint all-gaze
export into a standard sample-level master table.

Start from the all-gaze data imported by
[`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md)
or
[`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_folder.md):

``` r

all_gaze <- results$all_gaze
```

Create the master table:

``` r

master <- create_gazepoint_master(all_gaze)

dplyr::glimpse(master)
```

The master table standardises key fields when available, including:

- participant ID;
- media/stimulus ID;
- trial identifiers;
- time;
- gaze coordinates;
- pupil values;
- gaze-validity and pupil-validity fields;
- AOI state;
- fixation-related fields;
- event labels;
- response-related columns;
- screen and metadata fields.

Inspect the main columns:

``` r

names(master)

dplyr::glimpse(
  dplyr::select(
    master,
    dplyr::any_of(c(
      "subject",
      "MEDIA_ID",
      "trial_global",
      "time",
      "x",
      "y",
      "pupil",
      "aoi_current",
      "event_label"
    ))
  )
)
```

Audit the master table:

``` r

master_audit <- audit_gazepoint_master(master)

master_audit$overview
master_audit$subject_summary
master_audit$media_summary
master_audit$aoi_summary
master_audit$pupil_summary
master_audit$coordinate_summary
```

Use the audit to check whether the imported data contain the expected
participants, stimuli, AOI states, pupil values, and gaze-coordinate
availability.

Validate the master table before preprocessing or modelling:

``` r

validation <- validate_gazepoint_master(master)

validation$summary
validation$checks
```

Validation is a formal gate. Use it before pupil preprocessing, AOI
modelling, time-course analysis, or final reporting.

Export the master table and audit outputs:

``` r

master_outputs <- export_gazepoint_master_audit(
  master,
  output_dir = output_dir,
  prefix = "study1_master"
)

master_outputs
```

This writes the master table, audit tables, and validation outputs to
CSV files.

### Reading official Gazepoint Analysis summary exports

Official Gazepoint Analysis summary exports can be read separately:

``` r

summary_file <- file.path(
  export_dir,
  "Data_Summary_export_06-11-26-23.50.26.csv"
)

summary <- read_gazepoint_summary(summary_file)

summary$metadata
summary$aoi_summary
summary$aoi_by_user
```

Use
[`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_summary.md)
when you need official Gazepoint Analysis summary values. Metrics
recomputed from all-gaze and fixation files may not always exactly
reproduce Gazepoint’s internal calculations.

For transparent reproducible analysis from exported rows, continue with
the sample-level master table and the preprocessing/modelling helpers.

## 3. Pupil preprocessing: light branch

Use the light branch when you need a transparent, minimal pupil
preprocessing pipeline. This branch is appropriate for early inspection,
teaching examples, or studies where conservative artifact padding is not
required.

First summarise pupil availability:

``` r

pupil_summary <- summarise_gazepoint_pupil(master)

pupil_summary$overall
pupil_summary$subject_summary
pupil_summary$media_summary
```

Flag missing, non-finite, implausible, and IQR-outlying pupil samples:

``` r

flagged_pupil <- flag_gazepoint_pupil(
  master,
  pupil_col = "pupil"
)

attr(flagged_pupil, "gp3_pupil_flag_overview")
```

Interpolate short internal pupil gaps:

``` r

interpolated_pupil <- interpolate_gazepoint_pupil(
  flagged_pupil,
  pupil_col = "pupil_for_preprocessing",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

attr(interpolated_pupil, "gp3_pupil_interpolation_overview")
```

Apply baseline correction:

``` r

baseline_corrected <- baseline_correct_gazepoint_pupil(
  interpolated_pupil,
  pupil_col = "pupil_interpolated",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  baseline_window = c(0, 200),
  min_baseline_samples = 1
)

attr(baseline_corrected, "gp3_pupil_baseline_overview")
```

Smooth the baseline-corrected pupil signal:

``` r

smoothed_pupil <- smooth_gazepoint_pupil(
  baseline_corrected,
  pupil_col = "pupil_baseline_corrected",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  window_samples = 5,
  method = "mean",
  align = "center",
  min_points = 2
)

attr(smoothed_pupil, "gp3_pupil_smoothing_overview")
```

Summarise the processed pupil signal into analysis windows:

``` r

pupil_windows <- summarise_gazepoint_pupil_windows(
  smoothed_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  windows = c(0, 500, 1000, 2000, 5000),
  group_cols = c("subject", "MEDIA_ID", "trial_global", "condition"),
  min_valid_samples = 1
)

pupil_windows
```

The light branch gives a simple path from raw pupil values to
window-level summaries. For stricter cleaning, use the conservative
artifact-cleaned branch.

## 4. Pupil preprocessing: conservative artifact-cleaned branch

Use the conservative branch when blink/trackloss padding, pupil-speed
artifacts, binocular disagreement, or stricter preprocessing decisions
are important.

Create a reusable preprocessing registry:

``` r

registry <- create_gazepoint_preprocessing_registry(
  blink_padding_ms = 50,
  interpolation_max_gap_ms = 150,
  smoothing_window_samples = 5,
  baseline_window = c(0, 200)
)

registry
```

Apply conservative artifact flagging:

``` r

artifact_pupil <- flag_gazepoint_pupil_artifacts(
  master,
  registry = registry,
  pupil_col = "pupil",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

attr(artifact_pupil, "gp3_pupil_artifact_overview")
```

Interpolate short internal gaps after artifact removal:

``` r

interpolated_artifact_pupil <- interpolate_gazepoint_pupil(
  artifact_pupil,
  pupil_col = "pupil_clean",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)
```

Apply baseline correction:

``` r

baseline_artifact_pupil <- baseline_correct_gazepoint_pupil(
  interpolated_artifact_pupil,
  pupil_col = "pupil_interpolated",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  baseline_window = c(0, 200),
  min_baseline_samples = 1
)
```

Smooth the processed pupil signal:

``` r

smoothed_artifact_pupil <- smooth_gazepoint_pupil(
  baseline_artifact_pupil,
  pupil_col = "pupil_baseline_corrected",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  window_samples = 5,
  method = "mean",
  align = "center",
  min_points = 2
)
```

Create pupil-window summaries from the conservative branch:

``` r

pupil_windows_conservative <- summarise_gazepoint_pupil_windows(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  windows = c(0, 500, 1000, 2000, 5000),
  group_cols = c("subject", "MEDIA_ID", "trial_global", "condition"),
  min_valid_samples = 1
)

pupil_windows_conservative
```

The conservative branch is recommended when pupil preprocessing
decisions may affect interpretation or when the study will be reported
in a manuscript.

## 5. Pupil audits and reliability checks

After preprocessing, run pupil audits to document missingness,
interpolation, baseline quality, imbalance, drift, event-response
overlap, reliability, and sensitivity to preprocessing decisions.

Audit interpolation gaps:

``` r

gap_audit <- audit_gazepoint_pupil_gaps(
  interpolated_artifact_pupil,
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

gap_audit$overview
gap_audit$gap_summary
```

Audit baseline quality:

``` r

baseline_audit <- audit_gazepoint_pupil_baseline(
  baseline_artifact_pupil,
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

baseline_audit$overview
baseline_audit$baseline_summary
```

Audit preprocessing imbalance across conditions:

``` r

imbalance_audit <- audit_gazepoint_pupil_imbalance(
  smoothed_artifact_pupil,
  group_cols = "condition"
)

imbalance_audit$overview
imbalance_audit$imbalance_summary
```

Audit tonic pupil drift over time:

``` r

drift_audit <- audit_gazepoint_pupil_drift(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

drift_audit$overview
drift_audit$drift_summary
```

Audit possible event-response overlap risk:

``` r

overlap_audit <- audit_gazepoint_pupil_overlap_risk(
  master,
  time_col = "time",
  event_col = "event_label",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

overlap_audit$overview
overlap_audit$risk_summary
```

Check pupil reliability:

``` r

pupil_reliability <- audit_gazepoint_pupil_reliability(
  pupil_windows_conservative,
  subject_col = "subject",
  outcome_col = "mean_pupil",
  condition_col = "condition"
)

pupil_reliability$overview
pupil_reliability$reliability_summary
```

Run PCHIP interpolation sensitivity when method sensitivity is needed:

``` r

pchip_pupil <- interpolate_gazepoint_pupil_pchip(
  artifact_pupil,
  pupil_col = "pupil_clean",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

attr(pchip_pupil, "gp3_pchip_overview")
```

Audit stimulus luminance when image stimuli are used:

``` r

luminance_audit <- audit_gazepoint_stimulus_luminance(
  stimulus_data,
  stimulus_col = "stimulus_file",
  condition_col = "condition"
)

luminance_audit$overview
luminance_audit$stimulus_summary
luminance_audit$condition_summary
```

Apply an optional Hampel filter to pupil data:

``` r

hampel_pupil <- flag_gazepoint_pupil_hampel(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  grouping_cols = c("subject", "MEDIA_ID", "trial_global"),
  window_size_samples = 7,
  k = 3,
  min_valid_samples = 3,
  corrected_col = "pupil_hampel_corrected"
)

attr(hampel_pupil, "gp3_hampel_overview")
attr(hampel_pupil, "gp3_hampel_status_summary")
```

Define and run a preprocessing multiverse when conclusions need
sensitivity checks across reasonable preprocessing decisions:

``` r

preprocessing_mv <- create_gazepoint_preprocessing_multiverse(
  pupil_max_gap_ms = c(75, 150, 250),
  pupil_smoothing_window_samples = c(3, 5, 7),
  pupil_baseline_windows = list(c(0, 200), c(-200, 0)),
  pupil_artifact_padding_ms = c(0, 50),
  include_pupil = TRUE,
  include_aoi = FALSE,
  label_prefix = "study1"
)

pupil_mv_results <- run_gazepoint_pupil_multiverse(
  master,
  preprocessing_mv,
  pupil_col = "pupil",
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  keep_outputs = TRUE
)

pupil_mv_summary <- summarise_gazepoint_multiverse_results(
  pupil = pupil_mv_results
)

pupil_mv_summary$overview
pupil_mv_summary$branch_summary
pupil_mv_summary$failure_summary
```

Use these audits and sensitivity checks to make preprocessing decisions
transparent before fitting confirmatory models.

## 6. Pupil-window LMM workflow

Use pupil-window LMMs when the primary hypothesis concerns predefined
pupil windows.

Start from pupil-window summaries:

``` r

pupil_windows <- pupil_windows_conservative
```

Prepare window-level model data:

``` r

pupil_window_model_data <- prepare_gazepoint_pupil_window_model_data(
  pupil_windows,
  outcome_col = "mean_pupil",
  subject_col = "subject",
  condition_col = "condition",
  window_col = "window_label",
  trial_col = "trial_global",
  valid_samples_col = "n_valid_samples",
  total_samples_col = "n_samples",
  min_valid_samples = 1
)

dplyr::glimpse(pupil_window_model_data)

dplyr::count(
  pupil_window_model_data,
  pupil_window_condition,
  pupil_window_label,
  pupil_window_model_status
)
```

Fit the confirmatory pupil-window LMM:

``` r

pupil_window_lmm <- fit_gazepoint_pupil_window_lmm(
  pupil_window_model_data,
  random_window_slopes = FALSE,
  use_weights = TRUE,
  REML = FALSE
)

pupil_window_lmm$model_status
pupil_window_lmm$formula
pupil_window_lmm$comparison
pupil_window_lmm$fixed_effects
```

Run model-family sensitivity checks:

``` r

pupil_window_sensitivity <- fit_gazepoint_pupil_window_sensitivity(
  pupil_window_model_data,
  model_types = c(
    "unweighted_lmm",
    "weighted_lmm",
    "fixed_lm",
    "weighted_lm"
  ),
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE
)

pupil_window_sensitivity$comparison
pupil_window_sensitivity$fixed_effects
pupil_window_sensitivity$formulas
```

Compare nested models when needed:

``` r

nested_pupil_models <- compare_gazepoint_nested_models(
  models = list(
    null = pupil_m0,
    condition = pupil_m1,
    window = pupil_m2,
    interaction = pupil_m3
  ),
  comparison = "sequential"
)

nested_pupil_models$model_table
nested_pupil_models$lrt_table
nested_pupil_models$ranking_table
```

Run leave-one-participant sensitivity if the main effect may be driven
by a single participant:

``` r

pupil_loo <- run_gazepoint_model_leave_one_out(
  data = pupil_window_model_data,
  unit_col = "pupil_window_subject",
  fit_function = function(dat) {
    stats::lm(pupil_window_outcome ~ pupil_window_condition * pupil_window_label, data = dat)
  },
  effect_terms = "pupil_window_condition"
)

pupil_loo$overview
pupil_loo$effect_summary
```

Plot model-implied predictions when
[`predict()`](https://rdrr.io/r/stats/predict.html) is available:

``` r

pupil_prediction_plot <- plot_gazepoint_model_predictions(
  data = pupil_window_model_data,
  model = pupil_window_lmm$model,
  x_col = "pupil_window_label",
  outcome_col = "pupil_window_outcome",
  condition_col = "pupil_window_condition"
)

pupil_prediction_plot
```

For most confirmatory pupil-window analyses, the recommended sequence
is:

``` r

pupil_window_model_data <- prepare_gazepoint_pupil_window_model_data(...)
pupil_window_lmm <- fit_gazepoint_pupil_window_lmm(pupil_window_model_data)
pupil_window_sensitivity <- fit_gazepoint_pupil_window_sensitivity(pupil_window_model_data)
diagnostics <- diagnose_gazepoint_glmm(pupil_window_lmm)
model_summary <- tidy_gazepoint_model_summary(pupil_window_lmm)
```

## 7. AOI entries, AOI windows, and AOI GLMM workflow

Use AOI-window GLMMs when the primary hypothesis concerns predefined AOI
time windows.

Start from a validated master table with sample-level AOI states:

``` r

validation <- validate_gazepoint_master(master)

validation$summary
```

Create ordered AOI-entry episodes:

``` r

aoi_entries <- summarise_gazepoint_aoi_entries(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

dplyr::glimpse(aoi_entries)
```

Create AOI-window summaries:

``` r

aoi_windows <- summarise_gazepoint_aoi_windows(
  master,
  windows = c(0, 500, 1000, 2000, 5000, 10000),
  time_col = "time",
  aoi_col = "aoi_current",
  subject_col = "subject",
  condition_col = "condition",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

dplyr::glimpse(aoi_windows)
```

Inspect target-looking proportions:

``` r

dplyr::select(
  aoi_windows,
  subject,
  condition,
  MEDIA_ID,
  trial_global,
  window_label,
  n_window_samples,
  n_target_samples,
  n_distractor_samples,
  n_valid_denominator_samples,
  target_sample_prop_valid,
  aoi_window_status
)
```

Audit denominator adequacy before fitting binomial models:

``` r

aoi_window_denominator_audit <- audit_gazepoint_aoi_window_denominators(
  aoi_windows,
  min_denominator_samples = 5,
  min_valid_denominator_prop = 0.70,
  max_denominator_cv = 0.25,
  max_condition_ratio = 2
)

aoi_window_denominator_audit$overview
aoi_window_denominator_audit$window_summary
aoi_window_denominator_audit$condition_window_summary
aoi_window_denominator_audit$denominator_imbalance

dplyr::count(
  aoi_window_denominator_audit$row_audit,
  denominator_audit_status
)
```

Prepare binomial success/failure data for mixed-effects logistic
regression:

``` r

aoi_glmm_data <- prepare_gazepoint_aoi_glmm_data(
  aoi_windows,
  success_col = "n_target_samples",
  denominator = "valid",
  subject_col = "subject",
  condition_col = "condition",
  window_col = "window_label",
  window_start_col = "window_start_ms",
  window_end_col = "window_end_ms",
  min_denominator_samples = 5,
  outcome_label = "target"
)

dplyr::glimpse(aoi_glmm_data)

dplyr::count(
  aoi_glmm_data,
  aoi_glmm_condition,
  aoi_glmm_window,
  aoi_glmm_status
)
```

Fit the main AOI-window binomial GLMM:

``` r

aoi_glmm_fit <- fit_gazepoint_aoi_window_glmm(
  aoi_glmm_data,
  random_window_slopes = FALSE
)

aoi_glmm_fit$model_status
aoi_glmm_fit$formula
aoi_glmm_fit$random_effect_structure
aoi_glmm_fit$comparison

if (!is.null(aoi_glmm_fit$model)) {
  lme4::fixef(aoi_glmm_fit$model)
}
```

Transform AOI proportions into empirical logits for sensitivity models:

``` r

aoi_emp_logit <- transform_gazepoint_aoi_empirical_logit(
  aoi_glmm_data,
  numerator_col = "aoi_glmm_success",
  denominator_col = "aoi_glmm_denominator",
  correction = 0.5
)

dplyr::glimpse(aoi_emp_logit)
attr(aoi_emp_logit, "gp3_empirical_logit_overview")
```

Run AOI model-family sensitivity checks:

``` r

aoi_sensitivity <- fit_gazepoint_aoi_model_sensitivity(
  aoi_glmm_data,
  model_types = c(
    "binomial_glmm",
    "empirical_logit_lmm",
    "proportion_lmm",
    "quasibinomial_glm"
  ),
  include_condition = TRUE,
  include_window = TRUE,
  include_interaction = TRUE,
  random_intercept = TRUE
)

aoi_sensitivity$comparison
aoi_sensitivity$fixed_effects
aoi_sensitivity$formulas
```

For most confirmatory AOI-window analyses, the recommended sequence is:

``` r

aoi_windows <- summarise_gazepoint_aoi_windows(...)
aoi_window_denominator_audit <- audit_gazepoint_aoi_window_denominators(aoi_windows)
aoi_glmm_data <- prepare_gazepoint_aoi_glmm_data(aoi_windows)
aoi_glmm_fit <- fit_gazepoint_aoi_window_glmm(aoi_glmm_data)
aoi_sensitivity <- fit_gazepoint_aoi_model_sensitivity(aoi_glmm_data)
```

## 8. AOI/fixation/transition workflow

Use this workflow when the research question concerns looking episodes,
fixation summaries, AOI sequences, transition matrices, scanpath
structure, or event-contingent alignment.

Create transition-ready AOI sequences:

``` r

aoi_sequences <- prepare_gazepoint_aoi_sequences(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global")
)

dplyr::glimpse(aoi_sequences)
```

Summarise AOI transitions at the trial level:

``` r

aoi_transition_summary <- summarise_gazepoint_aoi_transitions(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

aoi_transition_summary
```

Create AOI transition matrices:

``` r

aoi_transition_matrix <- compute_gazepoint_aoi_transition_matrix(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

aoi_transition_matrix$count_matrix
aoi_transition_matrix$probability_matrix
aoi_transition_matrix$long_table
```

Plot the transition matrix:

``` r

plot_gazepoint_aoi_transition_matrix(
  aoi_transition_matrix,
  value = "prob",
  show_labels = TRUE,
  title = "AOI transition probabilities"
)
```

Create time-varying AOI transition matrices:

``` r

time_varying_transition_matrix <- compute_gazepoint_time_varying_transition_matrix(
  master,
  time_col = "time",
  state_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  windows = c(0, 500, 1000, 2000, 5000)
)

time_varying_transition_matrix$overview
time_varying_transition_matrix$transition_summary
```

Create trial-level AOI features:

``` r

aoi_trial_features <- summarise_gazepoint_aoi_trial_features(
  master,
  time_col = "time",
  aoi_col = "aoi_current",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

aoi_trial_features
```

Create trial-level fixation features from Gazepoint fixation exports:

``` r

fixation_trial_features <- summarise_gazepoint_fixation_trials(
  all_fix,
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

fixation_trial_features
```

Create fixation-, saccade-, or AOI-contingent aligned data:

``` r

fixation_aligned <- prepare_gazepoint_fixation_aligned_data(
  master,
  time_col = "time",
  participant_col = "subject",
  trial_col = "trial_global",
  aoi_col = "aoi_current",
  target_aoi = "AOI 2",
  fixation_col = "is_fixation",
  alignment_event = "first_fixation_to_target",
  baseline_window = c(-200, 0),
  analysis_window = c(0, 800)
)

fixation_aligned$overview
fixation_aligned$event_table
fixation_aligned$trial_summary
```

Prepare dependency-free advanced sequence-model objects:

``` r

markov_obj <- create_gazepoint_markovchain_object(
  aoi_sequences,
  state_col = "aoi"
)

semimarkov_data <- prepare_gazepoint_semimarkov_data(
  aoi_sequences,
  state_col = "aoi",
  time_col = "time"
)

hmm_data <- prepare_gazepoint_hmm_data(
  aoi_sequences,
  state_col = "aoi",
  observation_cols = c("x", "y", "pupil")
)

markov_obj$overview
semimarkov_data$overview
hmm_data$overview
```

These helpers prepare sequence and transition data for inspection,
reporting, export, or specialist modelling packages. They do not
directly fit Markov-chain, semi-Markov, or hidden Markov models.

## 9. Pupil GAMM, AOI GAMM, PFE-GAMM, and GCA workflow

Use time-course models when the research question concerns smooth
trajectories rather than predefined confirmatory windows.

### Pupil GAMM

Prepare binned pupil time-course data:

``` r

pupil_gamm_data <- prepare_gazepoint_pupil_gamm_data(
  smoothed_artifact_pupil,
  pupil_col = "pupil_smoothed",
  time_col = "time",
  subject_col = "subject",
  condition_col = "condition",
  trial_col = "trial_global",
  bin_width_ms = 50,
  min_valid_samples = 1
)

dplyr::glimpse(pupil_gamm_data)

dplyr::count(
  pupil_gamm_data,
  condition,
  condition_status,
  gamm_data_status
)
```

Fit the main pupil GAMM:

``` r

pupil_gamm_fit <- fit_gazepoint_pupil_gamm(
  pupil_gamm_data,
  n_time_basis = 10,
  discrete = TRUE
)

pupil_gamm_fit$model_status
pupil_gamm_fit$formula

summary(pupil_gamm_fit$model)
```

The model can include smooth time effects, condition-by-time smooths
when multiple conditions are available, subject random effects, optional
observation weights, and optional autocorrelation settings.

### Gaze-position / PFE sensitivity GAMM

Fit a gaze-position-adjusted PFE sensitivity model:

``` r

pupil_pfe_fit <- fit_gazepoint_pupil_pfe_gamm(
  pupil_gamm_data,
  n_time_basis = 10,
  n_position_basis = 8,
  discrete = TRUE
)

pupil_pfe_fit$sensitivity_status
pupil_pfe_fit$main_fit$formula
pupil_pfe_fit$pfe_formula
pupil_pfe_fit$comparison
```

The PFE model should be treated as a sensitivity analysis rather than as
the default pupil correction. It tests whether gaze position explains
additional pupil variance through a two-dimensional smooth term over
gaze coordinates:

``` r

te(mean_x, mean_y)
```

### Growth Curve Analysis

Prepare binned pupil data for Growth Curve Analysis:

``` r

pupil_gca_data <- prepare_gazepoint_gca_data(
  pupil_gamm_data,
  pupil_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition",
  degree = 3,
  orthogonal = TRUE,
  valid_samples_col = "n_valid_samples",
  min_valid_samples = 1
)

dplyr::glimpse(pupil_gca_data)

dplyr::count(
  pupil_gca_data,
  condition,
  condition_status,
  gca_data_status
)
```

Fit a GCA mixed model:

``` r

pupil_gca_fit <- fit_gazepoint_gca(
  pupil_gca_data,
  REML = FALSE
)

pupil_gca_fit$model_status
pupil_gca_fit$random_effect_structure
pupil_gca_fit$fallback_used
pupil_gca_fit$singular_fit
pupil_gca_fit$comparison
```

For quick inspection, avoid printing the full model summary unless
needed. Use:

``` r

lme4::fixef(pupil_gca_fit$model)
lme4::VarCorr(pupil_gca_fit$model)
```

Plot observed and fitted GCA trajectories:

``` r

plot_gazepoint_gca(
  pupil_gca_fit,
  title = "Observed and fitted GCA trajectory"
)
```

Subject-level observed trajectories can also be shown:

``` r

plot_gazepoint_gca(
  pupil_gca_fit,
  show_subjects = TRUE,
  title = "Subject-level and fitted GCA trajectories"
)
```

### AOI time-course GAMM

Prepare AOI-GAMM data from an AOI-state column:

``` r

aoi_gamm_data <- prepare_gazepoint_aoi_gamm_data(
  master,
  aoi_col = "aoi_current",
  target_aoi_values = "AOI 2",
  subject_col = "subject",
  condition_col = "condition",
  time_col = "time",
  trial_col = "trial_global",
  time_window = c(0, 2000),
  bin_size_ms = 50,
  denominator = "valid",
  min_denominator_samples = 1,
  outcome_label = "target_aoi"
)

dplyr::glimpse(aoi_gamm_data)

dplyr::count(
  aoi_gamm_data,
  .gp3_aoi_gamm_condition,
  .gp3_aoi_gamm_condition_status,
  .gp3_aoi_gamm_status
)
```

Alternatively, create a logical target-AOI indicator first:

``` r

master_for_aoi_gamm <- master |>
  dplyr::mutate(target_aoi = aoi_current == "AOI 2")

aoi_gamm_data <- prepare_gazepoint_aoi_gamm_data(
  master_for_aoi_gamm,
  outcome_col = "target_aoi",
  subject_col = "subject",
  condition_col = "condition",
  time_col = "time",
  trial_col = "trial_global",
  time_window = c(0, 2000),
  bin_size_ms = 50,
  min_denominator_samples = 1,
  outcome_label = "target_aoi"
)
```

Fit the AOI time-course GAMM:

``` r

aoi_gamm_fit <- fit_gazepoint_aoi_gamm(
  aoi_gamm_data,
  include_condition = TRUE,
  condition_smooths = TRUE,
  random_subject = TRUE,
  random_subject_time = FALSE,
  time_k = 10
)

aoi_gamm_fit$model_status
aoi_gamm_fit$condition_status
aoi_gamm_fit$formula_text
aoi_gamm_fit$diagnostics
aoi_gamm_fit$smooth_table
```

Plot observed AOI proportions and fitted GAMM trajectories:

``` r

aoi_gamm_plot <- plot_gazepoint_aoi_gamm(
  aoi_gamm_fit,
  n_time_points = 100,
  include_observed = TRUE,
  include_fitted = TRUE,
  show_ci = TRUE
)

aoi_gamm_plot
```

AOI time-course GAMMs are complementary to AOI-window GLMMs. Use
AOI-window GLMMs for predefined confirmatory windows and AOI-GAMMs when
the research question concerns smooth target-looking trajectories.

## 10. Cluster permutation and divergence-point workflow

Use cluster-based permutation testing for time-course inference. Use
divergence-point estimation as complementary onset/sensitivity evidence.

Prepare time-course data for cluster-based permutation testing:

``` r

cluster_data <- prepare_gazepoint_cluster_data(
  pupil_gamm_data,
  outcome_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition"
)

dplyr::glimpse(cluster_data)
```

Run the cluster-based permutation test:

``` r

cluster_results <- run_gazepoint_cluster_permutation(
  cluster_data,
  condition_levels = c("control", "treatment"),
  n_permutations = 1000,
  seed = 123
)

cluster_results$overview
cluster_results$clusters
```

Summarise cluster results:

``` r

cluster_summary <- summarise_gazepoint_clusters(cluster_results)

cluster_summary$overview
cluster_summary$all_clusters
cluster_summary$significant_clusters
cluster_summary$circularity_warning
```

Plot cluster results:

``` r

plot_gazepoint_cluster_results(
  cluster_results,
  plot = "difference"
)

plot_gazepoint_cluster_results(
  cluster_results,
  plot = "statistic"
)
```

Estimate the earliest reliable divergence point:

``` r

divergence <- estimate_gazepoint_divergence_point(
  data = timecourse_data,
  outcome_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  condition_col = "condition",
  participant_col = "subject",
  comparison = c("control", "treatment"),
  bootstrap_unit = "participant",
  n_boot = 1000,
  consecutive_points = 2,
  seed = 123
)

divergence$overview
divergence$divergence_point
divergence$difference_summary
divergence$bootstrap_onsets
```

The cluster-based permutation branch is intended for time-course
inference. Do not use a detected exploratory cluster to define a
confirmatory window and then retest that same window as if it had been
specified a priori.

## 11. Model diagnostics and manuscript tables

Use model diagnostics and reporting helpers after fitting confirmatory
models and sensitivity models.

Check convergence, singularity, and overdispersion separately:

``` r

check_gazepoint_model_convergence(aoi_glmm_fit$model)

check_gazepoint_model_singularity(aoi_glmm_fit$model)

check_gazepoint_model_overdispersion(aoi_glmm_fit$model)
```

Run a combined diagnostics bundle for GLMM, LMM, or GLM models:

``` r

glmm_diagnostics <- diagnose_gazepoint_glmm(
  aoi_glmm_fit,
  model_name = "aoi_window_glmm",
  use_dharma = FALSE
)

glmm_diagnostics$overview
glmm_diagnostics$convergence
glmm_diagnostics$singularity
glmm_diagnostics$overdispersion
glmm_diagnostics$dharma
```

Run a combined diagnostics bundle for
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models:

``` r

gamm_diagnostics <- diagnose_gazepoint_gamm(
  aoi_gamm_fit,
  model_name = "aoi_timecourse_gamm",
  use_dharma = FALSE
)

gamm_diagnostics$overview
gamm_diagnostics$convergence
gamm_diagnostics$basis
gamm_diagnostics$overdispersion
gamm_diagnostics$dharma
```

Optional DHARMa diagnostics can be requested when `DHARMa` is installed:

``` r

glmm_diagnostics_dharma <- diagnose_gazepoint_glmm(
  aoi_glmm_fit,
  use_dharma = TRUE,
  dharma_simulations = 250,
  seed = 123
)

glmm_diagnostics_dharma$dharma
```

If `DHARMa` is not installed, diagnostics return
`skipped_missing_package` rather than failing. `DHARMa` is an optional
suggested dependency, not a required import.

Compare nested model sequences:

``` r

nested_models <- compare_gazepoint_nested_models(
  models = list(
    null = m0,
    time = m1,
    condition = m2,
    interaction = m3
  ),
  comparison = "sequential"
)

nested_models$overview
nested_models$model_table
nested_models$lrt_table
nested_models$ranking_table
```

Plot observed summaries together with model-implied prediction
trajectories:

``` r

prediction_plot <- plot_gazepoint_model_predictions(
  data = model_data,
  model = fitted_model,
  x_col = "time",
  outcome_col = "outcome",
  condition_col = "condition",
  prediction_type = "response"
)

prediction_plot
attr(prediction_plot, "gp3_model_prediction_overview")
attr(prediction_plot, "gp3_model_prediction_prediction_summary")
```

Run leave-one-unit sensitivity checks for fitted models:

``` r

loo_sensitivity <- run_gazepoint_model_leave_one_out(
  data = model_data,
  unit_col = "subject",
  fit_function = function(dat) {
    stats::lm(outcome ~ condition * time, data = dat)
  },
  effect_terms = "conditiontreatment"
)

loo_sensitivity$overview
loo_sensitivity$effect_summary
loo_sensitivity$leave_one_effects
```

Create a fixed-effect summary table:

``` r

fixed_effects <- summarise_gazepoint_fixed_effects(
  aoi_glmm_fit,
  model_name = "aoi_window_glmm",
  exponentiate = TRUE
)

fixed_effects
```

Use `exponentiate = TRUE` for logistic, Poisson, or other log/link
models when odds ratios, rate ratios, or exponentiated coefficients are
preferred.

Create a complete tidy model-summary object:

``` r

model_summary <- tidy_gazepoint_model_summary(
  aoi_glmm_fit,
  model_name = "aoi_window_glmm",
  exponentiate = TRUE,
  use_dharma = FALSE
)

model_summary$overview
model_summary$model_info
model_summary$fixed_effects
model_summary$diagnostics$overview
```

Estimate marginal means and pairwise contrasts when `emmeans` is
installed:

``` r

emm_summary <- summarise_gazepoint_emmeans(
  aoi_glmm_fit,
  specs = "aoi_glmm_condition",
  model_name = "aoi_window_glmm",
  type = "response",
  adjust = "tukey"
)

emm_summary$overview
emm_summary$emmeans
emm_summary$contrasts
```

For condition-by-window models, estimated marginal means can also be
requested within windows:

``` r

emm_by_window <- summarise_gazepoint_emmeans(
  aoi_glmm_fit,
  specs = "aoi_glmm_condition",
  by = "aoi_glmm_window",
  model_name = "aoi_window_glmm",
  type = "response"
)

emm_by_window$emmeans
emm_by_window$contrasts
```

Export manuscript-ready model tables to CSV:

``` r

model_table_files <- export_gazepoint_model_tables(
  model_summary = model_summary,
  emmeans_summary = emm_summary,
  output_dir = output_dir,
  prefix = "aoi_window_glmm"
)

model_table_files
```

This writes an export index plus available tables such as:

``` r

aoi_window_glmm_model_overview.csv
aoi_window_glmm_model_info.csv
aoi_window_glmm_fixed_effects.csv
aoi_window_glmm_model_settings.csv
aoi_window_glmm_diagnostics_overview.csv
aoi_window_glmm_emmeans_overview.csv
aoi_window_glmm_emmeans.csv
aoi_window_glmm_contrasts.csv
aoi_window_glmm_emmeans_settings.csv
aoi_window_glmm_export_index.csv
```

The manuscript-table helpers are designed to return structured tables
rather than stopping on normal modelling issues. Missing optional
`emmeans`, unsupported model classes, failed contrasts, diagnostic
warnings, and disabled diagnostics are recorded through status fields.

## 12. Real-data readiness and reporting checklist

Use this final-stage workflow before interpreting real study results or
preparing a manuscript.

Run the real-data readiness gate:

``` r

readiness <- check_gazepoint_real_data_readiness(
  data = master,
  analysis_type = "combined",
  participant_col = "subject",
  trial_col = "trial_global",
  time_col = "time",
  condition_col = "condition",
  aoi_col = "aoi_current",
  pupil_col = "pupil_smoothed"
)

readiness$overview
readiness$gate_decision
readiness$checks
readiness$data_summary
readiness$condition_summary
```

Create explicit trial-level and participant-level exclusion
recommendations:

``` r

exclusion_recommendations <- recommend_gazepoint_exclusions(
  master,
  participant_col = "subject",
  trial_col = "trial_global",
  condition_col = "condition",
  validity_col = "valid",
  x_col = "x",
  y_col = "y",
  pupil_col = "pupil",
  artifact_col = "artifact",
  min_trial_samples = 10,
  max_trial_missing_prop = 0.50,
  max_trial_artifact_prop = 0.50,
  min_participant_trials = 2,
  min_participant_valid_trials = 1
)

exclusion_recommendations$overview
exclusion_recommendations$participant_recommendations
exclusion_recommendations$trial_recommendations
exclusion_recommendations$exclusion_table
```

[`recommend_gazepoint_exclusions()`](https://stefanosbalaskas.github.io/gp3tools/reference/recommend_gazepoint_exclusions.md)
recommends exclusions only. It does not remove participants, trials, or
samples automatically.

Apply offline gaze recalibration only when known target/check-target
coordinates are available:

``` r

recalibrated_gaze <- recalibrate_gazepoint_gaze(
  master,
  x_col = "x",
  y_col = "y",
  target_x_col = "target_x",
  target_y_col = "target_y",
  time_col = "time",
  grouping_cols = c("subject", "trial_global"),
  calibration_col = "is_check_target",
  calibration_value = TRUE,
  method = "median_shift",
  min_valid_points = 3
)

attr(recalibrated_gaze, "gp3_gaze_recalibration_overview")
attr(recalibrated_gaze, "gp3_gaze_recalibration_group_summary")
```

This helper estimates group-level horizontal and vertical gaze shifts
and applies them to gaze coordinates. It is useful only when the study
contains known target/check-target coordinates. It should not be used as
a generic correction when no target reference is available.

Create a reporting checklist:

``` r

reporting <- create_gazepoint_reporting_checklist(
  data = master,
  objects = list(
    readiness = readiness,
    exclusion_recommendations = exclusion_recommendations,
    pupil_audit = gap_audit,
    aoi_model = aoi_glmm_fit,
    model_summary = model_summary
  ),
  analysis_type = "combined",
  study_title = "Gazepoint eye-tracking study"
)

reporting$overview
reporting$checklist
reporting$section_summary
reporting$object_summary
```

Create a final analysis-decision audit after confirmatory models,
sensitivity checks, exploratory time-course analyses, diagnostics, and
manuscript-ready tables have been created:

``` r

branch_roles <- tibble::tibble(
  branch_name = c(
    "aoi_glmm",
    "aoi_sensitivity",
    "cluster_test",
    "model_summary"
  ),
  decision_type = c(
    "confirmatory",
    "sensitivity",
    "exploratory",
    "reporting"
  ),
  analysis_family = c(
    "aoi_window_glmm",
    "aoi_model_sensitivity",
    "cluster_permutation",
    "model_tables"
  )
)

decision_audit <- create_gazepoint_analysis_decision_audit(
  aoi_glmm = aoi_glmm_fit,
  aoi_sensitivity = aoi_sensitivity,
  cluster_test = cluster_results,
  model_summary = model_summary,
  branch_roles = branch_roles,
  required_confirmatory = "aoi_glmm",
  diagnostics_required = TRUE,
  require_clean_diagnostics = FALSE
)

decision_audit$overview
decision_audit$branch_audit
decision_audit$diagnostics_summary
decision_audit$interpretation_cautions
decision_audit$readiness
```

Possible readiness outcomes include:

``` r

"ready"
"ready_with_cautions"
"not_ready"
```

Use `ready_with_cautions` as a reporting signal, not as a package
failure. It means that the analysis can proceed, but the flagged
cautions should be reported transparently.

### Optional external cross-checks

Optional external cross-checks can be run when the relevant packages are
available:

``` r

gazer_crosscheck <- run_gazepoint_gazer_crosscheck(master)

eyetools_detection <- run_gazepoint_eyetools_fixation_detection(
  master,
  method = "dispersion"
)
```

Optional external checks skip or report controlled compatibility
statuses when optional packages or method branches are unavailable.

### Package-adapter exports

`gp3tools` includes dependency-free adapter helpers for preparing
Gazepoint master/sample tables for use in other R eye-tracking
workflows.

``` r

eyetrackingr_data <- prepare_gazepoint_eyetrackingr_data(master)

pupillometryr_data <- prepare_gazepoint_pupillometryr_data(master)

gazer_data <- prepare_gazepoint_gazer_data(master)

eyetools_data <- prepare_gazepoint_eyetools_data(master)
```

The adapter helpers do not import the external packages directly.
Instead, they return clean tibbles with standardised column names,
status fields, and metadata attributes.

## Current package status

Version 2.0.0 includes the core Gazepoint import, quality-control,
pupil, AOI, fixation/transition, time-course, model-diagnostics,
reporting, ecosystem-adapter, external face-data, and
workflow-documentation layers.

Recent local validation for version 2.0.0 should end with:

``` r

devtools::check()
# 0 errors | 0 warnings | 0 notes
```

Focused tests for the recent external face-data, multimodal, reporting,
and workflow-documentation branches have also passed with `FAIL 0`,
`WARN 0`, and `SKIP 0`.

During full tests, `boundary (singular) fit: see help('isSingular')`
messages may appear in mixed-model diagnostic contexts. These are
expected diagnostic messages from singular-fit test fixtures and are not
package failures when the final test summary reports `FAIL 0 | WARN 0`.

On some Windows systems, a Quarto/TMPDIR message may appear after
`devtools::check()`. This is harmless when the final R CMD check results
report:

``` text
0 errors | 0 warnings | 0 notes
```

The build may show a message that the package depends on R `>= 4.1.0`
because native pipe syntax is used. This is expected.

## Example datasets

`gp3tools` includes lightweight synthetic example datasets so examples,
vignettes, tests, and user workflows can be run without private
Gazepoint files.

``` r

data("gazepoint_example_master")
data("gazepoint_example_fixations")
data("gazepoint_example_aoi_geometry")
data("gazepoint_example_aoi_windows")
data("gazepoint_example_pupil_windows")
```

The example datasets are artificial and are not from a real participant
study. They are intended to show expected column names and runnable
workflow structure.

``` r

dplyr::glimpse(gazepoint_example_master)
dplyr::glimpse(gazepoint_example_fixations)
dplyr::glimpse(gazepoint_example_aoi_windows)
dplyr::glimpse(gazepoint_example_pupil_windows)
```

Use these objects when you want to try core workflows without first
importing private Gazepoint export files.

## Important note

`gp3tools` can parse official Gazepoint Analysis summary exports, but
metrics recomputed from all-gaze and fixation files may not always
exactly reproduce Gazepoint’s internal summary calculations. For
official Gazepoint summary values, use
[`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_summary.md).
For transparent reproducible calculations from exported rows, use the
sample-level and fixation-level summary functions.
