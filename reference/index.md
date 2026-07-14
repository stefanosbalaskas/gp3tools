# Package index

## Import, file checks, and master data

Read Gazepoint exports, inspect columns, create master tables, and
validate analysis-ready data.

- [`read_gazepoint()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint.md)
  : Read a Gazepoint all-gaze or fixation CSV export
- [`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_folder.md)
  : Read multiple Gazepoint CSV exports from a folder
- [`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_summary.md)
  : Read a Gazepoint Analysis Data Summary export
- [`classify_gazepoint_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/classify_gazepoint_export.md)
  : Classify a Gazepoint export
- [`inspect_gazepoint_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/inspect_gazepoint_columns.md)
  : Inspect Gazepoint columns
- [`standardise_gazepoint_names()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardise_gazepoint_names.md)
  : Standardise Gazepoint column names
- [`check_gazepoint_file_pairs()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_file_pairs.md)
  : Check Gazepoint all-gaze and fixation file pairs
- [`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md)
  : Create a master long-format dataset from Gazepoint all-gaze data
- [`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
  : Convert Gazepoint all-gaze data to a master sample table
- [`audit_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_master.md)
  : Audit a Gazepoint master sample table
- [`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md)
  : Validate a Gazepoint master sample table
- [`export_gazepoint_master_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_master_audit.md)
  : Export a Gazepoint master table, audit tables, and validation tables

## Quick workflows, reports, and outputs

Run folder-level workflows, create reports, save plots, and write
standard output tables.

- [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md)
  : Run a complete Gazepoint analysis workflow
- [`summarise_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_workflow.md)
  : Summarise a Gazepoint workflow result
- [`create_gazepoint_report()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_report.md)
  : Create a Gazepoint HTML diagnostic report
- [`write_gazepoint_outputs()`](https://stefanosbalaskas.github.io/gp3tools/reference/write_gazepoint_outputs.md)
  : Write standard Gazepoint analysis outputs
- [`export_gazepoint_tables()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_tables.md)
  : Export Gazepoint analysis tables to CSV files
- [`save_gazepoint_plots()`](https://stefanosbalaskas.github.io/gp3tools/reference/save_gazepoint_plots.md)
  : Save standard Gazepoint diagnostic plots

## Sampling, tracking quality, and QC summaries

Check sampling rate, tracking quality, screen bounds, coordinate
coverage, missingness, and task-phase coverage.

- [`check_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_sampling_rate.md)
  : Check sampling rate by group
- [`plot_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_sampling_rate.md)
  : Plot Gazepoint sampling-rate diagnostics
- [`summarise_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_tracking_quality.md)
  : Summarise Gazepoint tracking quality
- [`flag_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_tracking_quality.md)
  : Flag low-quality Gazepoint recordings
- [`clean_gazepoint_by_trackloss()`](https://stefanosbalaskas.github.io/gp3tools/reference/clean_gazepoint_by_trackloss.md)
  : Flag or filter Gazepoint data by trackloss
- [`plot_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_tracking_quality.md)
  : Plot Gazepoint tracking-quality diagnostics
- [`audit_gazepoint_gaze_signal_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_gaze_signal_quality.md)
  : Audit Gazepoint gaze-signal quality
- [`audit_gazepoint_screen_bounds()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_screen_bounds.md)
  : Audit Gazepoint gaze coordinates against screen bounds
- [`harmonize_gazepoint_screen_coordinates()`](https://stefanosbalaskas.github.io/gp3tools/reference/harmonize_gazepoint_screen_coordinates.md)
  : Harmonize Gazepoint screen coordinates across resolutions
- [`summarize_gazepoint_coordinate_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_coordinate_coverage.md)
  : Summarize gaze-coordinate coverage over a screen grid
- [`summarize_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md)
  [`summarise_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_missingness.md)
  : Summarize missingness in Gazepoint-style data
- [`plot_gazepoint_missingness_profile()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_missingness_profile.md)
  : Plot a Gazepoint missingness profile
- [`report_gazepoint_missingness()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_missingness.md)
  : Report Gazepoint missingness
- [`segment_gazepoint_task_phases()`](https://stefanosbalaskas.github.io/gp3tools/reference/segment_gazepoint_task_phases.md)
  : Segment Gazepoint-style data into task phases
- [`summarize_gazepoint_phase_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_phase_coverage.md)
  [`summarise_gazepoint_phase_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_phase_coverage.md)
  : Summarize task-phase coverage
- [`report_gazepoint_phase_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_phase_coverage.md)
  : Report task-phase coverage
- [`plot_gazepoint_phase_timeline()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_phase_timeline.md)
  : Plot a Gazepoint task-phase timeline
- [`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md)
  : Collect Gazepoint QC summaries
- [`summarize_gazepoint_qc_status()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_qc_status.md)
  [`summarise_gazepoint_qc_status()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_qc_status.md)
  : Summarize Gazepoint QC status
- [`report_gazepoint_qc_overview()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_qc_overview.md)
  : Report Gazepoint QC overview
- [`plot_gazepoint_qc_overview()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_qc_overview.md)
  : Plot a Gazepoint QC overview
- [`launch_gazepoint_qc_dashboard()`](https://stefanosbalaskas.github.io/gp3tools/reference/launch_gazepoint_qc_dashboard.md)
  : Launch or describe a lightweight QC dashboard

## Stimulus layout and AOI geometry QC

Audit AOI geometry, screen coverage, overlap, margins, and
stimulus-layout quality.

- [`audit_gazepoint_aoi_geometry()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_geometry.md)
  : Audit AOI geometry definitions
- [`audit_gazepoint_aoi_overlap()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_overlap.md)
  : Audit AOI overlap
- [`audit_gazepoint_aoi_margin_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_margin_sensitivity.md)
  : Audit AOI margin sensitivity
- [`audit_gazepoint_aoi_screen_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_screen_coverage.md)
  : Audit AOI coverage against screen bounds
- [`audit_gazepoint_aoi_coding_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_coding_matrix.md)
  : Audit AOI coding against geometry
- [`plot_gazepoint_aoi_verification()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_verification.md)
  : Plot AOI geometry for visual verification
- [`plot_gazepoint_stimulus_layout_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_stimulus_layout_qc.md)
  : Plot stimulus-layout quality control

## Pupil preprocessing, audits, and windows

Flag, interpolate, baseline-correct, smooth, audit, and summarise pupil
data.

- [`summarise_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil.md)
  : Summarise Gazepoint pupil data
- [`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md)
  : Flag invalid, missing, implausible, and outlying Gazepoint pupil
  samples
- [`combine_gazepoint_eyes()`](https://stefanosbalaskas.github.io/gp3tools/reference/combine_gazepoint_eyes.md)
  : Combine left and right Gazepoint eye channels
- [`flag_gazepoint_pupil_artifacts()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_artifacts.md)
  : Flag Gazepoint pupil artifacts before interpolation
- [`flag_gazepoint_pupil_hampel()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_hampel.md)
  : Flag pupil artifacts with a Hampel filter
- [`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md)
  : Interpolate short missing gaps in Gazepoint pupil data
- [`interpolate_gazepoint_pupil_pchip()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil_pchip.md)
  : Interpolate Gazepoint pupil data using PCHIP
- [`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md)
  : Baseline-correct Gazepoint pupil data
- [`smooth_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_pupil.md)
  : Smooth Gazepoint pupil data
- [`create_gazepoint_preprocessing_registry()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_registry.md)
  : Create a Gazepoint pupil-preprocessing registry
- [`create_gazepoint_preprocessing_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_multiverse.md)
  : Create a Gazepoint preprocessing multiverse
- [`run_gazepoint_pupil_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_pupil_multiverse.md)
  : Run a Gazepoint pupil preprocessing multiverse
- [`audit_gazepoint_pupil_gaps()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_gaps.md)
  : Audit Gazepoint pupil interpolation gaps
- [`audit_gazepoint_pupil_baseline()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_baseline.md)
  : Audit Gazepoint pupil baseline quality
- [`audit_gazepoint_pupil_drift()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_drift.md)
  : Audit Gazepoint pupil drift
- [`audit_gazepoint_pupil_imbalance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_imbalance.md)
  : Audit Gazepoint pupil preprocessing imbalance
- [`audit_gazepoint_pupil_overlap_risk()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_overlap_risk.md)
  : Audit Gazepoint pupil-response overlap risk
- [`audit_gazepoint_pupil_reliability()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_reliability.md)
  : Audit split-half reliability for Gazepoint pupil outcomes
- [`audit_gazepoint_stimulus_luminance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_stimulus_luminance.md)
  : Audit stimulus luminance and brightness for Gazepoint studies
- [`summarise_gazepoint_pupil_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_windows.md)
  : Summarise Gazepoint pupil responses within time windows
- [`summarise_gazepoint_pupil_trial_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_trial_features.md)
  : Summarise Gazepoint pupil trial-level features
- [`plot_gazepoint_pupil_status()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_pupil_status.md)
  : Plot Gazepoint pupil preprocessing status
- [`plot_gazepoint_pupil_preprocessing()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_pupil_preprocessing.md)
  : Plot Gazepoint pupil preprocessing for one trial
- [`plot_gazepoint_pupil_timecourse()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_pupil_timecourse.md)
  : Plot Gazepoint pupil time course

## Pupil modelling and time-course data

Prepare and fit pupil-window, GAMM, PFE-GAMM, and growth-curve models.

- [`prepare_gazepoint_pupil_window_model_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_window_model_data.md)
  : Prepare pupil-window data for confirmatory mixed models
- [`fit_gazepoint_pupil_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_window_lmm.md)
  : Fit confirmatory pupil-window linear mixed models
- [`fit_gazepoint_pupil_window_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_window_sensitivity.md)
  : Run sensitivity models for confirmatory pupil-window analyses
- [`prepare_gazepoint_pupil_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_gamm_data.md)
  : Prepare Gazepoint pupil GAMM data
- [`fit_gazepoint_pupil_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_gamm.md)
  : Fit a Gazepoint pupil GAMM
- [`fit_gazepoint_pupil_pfe_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_pfe_gamm.md)
  : Fit a gaze-position-adjusted pupil GAMM sensitivity model
- [`prepare_gazepoint_gca_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gca_data.md)
  : Prepare Gazepoint Growth Curve Analysis data
- [`fit_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_gca.md)
  : Fit a Gazepoint Growth Curve Analysis mixed model
- [`plot_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_gca.md)
  : Plot observed and fitted Growth Curve Analysis trajectories

## AOI summaries and AOI modelling

Summarise AOI behaviour, prepare AOI-window data, and fit
GLMM/GAMM/sensitivity models.

- [`summarise_aoi_samples()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_aoi_samples.md)
  : Summarise sample-level AOI viewing
- [`summarise_gazepoint_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi.md)
  : Summarise Gazepoint AOI metrics from gaze and fixation exports
- [`summarise_gazepoint_aoi_entries()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_entries.md)
  : Summarise Gazepoint AOI entry episodes
- [`summarise_gazepoint_aoi_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_windows.md)
  : Summarise Gazepoint AOI samples within predefined time windows
- [`summarise_gazepoint_aoi_trial_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_trial_features.md)
  : Summarise Gazepoint AOI trial features
- [`audit_gazepoint_aoi_window_denominators()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_window_denominators.md)
  : Audit AOI window denominators before binomial modelling
- [`prepare_gazepoint_aoi_glmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_glmm_data.md)
  : Prepare AOI-window data for binomial GLMMs
- [`fit_gazepoint_aoi_window_glmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_window_glmm.md)
  : Fit an AOI-window binomial GLMM
- [`transform_gazepoint_aoi_empirical_logit()`](https://stefanosbalaskas.github.io/gp3tools/reference/transform_gazepoint_aoi_empirical_logit.md)
  : Transform AOI proportions to empirical logits
- [`fit_gazepoint_aoi_model_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_model_sensitivity.md)
  : Fit AOI-window model-family sensitivity checks
- [`run_gazepoint_aoi_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_aoi_multiverse.md)
  : Run a Gazepoint AOI preprocessing multiverse
- [`prepare_gazepoint_aoi_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_gamm_data.md)
  : Prepare AOI time-course data for GAMM analysis
- [`fit_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_gamm.md)
  : Fit AOI time-course GAMMs
- [`plot_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_gamm.md)
  : Plot AOI time-course GAMM results
- [`fit_gazepoint_aoi_brms()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_brms.md)
  : Fit an optional Bayesian AOI model with brms

## Fixations, AOI sequences, transitions, and scanpaths

Summarise fixation data, AOI sequences, transition matrices, scanpaths,
and sequence reliability.

- [`summarise_fixations()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_fixations.md)
  : Summarise fixation-level AOI metrics
- [`summarise_gazepoint_fixation_trials()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_fixation_trials.md)
  : Summarise Gazepoint fixation trial features
- [`audit_gazepoint_fixation_reliability()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_fixation_reliability.md)
  : Audit split-half reliability of fixation or AOI metrics
- [`detect_gazepoint_fixations_ivt()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_fixations_ivt.md)
  : Detect simple I-VT fixations from gaze samples
- [`prepare_gazepoint_fixation_aligned_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_fixation_aligned_data.md)
  : Prepare fixation- or saccade-contingent aligned Gazepoint data
- [`prepare_gazepoint_aoi_sequences()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_sequences.md)
  : Prepare Gazepoint AOI sequences
- [`summarise_gazepoint_aoi_transitions()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_transitions.md)
  : Summarise Gazepoint AOI transition features
- [`compute_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_transition_matrix.md)
  : Compute an AOI transition matrix
- [`compute_gazepoint_aoi_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_transition_matrix.md)
  : Compute Gazepoint AOI transition matrices
- [`compute_gazepoint_time_varying_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_time_varying_transition_matrix.md)
  : Compute time-varying Gazepoint transition matrices
- [`compute_gazepoint_transition_network_metrics()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_transition_network_metrics.md)
  : Compute lightweight AOI transition-network metrics
- [`fit_gazepoint_transition_count_nb_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_transition_count_nb_sensitivity.md)
  : Fit optional negative-binomial transition-count sensitivity models
- [`compute_gazepoint_aoi_entropy()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_entropy.md)
  : Compute AOI entropy metrics
- [`compute_gazepoint_aoi_sequence_metrics()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_sequence_metrics.md)
  : Compute AOI sequence metrics
- [`compute_gazepoint_sequence_complexity()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_sequence_complexity.md)
  : Compute AOI sequence complexity metrics
- [`compute_gazepoint_sequence_distance()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_sequence_distance.md)
  : Compute AOI sequence distance
- [`compute_gazepoint_scanpath_similarity()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_scanpath_similarity.md)
  : Compute AOI scanpath similarity
- [`compute_gazepoint_sequence_recurrence()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_sequence_recurrence.md)
  : Compute simple categorical sequence recurrence metrics
- [`compute_gazepoint_saccade_metrics()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_saccade_metrics.md)
  : Compute basic saccade metrics from fixation coordinates
- [`flag_gazepoint_sequence_anomalies()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_sequence_anomalies.md)
  : Flag unusual AOI sequences
- [`plot_gazepoint_aoi_timeline()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_timeline.md)
  : Plot an AOI timeline
- [`plot_gazepoint_aoi_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_transition_matrix.md)
  : Plot Gazepoint AOI transition matrix
- [`plot_transition_heatmap()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_transition_heatmap.md)
  : Plot an AOI transition heatmap
- [`plot_gazepoint_scanpath()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_scanpath.md)
  : Plot a fixation scanpath
- [`plot_gazepoint_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_scanpaths.md)
  : Plot multiple Gazepoint scanpaths

## Cluster permutation and advanced time-course inference

Prepare, run, summarise, plot, and export cluster-based time-course
analyses.

- [`prepare_gazepoint_cluster_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_cluster_data.md)
  : Prepare time-course data for cluster-based permutation tests
- [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md)
  : Run paired cluster-based permutation tests
- [`run_gazepoint_cluster_permutation_anova()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation_anova.md)
  : Guardrail for cluster-permutation ANOVA
- [`run_gazepoint_cluster_permutation_covariate_adjusted()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation_covariate_adjusted.md)
  : Guardrail for covariate-adjusted cluster permutation
- [`run_gazepoint_cluster_permutation_lmer()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation_lmer.md)
  : Guardrail for mixed-model cluster permutation
- [`run_gazepoint_cluster_permutation_parallel()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation_parallel.md)
  : Guardrail for parallel cluster permutation
- [`run_gazepoint_multidimensional_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_multidimensional_cluster_permutation.md)
  : Guardrail for multidimensional cluster permutation
- [`run_gazepoint_cluster_threshold_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_threshold_sensitivity.md)
  : Run threshold-sensitivity checks for Gazepoint cluster permutation
- [`run_gazepoint_tfce()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_tfce.md)
  : Guardrail for threshold-free cluster enhancement
- [`summarise_gazepoint_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_clusters.md)
  : Summarise cluster-based permutation results
- [`summarize_gazepoint_time_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_time_clusters.md)
  : Summarize Gazepoint time clusters
- [`estimate_gazepoint_cluster_onset()`](https://stefanosbalaskas.github.io/gp3tools/reference/estimate_gazepoint_cluster_onset.md)
  : Guardrail for exact cluster-onset estimation
- [`estimate_gazepoint_cluster_offset()`](https://stefanosbalaskas.github.io/gp3tools/reference/estimate_gazepoint_cluster_offset.md)
  : Guardrail for exact cluster-offset estimation
- [`estimate_gazepoint_divergence_point()`](https://stefanosbalaskas.github.io/gp3tools/reference/estimate_gazepoint_divergence_point.md)
  : Estimate a bootstrapped divergence point between two Gazepoint time
  courses
- [`bootstrap_gazepoint_timecourse()`](https://stefanosbalaskas.github.io/gp3tools/reference/bootstrap_gazepoint_timecourse.md)
  : Bootstrap time-course summaries
- [`plot_gazepoint_cluster_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_cluster_results.md)
  : Plot cluster-based permutation results
- [`plot_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_cluster_permutation.md)
  : Plot a Gazepoint cluster-permutation result
- [`plot_gazepoint_cluster_null_distribution()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_cluster_null_distribution.md)
  : Plot the cluster-permutation null distribution
- [`plot_gazepoint_time_varying_effect()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_time_varying_effect.md)
  : Plot a time-varying effect curve
- [`report_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_cluster_permutation.md)
  : Report a Gazepoint cluster-permutation result
- [`export_gazepoint_cluster_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_cluster_results.md)
  : Export Gazepoint cluster-permutation results
- [`export_gazepoint_mne_cluster_input()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_mne_cluster_input.md)
  : Export Gazepoint time-course data for MNE-style cluster workflows
- [`export_gazepoint_permuco_cluster_input()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_permuco_cluster_input.md)
  : Export Gazepoint time-course data for permuco-style cluster
  workflows
- [`export_gazepoint_permutes_cluster_input()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_permutes_cluster_input.md)
  : Export Gazepoint time-course data for permutes-style cluster
  workflows
- [`prepare_gazepoint_timecourse_test_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_timecourse_test_data.md)
  : Prepare time-course data for Gazepoint cluster-permutation testing

## Model diagnostics, summaries, sensitivity, and reporting

Diagnose models, compare nested models, create model tables, and audit
final analysis decisions.

- [`check_gazepoint_model_convergence()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_model_convergence.md)
  : Check model convergence
- [`check_gazepoint_model_singularity()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_model_singularity.md)
  : Check model singularity
- [`check_gazepoint_model_overdispersion()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_model_overdispersion.md)
  : Check model overdispersion
- [`diagnose_gazepoint_glmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/diagnose_gazepoint_glmm.md)
  : Diagnose GLMM, LMM, and GLM models
- [`diagnose_gazepoint_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/diagnose_gazepoint_gamm.md)
  : Diagnose GAM and BAM models
- [`compare_gazepoint_nested_models()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_nested_models.md)
  : Compare nested Gazepoint models
- [`run_gazepoint_model_leave_one_out()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_model_leave_one_out.md)
  : Run leave-one-unit model sensitivity analysis
- [`plot_gazepoint_model_predictions()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_model_predictions.md)
  : Plot observed summaries and model-implied predictions
- [`plot_gazepoint_model_residuals()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_model_residuals.md)
  : Plot model residual diagnostics
- [`tidy_gazepoint_model_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/tidy_gazepoint_model_summary.md)
  : Create a tidy model summary for manuscript tables
- [`summarise_gazepoint_fixed_effects()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_fixed_effects.md)
  : Summarise fixed effects from fitted models
- [`summarise_gazepoint_emmeans()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_emmeans.md)
  : Summarise estimated marginal means and contrasts
- [`export_gazepoint_model_tables()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_model_tables.md)
  : Export manuscript-ready model tables
- [`check_gazepoint_real_data_readiness()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_real_data_readiness.md)
  : Check real-data readiness before Gazepoint analysis
- [`recommend_gazepoint_exclusions()`](https://stefanosbalaskas.github.io/gp3tools/reference/recommend_gazepoint_exclusions.md)
  : Recommend trial and participant exclusions
- [`create_gazepoint_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_reporting_checklist.md)
  : Create a Gazepoint reporting checklist
- [`create_gazepoint_analysis_decision_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_analysis_decision_audit.md)
  : Create a final Gazepoint analysis-decision audit
- [`report_gazepoint_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_multiverse.md)
  : Report multiverse-analysis results
- [`plot_gazepoint_multiverse_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_multiverse_results.md)
  : Plot Gazepoint preprocessing multiverse results
- [`summarise_gazepoint_multiverse_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_multiverse_results.md)
  : Summarise Gazepoint preprocessing multiverse results
- [`audit_gazepoint_condition_quality_imbalance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_condition_quality_imbalance.md)
  : Audit condition-level quality imbalance
- [`audit_gazepoint_design_balance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_design_balance.md)
  : Audit Gazepoint experimental design balance
- [`audit_gazepoint_event_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_event_sync.md)
  : Audit Gazepoint event and timing synchronisation
- [`audit_gazepoint_exclusion_flow()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_exclusion_flow.md)
  : Audit Gazepoint exclusion and retention flow
- [`audit_gazepoint_post_exclusion_balance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_post_exclusion_balance.md)
  : Audit post-exclusion condition balance
- [`audit_gazepoint_timecourse_grid()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_timecourse_grid.md)
  : Audit a Gazepoint time-course grid for cluster-permutation readiness
- [`diagnose_gazepoint_cluster_design()`](https://stefanosbalaskas.github.io/gp3tools/reference/diagnose_gazepoint_cluster_design.md)
  : Diagnose the design assumptions of a Gazepoint cluster-permutation
  workflow
- [`recalibrate_gazepoint_gaze()`](https://stefanosbalaskas.github.io/gp3tools/reference/recalibrate_gazepoint_gaze.md)
  : Offline gaze recalibration using known target coordinates

## Bayesian planning and advanced model preparation

- [`recommend_gazepoint_model_family()`](https://stefanosbalaskas.github.io/gp3tools/reference/recommend_gazepoint_model_family.md)
  : Recommend model families for Gazepoint-derived metrics
- [`check_gazepoint_bayesian_readiness()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_bayesian_readiness.md)
  : Check readiness of a Gazepoint-derived dataset for Bayesian or
  advanced models
- [`create_gazepoint_bayesian_sap()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_bayesian_sap.md)
  : Create a Bayesian ocular Statistical Analysis Plan checklist
- [`create_gazepoint_brms_template()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_brms_template.md)
  : Create brms formula and prior templates for Gazepoint-derived
  metrics
- [`prepare_gazepoint_hddm_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_hddm_export.md)
  : Prepare a trial-level export for Python HDDM
- [`summarize_gazepoint_pupil_response_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_pupil_response_features.md)
  : Summarize pupil response features by subject and trial
- [`compute_gazepoint_scanpath_geometry()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_scanpath_geometry.md)
  : Compute scanpath geometry features by subject and trial
- [`fit_gazepoint_brms_model()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_brms_model.md)
  : Fit an optional brms model for Gazepoint-derived data
- [`create_gazepoint_hddm_fit_script()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_hddm_fit_script.md)
  : Create a Python HDDM fitting script from a Gazepoint HDDM export
- [`select_gazepoint_adaptive_trial()`](https://stefanosbalaskas.github.io/gp3tools/reference/select_gazepoint_adaptive_trial.md)
  : Select the next adaptive trial from candidate stimuli
- [`classify_gazepoint_events_hmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/classify_gazepoint_events_hmm.md)
  : Classify gaze events with a lightweight unsupervised HMM
- [`impute_gazepoint_pupil_gp()`](https://stefanosbalaskas.github.io/gp3tools/reference/impute_gazepoint_pupil_gp.md)
  : Impute missing pupil samples with a lightweight Gaussian-process
  smoother
- [`filter_gazepoint_cnn_uncertainty()`](https://stefanosbalaskas.github.io/gp3tools/reference/filter_gazepoint_cnn_uncertainty.md)
  : Apply uncertainty filtering to Bayesian CNN or webcam gaze outputs

## External face-data and multimodal workflows

Work with externally generated face-analysis outputs and multimodal
response models.

- [`read_gazepoint_face_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_face_export.md)
  : Read external facial-analysis exports
- [`standardize_gazepoint_face_columns()`](https://stefanosbalaskas.github.io/gp3tools/reference/standardize_gazepoint_face_columns.md)
  : Standardise external facial-analysis columns
- [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md)
  : Audit external facial-behaviour data quality
- [`summarize_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_quality.md)
  [`summarise_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_quality.md)
  : Summarise external facial-behaviour data quality
- [`plot_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_face_quality.md)
  : Plot external facial-behaviour data quality
- [`sync_gazepoint_face_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/sync_gazepoint_face_data.md)
  : Synchronise external facial-behaviour data with Gazepoint data
- [`audit_gazepoint_face_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_sync.md)
  : Audit synchronisation between Gazepoint and external
  facial-behaviour data
- [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md)
  : Summarise external facial-behaviour data within analysis windows
- [`summarize_gazepoint_face_reactivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_reactivity.md)
  : Summarise facial-behaviour reactivity between two windows
- [`prepare_gazepoint_multimodal_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_multimodal_data.md)
  : Prepare multimodal Gazepoint and external face-window data
- [`fit_gazepoint_face_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_face_window_lmm.md)
  : Fit a facial-behaviour window mixed or linear model
- [`fit_gazepoint_multimodal_response_model()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_multimodal_response_model.md)
  : Fit a multimodal response model
- [`create_gazepoint_face_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_face_reporting_checklist.md)
  : Create a reporting checklist for external facial-behaviour workflows
- [`report_gazepoint_face_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_face_qc.md)
  : Report external facial-behaviour QC and reporting readiness

## Ecosystem adapters and external package bridges

Prepare Gazepoint data for other R eye-tracking, pupil, sequence, and
export ecosystems.

- [`prepare_gazepoint_eyetrackingr_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_eyetrackingr_data.md)
  : Prepare Gazepoint master data for eyetrackingR-style workflows
- [`prepare_gazepoint_pupillometryr_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupillometryr_data.md)
  : Prepare Gazepoint master data for pupillometryR-style workflows
- [`prepare_gazepoint_gazer_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gazer_data.md)
  : Prepare Gazepoint master data for gazer-style workflows
- [`prepare_gazepoint_eyetools_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_eyetools_data.md)
  : Prepare Gazepoint master data for eyetools-style workflows
- [`run_gazepoint_gazer_crosscheck()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_gazer_crosscheck.md)
  : Run an optional gazeR pupil-preprocessing cross-check
- [`run_gazepoint_eyetools_fixation_detection()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_eyetools_fixation_detection.md)
  : Run optional eyetools fixation and saccade detection
- [`prepare_gazepoint_traminer_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_traminer_data.md)
  : Prepare AOI sequences for TraMineR-style workflows
- [`prepare_gazepoint_semimarkov_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_semimarkov_data.md)
  : Prepare Gazepoint AOI sequences for semi-Markov modelling
- [`prepare_gazepoint_hmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_hmm_data.md)
  : Prepare Gazepoint AOI/state sequences for HMM-style workflows
- [`create_gazepoint_markovchain_object()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_markovchain_object.md)
  : Create a Gazepoint AOI Markov-chain object
- [`summarise_gazepoint_markovchain()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_markovchain.md)
  : Summarise a Gazepoint Markov-chain object
- [`summarise_gazepoint_semimarkov()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_semimarkov.md)
  : Summarise Gazepoint semi-Markov data
- [`export_gazepoint_to_bids()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_to_bids.md)
  : Export Gazepoint data to a lightweight BIDS-style folder

## Plotting and visual diagnostics

General-purpose visual diagnostics for time series, heatmaps, AOIs, and
spatial outputs.

- [`plot_gazepoint_time_series()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_time_series.md)
  : Plot a Gazepoint-style time series
- [`prepare_gazepoint_heatmap_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_heatmap_data.md)
  : Prepare gaze or fixation coordinates for heatmap plotting
- [`plot_gazepoint_heatmap()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_heatmap.md)
  : Plot a Gazepoint gaze or fixation heatmap
- [`plot_gazepoint_heatmap_overlay()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_heatmap_overlay.md)
  : Plot a Gazepoint heatmap over a background image
- [`export_gazepoint_heatmap_png()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_heatmap_png.md)
  : Export a Gazepoint heatmap plot to PNG

## Signal preprocessing extensions

Detect fixation and blink events, process binocular pupil signals,
smooth coordinates, label AOIs, create sliding-window summaries, and
simulate fixation data.

- [`detect_gazepoint_fixations_velocity()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_fixations_velocity.md)
  : Detect fixations with a velocity-threshold algorithm
- [`detect_gazepoint_blinks()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_blinks.md)
  : Detect blink intervals from pupil measurements
- [`interpolate_gazepoint_blinks()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_blinks.md)
  : Interpolate pupil values across detected blink intervals
- [`regress_gazepoint_pupils()`](https://stefanosbalaskas.github.io/gp3tools/reference/regress_gazepoint_pupils.md)
  : Fuse binocular pupil traces using cross-eye regression
- [`mean_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/mean_gazepoint_pupil.md)
  : Calculate mean binocular pupil size
- [`downsample_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/downsample_gazepoint_pupil.md)
  : Downsample pupil data by integer-factor aggregation
- [`smooth_gazepoint_coordinate()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_coordinate.md)
  : Smooth gaze coordinates within independent sequences
- [`add_gazepoint_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_aoi.md)
  : Add rectangular AOI membership to gaze data
- [`analyze_gazepoint_window()`](https://stefanosbalaskas.github.io/gp3tools/reference/analyze_gazepoint_window.md)
  : Summarise gaze or pupil measures in sliding time windows
- [`simulate_gazepoint_fixations()`](https://stefanosbalaskas.github.io/gp3tools/reference/simulate_gazepoint_fixations.md)
  : Simulate Gazepoint-like fixation events

## Example and simulated data

Synthetic data objects and simulation helpers for examples, vignettes,
tests, and teaching.

- [`gazepoint_example_master`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_example_master.md)
  : Example Gazepoint master table
- [`gazepoint_example_fixations`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_example_fixations.md)
  : Example Gazepoint fixation table
- [`gazepoint_example_aoi_geometry`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_example_aoi_geometry.md)
  : Example AOI geometry table
- [`gazepoint_example_aoi_windows`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_example_aoi_windows.md)
  : Example AOI-window summary table
- [`gazepoint_example_pupil_windows`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_example_pupil_windows.md)
  : Example pupil-window summary table
- [`simulate_gazepoint_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/simulate_gazepoint_data.md)
  : Simulate simple Gazepoint-style gaze data
- [`simulate_gazepoint_pupil_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/simulate_gazepoint_pupil_data.md)
  : Simulate Gazepoint-like pupil data
- [`simulate_gazepoint_cluster_timecourse_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/simulate_gazepoint_cluster_timecourse_data.md)
  : Simulate simple Gazepoint cluster time-course data
