# Changelog

## gp3tools 2.0.1.9000

### Scanpath-cluster stability

- Added
  [`bootstrap_gazepoint_scanpath_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/bootstrap_gazepoint_scanpath_clusters.md)
  for subsampling-based co-clustering, adjusted Rand,
  representative-scanpath, and linkage-sensitivity diagnostics.
- Added
  [`summarise_gazepoint_scanpath_cluster_stability()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_scanpath_cluster_stability.md)
  and
  [`plot_gazepoint_scanpath_cluster_stability()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_scanpath_cluster_stability.md)
  for compact stability tables and base-R visual diagnostics.
- Added a synthetic scanpath-cluster stability article and focused
  tests.

### Advanced AOI assignment

- Added
  [`add_gazepoint_polygon_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_polygon_aoi.md)
  for base-R assignment of gaze samples to non-rectangular polygon AOIs.
- Added
  [`add_gazepoint_dynamic_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_dynamic_aoi.md)
  for time-varying rectangular or polygonal AOIs with explicit
  definition-time matching.
- Added
  [`audit_gazepoint_dynamic_aoi_coverage()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_dynamic_aoi_coverage.md)
  for definition coverage, matching-gap, inside/outside, and
  missing-gaze diagnostics.
- Added a synthetic advanced AOI assignment article and focused tests.

### Event-detector comparison workflow

- Added
  [`compare_gazepoint_event_detectors()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_event_detectors.md)
  for standardized comparison of native velocity, HMM, and optional
  [eyetools](https://tombeesley.github.io/eyetools/) event definitions.
- Added
  [`summarise_gazepoint_event_detector_agreement()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_event_detector_agreement.md)
  and
  [`plot_gazepoint_event_detector_agreement()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_event_detector_agreement.md)
  for event counts, duration summaries, temporal overlap, unmatched
  events, and threshold sensitivity diagnostics.
- Added a synthetic event-detector comparison article and focused tests.

### Integrated signal preprocessing workflow

- Added
  [`preprocess_gazepoint_signals()`](https://stefanosbalaskas.github.io/gp3tools/reference/preprocess_gazepoint_signals.md)
  as an auditable orchestration layer for blink detection and
  interpolation, binocular pupil fusion, pupil and coordinate smoothing,
  downsampling, and velocity-based fixation detection.
- The workflow preserves original columns and returns processed data,
  detected events, diagnostics, a decision log, and resolved settings.
- Added a synthetic signal-preprocessing workflow article and focused
  tests.

### Scanpath clustering

- Added
  [`select_gazepoint_scanpath_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/select_gazepoint_scanpath_clusters.md)
  for comparing candidate cluster counts using mean silhouette width.

- Added
  [`extract_gazepoint_representative_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/extract_gazepoint_representative_scanpaths.md)
  for selecting central observed scanpaths within fitted clusters.

- Added
  [`plot_gazepoint_scanpath_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_scanpath_clusters.md)
  for base-R dendrogram, MDS, and silhouette diagnostics.

- Added a synthetic article covering cluster selection, representative
  scanpaths, hierarchical clustering, PAM sensitivity, and conservative
  interpretation.

- Added
  [`cluster_gazepoint_scanpaths()`](https://stefanosbalaskas.github.io/gp3tools/reference/cluster_gazepoint_scanpaths.md)
  for clustering AOI scanpaths from long-format AOI data, pairwise
  distance tables, numeric distance matrices, or `dist` objects.

- Added base-R hierarchical clustering and optional PAM clustering
  through the suggested
  [cluster](https://svn.r-project.org/R-packages/trunk/cluster/)
  package, with cluster assignments, medoids, and silhouette
  diagnostics.

### High-priority signal preprocessing and feature engineering

- Added
  [`detect_gazepoint_fixations_velocity()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_fixations_velocity.md)
  for event-level velocity-threshold fixation extraction.
- Added
  [`detect_gazepoint_blinks()`](https://stefanosbalaskas.github.io/gp3tools/reference/detect_gazepoint_blinks.md)
  and
  [`interpolate_gazepoint_blinks()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_blinks.md)
  for blink-aware pupil processing.
- Added
  [`mean_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/mean_gazepoint_pupil.md),
  [`regress_gazepoint_pupils()`](https://stefanosbalaskas.github.io/gp3tools/reference/regress_gazepoint_pupils.md),
  and
  [`downsample_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/downsample_gazepoint_pupil.md)
  for binocular pupil processing.
- Added
  [`smooth_gazepoint_coordinate()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_coordinate.md)
  for rolling gaze smoothing.
- Added
  [`add_gazepoint_aoi()`](https://stefanosbalaskas.github.io/gp3tools/reference/add_gazepoint_aoi.md)
  for rectangular AOI classification.
- Added
  [`analyze_gazepoint_window()`](https://stefanosbalaskas.github.io/gp3tools/reference/analyze_gazepoint_window.md)
  for sliding-window summaries.
- Added
  [`simulate_gazepoint_fixations()`](https://stefanosbalaskas.github.io/gp3tools/reference/simulate_gazepoint_fixations.md)
  for reproducible examples and workflow testing.
- Added three plot-rich workflow articles covering fixation and blink
  preprocessing, binocular pupil processing, and AOI/window feature
  engineering.

## gp3tools 2.0.1

CRAN release: 2026-07-14

- Added the peer-reviewed software-paper citation: gp3tools: An R
  Package for Reproducible Analysis and Reporting of Gazepoint GP3
  Eye-Tracking Exports (Journal of Eye Movement Research, 2026;
  <doi:10.3390/jemr19040076>).
- Updated `DESCRIPTION`, `README.md`, `inst/CITATION`, and
  `CITATION.cff` with the publication metadata.
- Prepared version 2.0.1 for website, GitHub, Zenodo, and CRAN release
  workflows.
- Retained the existing 2.0.0 Zenodo DOI as a prior-release archive
  rather than assigning it to version 2.0.1.

## gp3tools 2.0.0

- Added three plot-rich workflow articles covering Bayesian planning,
  experimental Bayesian bridges, and metric-extraction/model-boundary
  guidance.

- Added Zenodo DOI `10.5281/zenodo.21292384` to package citation
  metadata.

- Added external facial-behaviour reporting helpers
  [`create_gazepoint_face_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_face_reporting_checklist.md)
  and
  [`report_gazepoint_face_qc()`](https://stefanosbalaskas.github.io/gp3tools/reference/report_gazepoint_face_qc.md)
  for reviewer-facing QC and reporting summaries.

- Added the external face-data reporting article and the complete
  external face-data workflow article.

- Added ten workflow-oriented pkgdown articles covering end-to-end
  analysis, plot galleries, QC dashboards, pupil preprocessing, AOI
  modelling, transitions/scanpaths, model sensitivity, ecosystem
  exports, and synthetic showcases.

- Reorganised the pkgdown article navbar into clearer topical
  categories.

- Updated website documentation to reduce duplicate article labels and
  improve workflow discoverability.

- Development version after the 1.0.2 CRAN patch.

- Added heatmap and spatial visualisation helpers for preparing,
  plotting, overlaying, and exporting gaze heatmaps.

- Added scanpath and coordinate-QC helpers for binocular pupil
  combination, trackloss cleaning, synthetic pupil simulation,
  time-series plotting, multi-scanpath plotting, screen-bound auditing,
  and coordinate harmonisation.

- Added stimulus-layout QC helpers for AOI screen coverage, coordinate
  coverage summaries, and stimulus-layout QC plots.

- Added missingness and data-coverage reporting helpers for summarising,
  plotting, and reporting missing values across selected variables and
  groups.

- Added task-phase segmentation and coverage-reporting helpers for
  explicit phase-window labelling, phase-level data coverage, cautious
  report text, and phase-timeline visualisation.

- Added a QC reporting bundle for collecting existing gp3tools audit,
  readiness, checklist, diagnostic, and reporting objects into compact
  status summaries, cautious report text, and overview plots.

- Added pkgdown articles for heatmap spatial visualisation, scanpath QC
  quick wins, stimulus-layout QC, missingness/data-coverage reporting,
  task-phase segmentation/reporting, and the QC reporting bundle.

## gp3tools 1.0.2

CRAN release: 2026-06-30

- Made one
  [`fit_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_gca.md)
  test robust to platform-specific mixed-model fallback behavior
  observed under CRAN no-long-double checks.

## gp3tools 1.0.1

CRAN release: 2026-06-29

- Prepared the package for CRAN-oriented release validation.
- Added citation metadata through `CITATION.cff` and `inst/CITATION`.
- Added GitHub Actions R-CMD-check workflow for Windows, macOS, Linux,
  and R-devel validation.
- Added a paper-only synthetic showcase source folder outside the R
  package build.
- Updated README citation and validation information.

## gp3tools 1.0.0.9000

### Development version

- Added a public synthetic-realistic Gazepoint export dataset under
  `inst/extdata/gazepoint_realistic_demo_exports/`.
- Added the reproducible generator script
  `data-raw/create_gazepoint_realistic_demo_exports.R`.
- The synthetic demo mimics Gazepoint Analysis all-gaze/fixation export
  structure without including real participant rows.

## gp3tools 1.0.0

### New features

- Added
  [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md)
  for one-command Gazepoint folder analysis.

- Added folder-level import with
  [`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_folder.md).

- Added
  [`check_gazepoint_file_pairs()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_file_pairs.md)
  for checking whether Gazepoint all-gaze and fixation export files are
  correctly paired.

- Added
  [`flag_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_tracking_quality.md)
  for identifying recordings requiring review.

- Added diagnostic plotting functions:

  - [`plot_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_tracking_quality.md)
  - [`plot_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_sampling_rate.md)

- Added
  [`save_gazepoint_plots()`](https://stefanosbalaskas.github.io/gp3tools/reference/save_gazepoint_plots.md)
  for automatic diagnostic plot export.

- Added
  [`create_gazepoint_report()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_report.md)
  for lightweight HTML diagnostic reports.

- Integrated optional HTML report creation into
  [`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_workflow.md)
  with `create_report = TRUE`.

- Added standard CSV export helpers:

  - [`export_gazepoint_tables()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_tables.md)
  - [`write_gazepoint_outputs()`](https://stefanosbalaskas.github.io/gp3tools/reference/write_gazepoint_outputs.md)

- Added
  [`summarise_gazepoint_workflow()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_workflow.md)
  for creating a compact one-row summary of a completed workflow result
  object.

### Master-table tools

- Added
  [`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
  for converting Gazepoint all-gaze exports into a standard sample-level
  master table with time, gaze coordinates, pupil values, validity
  flags, missingness flags, off-screen gaze flags, AOI state, event
  labels, and fixation-related fields.
- Added
  [`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md)
  as the advanced Gazepoint master-table constructor for creating
  analysis-ready sample-level data with participant, media, timing,
  gaze, pupil, AOI, screen, event, response, and metadata fields.
- Added
  [`audit_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_master.md)
  for producing compact quality-audit tables from Gazepoint master
  tables, including overview, subject-level, media-level, AOI-state,
  pupil, and coordinate summaries.
- Added
  [`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md)
  as a formal validation gate before pupil preprocessing, AOI modelling,
  or advanced statistical analysis.
- Added
  [`export_gazepoint_master_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_master_audit.md)
  for exporting the master table, audit tables, and validation tables to
  CSV files.

### Pupil preprocessing tools

- Added
  [`summarise_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil.md)
  as the first pupil-preprocessing gate for Gazepoint master tables. The
  function summarises pupil availability, missing-pupil percentages,
  valid-pupil percentages, pupil distributions, plausible-value checks,
  and IQR-based outlier counts by subject, media, subject-by-media, or
  overall.
- Added
  [`flag_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil.md)
  for marking missing, non-finite, implausible, and IQR-outlying pupil
  samples in Gazepoint master tables. The function preserves the
  original master table, adds explicit pupil-quality flags, records the
  selected pupil/time columns and plausible-value thresholds, and
  creates `pupil_for_preprocessing`, where invalid pupil samples are set
  to `NA` before interpolation, filtering, or baseline correction.
- Added
  [`create_gazepoint_preprocessing_registry()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_registry.md)
  for storing reusable preprocessing parameters, including
  blink/artifact padding, interpolation gap thresholds, smoothing window
  size, baseline windows, physiological pupil thresholds, speed-outlier
  thresholds, binocular-disagreement thresholds, baseline-quality
  thresholds, and overlap-risk settings.
- Added
  [`flag_gazepoint_pupil_artifacts()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_artifacts.md)
  for conservative pupil artifact cleaning before interpolation. The
  function flags blink/trackloss contamination, missing pupil samples,
  non-finite and non-positive values, pupil-speed outliers, binocular
  left-right pupil disagreement, and temporal padding around bad
  samples. It also includes a scale-safety rule that suppresses
  millimetre-based physiological thresholds when they would remove
  nearly all non-missing samples, which protects Gazepoint raw-unit
  exports from being silently erased.
- Added
  [`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md)
  for linearly interpolating short internal gaps in pupil time series.
  The function automatically prefers `pupil_clean` when available,
  followed by `pupil_for_preprocessing`, preserves leading/trailing
  gaps, avoids long gaps, respects grouping by subject/media/trial,
  records interpolation status, gap size, gap duration, and produces
  `pupil_interpolated` for later filtering or baseline correction.
- Added
  [`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md)
  for flexible baseline correction of Gazepoint pupil data after
  flagging and interpolation. The function supports window-based
  baselines such as `c(-200, 0)` or `c(0, 200)`, as well as user-defined
  logical baseline/pre-stimulus flag columns. It produces absolute
  baseline-corrected values, percent change, ratio, z-scored baseline
  correction, baseline availability flags, baseline sample counts, and
  baseline-status labels.
- Added
  [`smooth_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/smooth_gazepoint_pupil.md)
  for sample-based rolling smoothing of Gazepoint pupil time series
  after interpolation and optional baseline correction. The function
  supports mean or median smoothing, centred/right/left-aligned windows,
  custom grouping by subject/media/trial or other columns, optional
  preservation of missing input rows, and records smoothing status,
  window size, input column, method, alignment, and minimum valid-points
  settings.
- Added
  [`summarise_gazepoint_pupil_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_windows.md)
  for aggregating processed Gazepoint pupil data into user-defined
  analysis windows. The function supports numeric window breakpoints and
  labelled custom window tables, flexible grouping by subject, media,
  trial, condition, AOI, or other columns, and produces analysis-ready
  summaries including valid/missing pupil counts, percentages, mean,
  median, SD, quantiles, min/max, AUC, time span, and window-validity
  status.

### Pupil preprocessing audit and sensitivity tools

- Added
  [`audit_gazepoint_pupil_gaps()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_gaps.md)
  for summarising pupil interpolation and missing-gap structure after
  [`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md).
- Added
  [`audit_gazepoint_pupil_baseline()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_baseline.md)
  for checking baseline-correction quality after
  [`baseline_correct_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/baseline_correct_gazepoint_pupil.md).
- Added
  [`audit_gazepoint_pupil_imbalance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_imbalance.md)
  for checking whether preprocessing loss differs across conditions or
  other groups.
- Added
  [`audit_gazepoint_pupil_drift()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_drift.md)
  for assessing tonic pupil/time-on-task drift.
- Added
  [`audit_gazepoint_pupil_overlap_risk()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_overlap_risk.md)
  as an event-response overlap and deconvolution-readiness gate.

### Pupil feature summaries and plotting tools

- Added
  [`summarise_gazepoint_pupil_trial_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_trial_features.md)
  for converting processed pupil time series into trial-level feature
  summaries.
- Added
  [`plot_gazepoint_pupil_status()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_pupil_status.md)
  for visualising observed, interpolated, missing, artifact, and other
  pupil-sample statuses over time or as grouped percentages.
- Added
  [`plot_gazepoint_pupil_timecourse()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_pupil_timecourse.md)
  for plotting binned pupil time courses with mean lines and confidence
  bands.
- Added
  [`plot_gazepoint_pupil_preprocessing()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_pupil_preprocessing.md)
  for single-trial visual audit plots of pupil preprocessing stages.

### Pupil confirmatory window modelling tools

- Added
  [`prepare_gazepoint_pupil_window_model_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_window_model_data.md)
  for preparing pupil-window summaries or pupil trial-feature tables for
  confirmatory window-level modelling. The function standardises
  outcome, subject, condition, window, trial/media identifiers,
  valid-sample counts, total-sample counts, valid-sample proportions,
  weights, model-readiness status, and settings. It also supports common
  Gazepoint pupil-window aliases such as `media_id`, `MEDIA_ID`,
  `n_valid_pupil`, `n_valid_samples`, and `n_samples`.
- Added
  [`fit_gazepoint_pupil_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_window_lmm.md)
  for fitting confirmatory pupil-window linear mixed models with
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html). The function
  supports condition, window, and condition-by-window fixed effects when
  available; automatic fallback for single-condition or single-window
  data; subject random intercepts; optional random window slopes;
  optional valid-sample weighting; singular-fit detection; fallback
  models; fixed-effect tables; model-comparison tables; fitted formulas;
  status labels; and settings.
- Added
  [`fit_gazepoint_pupil_window_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_window_sensitivity.md)
  for confirmatory pupil-window model-family sensitivity checks. The
  function compares unweighted LMMs, weighted LMMs, fixed-effects LMs,
  and weighted LMs without adding heavy robust-model dependencies, and
  returns fitted models, formulas, comparison tables, fixed-effect
  tables, model-status labels, error messages, and settings.

### AOI, fixation, and transition feature tools

- Added
  [`summarise_gazepoint_aoi_entries()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_entries.md)
  for converting sample-level AOI states into ordered AOI-entry
  episodes.
- Added
  [`prepare_gazepoint_aoi_sequences()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_sequences.md)
  for creating transition-ready AOI sequences from sample-level data or
  AOI-entry tables.
- Added
  [`summarise_gazepoint_aoi_transitions()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_transitions.md)
  for trial-level AOI transition summaries.
- Added
  [`compute_gazepoint_aoi_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_aoi_transition_matrix.md)
  for producing AOI transition count matrices, probability matrices,
  grouped matrices, and long-form transition tables.
- Added
  [`plot_gazepoint_aoi_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_transition_matrix.md)
  for plotting AOI transition count or probability heatmaps.
- Added
  [`summarise_gazepoint_aoi_trial_features()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_trial_features.md)
  for trial-level AOI feature extraction.
- Added
  [`summarise_gazepoint_fixation_trials()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_fixation_trials.md)
  for trial-level fixation feature extraction from Gazepoint fixation
  exports.

### AOI-window modelling tools

- Added
  [`summarise_gazepoint_aoi_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_windows.md)
  for converting sample-level AOI states into predefined AOI time-window
  summaries. The function supports numeric window breakpoints, labelled
  window tables, target/distractor AOI definitions, valid/all/AOI-only
  denominators, condition fallback to `all_data`, chronological window
  ordering, and status labels for target-observed and
  target-not-observed windows.
- Added
  [`audit_gazepoint_aoi_window_denominators()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_window_denominators.md)
  for checking denominator adequacy before binomial or logistic
  mixed-effects modelling. The function reports zero, low, missing,
  imbalanced, and variable denominators by row, window, and condition,
  and returns overview, row-audit, window-summary, condition-window,
  imbalance, and flagged-row tables.
- Added
  [`prepare_gazepoint_aoi_glmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_glmm_data.md)
  for preparing AOI-window summaries as binomial success/failure data.
  The function supports valid, all-sample, AOI-only, and custom
  denominators; creates success, failure, denominator, proportion,
  weight, subject, condition, and window columns; and records row-level
  GLMM-readiness status.
- Added
  [`fit_gazepoint_aoi_window_glmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_window_glmm.md)
  for fitting confirmatory AOI-window binomial mixed-effects logistic
  regression models using
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html). The
  function supports condition, window, and condition-by-window fixed
  effects, subject random intercepts, optional random window slopes,
  singular-fit detection, fallback models, model comparison tables, and
  explicit model-status reporting.
- Added
  [`fit_gazepoint_aoi_model_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_model_sensitivity.md)
  for AOI-window model-family sensitivity checks. The function compares
  the main binomial GLMM against empirical-logit LMM, weighted
  proportion LMM, and fixed-effects quasibinomial GLM specifications,
  returning model comparisons, formulas, fixed effects, status labels,
  and settings.

### Time-course modelling helpers

- Added
  [`prepare_gazepoint_pupil_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_gamm_data.md)
  for preparing binned pupil time-course data for
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models.
- Added
  [`fit_gazepoint_pupil_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_gamm.md)
  for fitting pupil time-course GAMMs with
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).
- Added
  [`fit_gazepoint_pupil_pfe_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_pupil_pfe_gamm.md)
  for gaze-position-adjusted pupil GAMM sensitivity analysis.
- Added
  [`prepare_gazepoint_gca_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gca_data.md)
  for Growth Curve Analysis preparation.
- Added
  [`fit_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_gca.md)
  for fitting GCA mixed models with
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html).
- Added
  [`plot_gazepoint_gca()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_gca.md)
  for plotting observed and fitted GCA trajectories.

### Cluster-based permutation testing tools

- Added
  [`prepare_gazepoint_cluster_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_cluster_data.md)
  for preparing sample-level or binned Gazepoint time-course data for
  cluster-based permutation testing. The function standardises subject,
  condition, time-bin, outcome, sample-count, trial-count, status,
  outcome-label, aggregation, bin-size, paired-design, and
  condition-status fields. It supports pupil time courses, AOI
  target-looking indicators, and other numeric or logical time-course
  outcomes.
- Added
  [`run_gazepoint_cluster_permutation()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_cluster_permutation.md)
  for paired within-subject cluster-based permutation testing of
  two-condition time-course divergence. The function uses sign-flip
  permutations, time-wise paired t-statistics, configurable
  cluster-forming thresholds, two-sided or directional tests, multiple
  cluster-statistic options, complete-subject filtering, permutation
  maximum-cluster distributions, cluster-level p-values, model-status
  labels, and explicit circularity warnings.
- Added
  [`summarise_gazepoint_clusters()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_clusters.md)
  for converting cluster-permutation results into compact reporting
  tables, including overview, all observed clusters, significant
  clusters, time-course summary, permutation-distribution summary,
  settings, and circularity warning tables.
- Added
  [`plot_gazepoint_cluster_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_cluster_results.md)
  for plotting cluster-permutation results. The function supports
  mean-difference, test-statistic, or two-panel plots; optional cluster
  shading; candidate-bin markers; threshold lines; zero-reference lines;
  custom titles and labels; and publication-ready `ggplot2` output.
- The cluster-based permutation branch is intended for time-course
  inference and explicitly warns against using the detected cluster to
  define a confirmatory window and then retesting that same window in a
  second confirmatory model.

### AOI time-course GAMM tools

- Added
  [`prepare_gazepoint_aoi_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_gamm_data.md)
  for preparing sample-level or binned AOI data for AOI time-course GAMM
  analysis. The function creates subject-by-condition-by-time-bin
  binomial success/failure summaries for target-AOI looking, supports
  AOI-column and logical/numeric indicator workflows, valid/all/AOI-only
  denominator definitions, condition fallback to `all_data`, custom time
  bins, denominator filtering, and model-readiness status fields.
- Added
  [`fit_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_aoi_gamm.md)
  for fitting binomial AOI target-looking GAMMs using
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html). The function
  models target-AOI looking over time using success/failure counts,
  supports condition fixed effects when available, condition-specific
  smooths, subject random-effect smooths, optional subject-specific time
  smooths, automatic single-condition fallback, model diagnostics,
  formula reporting, parametric and smooth tables, and captured model
  warnings.
- Added
  [`plot_gazepoint_aoi_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_gamm.md)
  for plotting observed AOI target-looking proportions and fitted
  AOI-GAMM trajectories. The function supports single-condition and
  multi-condition plots, observed-only and fitted-only views, confidence
  ribbons, population-level predictions with subject random effects
  excluded by default, custom labels, and publication-ready `ggplot2`
  output.
- The AOI time-course GAMM branch is intended for modelling smooth
  target-looking trajectories over time and is separate from
  confirmatory AOI-window GLMMs and cluster-based permutation tests.

### Model diagnostics tools

- Added
  [`check_gazepoint_model_convergence()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_model_convergence.md)
  for compact convergence diagnostics across fitted models used in
  `gp3tools` workflows. The helper supports `lme4` mixed models, `mgcv`
  GAM/BAM objects, `glm` objects, and ordinary `lm` objects where
  applicable, and returns a tidy diagnostic table with convergence
  status, model class, diagnostic status, and message.
- Added
  [`check_gazepoint_model_singularity()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_model_singularity.md)
  for checking singular random-effects structures in `lme4` mixed models
  using
  [`lme4::isSingular()`](https://rdrr.io/pkg/lme4/man/isSingular.html).
  The helper reports singular fits as structured diagnostic output
  rather than package failures, and returns `not_applicable` for model
  classes where singularity is not meaningful.
- Added
  [`check_gazepoint_model_overdispersion()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_model_overdispersion.md)
  for Pearson-residual overdispersion diagnostics in binomial,
  quasibinomial, Poisson, quasipoisson, and negative-binomial-like
  models. The helper returns dispersion ratios, Pearson chi-square
  values, residual degrees of freedom, threshold-based overdispersion
  flags, and diagnostic messages.
- Added
  [`diagnose_gazepoint_glmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/diagnose_gazepoint_glmm.md)
  as a reusable diagnostics bundle for GLMM, LMM, and GLM workflows. The
  function combines convergence, singularity, overdispersion, and
  optional DHARMa simulation-based residual diagnostics into a
  structured `gp3_model_diagnostics` object with overview, convergence,
  singularity, overdispersion, DHARMa, and settings tables.
- Added
  [`diagnose_gazepoint_gamm()`](https://stefanosbalaskas.github.io/gp3tools/reference/diagnose_gazepoint_gamm.md)
  as a reusable diagnostics bundle for
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) workflows. The
  function combines convergence checks,
  [`mgcv::k.check()`](https://rdrr.io/pkg/mgcv/man/k.check.html)
  basis-dimension diagnostics, overdispersion checks when meaningful,
  and optional DHARMa diagnostics into a structured
  `gp3_model_diagnostics` object.
- Added optional DHARMa support for model diagnostics. `DHARMa` is
  listed in `Suggests`, not `Imports`, and diagnostics skip cleanly with
  `skipped_missing_package` when DHARMa is not installed.

### Manuscript-ready model table tools

- Added
  [`summarise_gazepoint_fixed_effects()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_fixed_effects.md)
  for creating manuscript-ready fixed-effect summary tables from `lm`,
  `glm`, `lme4` mixed models, and `mgcv` GAM/BAM models. The function
  supports `gp3tools` fit objects containing a `$model` element, Wald
  confidence intervals, optional exponentiation for odds ratios or rate
  ratios, intercept filtering, significance stars, and structured
  diagnostic status fields.
- Added
  [`tidy_gazepoint_model_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/tidy_gazepoint_model_summary.md)
  for combining model metadata, fixed-effect summaries, and optional
  model diagnostics into a structured `gp3_model_summary` object. The
  returned object contains `overview`, `model_info`, `fixed_effects`,
  `diagnostics`, and `settings` components.
- Added
  [`summarise_gazepoint_emmeans()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_emmeans.md)
  for estimated marginal means and pairwise contrasts using optional
  `emmeans`. The function returns structured `overview`, `emmeans`,
  `contrasts`, and `settings` tables and skips cleanly with
  `skipped_missing_package` if `emmeans` is not installed.
- Added
  [`export_gazepoint_model_tables()`](https://stefanosbalaskas.github.io/gp3tools/reference/export_gazepoint_model_tables.md)
  for exporting manuscript-ready model summaries, fixed effects,
  diagnostics, estimated marginal means, contrasts, settings, and
  export-index tables to CSV files.
- Added optional `emmeans` support in `Suggests` for estimated marginal
  means and pairwise contrasts without making it a required package
  dependency.

### Final analysis-decision audit tools

- Added
  [`create_gazepoint_analysis_decision_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_analysis_decision_audit.md)
  for creating a final analysis-decision audit across completed
  Gazepoint analysis branches. The function records which branches were
  run, classifies each branch as confirmatory, sensitivity, exploratory,
  diagnostic, preprocessing, reporting, or unknown, summarises available
  diagnostics, flags interpretation cautions, and creates a final
  analysis-readiness table.
- The audit returns a structured `gp3_analysis_decision_audit` object
  with `overview`, `branch_audit`, `diagnostics_summary`,
  `interpretation_cautions`, `readiness`, and `settings` components.
- Added support for required confirmatory branches, optional
  clean-diagnostics requirements, missing-branch detection,
  fallback-model cautions, singular-fit cautions, exploratory-analysis
  cautions, and sensitivity-analysis cautions.
- The final analysis-decision audit is intended as the last reporting
  gate after confirmatory models, sensitivity analyses, exploratory
  time-course analyses, diagnostics, and manuscript-ready model tables
  have been created.

### Preprocessing multiverse / sensitivity-check tools

- Added
  [`create_gazepoint_preprocessing_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_multiverse.md)
  for defining preprocessing multiverse specifications across pupil and
  AOI workflows. The function creates structured pupil, AOI, and
  combined branch grids for alternative preprocessing decisions such as
  pupil interpolation gap thresholds, smoothing windows, baseline
  windows, artifact-padding settings, AOI denominator definitions, and
  minimum denominator thresholds.
- Added
  [`run_gazepoint_pupil_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_pupil_multiverse.md)
  for running pupil preprocessing branches from a preprocessing
  multiverse object. The runner applies branch-specific artifact
  flagging, interpolation, baseline correction, smoothing, and optional
  pupil-window summarisation, while recording completed and failed
  branches.
- Added
  [`run_gazepoint_aoi_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_aoi_multiverse.md)
  for running AOI preprocessing branches from a preprocessing multiverse
  object. The runner creates AOI-window summaries and branch-specific
  AOI GLMM preparation tables using alternative denominator and
  minimum-denominator decisions.
- Added
  [`summarise_gazepoint_multiverse_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_multiverse_results.md)
  for combining pupil and AOI multiverse results into overview,
  branch-summary, failure-summary, and settings tables.
- Added
  [`plot_gazepoint_multiverse_results()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_multiverse_results.md)
  for visualising multiverse branch status, retained rows, pupil
  preprocessing settings, and AOI denominator settings.
- The preprocessing multiverse branch is intended for transparent
  sensitivity analysis across reasonable preprocessing choices, not for
  selecting the most favourable result.

### General publication-level audit helpers

- Added
  [`audit_gazepoint_event_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_event_sync.md)
  for checking event-marker availability, expected event labels,
  duplicate timestamps, sparse units, and unusually large time gaps.
- Added
  [`audit_gazepoint_design_balance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_design_balance.md)
  for auditing observed subject-by-condition design balance before
  exclusions.
- Added
  [`audit_gazepoint_exclusion_flow()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_exclusion_flow.md)
  for summarising retained versus excluded analysis units, exclusion
  reasons, condition-level retention, and subject-level retention.
- Added
  [`audit_gazepoint_gaze_signal_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_gaze_signal_quality.md)
  for auditing gaze-coordinate availability, validity columns, missing
  gaze, off-screen gaze, and optional pupil availability.
- Added
  [`audit_gazepoint_condition_quality_imbalance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_condition_quality_imbalance.md)
  for checking whether quality metrics differ across experimental
  conditions.
- Added
  [`audit_gazepoint_post_exclusion_balance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_post_exclusion_balance.md)
  for checking whether retained analysis units remain balanced across
  subjects and conditions after exclusions.
- The general audit branch is intended as a publication-readiness layer
  before confirmatory modelling, sensitivity analysis, and final
  interpretation.

### AOI geometry and verification tools

- Added
  [`audit_gazepoint_aoi_geometry()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_geometry.md)
  for checking AOI size, area, coordinate validity, screen-bound status,
  and duplicate AOI geometry.
- Added
  [`audit_gazepoint_aoi_overlap()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_overlap.md)
  for identifying pairwise AOI overlap within each stimulus or media
  item.
- Added
  [`audit_gazepoint_aoi_margin_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_margin_sensitivity.md)
  for auditing whether AOI assignments are sensitive to small boundary
  expansions or shrinkages.
- Added
  [`audit_gazepoint_aoi_coding_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_aoi_coding_matrix.md)
  for validating observed AOI labels against geometry-derived AOI labels
  and producing coding/confusion matrices.
- Added
  [`plot_gazepoint_aoi_verification()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_aoi_verification.md)
  for visual AOI verification with optional gaze-point overlays.
- The AOI geometry and verification layer supports publication-readiness
  checks before AOI-window modelling, transition analysis, and
  confirmatory AOI interpretation.

### Advanced sequence-model adapters

- Added advanced AOI/state sequence-model preparation helpers:

  - [`create_gazepoint_markovchain_object()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_markovchain_object.md)
  - [`prepare_gazepoint_semimarkov_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_semimarkov_data.md)
  - [`prepare_gazepoint_hmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_hmm_data.md)

- Added
  [`create_gazepoint_markovchain_object()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_markovchain_object.md)
  for creating dependency-free Markov-chain-style AOI/state objects with
  transition counts, transition probabilities, transition matrices,
  sequence-level transition data, state ordering, optional state
  exclusion, optional missing-state labelling, optional self-transition
  handling, and Laplace smoothing.

- Added
  [`prepare_gazepoint_semimarkov_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_semimarkov_data.md)
  for converting ordered AOI/state observations into semi-Markov-ready
  state-visit and transition tables with dwell durations, next-state
  labels, terminal-state handling, sequence summaries, state summaries,
  transition summaries, optional covariate carry-through, and optional
  repeated-state collapsing.

- Added
  [`prepare_gazepoint_hmm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_hmm_data.md)
  for creating dependency-free HMM-ready AOI/state sequence structures
  with ordered observation data, initial-state probabilities, transition
  count/probability matrices, transition tables, observation summaries,
  emission-format data, optional numeric observation scaling, optional
  terminal-state transitions, optional covariate carry-through, and
  optional missing-state labelling.

### Package-adapter export layer

- Added dependency-free package-adapter helpers for exporting `gp3tools`
  master/sample tables to external R eye-tracking workflows:

  - [`prepare_gazepoint_eyetrackingr_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_eyetrackingr_data.md)
  - [`prepare_gazepoint_pupillometryr_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupillometryr_data.md)
  - [`prepare_gazepoint_gazer_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_gazer_data.md)
  - [`prepare_gazepoint_eyetools_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_eyetools_data.md)

- These helpers create clean, package-friendly tibbles without importing
  or depending on the external packages directly.

- Added eyetrackingR-style sample-level export with participant, trial,
  time, gaze coordinates, AOI labels, AOI indicator columns, trackloss
  status, and adapter metadata.

- Added pupillometryR-style sample-level export with participant, trial,
  time, pupil, event, baseline, pupil-validity, trackloss, and adapter
  metadata.

- Added gazer-style sample-level export with participant, trial, time,
  gaze coordinates, optional pupil, AOI labels, fixation IDs, validity
  flags, off-screen detection, trackloss status, and adapter metadata.

- Added eyetools-style sample-level export with participant, trial,
  time, primary and binocular gaze coordinates, pupil columns, AOI
  labels, fixation IDs, event labels, validity flags, off-screen
  detection, trackloss status, and adapter metadata.

### Advanced sensitivity, recalibration, and reporting helpers

- Added
  [`estimate_gazepoint_divergence_point()`](https://stefanosbalaskas.github.io/gp3tools/reference/estimate_gazepoint_divergence_point.md)
  for estimating the earliest reliable divergence between two condition
  time courses using bootstrap confidence intervals. The helper supports
  participant-, trial-, and row-level bootstrap units, mean or median
  summaries, directional testing, consecutive-point onset rules,
  no-divergence handling, optional bootstrap-output retention, and
  onset-time uncertainty summaries.

- Added
  [`run_gazepoint_model_leave_one_out()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_model_leave_one_out.md)
  for generic leave-one-unit model sensitivity analysis. The helper
  refits a user-supplied model while leaving out one participant, item,
  stimulus, trial, or other unit at a time. It supports custom effect
  extraction, effect-term filtering, fit/extraction error tracking,
  optional model retention, and effect-stability summaries including
  maximum absolute change, largest-change unit, percent change, and
  sign-flip detection.

- Added
  [`transform_gazepoint_aoi_empirical_logit()`](https://stefanosbalaskas.github.io/gp3tools/reference/transform_gazepoint_aoi_empirical_logit.md)
  for transforming bounded AOI proportions into finite empirical logits.
  The helper supports numerator/denominator count input, proportion-only
  input with pseudo-denominators, correction constants for 0/1
  proportions, custom output columns, overwrite protection, row-level
  transformation statuses, and overview/status/settings attributes.

- Added
  [`prepare_gazepoint_fixation_aligned_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_fixation_aligned_data.md)
  for fixation-, saccade-, and AOI-contingent alignment. The helper
  aligns observations to first target entry, first fixation to target,
  first saccade to AOI, first fixation, or custom event markers, and
  returns aligned data, event tables, trial summaries,
  baseline/analysis-window flags, pre/post-event phases,
  target-preexisting flags, and already-on-target-at-start indicators.

- Added
  [`plot_gazepoint_model_predictions()`](https://stefanosbalaskas.github.io/gp3tools/reference/plot_gazepoint_model_predictions.md)
  for plotting observed summaries together with model-implied prediction
  trajectories. The helper supports model objects for which
  [`predict()`](https://rdrr.io/r/stats/predict.html) works, including
  `lm`, `glm`, mixed-model, GAMM, and GCA-style workflows, and stores
  observed summaries, prediction summaries, overview metadata, and
  settings as plot attributes.

- Added
  [`compare_gazepoint_nested_models()`](https://stefanosbalaskas.github.io/gp3tools/reference/compare_gazepoint_nested_models.md)
  for comparing ordered nested models. The helper returns model-level
  AIC, BIC, log-likelihood, degrees of freedom, likelihood-ratio tests,
  model rankings, sequential or against-first comparisons,
  extraction/comparison statuses, and fallback support for simple custom
  list-like model wrappers.

- Added
  [`flag_gazepoint_pupil_hampel()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_gazepoint_pupil_hampel.md)
  as an optional Hampel-filter pupil artifact helper. The function
  applies a rolling Hampel filter to pupil data, supports grouping and
  time ordering, configurable window size, threshold multiplier, minimum
  valid samples, MAD scaling, optional corrected pupil output, custom
  output columns, overwrite protection, row-level statuses, and
  overview/status/settings attributes.

- Added
  [`recalibrate_gazepoint_gaze()`](https://stefanosbalaskas.github.io/gp3tools/reference/recalibrate_gazepoint_gaze.md)
  for offline gaze recalibration and drift correction using known target
  or check-target coordinates. The helper estimates group-level
  horizontal and vertical gaze shifts, applies median or mean drift
  correction, supports calibration-row filters, maximum-shift blocking,
  grouped correction summaries, row-level statuses, before/after
  target-error columns, custom output columns, and
  overview/status/settings attributes.

- Added
  [`recommend_gazepoint_exclusions()`](https://stefanosbalaskas.github.io/gp3tools/reference/recommend_gazepoint_exclusions.md)
  for creating explicit trial-level and participant-level exclusion
  recommendations. The helper uses validity flags, gaze-coordinate
  missingness, pupil missingness, artifact flags, sample-count
  thresholds, missingness thresholds, and artifact-rate thresholds to
  return transparent participant recommendations, trial recommendations,
  a combined exclusion table, overview metadata, and settings. The
  helper recommends exclusions only; it does not remove data.

### Improvements

- Added a complete standalone model-diagnostics branch for convergence
  checks, singular-fit checks, overdispersion diagnostics, optional
  DHARMa residual diagnostics, and GAM/BAM basis-dimension checks.
- Added reusable model-diagnostics wrappers for GLMM/LMM/GLM models and
  GAM/BAM models, returning compact overview tables and component
  diagnostic tables that can later be integrated into existing modelling
  helpers.
- Added a complete manuscript-ready model-table branch for publication
  outputs, including fixed-effect summaries, model metadata, diagnostics
  summaries, estimated marginal means, pairwise contrasts, and CSV
  export helpers.
- Expanded the time-course and modelling layer to cover pupil GAMMs,
  gaze-position/PFE pupil sensitivity GAMMs, GCA mixed models,
  cluster-based permutation tests, AOI target-looking GAMMs, standalone
  GLMM/GAMM diagnostics, and manuscript-ready model tables.
- Added a complete AOI, fixation, and transition feature-extraction
  layer for Gazepoint workflows, covering AOI-entry episodes, AOI
  sequences, transition summaries, transition matrices, transition
  heatmaps, AOI trial features, and fixation trial features.
- Added a complete AOI-window modelling branch for confirmatory
  target/distractor AOI analyses using predefined time windows,
  denominator audits, binomial GLMM preparation, mixed-effects logistic
  regression, and model-family sensitivity checks.
- Added a complete confirmatory pupil-window modelling branch for
  predefined pupil windows, including model-data preparation,
  trial/window-level LMM fitting, optional valid-sample weighting,
  fixed-effects LM sensitivity checks, fallback handling for
  single-condition or single-window data, singular-fit reporting,
  model-comparison tables, and fixed-effect summaries.
- Added a complete cluster-based permutation testing branch for
  time-course divergence analysis, including cluster-data preparation,
  paired sign-flip permutation testing, cluster-level summaries, and
  publication-ready cluster-result plotting.
- Improved AOI-state handling by consistently distinguishing AOI,
  non-AOI/background, and missing AOI states across entry, sequence,
  transition, matrix, plotting, trial-feature, AOI-window modelling, and
  cluster-preparation functions.
- Improved support for target-versus-distractor AOI analysis, including
  target/distractor dwell, TTFF, revisits, fixation counts, fixation
  duration, transition direction, windowed target proportions,
  denominator checking, status labels when target or distractor AOIs are
  not observed, and AOI target-looking time-course preparation for
  cluster testing.
- Improved fixation-trial summarisation for Gazepoint fixation exports
  by automatically detecting common fixation columns such as `FPOGS`,
  `FPOGD`, `FPOGX`, `FPOGY`, `FPOGID`, `FPOGV`, `AOI`, `MEDIA_ID`, and
  informative participant identifiers such as `USER_FILE`.
- Added a modelling layer for pupil time-course analysis, including GAMM
  preparation, main pupil GAMMs, gaze-position/PFE sensitivity GAMMs,
  GCA preparation, GCA mixed models, GCA visualisation, and
  cluster-based time-course inference.
- Improved support for datasets without usable condition labels by using
  a consistent `all_data` fallback in pupil time-course preparation,
  GAMM modelling, GCA preparation, GCA plotting, AOI-window summaries,
  AOI GLMM preparation, AOI-window mixed modelling, pupil-window
  model-data preparation, pupil-window mixed modelling, and cluster-data
  preparation.
- Improved modelling diagnostics by returning explicit status fields,
  fallback indicators, model-comparison tables, fitted formulas,
  fixed-effect tables, error messages, model settings, permutation
  distributions, cluster-level p-values, and circularity warnings.
- Added tests for AOI entries, AOI sequences, AOI transition summaries,
  AOI transition matrices, AOI transition heatmaps, AOI trial features,
  fixation trial features, AOI-window summaries, AOI denominator audits,
  AOI GLMM preparation, AOI-window GLMM fitting, AOI model-family
  sensitivity, pupil GAMM preparation, pupil GAMM fitting, PFE-adjusted
  pupil GAMMs, GCA data preparation, GCA mixed-model fitting, GCA
  plotting, pupil-window model-data preparation, pupil-window LMM
  fitting, pupil-window model-family sensitivity, cluster-data
  preparation, cluster-based permutation testing, cluster-result
  summarisation, cluster-result plotting, AOI-GAMM data preparation,
  AOI-GAMM fitting, AOI-GAMM plotting, model convergence checks,
  singularity checks, overdispersion checks, GLMM diagnostics, GAMM
  diagnostics, fixed-effect summaries, tidy model summaries, estimated
  marginal means, and model-table exports.
- Added a complete final analysis-decision audit branch for recording
  completed analysis branches, distinguishing confirmatory, sensitivity,
  exploratory, diagnostic, preprocessing, and reporting outputs,
  summarising diagnostics, flagging interpretation cautions, and
  producing a final analysis-readiness decision table.
- Expanded the reporting layer to cover manuscript-ready fixed-effect
  tables, estimated marginal means, pairwise contrasts, model-table CSV
  exports, and final analysis-decision/readiness audits.
- Added tests for final analysis-decision auditing, including missing
  required confirmatory branches, confirmatory models without
  diagnostics, diagnostic warnings, clean-diagnostics requirements,
  fallback models, singular fits, invalid branch-role inputs, and named
  results-list workflows.
- Added a complete preprocessing multiverse branch for defining,
  running, summarising, and plotting pupil and AOI preprocessing
  sensitivity checks across alternative preprocessing decisions.
- Expanded the sensitivity layer beyond model-family sensitivity checks
  by supporting preprocessing-decision multiverses for pupil
  interpolation, smoothing, baseline windows, artifact padding, AOI
  denominator definitions, and minimum denominator thresholds.
- Added tests for preprocessing multiverse specification, pupil
  multiverse runners, AOI multiverse runners, multiverse summaries, and
  multiverse plots.
- Added a complete general publication-level audit branch for checking
  event synchronisation, design balance, exclusion flow, gaze-signal
  quality, condition-level quality imbalance, and post-exclusion
  retained-sample balance.
- Added a complete AOI geometry and verification branch for checking AOI
  geometry validity, AOI overlap, margin sensitivity,
  observed-versus-derived AOI coding consistency, and visual AOI
  verification.
- Added
  [`check_gazepoint_real_data_readiness()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_gazepoint_real_data_readiness.md),
  an explicit final readiness gate for real-data analysis. The helper
  returns structured `overview`, `gate_decision`, `checks`,
  `detected_columns`, `data_summary`, `condition_summary`, and
  `settings` outputs, with pass/warn/fail readiness status.
- Added
  [`run_gazepoint_eyetools_fixation_detection()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_eyetools_fixation_detection.md),
  an optional external-detector wrapper for `eyetools`. The wrapper
  prepares Gazepoint data using the expected `pID`, `trial`, `time`,
  `x`, and `y` schema, supports dispersion, VTI fixation, and VTI
  saccade branches, and records clean skipped, complete,
  partial-complete, and error statuses.
- Added
  [`create_gazepoint_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_reporting_checklist.md),
  an auto-generated reporting checklist for manuscript/report
  preparation. It summarises reporting readiness across data structure,
  readiness gates, import/workflow checks, sampling/tracking quality,
  AOI reporting, pupil reporting, model diagnostics, sensitivity
  analyses, reproducibility, and optional advanced methods.
- Added
  [`compute_gazepoint_time_varying_transition_matrix()`](https://stefanosbalaskas.github.io/gp3tools/reference/compute_gazepoint_time_varying_transition_matrix.md),
  a dedicated helper for transition-count and transition-probability
  matrices by time window and grouping variables.
- Added
  [`fit_gazepoint_transition_count_nb_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_transition_count_nb_sensitivity.md),
  an optional negative-binomial transition-count sensitivity model using
  `glmmTMB` when available.
- Added
  [`run_gazepoint_gazer_crosscheck()`](https://stefanosbalaskas.github.io/gp3tools/reference/run_gazepoint_gazer_crosscheck.md),
  an optional external gazeR preprocessing cross-check wrapper.
- Added
  [`audit_gazepoint_stimulus_luminance()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_stimulus_luminance.md),
  [`audit_gazepoint_pupil_reliability()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_pupil_reliability.md),
  and
  [`interpolate_gazepoint_pupil_pchip()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil_pchip.md)
  for pupil-analysis robustness and reporting support.
- Added advanced validation tests for bootstrapped divergence-point
  analysis, leave-one-unit model sensitivity,
  fixation/saccade-contingent alignment, AOI empirical-logit
  transformation, model-prediction plotting, nested model comparison,
  Hampel pupil artifact detection, offline gaze recalibration, and
  explicit trial/participant exclusion recommendations.
- Resolved R CMD check notes caused by `.env` pronoun use in internal
  helper functions by replacing dplyr pronoun-based assignment with
  explicit local data-frame assignment.

### Current validation status

Recent focused validations completed successfully for the following
advanced helpers:

``` r

devtools::test(filter = "estimate_gazepoint_divergence_point")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 95 ]

devtools::test(filter = "run_gazepoint_model_leave_one_out")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 98 ]

devtools::test(filter = "prepare_gazepoint_fixation_aligned_data")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 117 ]

devtools::test(filter = "transform_gazepoint_aoi_empirical_logit")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 89 ]

devtools::test(filter = "plot_gazepoint_model_predictions")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 83 ]

devtools::test(filter = "compare_gazepoint_nested_models")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 95 ]

devtools::test(filter = "flag_gazepoint_pupil_hampel")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 77 ]

devtools::test(filter = "recalibrate_gazepoint_gaze")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 109 ]

devtools::test(filter = "recommend_gazepoint_exclusions")
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 89 ]
```

The current full-package validation status after the README, vignette,
and example-data branches is:

``` r

devtools::test()
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 6788 ]

devtools::check()
# 0 errors | 0 warnings | 0 notes
```

During full tests, `boundary (singular) fit: see help('isSingular')`
messages may appear in mixed-model diagnostic contexts. These are
expected diagnostic messages from singular-fit test fixtures and are not
package failures when the final test summary reports `FAIL 0 | WARN 0`.

On some Windows systems, a Quarto/TMPDIR message may appear after
`devtools::check()`. This is harmless when the final
`R CMD check results` report:

``` r
0 errors | 0 warnings | 0 notes
```

#### Example datasets for runnable documentation

- Added lightweight built-in example datasets so README examples,
  vignettes, tests, and user workflows can run without private Gazepoint
  files:

  - `gazepoint_example_master`
  - `gazepoint_example_fixations`
  - `gazepoint_example_aoi_geometry`
  - `gazepoint_example_aoi_windows`
  - `gazepoint_example_pupil_windows`

- Added `data-raw/create_gazepoint_example_data.R` to regenerate the
  example datasets reproducibly.

- Added dataset documentation in `R/data.R`.

- Added focused tests for the example datasets and their compatibility
  with core master-table, pupil-window, AOI-window, and AOI-geometry
  workflows.

- Excluded `data-raw/` from the built package via `.Rbuildignore` to
  avoid R CMD check top-level file notes.

- Current validation after the example-data branch:

  - `devtools::test()` reports `FAIL 0 | WARN 0 | SKIP 0 | PASS 6788`.
  - `devtools::check()` reports `0 errors | 0 warnings | 0 notes`.

### Notes

- This version is intended as the first stable internal prototype for
  Gazepoint GP3 export workflows.
- The current final analysis-decision audit set is complete for
  recording completed branches, classifying analyses as confirmatory,
  sensitivity, exploratory, diagnostic, preprocessing, or reporting,
  summarising diagnostics, flagging interpretation cautions, and
  producing final readiness decisions.
- The current pupil preprocessing sensitivity/audit set is complete for
  interpolation gaps, baseline quality, preprocessing imbalance, drift,
  event-response overlap risk, split-half pupil reliability, PCHIP
  interpolation sensitivity, stimulus-luminance auditing, and optional
  Hampel-filter pupil artifact sensitivity.
- The current pupil feature and visual-diagnostics set is complete for
  trial-level feature extraction, preprocessing-status plots,
  condition/time-course plots, single-trial preprocessing audit plots,
  and model-implied prediction plots.
- The current pupil confirmatory window modelling set is complete for
  model-data preparation, confirmatory pupil-window LMM fitting,
  optional valid-sample weighting, fixed-effects LM sensitivity checks,
  fallback handling, singular-fit reporting, model-comparison tables,
  nested model comparisons, fixed-effect summaries, and leave-one-unit
  model sensitivity.
- The current AOI, fixation, and transition feature set is complete for
  AOI entries, AOI sequences, AOI transition summaries, AOI transition
  matrices, time-varying transition matrices, transition heatmaps, AOI
  trial-level features, fixation trial-level features, and
  fixation/saccade-contingent alignment.
- The current AOI-window modelling set is complete for window summaries,
  denominator audits, binomial GLMM preparation, AOI-window
  mixed-effects logistic regression, empirical-logit AOI transformation,
  AOI-window model-family sensitivity checks, and nested
  model-comparison support.
- The current cluster-based permutation testing set is complete for
  time-course cluster-data preparation, paired sign-flip cluster
  permutation testing, cluster-level result summaries, publication-ready
  cluster-result plotting, bootstrapped divergence-point estimation, and
  explicit circularity warnings about exploratory window selection.
- The current modelling helper set is complete for binned pupil-GAMM
  preparation, main pupil GAMMs, gaze-position/PFE sensitivity GAMMs,
  GCA data preparation, GCA mixed-model fitting, observed-versus-fitted
  GCA plots, general model-prediction plots, AOI-window GLMM modelling,
  AOI-window model-family sensitivity, confirmatory pupil-window LMMs,
  pupil-window model-family sensitivity, AOI time-course GAMMs,
  cluster-based time-course inference, standalone model diagnostics,
  nested model comparison, fixed-effect summaries, estimated marginal
  means, manuscript-ready model-table exports, and leave-one-unit
  sensitivity checks.
- The current AOI time-course GAMM set is complete for target-AOI
  time-course preparation, binomial GAMM fitting, single-condition
  fallback, condition-specific smooths, subject random-effect smooths,
  model diagnostics, and observed-versus-fitted AOI trajectory plots.
- The current standalone model-diagnostics set is complete for
  convergence checks, singular-fit checks, overdispersion checks,
  GLMM/LMM/GLM diagnostic bundles, GAM/BAM diagnostic bundles, optional
  DHARMa residual diagnostics, GAM basis-dimension checks, nested model
  comparison, and model-implied prediction visualisation.
- The current manuscript-ready model-table set is complete for
  fixed-effect summaries, tidy model-summary objects, estimated marginal
  means, pairwise contrasts, optional `emmeans` support, CSV export of
  model tables, model-implied prediction visualisation, and nested
  model-comparison reporting.
- The current AOI geometry and verification set is complete for AOI
  geometry validity checks, AOI overlap audits, AOI-margin sensitivity
  checks, observed-versus-derived AOI coding validation, and visual AOI
  verification plots.
- The current external cross-check and adapter set is complete for
  dependency-free exports to eyetrackingR-style, pupillometryR-style,
  gazer-style, and eyetools-style workflows, optional external gazeR
  preprocessing cross-checks, and optional external eyetools
  fixation/saccade detection wrappers.
- The current recalibration and exclusion-decision set is complete for
  offline gaze drift correction using known target/check-target
  coordinates and explicit trial/participant exclusion recommendation
  tables.
- Current full package status after the README, vignette, and
  example-data branches: `devtools::test()` passes with 6788 tests, and
  `devtools::check()` returns 0 errors, 0 warnings, and 0 notes.
