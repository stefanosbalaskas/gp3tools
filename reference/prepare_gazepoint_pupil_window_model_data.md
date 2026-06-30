# Prepare pupil-window data for confirmatory mixed models

Prepare pupil-window summaries or pupil trial-feature tables for
confirmatory window-level modelling. The function standardises subject,
condition, window, trial/media identifiers, outcome, valid-sample
counts, total-sample counts, valid-sample proportions, weights, and
model-readiness status columns.

## Usage

``` r
prepare_gazepoint_pupil_window_model_data(
  data,
  outcome_col = "mean_pupil",
  subject_col = "subject",
  condition_col = "condition",
  window_col = "window_label",
  window_start_col = "window_start_ms",
  window_end_col = "window_end_ms",
  trial_col = NULL,
  media_col = "media_id",
  valid_samples_col = "n_valid_pupil",
  total_samples_col = "n_samples",
  min_valid_samples = 5,
  min_valid_prop = 0.7,
  drop_invalid = TRUE,
  missing_condition_label = "all_data",
  outcome_label = "pupil"
)
```

## Arguments

- data:

  Pupil-window summary data.

- outcome_col:

  Column containing the pupil outcome to model. The default is
  `mean_pupil`.

- subject_col:

  Subject/participant column.

- condition_col:

  Optional condition column. Common aliases such as `condition`,
  `Condition`, and `CONDITION` are detected when available.

- window_col:

  Pupil-window label column.

- window_start_col:

  Optional window-start column.

- window_end_col:

  Optional window-end column.

- trial_col:

  Optional trial identifier column.

- media_col:

  Optional media/stimulus identifier column. Common aliases such as
  `media_id` and `MEDIA_ID` are detected when available.

- valid_samples_col:

  Optional column containing the number of valid pupil samples in the
  window. Common aliases such as `n_valid_pupil` and `n_valid_samples`
  are detected when available.

- total_samples_col:

  Optional column containing the total number of samples in the window.
  Common aliases such as `n_samples` and `n_window_samples` are detected
  when available.

- min_valid_samples:

  Minimum acceptable number of valid pupil samples.

- min_valid_prop:

  Minimum acceptable valid-sample proportion.

- drop_invalid:

  Logical. If `TRUE`, rows with invalid or low-quality model inputs are
  removed.

- missing_condition_label:

  Label used when condition is missing.

- outcome_label:

  Label stored in the output to identify the modelled pupil outcome.

## Value

A tibble of pupil-window rows prepared for confirmatory modelling.
