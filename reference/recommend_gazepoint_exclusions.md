# Recommend trial and participant exclusions

Create explicit trial-level and participant-level exclusion
recommendations from Gazepoint sample-level quality information. The
helper can use validity flags, gaze-coordinate missingness, pupil
missingness, and optional artifact flags to produce transparent
exclusion tables.

## Usage

``` r
recommend_gazepoint_exclusions(
  data,
  participant_col,
  trial_col = NULL,
  condition_col = NULL,
  validity_col = NULL,
  x_col = NULL,
  y_col = NULL,
  pupil_col = NULL,
  artifact_col = NULL,
  min_trial_samples = 10L,
  max_trial_missing_prop = 0.5,
  max_trial_artifact_prop = 0.5,
  min_participant_trials = 2L,
  min_participant_valid_trials = 1L,
  max_participant_missing_prop = 0.5,
  max_participant_artifact_prop = 0.5,
  require_both_gaze_coordinates = TRUE,
  name = "gazepoint_exclusion_recommendations"
)
```

## Arguments

- data:

  A data frame containing sample-level or trial-level data.

- participant_col:

  Participant identifier column.

- trial_col:

  Optional trial identifier column.

- condition_col:

  Optional condition column retained in summaries.

- validity_col:

  Optional logical/numeric/character validity column.

- x_col:

  Optional horizontal gaze coordinate column.

- y_col:

  Optional vertical gaze coordinate column.

- pupil_col:

  Optional pupil column.

- artifact_col:

  Optional logical/numeric/character artifact flag column.

- min_trial_samples:

  Minimum samples required per trial.

- max_trial_missing_prop:

  Maximum missing/unusable sample proportion per trial.

- max_trial_artifact_prop:

  Maximum artifact proportion per trial.

- min_participant_trials:

  Minimum total trials required per participant.

- min_participant_valid_trials:

  Minimum retained trials required per participant.

- max_participant_missing_prop:

  Maximum missing/unusable sample proportion per participant.

- max_participant_artifact_prop:

  Maximum artifact proportion per participant.

- require_both_gaze_coordinates:

  Logical. If both gaze columns are supplied, should a sample be usable
  only when both coordinates are finite?

- name:

  Character label stored in object attributes.

## Value

A list with overview, trial recommendations, participant
recommendations, an explicit exclusion table, and settings.

## Details

This function recommends exclusions only. It does not remove rows.
