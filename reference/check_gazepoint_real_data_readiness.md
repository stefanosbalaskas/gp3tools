# Check real-data readiness before Gazepoint analysis

Create an explicit final readiness gate for real Gazepoint or gp3tools
master data before confirmatory analysis. The gate checks required
identifiers, trial/time structure, analysis-specific columns,
missingness, basic design balance, and optional upstream audit objects.

## Usage

``` r
check_gazepoint_real_data_readiness(
  data,
  analysis_type = c("general", "pupil", "aoi", "combined"),
  participant_col = NULL,
  trial_col = NULL,
  time_col = NULL,
  condition_col = NULL,
  stimulus_col = NULL,
  aoi_col = NULL,
  pupil_col = NULL,
  gaze_x_col = NULL,
  gaze_y_col = NULL,
  tracking_valid_col = NULL,
  required_cols = NULL,
  audit_objects = NULL,
  min_rows = 1L,
  min_participants = 1L,
  min_trials = 1L,
  max_missing_pupil_prop = 0.4,
  max_missing_gaze_prop = 0.4,
  max_condition_imbalance_ratio = 3,
  name = "gazepoint_real_data_readiness_gate"
)
```

## Arguments

- data:

  A Gazepoint or gp3tools data frame.

- analysis_type:

  Analysis target. Options are `"general"`, `"pupil"`, `"aoi"`, and
  `"combined"`.

- participant_col:

  Optional participant column. If `NULL`, common names are detected.

- trial_col:

  Optional trial column. If `NULL`, common names are detected.

- time_col:

  Optional time column. If `NULL`, common names are detected.

- condition_col:

  Optional condition/group column. If `NULL`, common names are detected
  when present.

- stimulus_col:

  Optional stimulus/media column. If `NULL`, common names are detected
  when present.

- aoi_col:

  Optional AOI/state column. If `NULL`, common names are detected when
  present.

- pupil_col:

  Optional pupil column. If `NULL`, common names are detected when
  present.

- gaze_x_col:

  Optional horizontal gaze coordinate column.

- gaze_y_col:

  Optional vertical gaze coordinate column.

- tracking_valid_col:

  Optional tracking-validity column.

- required_cols:

  Additional required columns.

- audit_objects:

  Optional list of upstream audit/validation objects.

- min_rows:

  Minimum acceptable number of rows.

- min_participants:

  Minimum acceptable number of participants.

- min_trials:

  Minimum acceptable number of participant-trial units.

- max_missing_pupil_prop:

  Maximum acceptable pupil missingness proportion when pupil data are
  required.

- max_missing_gaze_prop:

  Maximum acceptable gaze-coordinate missingness proportion when gaze
  coordinate columns are present.

- max_condition_imbalance_ratio:

  Warning threshold for condition imbalance.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_real_data_readiness_gate`.

## Details

This helper is a final decision wrapper. It complements, but does not
replace,
[`validate_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/validate_gazepoint_master.md)
or
[`create_gazepoint_analysis_decision_audit()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_analysis_decision_audit.md).
