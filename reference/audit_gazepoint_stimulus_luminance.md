# Audit stimulus luminance and brightness for Gazepoint studies

Compute stimulus-level luminance, brightness, and contrast summaries for
image stimuli used in Gazepoint eye-tracking and pupillometry studies.
This helper is intended as a publication-readiness audit because pupil
size is strongly affected by stimulus brightness.

## Usage

``` r
audit_gazepoint_stimulus_luminance(
  data,
  stimulus_file_col = NULL,
  stimulus_id_col = NULL,
  condition_col = NULL,
  image_dir = NULL,
  recursive = TRUE,
  name = "gazepoint_stimulus_luminance"
)
```

## Arguments

- data:

  A data frame containing at least a stimulus image/file column.

- stimulus_file_col:

  Name of the stimulus image/file column. If `NULL`, common file-column
  names are detected automatically.

- stimulus_id_col:

  Optional stimulus identifier column. If `NULL`, common
  stimulus/media/item identifier columns are detected automatically.

- condition_col:

  Optional experimental condition column. If `NULL`, common condition
  columns are detected automatically. If no condition column exists, all
  rows are assigned to `"all_data"`.

- image_dir:

  Optional directory prepended to relative stimulus paths.

- recursive:

  If `TRUE`, unresolved relative file names are searched for recursively
  under `image_dir`.

- name:

  Character label stored in the audit object.

## Value

A list with class `gp3_stimulus_luminance_audit`.
