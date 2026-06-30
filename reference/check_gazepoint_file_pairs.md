# Check Gazepoint all-gaze and fixation file pairs

Checks whether each Gazepoint participant/source has both an all-gaze
CSV file and a fixation CSV file before running the full workflow.

## Usage

``` r
check_gazepoint_file_pairs(
  folder,
  all_gaze_pattern = "_all_gaze\\.csv$",
  fixation_pattern = "_fixations\\.csv$",
  recursive = FALSE
)
```

## Arguments

- folder:

  Folder containing Gazepoint CSV export files.

- all_gaze_pattern:

  Regular expression for selecting all-gaze files.

- fixation_pattern:

  Regular expression for selecting fixation files.

- recursive:

  Logical. If `TRUE`, search subfolders recursively.

## Value

A tibble with one row per detected participant/source and file-pair
status.
