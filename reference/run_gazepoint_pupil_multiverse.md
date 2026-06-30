# Run a Gazepoint pupil preprocessing multiverse

Run all pupil preprocessing branches defined by
[`create_gazepoint_preprocessing_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_multiverse.md).
Each branch can apply pupil artifact flagging, interpolation, baseline
correction, smoothing, and optional pupil-window summarisation.

## Usage

``` r
run_gazepoint_pupil_multiverse(
  data,
  multiverse,
  branch_ids = NULL,
  pupil_col = NULL,
  time_col = NULL,
  group_cols = NULL,
  summarise_windows = FALSE,
  windows = NULL,
  keep_outputs = TRUE,
  stop_on_error = FALSE
)
```

## Arguments

- data:

  A Gazepoint master table or processed pupil table.

- multiverse:

  A `gp3_preprocessing_multiverse` object returned by
  [`create_gazepoint_preprocessing_multiverse()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_preprocessing_multiverse.md).

- branch_ids:

  Optional character vector of pupil branch IDs to run.

- pupil_col:

  Optional pupil column passed to downstream preprocessing helpers when
  supported.

- time_col:

  Optional time column passed to downstream preprocessing helpers when
  supported.

- group_cols:

  Optional grouping columns passed to downstream preprocessing helpers
  when supported.

- summarise_windows:

  Logical. If `TRUE`, summarise each processed pupil branch into pupil
  analysis windows.

- windows:

  Optional windows passed to
  [`summarise_gazepoint_pupil_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_pupil_windows.md)
  when `summarise_windows = TRUE`.

- keep_outputs:

  Logical. If `TRUE`, keep processed branch data in `branch_outputs`.

- stop_on_error:

  Logical. If `TRUE`, stop when a branch fails. If `FALSE`, record the
  branch error and continue.

## Value

A list with class `gp3_pupil_multiverse_results` containing overview,
branch results, optional branch outputs, and settings.
