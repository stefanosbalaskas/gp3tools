# Run a complete Gazepoint analysis workflow

Reads Gazepoint all-gaze and fixation exports from a folder, computes
sampling-rate checks, tracking-quality summaries, quality flags, and
AOI-level metrics, and optionally exports output tables, diagnostic
plots, and an HTML diagnostic report.

## Usage

``` r
run_gazepoint_workflow(
  export_dir,
  all_gaze_pattern = "_all_gaze\\.csv$",
  fixation_pattern = "_fixations\\.csv$",
  check_file_pairs = TRUE,
  group_cols = c("USER_FILE", "MEDIA_ID"),
  user_col = "USER_FILE",
  sample_rate = 60,
  min_gaze_valid_pct = 70,
  min_pupil_valid_pct = 70,
  expected_hz = 60,
  hz_tolerance = 5,
  min_duration_sec = NULL,
  output_dir = NULL,
  prefix = "gazepoint",
  overwrite = TRUE,
  save_plots = FALSE,
  plot_output_dir = NULL,
  create_report = FALSE,
  report_file = NULL,
  report_title = "Gazepoint diagnostic report",
  report_plot_dir = NULL,
  report_max_rows = 30
)
```

## Arguments

- export_dir:

  Folder containing Gazepoint CSV export files.

- all_gaze_pattern:

  Regular expression for selecting all-gaze files.

- fixation_pattern:

  Regular expression for selecting fixation files.

- check_file_pairs:

  Logical. If `TRUE`, check that each participant/source has both an
  all-gaze file and a fixation file before importing.

- group_cols:

  Columns used for grouped sampling and tracking-quality summaries.

- user_col:

  Column name used to identify the source/user file.

- sample_rate:

  Sampling rate used for approximate sample-based AOI viewed time.

- min_gaze_valid_pct:

  Minimum acceptable FPOGV validity percentage.

- min_pupil_valid_pct:

  Minimum acceptable pupil validity percentage.

- expected_hz:

  Expected sampling rate.

- hz_tolerance:

  Allowed deviation from the expected sampling rate.

- min_duration_sec:

  Optional minimum acceptable recording duration in seconds.

- output_dir:

  Optional folder where output CSV files should be written.

- prefix:

  Filename prefix used when exporting output tables, plots, and report.

- overwrite:

  Logical. If `FALSE`, stop when output files already exist.

- save_plots:

  Logical. If `TRUE`, save standard diagnostic plots.

- plot_output_dir:

  Optional folder where diagnostic plots should be saved. If `NULL`,
  `output_dir` is used.

- create_report:

  Logical. If `TRUE`, create an HTML diagnostic report.

- report_file:

  Optional path to the HTML report. If `NULL`, a report file is created
  in `output_dir` using `prefix`.

- report_title:

  Title used in the HTML diagnostic report.

- report_plot_dir:

  Optional folder for plots used inside the HTML report.

- report_max_rows:

  Maximum number of rows shown in report preview tables.

## Value

A named list containing file-pair checks, imported data, analysis
tables, quality flags, written table paths, written plot paths, and
written report path.
