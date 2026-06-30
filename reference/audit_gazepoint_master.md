# Audit a Gazepoint master sample table

Creates compact quality-audit tables from a master sample-level table
created by
[`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
or
[`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md).
The audit summarises missing gaze, missing pupil, off-screen gaze, AOI
states, pupil availability, coordinate ranges, and subject/media-level
quality.

## Usage

``` r
audit_gazepoint_master(master)
```

## Arguments

- master:

  A master sample-level table created by
  [`as_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/as_gazepoint_master.md)
  or
  [`create_gazepoint_master()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_master.md).

## Value

A named list of tibbles:

- overview:

  One-row overview of the master table.

- by_subject:

  Quality summary by participant/source.

- by_media:

  Quality summary by media/stimulus.

- by_subject_media:

  Quality summary by participant/source and media/stimulus.

- aoi_states:

  Counts and percentages of AOI states.

- pupil_summary:

  Pupil summary by participant/source and media/stimulus.

- coordinate_summary:

  Coordinate range and off-screen summary.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_gazepoint_workflow(
  export_dir = "C:/Users/YourName/Desktop/gp3_test_exports",
  output_dir = "C:/Users/YourName/Desktop/gp3_outputs"
)

master <- create_gazepoint_master(
  gaze_data = results$all_gaze,
  screen_width_px = 1920,
  screen_height_px = 1080
)

audit <- audit_gazepoint_master(master)

audit$overview
audit$by_subject
audit$aoi_states
} # }
```
