# Prepare Gazepoint Growth Curve Analysis data

Prepare binned pupil time-course data for Growth Curve Analysis (GCA).
The function creates orthogonal polynomial time terms, preserves subject
and condition information, and standardises key columns for later
mixed-model fitting.

## Usage

``` r
prepare_gazepoint_gca_data(
  data,
  pupil_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition",
  degree = 3,
  orthogonal = TRUE,
  time_window = NULL,
  valid_samples_col = "n_valid_samples",
  min_valid_samples = 1,
  weights_col = NULL,
  missing_condition_label = "all_data",
  drop_missing = TRUE
)
```

## Arguments

- data:

  A binned pupil time-course data frame, usually created by
  [`prepare_gazepoint_pupil_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_gamm_data.md).

- pupil_col:

  Name of the pupil outcome column.

- time_col:

  Name of the time column.

- subject_col:

  Name of the subject column.

- condition_col:

  Name of the condition column. If unavailable or entirely missing, a
  single condition label is used.

- degree:

  Number of polynomial time terms to create.

- orthogonal:

  Logical. If `TRUE`, use orthogonal polynomial terms from
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html). If `FALSE`, use
  raw powers of z-scored time.

- time_window:

  Optional numeric vector of length 2 giving the time window to retain.

- valid_samples_col:

  Optional column containing valid sample counts.

- min_valid_samples:

  Minimum valid samples required per row when `valid_samples_col` is
  available.

- weights_col:

  Optional weights column to preserve for later modelling.

- missing_condition_label:

  Label used when condition values are missing.

- drop_missing:

  Logical. If `TRUE`, rows with missing outcome/time/subject values are
  removed.

## Value

A tibble of class `gp3_gca_data` with standard GCA columns and
polynomial time terms.
