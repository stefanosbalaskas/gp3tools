# Fit a gaze-position-adjusted pupil GAMM sensitivity model

Fit and compare a main pupil GAMM with a gaze-position-adjusted
sensitivity model. The adjusted model adds a two-dimensional
tensor-product smooth over mean gaze x/y position using
`te(mean_x, mean_y)`.

## Usage

``` r
fit_gazepoint_pupil_pfe_gamm(
  data,
  pupil_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition",
  x_col = "mean_x",
  y_col = "mean_y",
  n_time_basis = 10,
  n_position_basis = 8,
  use_condition_smooths = TRUE,
  include_subject_random_effect = TRUE,
  family = c("gaussian", "scat"),
  method = "fREML",
  discrete = TRUE,
  rho = NULL,
  ar_start_col = "AR.start",
  weights_col = NULL,
  drop_missing = TRUE
)
```

## Arguments

- data:

  A binned pupil time-course data frame, usually created by
  [`prepare_gazepoint_pupil_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_gamm_data.md).

- pupil_col:

  Name of the dependent pupil column.

- time_col:

  Name of the time-bin centre column.

- subject_col:

  Name of the subject column.

- condition_col:

  Name of the condition column.

- x_col:

  Name of the mean gaze x-position column.

- y_col:

  Name of the mean gaze y-position column.

- n_time_basis:

  Basis dimension for time smooths.

- n_position_basis:

  Basis dimension for gaze-position smooths.

- use_condition_smooths:

  Logical. If `TRUE`, condition-specific time smooths are used when
  multiple conditions are present.

- include_subject_random_effect:

  Logical. If `TRUE`, adds a subject random-effect smooth.

- family:

  Model family. Use `"gaussian"` or `"scat"`.

- method:

  Smoothing-parameter estimation method passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- discrete:

  Logical passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- rho:

  Optional AR(1) correlation parameter passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- ar_start_col:

  Optional AR-start column.

- weights_col:

  Optional weights column.

- drop_missing:

  Logical. If `TRUE`, rows with missing model variables are removed
  before fitting.

## Value

A list of class `gp3_pupil_pfe_gamm` containing the main model, the
gaze-position-adjusted model, formulas, comparison table, settings, and
status information.
