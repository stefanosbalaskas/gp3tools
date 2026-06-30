# Fit a Gazepoint pupil GAMM

Fit a generalized additive mixed model for binned pupil time-course data
using [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html). The
function is designed to work with data prepared by
[`prepare_gazepoint_pupil_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_pupil_gamm_data.md).

## Usage

``` r
fit_gazepoint_pupil_gamm(
  data,
  pupil_col = "mean_pupil",
  time_col = "time_bin_center_ms",
  subject_col = "subject",
  condition_col = "condition",
  n_time_basis = 10,
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

  A binned pupil time-course data frame.

- pupil_col:

  Name of the dependent pupil column.

- time_col:

  Name of the time-bin centre column.

- subject_col:

  Name of the subject column.

- condition_col:

  Name of the condition column.

- n_time_basis:

  Basis dimension for smooth time terms.

- use_condition_smooths:

  Logical. If `TRUE`, condition-specific smooths are added when the
  condition column has more than one level.

- include_subject_random_effect:

  Logical. If `TRUE`, adds a subject random-effect smooth.

- family:

  Model family. Use `"gaussian"` for the default Gaussian model or
  `"scat"` for mgcv's scaled-t family.

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

  Optional AR-start column. If present and `rho` is not `NULL`, it is
  passed to [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) as
  `AR.start`.

- weights_col:

  Optional weights column.

- drop_missing:

  Logical. If `TRUE`, rows with missing model variables are removed
  before fitting.

## Value

A list of class `gp3_pupil_gamm` containing the fitted model, formula,
data, settings, and status information.
