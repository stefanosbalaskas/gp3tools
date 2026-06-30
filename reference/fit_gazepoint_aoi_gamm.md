# Fit AOI time-course GAMMs

Fit binomial GAMMs for AOI target-looking time courses prepared by
[`prepare_gazepoint_aoi_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_gamm_data.md).
The model uses target-looking successes and failures over time and can
include condition effects, condition-specific smooths, and subject
random-effect smooths.

## Usage

``` r
fit_gazepoint_aoi_gamm(
  data,
  include_condition = TRUE,
  condition_smooths = TRUE,
  random_subject = TRUE,
  random_subject_time = FALSE,
  time_k = 10,
  subject_time_k = 5,
  family = stats::binomial(),
  method = "fREML",
  discrete = FALSE,
  select = FALSE,
  drop_non_ok = TRUE,
  min_rows = 10,
  min_subjects = 2,
  min_time_bins = 4,
  ...
)
```

## Arguments

- data:

  A data frame returned by
  [`prepare_gazepoint_aoi_gamm_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_aoi_gamm_data.md).

- include_condition:

  Logical. If `TRUE`, include condition as a parametric fixed effect
  when two or more conditions are available.

- condition_smooths:

  Logical. If `TRUE`, fit condition-specific time smooths when two or
  more conditions are available.

- random_subject:

  Logical. If `TRUE`, include a subject random-effect smooth.

- random_subject_time:

  Logical. If `TRUE`, include subject-specific factor-smooth time
  deviations. This can be useful for repeated-measures time-course data
  but may be too heavy for very small datasets.

- time_k:

  Basis dimension for the main time smooth.

- subject_time_k:

  Basis dimension for subject-specific factor-smooth time deviations.

- family:

  Model family. Defaults to
  [`stats::binomial()`](https://rdrr.io/r/stats/family.html).

- method:

  Smoothing-parameter estimation method passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- discrete:

  Logical passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- select:

  Logical passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- drop_non_ok:

  Logical. If `TRUE`, keep only rows with `.gp3_aoi_gamm_status == "ok"`
  before fitting.

- min_rows:

  Minimum number of rows required for model fitting.

- min_subjects:

  Minimum number of subjects required for model fitting.

- min_time_bins:

  Minimum number of time bins required for model fitting.

- ...:

  Additional arguments passed to
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

## Value

A list containing the fitted model, formula, model status, diagnostics,
parametric table, smooth table, and settings.

## Details

This function is intended for AOI time-course modelling. It is separate
from confirmatory AOI-window GLMMs and from cluster-based permutation
tests.
