# Impute missing pupil samples with a lightweight Gaussian-process smoother

Performs within-subject/trial Gaussian-process interpolation using a
squared exponential kernel. This helper is intended for short missing
segments after blink detection, not for reconstructing long unusable
trials.

## Usage

``` r
impute_gazepoint_pupil_gp(
  data,
  pupil,
  time,
  subject = NULL,
  trial = NULL,
  length_scale = NULL,
  noise = 1e-04,
  max_train = 300,
  output = "pupil_gp_imputed",
  flag = "pupil_was_gp_imputed"
)
```

## Arguments

- data:

  A data frame.

- pupil:

  Pupil column.

- time:

  Time column.

- subject:

  Optional subject column.

- trial:

  Optional trial column.

- length_scale:

  Kernel length scale in the same unit as `time`.

- noise:

  Observation noise variance.

- max_train:

  Maximum number of observed samples used per sequence.

- output:

  Name of the imputed output column.

- flag:

  Name of the logical imputation flag column.

## Value

Data frame with imputed pupil values and an imputation flag.
