# Prepare a trial-level export for Python HDDM

Prepares a clean trial-level CSV-compatible data frame for Python HDDM
or related drift-diffusion modelling workflows. This function does not
fit HDDM.

## Usage

``` r
prepare_gazepoint_hddm_export(
  data,
  subject,
  rt,
  response,
  predictors = NULL,
  zscore_within_subject = TRUE,
  drop_missing = TRUE,
  file = NULL
)
```

## Arguments

- data:

  Trial-level data frame.

- subject:

  Subject identifier column.

- rt:

  Response-time column.

- response:

  Binary response column coded as 0/1.

- predictors:

  Optional continuous predictors to include.

- zscore_within_subject:

  Logical; z-score predictors within subject.

- drop_missing:

  Logical; drop rows with missing required values.

- file:

  Optional CSV path. If supplied, the export is written to disk.

## Value

A data frame ready for HDDM-style workflows.
