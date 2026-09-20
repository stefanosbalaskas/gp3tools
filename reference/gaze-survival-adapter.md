# Gazepoint adapters for censored gaze-latency survival analysis

Thin Gazepoint-specific convenience wrappers around the vendor-neutral
eyeprocess survival engine. Scientific event/censoring logic, model
fitting, diagnostics, sensitivity semantics, and reporting are not
duplicated in gp3tools.

## Usage

``` r
prepare_gazepoint_survival_data(trials, events = NULL, ...)

run_gazepoint_latency_analysis(
  trials,
  events = NULL,
  target_aoi,
  formula,
  participant_col = "participant_id",
  cox_structure = NULL,
  aft_distribution = NULL,
  km_group = NULL,
  ...
)
```

## Arguments

- trials:

  Trial-window table accepted by eyeprocess.

- events:

  Fixation/AOI-visit/event table accepted by eyeprocess.

- target_aoi:

  Target AOI identifier.

- formula:

  Model right-hand-side formula/string accepted by eyeprocess.

- participant_col:

  Participant identifier column.

- cox_structure:

  Required repeated-participant Cox estimator: `"cluster_robust"` or
  `"frailty"`.

- aft_distribution:

  Required AFT family: `"weibull"` or `"lognormal"`.

- km_group:

  Optional grouping column for Kaplan-Meier estimation.

- ...:

  Preparation arguments forwarded unchanged to
  eyeprocess::prepare_gaze_survival_data().

## Details

A target event absent from a complete usable trial can be
right-censored. Missing or unusable gaze is not automatically converted
to censoring. The end-to-end wrapper requires the Cox
repeated-observation structure and the AFT family to be chosen
explicitly and records both choices in the returned specification.

## Value

prepare_gazepoint_survival_data() returns the canonical survival-ready
data.frame from eyeprocess. run_gazepoint_latency_analysis() returns a
gazepoint_survival_analysis list with data, validation, censoring,
Kaplan-Meier output, Cox/AFT models, diagnostics, comparison, reporting,
and the explicit specification.
