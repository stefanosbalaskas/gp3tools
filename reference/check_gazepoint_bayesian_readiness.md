# Check readiness of a Gazepoint-derived dataset for Bayesian or advanced models

Performs lightweight structural checks before Bayesian, GAMM, HDDM, or
other advanced modelling workflows. The function does not fit any model.

## Usage

``` r
check_gazepoint_bayesian_readiness(
  data,
  outcome,
  subject,
  trial = NULL,
  time = NULL,
  condition = NULL,
  metric_type = "continuous",
  baseline_window = NULL,
  min_observations_per_subject = 10,
  max_missing_trial_prop = 0.2
)
```

## Arguments

- data:

  A data frame.

- outcome:

  Name of the outcome column.

- subject:

  Name of the subject/participant column.

- trial:

  Optional trial column.

- time:

  Optional time column.

- condition:

  Optional condition column.

- metric_type:

  Character scalar describing the planned metric/model type.

- baseline_window:

  Optional numeric vector of length two.

- min_observations_per_subject:

  Minimum number of observations expected per subject.

- max_missing_trial_prop:

  Maximum acceptable missingness proportion per subject-trial cell
  before a warning is raised.

## Value

A data frame of checks, status values, and messages.
