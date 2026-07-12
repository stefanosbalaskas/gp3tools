# Create a Bayesian ocular Statistical Analysis Plan checklist

Generates a structured checklist for Bayesian or advanced eye-tracking
and pupillometry analysis planning.

## Usage

``` r
create_gazepoint_bayesian_sap(
  outcome,
  design,
  primary_model,
  baseline_window = NULL,
  analysis_window = NULL,
  missingness_threshold = 0.2,
  blink_padding_ms = 50,
  output = c("data.frame", "markdown")
)
```

## Arguments

- outcome:

  Outcome name or outcome family.

- design:

  Study design description.

- primary_model:

  Planned primary model.

- baseline_window:

  Optional baseline window.

- analysis_window:

  Optional analysis window.

- missingness_threshold:

  Trial-level missingness threshold.

- blink_padding_ms:

  Blink padding in milliseconds.

- output:

  Either `"data.frame"` or `"markdown"`.

## Value

A data frame or markdown character vector.
