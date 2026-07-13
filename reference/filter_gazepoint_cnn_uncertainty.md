# Apply uncertainty filtering to Bayesian CNN or webcam gaze outputs

Provides a lightweight post-processing helper for externally generated
webcam/CNN gaze predictions. The function does not train a CNN. It
filters or down-weights frame-level gaze estimates using an uncertainty
column.

## Usage

``` r
filter_gazepoint_cnn_uncertainty(
  data,
  x,
  y,
  uncertainty = NULL,
  max_uncertainty = NULL,
  weight_output = "cnn_uncertainty_weight",
  valid_output = "cnn_valid_frame"
)
```

## Arguments

- data:

  A data frame containing frame-level gaze predictions.

- x:

  Predicted x-coordinate column.

- y:

  Predicted y-coordinate column.

- uncertainty:

  Optional uncertainty column.

- max_uncertainty:

  Optional maximum allowed uncertainty.

- weight_output:

  Name of the output weight column.

- valid_output:

  Name of the output validity column.

## Value

Data frame with uncertainty weights and validity flags.
