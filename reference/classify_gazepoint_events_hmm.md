# Classify gaze events with a lightweight unsupervised HMM

Estimates gaze velocity, initializes hidden states by k-means, estimates
a Gaussian-emission HMM, and decodes the most likely sequence with
Viterbi. This is a lightweight package-internal HMM classifier and not a
replacement for validated laboratory event-detection software.

## Usage

``` r
classify_gazepoint_events_hmm(
  data,
  x,
  y,
  time,
  subject = NULL,
  n_states = 3,
  state_labels = NULL
)
```

## Arguments

- data:

  A data frame.

- x:

  X-coordinate column.

- y:

  Y-coordinate column.

- time:

  Time column.

- subject:

  Optional subject column for within-subject sequences.

- n_states:

  Number of hidden states.

- state_labels:

  Optional labels for states. If `NULL`, states are ordered by
  increasing mean velocity.

## Value

The input data with velocity, hmm_state, and hmm_event columns.
