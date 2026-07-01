# Summarise Gazepoint semi-Markov data

Summarise state visits and state-to-state transitions from semi-Markov
preparation output or a compatible data frame. The function returns a
list with a state-duration summary and a transition summary.

## Usage

``` r
summarise_gazepoint_semimarkov(
  semimarkov_data,
  state_col = NULL,
  duration_col = NULL,
  sequence_col = NULL,
  time_col = NULL,
  from_col = NULL,
  to_col = NULL
)
```

## Arguments

- semimarkov_data:

  Output from
  [`prepare_gazepoint_semimarkov_data()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_semimarkov_data.md)
  or a compatible data frame/list.

- state_col:

  Optional state/AOI column.

- duration_col:

  Optional state-duration column.

- sequence_col:

  Optional sequence, subject, or trial column.

- time_col:

  Optional time/order column.

- from_col:

  Optional transition source-state column.

- to_col:

  Optional transition destination-state column.

## Value

A list with `state_summary`, `transition_summary`, `columns`, and
`summary_status`.
