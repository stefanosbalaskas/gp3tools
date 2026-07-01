# Summarise a Gazepoint Markov-chain object

Convert a Gazepoint Markov-chain object, transition matrix, or
transition data frame into a tidy transition summary. The function is
deliberately permissive so that it can summarise objects created by
[`create_gazepoint_markovchain_object()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_markovchain_object.md)
as well as simple matrices used in examples or tests.

## Usage

``` r
summarise_gazepoint_markovchain(
  markov_object,
  include_zero = FALSE,
  from_col = NULL,
  to_col = NULL,
  count_col = NULL,
  probability_col = NULL
)
```

## Arguments

- markov_object:

  A Markov-chain object, matrix, table, list, or data frame containing
  transition information.

- include_zero:

  Should zero-valued transitions be retained?

- from_col:

  Optional source-state column when `markov_object` is a data frame.

- to_col:

  Optional destination-state column when `markov_object` is a data
  frame.

- count_col:

  Optional transition-count column when available.

- probability_col:

  Optional transition-probability column when available.

## Value

A data frame with source state, destination state, transition count or
weight, row total, transition probability, and status columns.
