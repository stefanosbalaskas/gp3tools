# Compute lightweight AOI transition-network metrics

Compute graph-style summaries from AOI transitions without requiring
network packages. Metrics include state count, edge count, density,
self-loops, and in/out-degree summaries.

## Usage

``` r
compute_gazepoint_transition_network_metrics(
  data,
  aoi_col = NULL,
  from_col = NULL,
  to_col = NULL,
  group_cols = NULL,
  time_col = NULL,
  include_self_loops = TRUE
)
```

## Arguments

- data:

  Optional data frame of AOI observations or transition rows.

- aoi_col:

  AOI column for raw sequence data.

- from_col:

  Source-state column for transition data.

- to_col:

  Destination-state column for transition data.

- group_cols:

  Optional grouping columns for raw sequence data.

- time_col:

  Optional ordering column.

- include_self_loops:

  Should self-transitions be included?

## Value

A list with graph-level and state-level summaries.
