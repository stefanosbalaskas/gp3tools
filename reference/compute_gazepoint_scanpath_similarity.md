# Compute AOI scanpath similarity

Compute pairwise AOI-sequence similarity between grouped scanpaths using
a lightweight Levenshtein edit-distance implementation. Similarity is
reported as `1 - normalized_distance`, where normalized distance is
divided by the longer sequence length.

## Usage

``` r
compute_gazepoint_scanpath_similarity(
  data,
  aoi_col,
  group_cols,
  time_col = NULL,
  include_missing = FALSE,
  missing_label = "missing",
  collapse_repeats = FALSE,
  max_sequences = 200
)
```

## Arguments

- data:

  A data frame containing AOI observations.

- aoi_col:

  Name of the AOI column.

- group_cols:

  Columns defining each scanpath, for example subject and trial.

- time_col:

  Optional time/order column.

- include_missing:

  Should missing AOI labels be retained as a state?

- missing_label:

  Label used when retaining missing AOIs.

- collapse_repeats:

  Should consecutive repeated AOI labels be collapsed?

- max_sequences:

  Maximum number of grouped sequences to compare.

## Value

A long-format data frame containing pairwise edit distances, normalized
distances, and similarities.
