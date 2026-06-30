# Summarise Gazepoint AOI trial features

Create trial-level AOI features from sample-level Gazepoint AOI data or
from AOI-entry tables created by
[`summarise_gazepoint_aoi_entries()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_gazepoint_aoi_entries.md).
The output includes AOI dwell, entry, TTFF, revisit, and transition
features.

## Usage

``` r
summarise_gazepoint_aoi_trial_features(
  data,
  aoi_col = NULL,
  time_col = "time",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  include_non_aoi = TRUE,
  target_aoi_values = NULL,
  distractor_aoi_values = NULL,
  non_aoi_values = c("non_aoi", "none", "background", "outside", "outside_aoi",
    "missing", "missing_aoi"),
  missing_aoi_label = "missing_aoi"
)
```

## Arguments

- data:

  A Gazepoint sample-level data frame, AOI-entry table, or compatible
  AOI table.

- aoi_col:

  Name of the AOI-state column. Used only when `data` is sample-level
  data. If `NULL`, the function tries `aoi_current`, `AOI`, and
  `aoi_state`.

- time_col:

  Name of the time column, in milliseconds. Used only when `data` is
  sample-level data.

- group_cols:

  Character vector of columns defining independent trials, usually
  subject/media/trial.

- include_non_aoi:

  Logical. If `TRUE`, non-AOI/background states are kept when computing
  trial-duration and transition features.

- target_aoi_values:

  Optional character vector defining target AOI labels.

- distractor_aoi_values:

  Optional character vector defining distractor AOI labels.

- non_aoi_values:

  Character vector of AOI labels treated as background or non-AOI
  states.

- missing_aoi_label:

  Label used when the AOI value is missing.

## Value

A tibble with one row per trial/group and AOI trial-level features.
