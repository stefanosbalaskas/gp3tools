# Flag pupil artifacts with a Hampel filter

Apply a rolling Hampel filter to a Gazepoint pupil column. The helper
computes a rolling median and median absolute deviation (MAD) within a
centred sample window, then flags pupil samples whose absolute deviation
from the local median exceeds `k * MAD`.

## Usage

``` r
flag_gazepoint_pupil_hampel(
  data,
  pupil_col,
  time_col = NULL,
  grouping_cols = NULL,
  window_size_samples = 7L,
  k = 3,
  min_valid_samples = 3L,
  scale_mad = 1.4826,
  flag_col = "pupil_hampel_outlier",
  median_col = "pupil_hampel_median",
  mad_col = "pupil_hampel_mad",
  threshold_col = "pupil_hampel_threshold",
  corrected_col = NULL,
  status_col = "pupil_hampel_status",
  overwrite = FALSE,
  name = "gazepoint_pupil_hampel"
)
```

## Arguments

- data:

  A data frame containing pupil observations.

- pupil_col:

  Pupil column to screen.

- time_col:

  Optional time column used to order samples within groups.

- grouping_cols:

  Optional grouping columns, for example participant and trial.

- window_size_samples:

  Odd integer rolling-window size in samples.

- k:

  Hampel threshold multiplier.

- min_valid_samples:

  Minimum finite pupil samples required inside a rolling window.

- scale_mad:

  Scaling factor applied to MAD. The default `1.4826` makes MAD
  comparable to the standard deviation under normality.

- flag_col:

  Name of the logical Hampel-flag output column.

- median_col:

  Name of the rolling median output column.

- mad_col:

  Name of the rolling MAD output column.

- threshold_col:

  Name of the rolling threshold output column.

- corrected_col:

  Optional name of a corrected pupil column. If supplied, flagged
  samples are replaced with the local rolling median.

- status_col:

  Name of the row-level Hampel status column.

- overwrite:

  Logical. If `FALSE`, the function errors when output columns already
  exist.

- name:

  Character label stored in object attributes.

## Value

A tibble with Hampel-filter columns added. The object has class
`gp3_pupil_hampel_flags`.

## Details

This helper is intended as an optional sensitivity/artifact-flagging
branch. It complements existing pupil artifact checks and should not
automatically replace confirmatory preprocessing decisions.
