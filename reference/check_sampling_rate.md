# Check sampling rate by group

Check sampling rate by group

## Usage

``` r
check_sampling_rate(data, group_cols = "MEDIA_ID", time_col = "TIME")
```

## Arguments

- data:

  A Gazepoint all-gaze data frame.

- group_cols:

  Character vector of grouping columns, usually `MEDIA_ID` or
  `c("participant_id", "MEDIA_ID")`.

- time_col:

  Name of elapsed-time column.

## Value

A tibble with sample interval and estimated Hz.
