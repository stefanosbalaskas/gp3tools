# Plot Gazepoint tracking-quality diagnostics

Creates a readable diagnostic plot of selected gaze and pupil validity
percentages by participant/file and media stimulus.

## Usage

``` r
plot_tracking_quality(
  data,
  metric_cols = NULL,
  user_col = "USER_FILE",
  media_col = "MEDIA_ID",
  review_col = "review_required",
  min_valid_pct = 70
)
```

## Arguments

- data:

  A tracking-quality or flagged-quality table, usually from
  [`summarise_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarise_tracking_quality.md)
  or
  [`flag_tracking_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/flag_tracking_quality.md).

- metric_cols:

  Validity percentage columns to plot. If `NULL`, the default is
  `FPOGV_valid_pct` and `RPV_valid_pct`, which provide a compact
  diagnostic view of gaze and right-pupil validity.

- user_col:

  Column identifying the source/user file.

- media_col:

  Column identifying the media/stimulus.

- review_col:

  Optional column indicating whether a row requires review.

- min_valid_pct:

  Vertical threshold line for acceptable validity.

## Value

A ggplot object.
