# Plot Gazepoint sampling-rate diagnostics

Creates a diagnostic plot of estimated sampling rate by participant/file
and media stimulus.

## Usage

``` r
plot_sampling_rate(
  sampling,
  user_col = "USER_FILE",
  media_col = "MEDIA_ID",
  expected_hz = 60,
  hz_tolerance = 5
)
```

## Arguments

- sampling:

  Sampling-rate table, usually from
  [`check_sampling_rate()`](https://stefanosbalaskas.github.io/gp3tools/reference/check_sampling_rate.md).

- user_col:

  Column identifying the source/user file.

- media_col:

  Column identifying the media/stimulus.

- expected_hz:

  Expected sampling rate.

- hz_tolerance:

  Allowed deviation from the expected sampling rate.

## Value

A ggplot object.
