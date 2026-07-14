# Downsample pupil data by integer-factor aggregation

Aggregates consecutive samples within independent sequences. The default
mean method is safer than simple decimation because it reduces aliasing
and preserves a representative timestamp for each bin.

## Usage

``` r
downsample_gazepoint_pupil(
  master_df,
  factor = 2,
  pupil_cols = NULL,
  id_col = "USER_ID",
  group_cols = NULL,
  ts_col = "TIME",
  method = c("mean", "first"),
  keep_bin = FALSE
)
```

## Arguments

- master_df:

  A sample-level data frame.

- factor:

  Positive integer downsampling factor.

- pupil_cols:

  Pupil columns to aggregate. When `NULL`, common pupil columns are
  detected automatically.

- id_col:

  Participant identifier.

- group_cols:

  Optional additional independent-sequence columns.

- ts_col:

  Optional timestamp column. Its bin value is the finite mean.

- method:

  `"mean"` aggregation or `"first"`-sample decimation.

- keep_bin:

  Keep the generated downsample-bin identifier.

## Value

A downsampled data frame.

## Examples

``` r
pupil <- data.frame(
  USER_ID = "P01",
  TIME = seq(0, 0.09, by = 0.01),
  mean_pupil = seq(3, 4, length.out = 10)
)
downsample_gazepoint_pupil(pupil, factor = 2)
#>   USER_ID  TIME mean_pupil n_samples_aggregated downsample_factor
#> 1     P01 0.005   3.055556                    2                 2
#> 2     P01 0.025   3.277778                    2                 2
#> 3     P01 0.045   3.500000                    2                 2
#> 4     P01 0.065   3.722222                    2                 2
#> 5     P01 0.085   3.944444                    2                 2
```
