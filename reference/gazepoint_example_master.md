# Example Gazepoint master table

A lightweight synthetic Gazepoint-style sample-level master table for
examples, tests, README workflows, and vignettes. The data are
artificial and are not from a real participant study.

## Usage

``` r
gazepoint_example_master
```

## Format

A tibble with sample-level rows and columns including:

- subject:

  Synthetic participant identifier.

- USER_FILE:

  Gazepoint-style participant/file identifier.

- MEDIA_ID:

  Synthetic stimulus identifier.

- trial_global:

  Synthetic trial identifier.

- condition:

  Synthetic experimental condition.

- time:

  Sample time in milliseconds.

- x, y:

  Normalised gaze coordinates.

- pupil:

  Synthetic pupil value.

- valid:

  Logical gaze/pupil validity flag.

- artifact:

  Logical synthetic pupil-artifact flag.

- aoi_current:

  Synthetic AOI state.

- is_fixation, is_saccade:

  Synthetic fixation and saccade indicators.

- event_label:

  Synthetic event marker.

## Examples

``` r
data(gazepoint_example_master)
head(gazepoint_example_master)
#> # A tibble: 6 × 28
#>   subject USER_FILE MEDIA_ID trial_global trial_index condition  time  TIME
#>   <chr>   <chr>     <chr>    <chr>              <int> <chr>     <dbl> <dbl>
#> 1 S01     S01       stim1    control_T1             1 control       0     0
#> 2 S01     S01       stim1    control_T1             1 control      50    50
#> 3 S01     S01       stim1    control_T1             1 control     100   100
#> 4 S01     S01       stim1    control_T1             1 control     150   150
#> 5 S01     S01       stim1    control_T1             1 control     200   200
#> 6 S01     S01       stim1    control_T1             1 control     250   250
#> # ℹ 20 more variables: x <dbl>, y <dbl>, BPOGX <dbl>, BPOGY <dbl>, pupil <dbl>,
#> #   LPMM <dbl>, RPMM <dbl>, valid <lgl>, BPOGV <lgl>, artifact <lgl>,
#> #   aoi_current <chr>, AOI <chr>, is_fixation <lgl>, is_saccade <lgl>,
#> #   event_label <chr>, target_x <dbl>, target_y <dbl>, is_check_target <lgl>,
#> #   screen_width <dbl>, screen_height <dbl>
```
