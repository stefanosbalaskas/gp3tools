# Example Gazepoint fixation table

A lightweight synthetic Gazepoint-style fixation table for examples,
tests, README workflows, and vignettes. The data are artificial and are
not from a real participant study.

## Usage

``` r
gazepoint_example_fixations
```

## Format

A tibble with fixation-level rows and columns including:

- USER_FILE:

  Synthetic participant/file identifier.

- subject:

  Synthetic participant identifier.

- MEDIA_ID:

  Synthetic stimulus identifier.

- trial_global:

  Synthetic trial identifier.

- condition:

  Synthetic experimental condition.

- FPOGID:

  Synthetic fixation identifier.

- FPOGS:

  Synthetic fixation start time.

- FPOGD:

  Synthetic fixation duration.

- FPOGX, FPOGY:

  Synthetic fixation coordinates.

- FPOGV:

  Synthetic fixation validity flag.

- AOI:

  Synthetic AOI label.

## Examples

``` r
data(gazepoint_example_fixations)
head(gazepoint_example_fixations)
#> # A tibble: 6 × 12
#>   USER_FILE subject MEDIA_ID trial_global condition FPOGID FPOGS FPOGD FPOGX
#>   <chr>     <chr>   <chr>    <chr>        <chr>      <int> <dbl> <dbl> <dbl>
#> 1 S01       S01     stim1    control_T1   control        1     0   120 0.499
#> 2 S01       S01     stim1    control_T1   control        2   200   120 0.539
#> 3 S01       S01     stim1    control_T1   control        3   400   120 0.337
#> 4 S01       S01     stim1    control_T1   control        4   600   120 0.265
#> 5 S01       S01     stim1    control_T1   control        5   800   120 0.531
#> 6 S01       S01     stim1    control_T1   control        6  1000   120 0.527
#> # ℹ 3 more variables: FPOGY <dbl>, FPOGV <lgl>, AOI <chr>
```
