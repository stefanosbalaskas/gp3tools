# Simulate Gazepoint-like fixation events

Generates event-level fixation data with bounded random spatial drift.

## Usage

``` r
simulate_gazepoint_fixations(
  n_subjects = 10,
  n_fix = 50,
  sd = 10,
  coordinate_system = c("pixels", "normalized"),
  screen_width = 1920,
  screen_height = 1080,
  duration_mean = 250,
  duration_sd = 80,
  saccade_gap_mean = 40,
  seed = NULL
)
```

## Arguments

- n_subjects:

  Number of simulated participants.

- n_fix:

  Number of fixations per participant.

- sd:

  Standard deviation of spatial random-walk increments.

- coordinate_system:

  Coordinate representation.

- screen_width, screen_height:

  Pixel dimensions when `coordinate_system = "pixels"`.

- duration_mean, duration_sd:

  Mean and standard deviation of fixation duration in milliseconds.

- saccade_gap_mean:

  Mean interval between fixations in milliseconds.

- seed:

  Optional random seed.

## Value

A tibble resembling a Gazepoint fixation export.

## Examples

``` r
simulate_gazepoint_fixations(
  n_subjects = 2,
  n_fix = 10,
  seed = 1
)
#> # A tibble: 20 × 17
#>    USER_ID MEDIA_ID     FPOGID FPOGS FPOGD FPOGX FPOGY FPOGV subject fixation_id
#>    <chr>   <chr>         <int> <dbl> <dbl> <dbl> <dbl> <int> <chr>         <int>
#>  1 P001    simulated_s…      1 0     0.200 0.680 0.269     1 P001              1
#>  2 P001    simulated_s…      2 0.206 0.265 0.684 0.281     1 P001              2
#>  3 P001    simulated_s…      3 0.526 0.183 0.688 0.280     1 P001              3
#>  4 P001    simulated_s…      4 0.740 0.378 0.689 0.284     1 P001              4
#>  5 P001    simulated_s…      5 1.17  0.276 0.679 0.283     1 P001              5
#>  6 P001    simulated_s…      6 1.62  0.184 0.682 0.271     1 P001              6
#>  7 P001    simulated_s…      7 1.85  0.289 0.681 0.267     1 P001              7
#>  8 P001    simulated_s…      8 2.18  0.309 0.681 0.263     1 P001              8
#>  9 P001    simulated_s…      9 2.56  0.296 0.673 0.263     1 P001              9
#> 10 P001    simulated_s…     10 2.88  0.226 0.670 0.273     1 P001             10
#> 11 P002    simulated_s…      1 0     0.311 0.629 0.282     1 P002              1
#> 12 P002    simulated_s…      2 0.336 0.237 0.641 0.286     1 P002              2
#> 13 P002    simulated_s…      3 0.581 0.230 0.641 0.280     1 P002              3
#> 14 P002    simulated_s…      4 0.823 0.306 0.645 0.286     1 P002              4
#> 15 P002    simulated_s…      5 1.17  0.295 0.645 0.277     1 P002              5
#> 16 P002    simulated_s…      6 1.50  0.195 0.641 0.265     1 P002              6
#> 17 P002    simulated_s…      7 1.70  0.193 0.642 0.268     1 P002              7
#> 18 P002    simulated_s…      8 1.93  0.279 0.632 0.264     1 P002              8
#> 19 P002    simulated_s…      9 2.22  0.311 0.640 0.264     1 P002              9
#> 20 P002    simulated_s…     10 2.60  0.241 0.641 0.265     1 P002             10
#> # ℹ 7 more variables: start_time <dbl>, end_time <dbl>, duration <dbl>,
#> #   duration_ms <dbl>, x <dbl>, y <dbl>, coordinate_system <chr>
```
