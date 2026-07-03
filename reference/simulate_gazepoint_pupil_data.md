# Simulate Gazepoint-like pupil data

Generates a privacy-safe synthetic pupil data set with balanced
conditions, left/right pupil channels, a combined pupil column,
blink/trackloss flags, and simple gaze coordinates. The generator is
intended for examples and unit tests, not for claims about empirical
pupil physiology.

## Usage

``` r
simulate_gazepoint_pupil_data(
  n_subjects = 12,
  n_trials = 8,
  n_time_bins = 60,
  conditions = c("control", "treatment"),
  baseline_mean = 3.5,
  condition_effect = 0.15,
  noise_sd = 0.08,
  subject_sd = 0.25,
  blink_probability = 0.03,
  seed = NULL
)
```

## Arguments

- n_subjects:

  Number of synthetic participants.

- n_trials:

  Number of trials per participant.

- n_time_bins:

  Number of time bins per trial.

- conditions:

  Character vector of condition labels.

- baseline_mean:

  Mean pupil size around which synthetic data are generated.

- condition_effect:

  Numeric effect added to non-reference conditions. If a single value is
  supplied, it is applied to all non-reference conditions. If multiple
  values are supplied, they are recycled across `conditions`.

- noise_sd:

  Standard deviation of sample-level noise.

- subject_sd:

  Standard deviation of participant-level random offsets.

- blink_probability:

  Probability that a sample is marked as blink/trackloss.

- seed:

  Optional random seed.

## Value

A data frame with synthetic Gazepoint-like pupil and gaze columns.

## Examples

``` r
simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#>    subject trial condition time_bin timestamp_ms    gaze_x   gaze_y pupil_left
#> 1     S001     1   control        1         0.00 1000.9344 588.8581   3.264536
#> 2     S001     1   control        2        16.67  824.4764 465.2722   3.465009
#> 3     S001     1   control        3        33.34 1131.9628 439.7093   3.369747
#> 4     S001     1   control        4        50.01 1197.6480 563.3157   3.283749
#> 5     S001     1   control        5        66.68  915.9334 504.5367   3.394381
#> 6     S001     2 treatment        1         0.00  834.7038 540.0884   3.478620
#> 7     S001     2 treatment        2        16.67 1028.3664 545.9473   3.526108
#> 8     S001     2 treatment        3        33.34  943.7934 492.8383   3.461614
#> 9     S001     2 treatment        4        50.01 1248.1941 494.5065   3.558497
#> 10    S001     2 treatment        5        66.68  955.2912 529.1857   3.386574
#> 11    S002     1   control        1         0.00 1042.7687 634.2470   3.484212
#> 12    S002     1   control        2        16.67  963.3603 418.1147   3.362735
#> 13    S002     1   control        3        33.34  870.8072 587.5157   3.635905
#> 14    S002     1   control        4        50.01  982.6551 566.6360   3.548316
#> 15    S002     1   control        5        66.68  743.4050 625.0480   3.556616
#> 16    S002     2 treatment        1         0.00 1135.8666 515.6653   3.697586
#> 17    S002     2 treatment        2        16.67  978.3904 569.6015   3.748267
#> 18    S002     2 treatment        3        33.34 1220.7134 561.3679   3.736081
#> 19    S002     2 treatment        4        50.01 1017.0611 496.5984   3.713597
#> 20    S002     2 treatment        5        66.68  874.8064 636.6294   3.620482
#>    pupil_right blink trackloss    pupil
#> 1     3.337352 FALSE     FALSE 3.300944
#> 2     3.178238 FALSE     FALSE 3.321624
#> 3     3.392973 FALSE     FALSE 3.381360
#> 4     3.344896 FALSE     FALSE 3.314323
#> 5     3.342923 FALSE     FALSE 3.368652
#> 6     3.301894 FALSE     FALSE 3.390257
#> 7     3.441793 FALSE     FALSE 3.483950
#> 8     3.519480 FALSE     FALSE 3.490547
#> 9     3.546249 FALSE     FALSE 3.552373
#> 10    3.347164 FALSE     FALSE 3.366869
#> 11    3.564925 FALSE     FALSE 3.524568
#> 12    3.535606 FALSE     FALSE 3.449171
#> 13    3.435746 FALSE     FALSE 3.535826
#> 14    3.518711 FALSE     FALSE 3.533514
#> 15    3.526368 FALSE     FALSE 3.541492
#> 16    3.617334 FALSE     FALSE 3.657460
#> 17    3.770571 FALSE     FALSE 3.759419
#> 18    3.749623 FALSE     FALSE 3.742852
#> 19    3.626917 FALSE     FALSE 3.670257
#> 20    3.537642 FALSE     FALSE 3.579062
```
