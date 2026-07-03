# Audit Gazepoint gaze coordinates against screen bounds

Checks whether gaze coordinates are missing, equal to `(0, 0)`, or
outside the expected screen/stimulus bounds. The helper is intended for
transparent quality-control reporting, not for automatic exclusion
decisions.

## Usage

``` r
audit_gazepoint_screen_bounds(
  data,
  x_col,
  y_col,
  screen_width,
  screen_height,
  group_cols = NULL,
  margin = 0,
  treat_zero_zero_as_out_of_bounds = TRUE
)
```

## Arguments

- data:

  A data frame.

- x_col, y_col:

  Character names of gaze-coordinate columns.

- screen_width, screen_height:

  Numeric screen or stimulus dimensions.

- group_cols:

  Optional grouping columns for group-level summaries.

- margin:

  Numeric tolerance around the screen bounds. A positive value allows
  coordinates slightly outside the nominal screen area.

- treat_zero_zero_as_out_of_bounds:

  If `TRUE`, `(0, 0)` coordinates are flagged separately and counted as
  invalid.

## Value

A list with row-level flags, group-level summary, overall summary, and
settings.

## Examples

``` r
x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
audit_gazepoint_screen_bounds(x, "gaze_x", "gaze_y", 1920, 1080)
#> $row_flags
#>    row_id         x        y missing_coordinate zero_zero outside_x outside_y
#> 1       1 1000.9344 588.8581              FALSE     FALSE     FALSE     FALSE
#> 2       2  824.4764 465.2722              FALSE     FALSE     FALSE     FALSE
#> 3       3 1131.9628 439.7093              FALSE     FALSE     FALSE     FALSE
#> 4       4 1197.6480 563.3157              FALSE     FALSE     FALSE     FALSE
#> 5       5  915.9334 504.5367              FALSE     FALSE     FALSE     FALSE
#> 6       6  834.7038 540.0884              FALSE     FALSE     FALSE     FALSE
#> 7       7 1028.3664 545.9473              FALSE     FALSE     FALSE     FALSE
#> 8       8  943.7934 492.8383              FALSE     FALSE     FALSE     FALSE
#> 9       9 1248.1941 494.5065              FALSE     FALSE     FALSE     FALSE
#> 10     10  955.2912 529.1857              FALSE     FALSE     FALSE     FALSE
#> 11     11 1042.7687 634.2470              FALSE     FALSE     FALSE     FALSE
#> 12     12  963.3603 418.1147              FALSE     FALSE     FALSE     FALSE
#> 13     13  870.8072 587.5157              FALSE     FALSE     FALSE     FALSE
#> 14     14  982.6551 566.6360              FALSE     FALSE     FALSE     FALSE
#> 15     15  743.4050 625.0480              FALSE     FALSE     FALSE     FALSE
#> 16     16 1135.8666 515.6653              FALSE     FALSE     FALSE     FALSE
#> 17     17  978.3904 569.6015              FALSE     FALSE     FALSE     FALSE
#> 18     18 1220.7134 561.3679              FALSE     FALSE     FALSE     FALSE
#> 19     19 1017.0611 496.5984              FALSE     FALSE     FALSE     FALSE
#> 20     20  874.8064 636.6294              FALSE     FALSE     FALSE     FALSE
#>    outside_bounds invalid_coordinate .gp3_group_id
#> 1           FALSE              FALSE           all
#> 2           FALSE              FALSE           all
#> 3           FALSE              FALSE           all
#> 4           FALSE              FALSE           all
#> 5           FALSE              FALSE           all
#> 6           FALSE              FALSE           all
#> 7           FALSE              FALSE           all
#> 8           FALSE              FALSE           all
#> 9           FALSE              FALSE           all
#> 10          FALSE              FALSE           all
#> 11          FALSE              FALSE           all
#> 12          FALSE              FALSE           all
#> 13          FALSE              FALSE           all
#> 14          FALSE              FALSE           all
#> 15          FALSE              FALSE           all
#> 16          FALSE              FALSE           all
#> 17          FALSE              FALSE           all
#> 18          FALSE              FALSE           all
#> 19          FALSE              FALSE           all
#> 20          FALSE              FALSE           all
#> 
#> $group_summary
#>   group_id n_rows n_missing_coordinate n_zero_zero n_outside_bounds
#> 1      all     20                    0           0                0
#>   n_invalid_coordinate missing_coordinate_rate zero_zero_rate
#> 1                    0                       0              0
#>   outside_bounds_rate invalid_coordinate_rate
#> 1                   0                       0
#> 
#> $overall_summary
#>   n_rows n_missing_coordinate n_zero_zero n_outside_bounds n_invalid_coordinate
#> 1     20                    0           0                0                    0
#>   missing_coordinate_rate zero_zero_rate outside_bounds_rate
#> 1                       0              0                   0
#>   invalid_coordinate_rate
#> 1                       0
#> 
#> $settings
#> $settings$x_col
#> [1] "gaze_x"
#> 
#> $settings$y_col
#> [1] "gaze_y"
#> 
#> $settings$screen_width
#> [1] 1920
#> 
#> $settings$screen_height
#> [1] 1080
#> 
#> $settings$group_cols
#> NULL
#> 
#> $settings$margin
#> [1] 0
#> 
#> $settings$treat_zero_zero_as_out_of_bounds
#> [1] TRUE
#> 
#> 
```
