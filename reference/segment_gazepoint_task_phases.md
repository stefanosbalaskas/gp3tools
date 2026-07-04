# Segment Gazepoint-style data into task phases

Assigns each row of a Gazepoint-style data set to a task phase using a
user-supplied phase-window table. The helper is deterministic and
descriptive: it labels rows according to declared time windows and does
not infer phases.

## Usage

``` r
segment_gazepoint_task_phases(
  data,
  time_col,
  phase_windows,
  phase_col = "task_phase",
  window_phase_col = "phase",
  window_start_col = "start",
  window_end_col = "end",
  outside_label = "outside",
  include_lower = TRUE,
  include_upper = FALSE,
  keep_window_metadata = FALSE
)
```

## Arguments

- data:

  A data frame.

- time_col:

  Character name of the time column.

- phase_windows:

  A data frame containing phase labels and start/end times.

- phase_col:

  Output column name for assigned phases.

- window_phase_col, window_start_col, window_end_col:

  Column names in `phase_windows`.

- outside_label:

  Label assigned to rows outside all phase windows. If `NULL`, outside
  rows receive `NA_character_`.

- include_lower, include_upper:

  Logical values controlling whether phase window boundaries are closed
  on the lower and upper sides.

- keep_window_metadata:

  If `TRUE`, adds assigned phase-window start and end columns.

## Value

A copy of `data` with phase labels and assignment diagnostics.

## Examples

``` r
x <- data.frame(time_ms = c(0, 250, 750, 1250, 1750))
windows <- data.frame(
  phase = c("baseline", "stimulus", "response"),
  start = c(0, 500, 1000),
  end = c(500, 1000, 2000)
)
segment_gazepoint_task_phases(x, "time_ms", windows)
#>   time_ms task_phase .gp3_phase_assigned
#> 1       0   baseline                TRUE
#> 2     250   baseline                TRUE
#> 3     750   stimulus                TRUE
#> 4    1250   response                TRUE
#> 5    1750   response                TRUE
```
