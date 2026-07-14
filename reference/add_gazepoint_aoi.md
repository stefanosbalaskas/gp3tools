# Add rectangular AOI membership to gaze data

Labels gaze samples using one or more rectangular AOI definitions.

## Usage

``` r
add_gazepoint_aoi(
  master_df,
  aoi_defs,
  x_col = "FPOGX",
  y_col = "FPOGY",
  aoi_name = NULL,
  output = c("logical", "label", "both"),
  prefix = "aoi_",
  label_col = "aoi_current",
  outside_label = "outside",
  overlap = c("first", "last", "error"),
  include_overlap_count = TRUE
)
```

## Arguments

- master_df:

  A sample-level gaze data frame.

- aoi_defs:

  AOI definition data frame. Recognised aliases include
  `name`/`aoi_name`, `L`/`left`/`xmin`, `R`/`right`/`xmax`,
  `T`/`top`/`ymin`, and `B`/`bottom`/`ymax`.

- x_col, y_col:

  Gaze-coordinate columns.

- aoi_name:

  Optional AOI name selecting one row from `aoi_defs`.

- output:

  Add logical AOI columns, a single label column, or both.

- prefix:

  Prefix for logical AOI columns.

- label_col:

  Name of the single AOI-label column.

- outside_label:

  Label for samples outside all AOIs.

- overlap:

  Overlap handling for the label column.

- include_overlap_count:

  Add the number of AOIs containing each sample.

## Value

The input data with AOI membership columns.

## Examples

``` r
gaze <- data.frame(
  FPOGX = c(0.2, 0.5, 0.8),
  FPOGY = c(0.2, 0.5, 0.8)
)
defs <- data.frame(
  name = c("top_left", "bottom_right"),
  L = c(0, 0.6),
  R = c(0.4, 1),
  T = c(0, 0.6),
  B = c(0.4, 1)
)
add_gazepoint_aoi(gaze, defs, output = "both")
#>   FPOGX FPOGY aoi_top_left aoi_bottom_right  aoi_current aoi_overlap_count
#> 1   0.2   0.2         TRUE            FALSE     top_left                 1
#> 2   0.5   0.5        FALSE            FALSE      outside                 0
#> 3   0.8   0.8        FALSE             TRUE bottom_right                 1
```
