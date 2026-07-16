# Add polygon AOI membership to Gazepoint data

Classify sample-level gaze coordinates against one or more polygon AOIs
using a base-R ray-casting implementation. Polygon vertices are grouped
by AOI name and ordered either by row order or an explicit vertex-order
column.

## Usage

``` r
add_gazepoint_polygon_aoi(
  master_df,
  vertices,
  x_col = "FPOGX",
  y_col = "FPOGY",
  aoi_col = "aoi_name",
  vertex_x_col = "vertex_x",
  vertex_y_col = "vertex_y",
  vertex_order_col = NULL,
  output = c("label", "logical", "both"),
  prefix = "aoi_",
  label_col = "aoi_current",
  outside_label = "outside",
  overlap = c("first", "last", "error"),
  boundary = c("inside", "outside"),
  include_overlap_count = TRUE
)
```

## Arguments

- master_df:

  A sample-level gaze data frame.

- vertices:

  Polygon-vertex data frame.

- x_col, y_col:

  Gaze-coordinate columns.

- aoi_col:

  AOI-name column in `vertices`.

- vertex_x_col, vertex_y_col:

  Vertex-coordinate columns.

- vertex_order_col:

  Optional vertex-order column.

- output:

  Add logical AOI columns, one label column, or both.

- prefix:

  Prefix for logical AOI columns.

- label_col:

  Name of the AOI-label column.

- outside_label:

  Label used outside all polygons.

- overlap:

  Overlap handling for the label column.

- boundary:

  Should points on polygon boundaries be treated as inside?

- include_overlap_count:

  Add the number of containing AOIs.

## Value

The input data with polygon AOI membership columns.

## Examples

``` r
gaze <- data.frame(
  FPOGX = c(0.2, 0.7, 0.9),
  FPOGY = c(0.2, 0.7, 0.1)
)

triangle <- data.frame(
  aoi_name = "triangle",
  vertex_x = c(0, 1, 0),
  vertex_y = c(0, 0, 1)
)

add_gazepoint_polygon_aoi(
  gaze,
  triangle,
  output = "both"
)
#>   FPOGX FPOGY aoi_triangle aoi_current aoi_overlap_count
#> 1   0.2   0.2         TRUE    triangle                 1
#> 2   0.7   0.7        FALSE     outside                 0
#> 3   0.9   0.1         TRUE    triangle                 1
```
