# Add time-varying AOI membership to Gazepoint data

Match each gaze sample to a time-indexed AOI definition and classify the
sample against rectangular or polygon geometry. Definitions can also be
grouped by participant, stimulus, trial, or other shared columns.

## Usage

``` r
add_gazepoint_dynamic_aoi(
  master_df,
  aoi_defs,
  x_col = "FPOGX",
  y_col = "FPOGY",
  time_col = "TIME",
  aoi_time_col = "aoi_time",
  aoi_name_col = "aoi_name",
  shape = c("auto", "rectangle", "polygon"),
  group_cols = NULL,
  match = c("nearest", "previous", "next"),
  max_time_gap = Inf,
  left_col = "left",
  right_col = "right",
  top_col = "top",
  bottom_col = "bottom",
  vertex_x_col = "vertex_x",
  vertex_y_col = "vertex_y",
  vertex_order_col = NULL,
  output = c("label", "logical", "both"),
  prefix = "aoi_",
  label_col = "aoi_current",
  outside_label = "outside",
  overlap = c("first", "last", "error"),
  boundary = c("inside", "outside"),
  definition_time_col = "aoi_definition_time",
  time_gap_col = "aoi_time_gap",
  include_overlap_count = TRUE
)
```

## Arguments

- master_df:

  A sample-level gaze data frame.

- aoi_defs:

  Time-indexed AOI definitions.

- x_col, y_col:

  Gaze-coordinate columns in `master_df`.

- time_col:

  Sample timestamp column.

- aoi_time_col:

  Definition timestamp column.

- aoi_name_col:

  AOI-name column.

- shape:

  Geometry type: `"auto"`, `"rectangle"`, or `"polygon"`.

- group_cols:

  Columns shared by `master_df` and `aoi_defs` that define independent
  dynamic-definition streams.

- match:

  Time-matching rule: nearest, previous, or next definition.

- max_time_gap:

  Maximum permitted absolute gap between a sample and its matched
  definition timestamp, in native time units.

- left_col, right_col, top_col, bottom_col:

  Rectangle boundary columns.

- vertex_x_col, vertex_y_col:

  Polygon vertex columns.

- vertex_order_col:

  Optional polygon vertex-order column.

- output:

  Add logical columns, a label column, or both.

- prefix:

  Prefix for logical AOI columns.

- label_col:

  AOI-label column.

- outside_label:

  Label used outside all AOIs.

- overlap:

  Overlap handling.

- boundary:

  Polygon-boundary handling.

- definition_time_col, time_gap_col:

  Names of dynamic-match diagnostics.

- include_overlap_count:

  Add the number of containing AOIs.

## Value

The input data with dynamic AOI membership and match-diagnostic columns.
