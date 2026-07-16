test_that("polygon AOIs classify inside, outside, and boundary samples", {
  samples <- data.frame(
    x = c(0.25, 0.75, 1.5, 0),
    y = c(0.25, 0.10, 0.5, 0.5),
    stringsAsFactors = FALSE
  )

  vertices <- data.frame(
    aoi_name = rep("triangle", 3L),
    vertex_order = 1:3,
    vertex_x = c(0, 1, 0),
    vertex_y = c(0, 0, 1),
    stringsAsFactors = FALSE
  )

  result <- add_gazepoint_polygon_aoi(
    master_df = samples,
    vertices = vertices,
    x_col = "x",
    y_col = "y",
    vertex_order_col = "vertex_order",
    output = "both",
    label_col = "aoi_label"
  )

  expect_equal(
    result$aoi_label,
    c("triangle", "triangle", "outside", "triangle")
  )

  expect_true(all(
    c("aoi_triangle", "aoi_overlap_count") %in%
      names(result)
  ))

  expect_equal(
    result$aoi_triangle,
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("polygon overlap handling can reject ambiguous labels", {
  samples <- data.frame(
    x = 0.5,
    y = 0.5,
    stringsAsFactors = FALSE
  )

  square <- function(name, left, right) {
    data.frame(
      aoi_name = name,
      vertex_order = 1:4,
      vertex_x = c(left, right, right, left),
      vertex_y = c(0, 0, 1, 1),
      stringsAsFactors = FALSE
    )
  }

  vertices <- rbind(
    square("left", 0, 0.75),
    square("right", 0.25, 1)
  )

  expect_error(
    add_gazepoint_polygon_aoi(
      master_df = samples,
      vertices = vertices,
      x_col = "x",
      y_col = "y",
      vertex_order_col = "vertex_order",
      overlap = "error"
    ),
    "overlap"
  )
})

test_that("dynamic rectangle AOIs follow matched definition times", {
  samples <- data.frame(
    participant = "P1",
    time = c(0, 0.4, 0.6, 1),
    x = c(0.25, 0.25, 1.25, 1.25),
    y = rep(0.5, 4L),
    stringsAsFactors = FALSE
  )

  definitions <- data.frame(
    participant = rep("P1", 2L),
    aoi_time = c(0, 1),
    aoi_name = rep("target", 2L),
    left = c(0, 1),
    right = c(0.5, 1.5),
    top = c(0, 0),
    bottom = c(1, 1),
    stringsAsFactors = FALSE
  )

  result <- add_gazepoint_dynamic_aoi(
    master_df = samples,
    aoi_defs = definitions,
    x_col = "x",
    y_col = "y",
    time_col = "time",
    group_cols = "participant",
    match = "nearest",
    output = "both",
    label_col = "aoi_label"
  )

  expect_equal(
    result$aoi_definition_time,
    c(0, 0, 1, 1)
  )

  expect_equal(
    result$aoi_label,
    rep("target", 4L)
  )

  expect_true(all(result$aoi_target))
  expect_equal(result$aoi_time_gap, c(0, 0.4, 0.4, 0))
})

test_that("dynamic polygon AOIs are supported without sf", {
  samples <- data.frame(
    time = c(0, 1),
    x = c(0.2, 1.2),
    y = c(0.2, 0.2),
    stringsAsFactors = FALSE
  )

  definitions <- rbind(
    data.frame(
      aoi_time = 0,
      aoi_name = "target",
      vertex_order = 1:4,
      vertex_x = c(0, 0.5, 0.5, 0),
      vertex_y = c(0, 0, 0.5, 0.5)
    ),
    data.frame(
      aoi_time = 1,
      aoi_name = "target",
      vertex_order = 1:4,
      vertex_x = c(1, 1.5, 1.5, 1),
      vertex_y = c(0, 0, 0.5, 0.5)
    )
  )

  result <- add_gazepoint_dynamic_aoi(
    master_df = samples,
    aoi_defs = definitions,
    x_col = "x",
    y_col = "y",
    time_col = "time",
    shape = "polygon",
    vertex_order_col = "vertex_order",
    label_col = "aoi_label"
  )

  expect_equal(result$aoi_label, c("target", "target"))
  expect_equal(result$aoi_definition_time, c(0, 1))
})

test_that("maximum dynamic-definition gaps can leave samples unmatched", {
  samples <- data.frame(
    time = c(0, 5),
    x = c(0.5, 0.5),
    y = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )

  definitions <- data.frame(
    aoi_time = 0,
    aoi_name = "target",
    left = 0,
    right = 1,
    top = 0,
    bottom = 1,
    stringsAsFactors = FALSE
  )

  result <- add_gazepoint_dynamic_aoi(
    master_df = samples,
    aoi_defs = definitions,
    x_col = "x",
    y_col = "y",
    time_col = "time",
    max_time_gap = 1,
    label_col = "aoi_label"
  )

  expect_equal(result$aoi_label[[1L]], "target")
  expect_true(is.na(result$aoi_label[[2L]]))
  expect_true(is.na(result$aoi_definition_time[[2L]]))
})

test_that("dynamic AOI audit reports coverage and flagged rows", {
  samples <- data.frame(
    participant = "P1",
    time = c(0, 0.5, 2),
    x = c(0.5, 2, NA_real_),
    y = c(0.5, 2, 0.5),
    stringsAsFactors = FALSE
  )

  definitions <- data.frame(
    participant = "P1",
    aoi_time = 0,
    aoi_name = "target",
    left = 0,
    right = 1,
    top = 0,
    bottom = 1,
    stringsAsFactors = FALSE
  )

  assigned <- add_gazepoint_dynamic_aoi(
    master_df = samples,
    aoi_defs = definitions,
    x_col = "x",
    y_col = "y",
    time_col = "time",
    group_cols = "participant",
    max_time_gap = 3,
    label_col = "aoi_label"
  )

  audit <- audit_gazepoint_dynamic_aoi_coverage(
    assigned,
    label_col = "aoi_label",
    group_cols = "participant",
    max_time_gap = 1,
    x_col = "x",
    y_col = "y"
  )

  expect_s3_class(
    audit,
    "gp3_dynamic_aoi_coverage_audit"
  )

  expect_equal(audit$overview$n_rows, 3L)
  expect_equal(audit$overview$n_inside_aoi, 1L)
  expect_equal(audit$overview$n_outside_aoi, 1L)
  expect_equal(audit$overview$n_missing_gaze, 1L)
  expect_equal(audit$overview$n_excessive_gap, 1L)
  expect_equal(audit$overview$audit_status, "review")
  expect_true(nrow(audit$flagged_rows) >= 2L)
  expect_equal(nrow(audit$group_summary), 1L)
})
