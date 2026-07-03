test_that("audit_gazepoint_aoi_screen_coverage summarizes AOI geometry", {
  aoi <- data.frame(
    aoi = c("inside", "partial", "invalid", "missing"),
    x_min = c(100, 1800, 300, NA),
    x_max = c(500, 2000, 250, 600),
    y_min = c(100, 900, 100, 100),
    y_max = c(400, 1100, 200, 300)
  )

  out <- audit_gazepoint_aoi_screen_coverage(
    aoi,
    screen_width = 1920,
    screen_height = 1080,
    aoi_col = "aoi"
  )

  expect_type(out, "list")
  expect_s3_class(out$aoi_summary, "data.frame")
  expect_s3_class(out$overall_summary, "data.frame")

  expect_equal(out$overall_summary$n_aois, 4)
  expect_equal(out$overall_summary$n_missing_geometry, 1)
  expect_equal(out$overall_summary$n_invalid_rectangles, 1)
  expect_equal(out$overall_summary$n_outside_screen, 1)

  partial <- out$aoi_summary[out$aoi_summary$aoi_id == "partial", ]
  expect_true(partial$outside_screen)
  expect_lt(partial$clipped_area, partial$raw_area)
})


test_that("audit_gazepoint_aoi_screen_coverage supports margin tolerance", {
  aoi <- data.frame(
    x_min = -1,
    x_max = 100,
    y_min = 100,
    y_max = 200
  )

  no_margin <- audit_gazepoint_aoi_screen_coverage(aoi, 1920, 1080, margin = 0)
  with_margin <- audit_gazepoint_aoi_screen_coverage(aoi, 1920, 1080, margin = 2)

  expect_true(no_margin$aoi_summary$outside_screen)
  expect_false(with_margin$aoi_summary$outside_screen)
})


test_that("audit_gazepoint_aoi_screen_coverage validates inputs", {
  aoi <- data.frame(x_min = 1, x_max = 2, y_min = 1, y_max = 2)

  expect_error(
    audit_gazepoint_aoi_screen_coverage(aoi, 0, 1080),
    "screen_width"
  )

  expect_error(
    audit_gazepoint_aoi_screen_coverage(aoi, 1920, 1080, x_min_col = "missing"),
    "missing required column"
  )

  expect_error(
    audit_gazepoint_aoi_screen_coverage(aoi, 1920, 1080, margin = -1),
    "margin"
  )
})


test_that("summarize_gazepoint_coordinate_coverage summarizes grouped coordinate coverage", {
  x <- data.frame(
    condition = rep(c("A", "B"), each = 4),
    gaze_x = c(0, 100, 500, 1920, 10, NA, 2100, 1000),
    gaze_y = c(0, 100, 500, 1080, 10, 100, 100, 500)
  )

  out <- summarize_gazepoint_coordinate_coverage(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    screen_width = 1920,
    screen_height = 1080,
    group_cols = "condition",
    grid_n_x = 4,
    grid_n_y = 4
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)

  a <- out[out$group_id == "A", ]
  b <- out[out$group_id == "B", ]

  expect_equal(a$n_rows, 4)
  expect_equal(a$n_inside_screen, 4)
  expect_equal(a$inside_screen_rate, 1)

  expect_equal(b$n_rows, 4)
  expect_equal(b$n_inside_screen, 2)
  expect_equal(b$inside_screen_rate, 0.5)
  expect_true(b$occupied_grid_cells >= 1)
})


test_that("summarize_gazepoint_coordinate_coverage can run without groups", {
  x <- data.frame(
    gaze_x = c(0, 960, 1920),
    gaze_y = c(0, 540, 1080)
  )

  out <- summarize_gazepoint_coordinate_coverage(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    screen_width = 1920,
    screen_height = 1080,
    grid_n_x = 2,
    grid_n_y = 2
  )

  expect_equal(out$group_id, "all")
  expect_equal(out$n_inside_screen, 3)
  expect_equal(out$total_grid_cells, 4)
  expect_true(out$occupied_grid_rate > 0)
})


test_that("summarize_gazepoint_coordinate_coverage validates inputs", {
  x <- data.frame(gaze_x = 1, gaze_y = 1)

  expect_error(
    summarize_gazepoint_coordinate_coverage(x, "missing", "gaze_y", 1920, 1080),
    "missing required column"
  )

  expect_error(
    summarize_gazepoint_coordinate_coverage(x, "gaze_x", "gaze_y", 0, 1080),
    "screen_width"
  )

  expect_error(
    summarize_gazepoint_coordinate_coverage(x, "gaze_x", "gaze_y", 1920, 1080, grid_n_x = 0),
    "positive integer"
  )
})
