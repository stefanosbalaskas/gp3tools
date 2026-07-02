test_that("prepare_gazepoint_heatmap_data handles normalised coordinates", {
  gaze <- data.frame(
    x = c(0.10, 0.50, NA, 1.20),
    y = c(0.20, 0.60, 0.30, 0.40),
    duration = c(100, 200, 150, 120)
  )

  out <- prepare_gazepoint_heatmap_data(
    gaze,
    x_col = "x",
    y_col = "y",
    weight_col = "duration",
    display_width = 1000,
    display_height = 500,
    coordinate_space = "normalized"
  )

  expect_s3_class(out, "gp3_heatmap_data")
  expect_equal(nrow(out), 2)
  expect_equal(out$.gp3_x_px, c(100, 500))
  expect_equal(out$.gp3_y_px, c(100, 300))
  expect_equal(out$.gp3_weight, c(100, 200))
  expect_true(all(out$.gp3_coordinate_space == "normalized"))
})


test_that("prepare_gazepoint_heatmap_data handles pixel coordinates", {
  gaze <- data.frame(
    x = c(100, 250, 800),
    y = c(120, 300, 450)
  )

  out <- prepare_gazepoint_heatmap_data(
    gaze,
    x_col = "x",
    y_col = "y",
    coordinate_space = "pixel",
    display_width = 1000,
    display_height = 600
  )

  expect_equal(nrow(out), 3)
  expect_equal(out$.gp3_x_px, gaze$x)
  expect_equal(out$.gp3_y_px, gaze$y)
  expect_true(all(out$.gp3_weight == 1))
  expect_true(all(out$.gp3_coordinate_space == "pixel"))
})


test_that("plot_gazepoint_heatmap returns a ggplot object", {
  gaze <- data.frame(
    x = c(0.20, 0.25, 0.70, 0.75),
    y = c(0.30, 0.35, 0.60, 0.65),
    duration = c(100, 150, 80, 90)
  )

  p <- plot_gazepoint_heatmap(
    gaze,
    x_col = "x",
    y_col = "y",
    weight_col = "duration",
    display_width = 800,
    display_height = 600,
    bins = 10
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_heatmap accepts prepared data", {
  gaze <- data.frame(
    x = c(0.20, 0.25, 0.70),
    y = c(0.30, 0.35, 0.60)
  )

  prepared <- prepare_gazepoint_heatmap_data(
    gaze,
    x_col = "x",
    y_col = "y",
    display_width = 800,
    display_height = 600
  )

  p <- plot_gazepoint_heatmap(prepared, bins = c(8, 6))

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_heatmap_overlay works with PNG background", {
  testthat::skip_if_not_installed("png")

  gaze <- data.frame(
    x = c(0.20, 0.25, 0.70),
    y = c(0.30, 0.35, 0.60),
    duration = c(100, 150, 80)
  )

  bg <- tempfile(fileext = ".png")
  img <- array(1, dim = c(60, 80, 3))
  png::writePNG(img, bg)

  p <- plot_gazepoint_heatmap_overlay(
    gaze,
    background_image = bg,
    x_col = "x",
    y_col = "y",
    weight_col = "duration",
    bins = 8
  )

  expect_s3_class(p, "ggplot")
})


test_that("export_gazepoint_heatmap_png writes a PNG file", {
  gaze <- data.frame(
    x = c(0.20, 0.25, 0.70),
    y = c(0.30, 0.35, 0.60)
  )

  p <- plot_gazepoint_heatmap(
    gaze,
    x_col = "x",
    y_col = "y",
    bins = 6
  )

  out <- tempfile(fileext = ".png")

  exported <- export_gazepoint_heatmap_png(
    p,
    out,
    width = 4,
    height = 3
  )

  expect_true(file.exists(out))
  expect_equal(normalizePath(out, winslash = "/", mustWork = FALSE), exported)
})


test_that("invalid heatmap inputs fail clearly", {
  gaze <- data.frame(x = c(NA, NA), y = c(NA, NA))

  expect_error(
    prepare_gazepoint_heatmap_data(gaze, x_col = "x", y_col = "y"),
    "No finite gaze coordinates"
  )

  expect_error(
    plot_gazepoint_heatmap(gaze),
    "`x_col` and `y_col` are required"
  )
})
