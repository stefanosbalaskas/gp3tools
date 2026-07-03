test_that("audit_gazepoint_screen_bounds flags missing, zero-zero, and outside coordinates", {
  x <- data.frame(
    participant = c("P1", "P1", "P2", "P2", "P2"),
    gaze_x = c(100, 0, 1921, NA, 500),
    gaze_y = c(100, 0, 500, 400, 1081)
  )

  out <- audit_gazepoint_screen_bounds(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    screen_width = 1920,
    screen_height = 1080,
    group_cols = "participant"
  )

  expect_type(out, "list")
  expect_s3_class(out$row_flags, "data.frame")
  expect_s3_class(out$group_summary, "data.frame")
  expect_s3_class(out$overall_summary, "data.frame")

  expect_equal(out$overall_summary$n_rows, 5)
  expect_equal(out$overall_summary$n_missing_coordinate, 1)
  expect_equal(out$overall_summary$n_zero_zero, 1)
  expect_equal(out$overall_summary$n_outside_bounds, 2)
  expect_equal(out$overall_summary$n_invalid_coordinate, 4)
  expect_equal(nrow(out$group_summary), 2)
})


test_that("audit_gazepoint_screen_bounds supports margin tolerance", {
  x <- data.frame(
    gaze_x = c(1921, 1925),
    gaze_y = c(500, 500)
  )

  no_margin <- audit_gazepoint_screen_bounds(
    x,
    "gaze_x",
    "gaze_y",
    screen_width = 1920,
    screen_height = 1080,
    margin = 0
  )

  with_margin <- audit_gazepoint_screen_bounds(
    x,
    "gaze_x",
    "gaze_y",
    screen_width = 1920,
    screen_height = 1080,
    margin = 2
  )

  expect_equal(no_margin$overall_summary$n_outside_bounds, 2)
  expect_equal(with_margin$overall_summary$n_outside_bounds, 1)
})


test_that("audit_gazepoint_screen_bounds can keep zero-zero as separate non-invalid flag", {
  x <- data.frame(gaze_x = c(0, 100), gaze_y = c(0, 100))

  out <- audit_gazepoint_screen_bounds(
    x,
    "gaze_x",
    "gaze_y",
    screen_width = 1920,
    screen_height = 1080,
    treat_zero_zero_as_out_of_bounds = FALSE
  )

  expect_equal(out$overall_summary$n_zero_zero, 1)
  expect_equal(out$overall_summary$n_invalid_coordinate, 0)
})


test_that("audit_gazepoint_screen_bounds validates columns and screen dimensions", {
  x <- data.frame(gaze_x = 1, gaze_y = 1)

  expect_error(
    audit_gazepoint_screen_bounds(x, "missing", "gaze_y", 1920, 1080),
    "missing required column"
  )

  expect_error(
    audit_gazepoint_screen_bounds(x, "gaze_x", "gaze_y", 0, 1080),
    "screen_width"
  )

  expect_error(
    audit_gazepoint_screen_bounds(x, "gaze_x", "gaze_y", 1920, 1080, margin = -1),
    "margin"
  )
})


test_that("harmonize_gazepoint_screen_coordinates rescales coordinates", {
  x <- data.frame(
    id = 1:3,
    gaze_x = c(0, 960, 1920),
    gaze_y = c(0, 540, 1080)
  )

  out <- harmonize_gazepoint_screen_coordinates(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    from_width = 1920,
    from_height = 1080,
    to_width = 1280,
    to_height = 720
  )

  expect_equal(out$gaze_x_harmonized, c(0, 640, 1280))
  expect_equal(out$gaze_y_harmonized, c(0, 360, 720))
  expect_true("gaze_x" %in% names(out))
  expect_true("gaze_y" %in% names(out))

  settings <- attr(out, "gp3_screen_harmonization")
  expect_equal(settings$x_scale, 1280 / 1920)
  expect_equal(settings$y_scale, 720 / 1080)
})


test_that("harmonize_gazepoint_screen_coordinates can remove original columns", {
  x <- data.frame(gaze_x = c(100, 200), gaze_y = c(50, 100))

  out <- harmonize_gazepoint_screen_coordinates(
    x,
    x_col = "gaze_x",
    y_col = "gaze_y",
    from_width = 1000,
    from_height = 500,
    to_width = 2000,
    to_height = 1000,
    output_x_col = "x2",
    output_y_col = "y2",
    keep_original = FALSE
  )

  expect_false("gaze_x" %in% names(out))
  expect_false("gaze_y" %in% names(out))
  expect_equal(out$x2, c(200, 400))
  expect_equal(out$y2, c(100, 200))
})


test_that("harmonize_gazepoint_screen_coordinates validates arguments", {
  x <- data.frame(gaze_x = 1, gaze_y = 1)

  expect_error(
    harmonize_gazepoint_screen_coordinates(x, "missing", "gaze_y", 1920, 1080, 1280, 720),
    "missing required column"
  )

  expect_error(
    harmonize_gazepoint_screen_coordinates(x, "gaze_x", "gaze_y", 0, 1080, 1280, 720),
    "from_width"
  )

  expect_error(
    harmonize_gazepoint_screen_coordinates(
      x,
      "gaze_x",
      "gaze_y",
      1920,
      1080,
      1280,
      720,
      output_x_col = ""
    ),
    "output_x_col"
  )
})
