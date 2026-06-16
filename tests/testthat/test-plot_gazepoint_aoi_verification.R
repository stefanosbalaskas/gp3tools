make_test_aoi_verification_geometry <- function() {
  tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("logo", "product"),
    x_min = c(0.10, 0.50),
    y_min = c(0.10, 0.50),
    x_max = c(0.30, 0.70),
    y_max = c(0.30, 0.70)
  )
}

make_test_aoi_verification_gaze <- function() {
  tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    x = c(0.20, 0.60, 0.80),
    y = c(0.20, 0.60, 0.80)
  )
}

test_that("plot_gazepoint_aoi_verification creates a ggplot with AOI and gaze layers", {
  toy_geometry <- make_test_aoi_verification_geometry()
  toy_gaze <- make_test_aoi_verification_gaze()

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    gaze_data = toy_gaze,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id"
  )

  expect_s3_class(p, "ggplot")
  expect_true("ggplot2::ggplot" %in% class(p))
  expect_equal(length(p$layers), 3)
  expect_equal(p$labels$title, "AOI verification plot")
  expect_equal(p$labels$x, "X coordinate")
  expect_equal(p$labels$y, "Y coordinate")
  expect_equal(p$labels$colour, "AOI")
})

test_that("plot_gazepoint_aoi_verification works without gaze data", {
  toy_geometry <- make_test_aoi_verification_geometry()

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    show_gaze = TRUE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 2)
})

test_that("plot_gazepoint_aoi_verification can hide labels", {
  toy_geometry <- make_test_aoi_verification_geometry()
  toy_gaze <- make_test_aoi_verification_gaze()

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    gaze_data = toy_gaze,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    show_labels = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 2)
})

test_that("plot_gazepoint_aoi_verification can hide gaze points", {
  toy_geometry <- make_test_aoi_verification_geometry()
  toy_gaze <- make_test_aoi_verification_gaze()

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    gaze_data = toy_gaze,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    show_gaze = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 2)
})

test_that("plot_gazepoint_aoi_verification supports origin-size geometry", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("logo", "product"),
    x = c(0.10, 0.50),
    y = c(0.10, 0.50),
    width = c(0.20, 0.20),
    height = c(0.20, 0.20)
  )

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_col = "x",
    y_col = "y",
    width_col = "width",
    height_col = "height"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 2)
})

test_that("plot_gazepoint_aoi_verification supports aliases and automatic detection", {
  toy_geometry <- tibble::tibble(
    MEDIA_ID = "stim1",
    AOI = "logo",
    left = 0.10,
    top = 0.10,
    right = 0.30,
    bottom = 0.30
  )

  toy_gaze <- tibble::tibble(
    MEDIA_ID = "stim1",
    FPOGX = 0.20,
    FPOGY = 0.20
  )

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    gaze_data = toy_gaze,
    geometry_stimulus_col = "MEDIA_ID",
    gaze_stimulus_col = "MEDIA_ID"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 3)
})

test_that("plot_gazepoint_aoi_verification can facet multiple stimuli", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim2", "stim2"),
    aoi = c("logo", "product", "logo", "product"),
    x_min = c(0.10, 0.50, 0.15, 0.55),
    y_min = c(0.10, 0.50, 0.15, 0.55),
    x_max = c(0.30, 0.70, 0.35, 0.75),
    y_max = c(0.30, 0.70, 0.35, 0.75)
  )

  toy_gaze <- tibble::tibble(
    media_id = c("stim1", "stim2"),
    x = c(0.20, 0.65),
    y = c(0.20, 0.65)
  )

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    gaze_data = toy_gaze,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    facet_by_stimulus = TRUE
  )

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_gazepoint_aoi_verification can disable faceting", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim2"),
    aoi = c("logo", "logo"),
    x_min = c(0.10, 0.60),
    y_min = c(0.10, 0.60),
    x_max = c(0.30, 0.80),
    y_max = c(0.30, 0.80)
  )

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    facet_by_stimulus = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_false(inherits(p$facet, "FacetWrap"))
})

test_that("plot_gazepoint_aoi_verification builds successfully", {
  toy_geometry <- make_test_aoi_verification_geometry()
  toy_gaze <- make_test_aoi_verification_gaze()

  p <- plot_gazepoint_aoi_verification(
    aoi_geometry = toy_geometry,
    gaze_data = toy_gaze,
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id"
  )

  built <- ggplot2::ggplot_build(p)

  expect_true(any(c("ggplot_built", "ggplot2::ggplot_built") %in% class(built)))
  expect_true(length(built$data) >= 2)
})

test_that("plot_gazepoint_aoi_verification checks invalid inputs", {
  toy_geometry <- make_test_aoi_verification_geometry()
  toy_gaze <- make_test_aoi_verification_gaze()

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = list()
    ),
    "`aoi_geometry` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry[0, ]
    ),
    "`aoi_geometry` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      gaze_data = list()
    ),
    "`gaze_data` must be NULL or a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      facet_by_stimulus = NA
    ),
    "`facet_by_stimulus` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      show_labels = NA
    ),
    "`show_labels` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      show_gaze = NA
    ),
    "`show_gaze` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      invert_y = NA
    ),
    "`invert_y` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      point_alpha = -0.1
    ),
    "`point_alpha` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      point_alpha = 1.5
    ),
    "`point_alpha` must be between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      point_size = -1
    ),
    "`point_size` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      line_width = -1
    ),
    "`line_width` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      label_size = -1
    ),
    "`label_size` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      gaze_data = toy_gaze,
      geometry_aoi_col = "aoi",
      geometry_stimulus_col = "media_id",
      x_min_col = "x_min",
      y_min_col = "y_min",
      x_max_col = "x_max",
      y_max_col = "y_max",
      gaze_x_col = "bad_x"
    ),
    "`gaze_x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_verification(
      aoi_geometry = toy_geometry,
      gaze_data = toy_gaze,
      geometry_aoi_col = "aoi",
      geometry_stimulus_col = "media_id",
      x_min_col = "x_min",
      y_min_col = "y_min",
      x_max_col = "x_max",
      y_max_col = "y_max",
      gaze_y_col = "bad_y"
    ),
    "`gaze_y_col` must be present in `data`",
    fixed = TRUE
  )
})
