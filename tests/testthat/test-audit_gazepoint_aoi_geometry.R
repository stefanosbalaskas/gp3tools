make_test_aoi_geometry_data <- function() {
  tibble::tibble(
    media_id = c("stim1", "stim1", "stim1", "stim1"),
    aoi = c("logo", "product", "price", "bad_box"),
    x_min = c(0.05, 0.30, 0.70, 0.90),
    y_min = c(0.05, 0.25, 0.70, 0.90),
    x_max = c(0.20, 0.60, 0.85, 1.20),
    y_max = c(0.20, 0.60, 0.85, 1.10)
  )
}

test_that("audit_gazepoint_aoi_geometry creates a complete geometry audit", {
  toy_geometry <- make_test_aoi_geometry_data()

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1),
    min_width = 0.05,
    min_height = 0.05,
    min_area = 0.005,
    max_area_prop = 0.50,
    require_within_screen = TRUE
  )

  expect_s3_class(out, "gp3_aoi_geometry_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "geometry_summary",
      "size_summary",
      "duplicate_geometry",
      "flagged_aois",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$geometry_summary, "tbl_df")
  expect_s3_class(out$size_summary, "tbl_df")
  expect_s3_class(out$duplicate_geometry, "tbl_df")
  expect_s3_class(out$flagged_aois, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 4)
  expect_equal(out$overview$n_aois, 4)
  expect_equal(out$overview$n_stimuli, 1)
  expect_equal(out$overview$n_flagged_aois, 1)
  expect_equal(out$overview$n_duplicate_geometry_groups, 0)
  expect_equal(out$overview$coordinate_format, "bounds")
  expect_equal(out$overview$screen_width, 1)
  expect_equal(out$overview$screen_height, 1)
  expect_equal(out$overview$screen_area, 1)
  expect_equal(out$overview$aoi_geometry_status, "review")

  expect_equal(nrow(out$geometry_summary), 4)
  expect_equal(nrow(out$flagged_aois), 1)
  expect_equal(out$flagged_aois$aoi, "bad_box")
  expect_equal(out$flagged_aois$outside_screen, TRUE)
  expect_equal(out$flagged_aois$aoi_geometry_status, "outside_screen")

  logo <- out$geometry_summary[
    out$geometry_summary$aoi == "logo",
    ,
    drop = FALSE
  ]

  expect_equal(logo$width, 0.15)
  expect_equal(logo$height, 0.15)
  expect_equal(logo$area, 0.0225)
  expect_equal(logo$area_prop, 0.0225)
  expect_equal(logo$aoi_geometry_status, "ok")
})

test_that("audit_gazepoint_aoi_geometry reports ok for valid AOI geometry", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    aoi = c("logo", "product", "price"),
    x_min = c(0.05, 0.30, 0.70),
    y_min = c(0.05, 0.25, 0.70),
    x_max = c(0.20, 0.60, 0.85),
    y_max = c(0.20, 0.60, 0.85)
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_width = 0.05,
    min_height = 0.05,
    min_area = 0.005
  )

  expect_equal(out$overview$n_flagged_aois, 0)
  expect_equal(out$overview$n_duplicate_geometry_groups, 0)
  expect_equal(out$overview$aoi_geometry_status, "ok")
  expect_equal(nrow(out$flagged_aois), 0)
  expect_true(all(out$geometry_summary$aoi_geometry_status == "ok"))
})

test_that("audit_gazepoint_aoi_geometry supports origin-size geometry", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("logo", "product"),
    x = c(0.10, 0.40),
    y = c(0.20, 0.50),
    width = c(0.20, 0.30),
    height = c(0.10, 0.20)
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_col = "x",
    y_col = "y",
    width_col = "width",
    height_col = "height",
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1)
  )

  expect_equal(out$overview$coordinate_format, "origin_size")
  expect_equal(out$overview$aoi_geometry_status, "ok")
  expect_equal(out$geometry_summary$x_min[1], 0.10)
  expect_equal(out$geometry_summary$x_max[1], 0.30)
  expect_equal(out$geometry_summary$y_min[1], 0.20)
  expect_equal(out$geometry_summary$y_max[1], 0.30)
  expect_equal(out$geometry_summary$area[1], 0.02)
})

test_that("audit_gazepoint_aoi_geometry detects too-small AOIs", {
  toy_geometry <- tibble::tibble(
    media_id = "stim1",
    aoi = "tiny",
    x_min = 0.10,
    y_min = 0.10,
    x_max = 0.12,
    y_max = 0.12
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_width = 0.05,
    min_height = 0.05,
    min_area = 0.005
  )

  expect_equal(out$overview$aoi_geometry_status, "review")
  expect_equal(out$geometry_summary$aoi_geometry_status, "too_small")
  expect_equal(out$flagged_aois$aoi, "tiny")
})

test_that("audit_gazepoint_aoi_geometry detects too-large AOIs", {
  toy_geometry <- tibble::tibble(
    media_id = "stim1",
    aoi = "huge",
    x_min = 0.00,
    y_min = 0.00,
    x_max = 1.00,
    y_max = 1.00
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    max_area_prop = 0.50
  )

  expect_equal(out$overview$aoi_geometry_status, "review")
  expect_equal(out$geometry_summary$aoi_geometry_status, "too_large")
  expect_equal(out$flagged_aois$aoi, "huge")
})

test_that("audit_gazepoint_aoi_geometry detects invalid dimensions", {
  toy_geometry <- tibble::tibble(
    media_id = "stim1",
    aoi = "invalid",
    x_min = 0.50,
    y_min = 0.10,
    x_max = 0.20,
    y_max = 0.30
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$aoi_geometry_status, "review")
  expect_equal(out$geometry_summary$aoi_geometry_status, "invalid_dimension")
})

test_that("audit_gazepoint_aoi_geometry detects invalid coordinates", {
  toy_geometry <- tibble::tibble(
    media_id = "stim1",
    aoi = "missing_coord",
    x_min = NA_real_,
    y_min = 0.10,
    x_max = 0.20,
    y_max = 0.30
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$aoi_geometry_status, "review")
  expect_equal(out$geometry_summary$aoi_geometry_status, "invalid_coordinate")
})

test_that("audit_gazepoint_aoi_geometry can ignore outside-screen status when requested", {
  toy_geometry <- tibble::tibble(
    media_id = "stim1",
    aoi = "edge_box",
    x_min = 0.90,
    y_min = 0.90,
    x_max = 1.20,
    y_max = 1.10
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    require_within_screen = FALSE
  )

  expect_equal(out$geometry_summary$outside_screen, TRUE)
  expect_equal(out$geometry_summary$aoi_geometry_status, "ok")
  expect_equal(out$overview$aoi_geometry_status, "ok")
})

test_that("audit_gazepoint_aoi_geometry detects duplicate geometry", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    aoi = c("logo", "logo_copy", "product"),
    x_min = c(0.10, 0.10, 0.40),
    y_min = c(0.10, 0.10, 0.40),
    x_max = c(0.30, 0.30, 0.70),
    y_max = c(0.30, 0.30, 0.70)
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$n_duplicate_geometry_groups, 1)
  expect_equal(out$overview$aoi_geometry_status, "review")
  expect_equal(nrow(out$duplicate_geometry), 1)
  expect_equal(out$duplicate_geometry$n_aois, 2)
  expect_equal(out$duplicate_geometry$duplicate_geometry_status, "duplicate_geometry")
})

test_that("audit_gazepoint_aoi_geometry supports aliases and automatic detection", {
  toy_geometry <- tibble::tibble(
    MEDIA_ID = c("stim1", "stim1"),
    AOI = c("logo", "product"),
    left = c(0.10, 0.40),
    top = c(0.10, 0.40),
    right = c(0.30, 0.70),
    bottom = c(0.30, 0.70)
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    stimulus_col = "MEDIA_ID"
  )

  expect_equal(out$overview$aoi_geometry_status, "ok")
  expect_true("media_id" %in% names(out$geometry_summary))
  expect_true("aoi" %in% names(out$geometry_summary))
  expect_equal(out$overview$coordinate_format, "bounds")
  expect_equal(
    out$settings$value[out$settings$setting == "aoi_col"],
    "aoi"
  )
  expect_equal(
    out$settings$value[out$settings$setting == "stimulus_col"],
    "media_id"
  )
})

test_that("audit_gazepoint_aoi_geometry works without stimulus column", {
  toy_geometry <- tibble::tibble(
    aoi = c("logo", "product"),
    x_min = c(0.10, 0.40),
    y_min = c(0.10, 0.40),
    x_max = c(0.30, 0.70),
    y_max = c(0.30, 0.70)
  )

  out <- audit_gazepoint_aoi_geometry(
    toy_geometry,
    aoi_col = "aoi",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$n_aois, 2)
  expect_true(is.na(out$overview$n_stimuli))
  expect_equal(out$overview$aoi_geometry_status, "ok")
})

test_that("audit_gazepoint_aoi_geometry checks invalid inputs", {
  toy_geometry <- make_test_aoi_geometry_data()

  expect_error(
    audit_gazepoint_aoi_geometry(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      aoi_col = "bad_aoi"
    ),
    "`aoi_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      stimulus_col = "bad_stimulus"
    ),
    "`stimulus_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      x_min_col = "bad_x_min"
    ),
    "`x_min_col` must be present in `data`",
    fixed = TRUE
  )

  incomplete_geometry <- tibble::tibble(
    aoi = "logo",
    x_min = 0.10,
    y_min = 0.10
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      incomplete_geometry,
      aoi_col = "aoi"
    ),
    "AOI geometry requires either x/y min-max columns or x/y plus width/height columns",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      screen_x_range = c(1, 0)
    ),
    "`screen_x_range` must be a numeric length-2 vector with lower < upper",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      screen_y_range = c(1, 1)
    ),
    "`screen_y_range` must be a numeric length-2 vector with lower < upper",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      min_width = -1
    ),
    "`min_width` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      min_height = -1
    ),
    "`min_height` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      min_area = -1
    ),
    "`min_area` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      max_area_prop = 2
    ),
    "`max_area_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_geometry(
      toy_geometry,
      require_within_screen = NA
    ),
    "`require_within_screen` must be TRUE or FALSE",
    fixed = TRUE
  )
})
