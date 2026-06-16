make_test_aoi_margin_geometry <- function() {
  tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("logo", "product"),
    x_min = c(0.10, 0.50),
    y_min = c(0.10, 0.50),
    x_max = c(0.30, 0.70),
    y_max = c(0.30, 0.70)
  )
}

make_test_aoi_margin_gaze <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    media_id = c("stim1", "stim1", "stim1", "stim1"),
    sample_id = 1:4,
    x = c(0.20, 0.31, 0.49, 0.80),
    y = c(0.20, 0.20, 0.60, 0.80)
  )
}

test_that("audit_gazepoint_aoi_margin_sensitivity creates a complete margin audit", {
  toy_geometry <- make_test_aoi_margin_geometry()
  toy_gaze <- make_test_aoi_margin_gaze()

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = c("subject", "media_id", "sample_id"),
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.02),
    max_margin_change_prop = 0.20,
    max_ambiguous_prop = 0.05
  )

  expect_s3_class(out, "gp3_aoi_margin_sensitivity_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "geometry_summary",
      "sample_sensitivity",
      "margin_summary",
      "aoi_margin_summary",
      "flagged_samples",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$geometry_summary, "tbl_df")
  expect_s3_class(out$sample_sensitivity, "tbl_df")
  expect_s3_class(out$margin_summary, "tbl_df")
  expect_s3_class(out$aoi_margin_summary, "tbl_df")
  expect_s3_class(out$flagged_samples, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_gaze_rows, 4)
  expect_equal(out$overview$n_geometry_rows, 2)
  expect_equal(out$overview$n_aois, 2)
  expect_equal(out$overview$n_aois_used, 2)
  expect_equal(out$overview$n_margins, 2)
  expect_equal(out$overview$n_sample_margin_rows, 8)
  expect_equal(out$overview$n_flagged_margins, 1)
  expect_equal(out$overview$max_margin_change_prop_observed, 0.5)
  expect_equal(out$overview$max_ambiguous_prop_observed, 0)
  expect_equal(out$overview$aoi_margin_sensitivity_status, "review")

  expect_equal(nrow(out$sample_sensitivity), 8)
  expect_equal(nrow(out$margin_summary), 2)
  expect_equal(nrow(out$flagged_samples), 2)

  margin_0 <- out$margin_summary[
    out$margin_summary$margin == 0,
    ,
    drop = FALSE
  ]

  margin_002 <- out$margin_summary[
    out$margin_summary$margin == 0.02,
    ,
    drop = FALSE
  ]

  expect_equal(margin_0$margin_sensitivity_status, "base")
  expect_equal(margin_0$n_changed_from_base, 0)
  expect_equal(margin_002$n_changed_from_base, 2)
  expect_equal(margin_002$margin_change_prop, 0.5)
  expect_equal(margin_002$margin_sensitivity_status, "margin_sensitive")

  expect_equal(out$flagged_samples$sample_id, c(2L, 3L))
  expect_true(all(out$flagged_samples$changed_from_base))
})

test_that("audit_gazepoint_aoi_margin_sensitivity reports ok when margin changes stay below threshold", {
  toy_geometry <- make_test_aoi_margin_geometry()
  toy_gaze <- make_test_aoi_margin_gaze()

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = c("subject", "media_id", "sample_id"),
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.02),
    max_margin_change_prop = 0.75,
    max_ambiguous_prop = 0.05
  )

  expect_equal(out$overview$n_flagged_margins, 0)
  expect_equal(out$overview$aoi_margin_sensitivity_status, "ok")

  margin_002 <- out$margin_summary[
    out$margin_summary$margin == 0.02,
    ,
    drop = FALSE
  ]

  expect_equal(margin_002$margin_change_prop, 0.5)
  expect_equal(margin_002$margin_sensitivity_status, "ok")
})

test_that("audit_gazepoint_aoi_margin_sensitivity always includes zero margin", {
  toy_geometry <- make_test_aoi_margin_geometry()
  toy_gaze <- make_test_aoi_margin_gaze()

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = 0.02
  )

  expect_equal(out$overview$n_margins, 2)
  expect_equal(sort(unique(out$sample_sensitivity$margin)), c(0, 0.02))
  expect_equal(
    out$settings$value[out$settings$setting == "margins_used"],
    "0, 0.02"
  )
})

test_that("audit_gazepoint_aoi_margin_sensitivity supports negative shrink margins", {
  toy_geometry <- tibble::tibble(
    media_id = "stim1",
    aoi = "logo",
    x_min = 0.10,
    y_min = 0.10,
    x_max = 0.30,
    y_max = 0.30
  )

  toy_gaze <- tibble::tibble(
    media_id = "stim1",
    sample_id = 1:3,
    x = c(0.11, 0.20, 0.29),
    y = c(0.20, 0.20, 0.20)
  )

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = "sample_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(-0.02, 0),
    max_margin_change_prop = 0.50
  )

  shrink <- out$margin_summary[
    out$margin_summary$margin == -0.02,
    ,
    drop = FALSE
  ]

  expect_equal(shrink$n_changed_from_base, 2)
  expect_equal(shrink$margin_change_prop, 2 / 3)
  expect_equal(shrink$margin_sensitivity_status, "margin_sensitive")
  expect_equal(out$overview$aoi_margin_sensitivity_status, "review")
})

test_that("audit_gazepoint_aoi_margin_sensitivity detects ambiguous expanded AOI hits", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("left", "right"),
    x_min = c(0.10, 0.31),
    y_min = c(0.10, 0.10),
    x_max = c(0.30, 0.50),
    y_max = c(0.50, 0.50)
  )

  toy_gaze <- tibble::tibble(
    media_id = "stim1",
    sample_id = 1,
    x = 0.305,
    y = 0.30
  )

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = "sample_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.01),
    tie_method = "ambiguous",
    max_margin_change_prop = 1,
    max_ambiguous_prop = 0
  )

  expanded <- out$sample_sensitivity[
    out$sample_sensitivity$margin == 0.01,
    ,
    drop = FALSE
  ]

  expect_equal(expanded$assigned_aoi, "ambiguous")
  expect_equal(expanded$n_matching_aois, 2)
  expect_equal(expanded$margin_assignment_status, "ambiguous_aoi")

  expanded_summary <- out$margin_summary[
    out$margin_summary$margin == 0.01,
    ,
    drop = FALSE
  ]

  expect_equal(expanded_summary$ambiguous_prop, 1)
  expect_equal(expanded_summary$margin_sensitivity_status, "ambiguous_margin")
})

test_that("audit_gazepoint_aoi_margin_sensitivity supports first-hit tie resolution", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("left", "right"),
    x_min = c(0.10, 0.31),
    y_min = c(0.10, 0.10),
    x_max = c(0.30, 0.50),
    y_max = c(0.50, 0.50)
  )

  toy_gaze <- tibble::tibble(
    media_id = "stim1",
    sample_id = 1,
    x = 0.305,
    y = 0.30
  )

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = "sample_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.01),
    tie_method = "first",
    max_margin_change_prop = 1,
    max_ambiguous_prop = 0
  )

  expanded <- out$sample_sensitivity[
    out$sample_sensitivity$margin == 0.01,
    ,
    drop = FALSE
  ]

  expect_equal(expanded$assigned_aoi, "left")
  expect_equal(expanded$n_matching_aois, 2)
  expect_equal(expanded$margin_assignment_status, "multiple_aoi_resolved")
})

test_that("audit_gazepoint_aoi_margin_sensitivity handles missing coordinates", {
  toy_geometry <- make_test_aoi_margin_geometry()

  toy_gaze <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    sample_id = 1:2,
    x = c(NA_real_, 0.20),
    y = c(0.20, NA_real_)
  )

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = "sample_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.02)
  )

  expect_true(all(out$sample_sensitivity$assigned_aoi == "missing_coordinate"))
  expect_true(all(out$sample_sensitivity$margin_assignment_status == "missing_coordinate"))
  expect_equal(out$margin_summary$n_missing_coordinate, c(2L, 2L))
  expect_equal(out$margin_summary$missing_coordinate_prop, c(1, 1))
})

test_that("audit_gazepoint_aoi_margin_sensitivity supports multiple stimuli", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim2"),
    aoi = c("logo", "logo"),
    x_min = c(0.10, 0.60),
    y_min = c(0.10, 0.60),
    x_max = c(0.30, 0.80),
    y_max = c(0.30, 0.80)
  )

  toy_gaze <- tibble::tibble(
    media_id = c("stim1", "stim2"),
    sample_id = 1:2,
    x = c(0.20, 0.70),
    y = c(0.20, 0.70)
  )

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = c("media_id", "sample_id"),
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.02)
  )

  base_rows <- out$sample_sensitivity[
    out$sample_sensitivity$margin == 0,
    ,
    drop = FALSE
  ]

  expect_equal(base_rows$assigned_aoi, c("logo", "logo"))
  expect_equal(out$overview$n_gaze_rows, 2)
  expect_equal(out$overview$n_aois, 2)
})

test_that("audit_gazepoint_aoi_margin_sensitivity requires gaze stimulus when geometry has multiple stimuli", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim2"),
    aoi = c("logo", "logo"),
    x_min = c(0.10, 0.60),
    y_min = c(0.10, 0.60),
    x_max = c(0.30, 0.80),
    y_max = c(0.30, 0.80)
  )

  toy_gaze <- tibble::tibble(
    sample_id = 1,
    x = 0.20,
    y = 0.20
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      gaze_x_col = "x",
      gaze_y_col = "y",
      geometry_aoi_col = "aoi",
      geometry_stimulus_col = "media_id",
      x_min_col = "x_min",
      y_min_col = "y_min",
      x_max_col = "x_max",
      y_max_col = "y_max"
    ),
    "`gaze_stimulus_col` is required when `aoi_geometry` contains multiple stimuli",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_aoi_margin_sensitivity supports aliases and automatic detection", {
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

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_stimulus_col = "MEDIA_ID",
    geometry_stimulus_col = "MEDIA_ID",
    margins = c(0, 0.02)
  )

  expect_equal(out$overview$aoi_margin_sensitivity_status, "ok")
  expect_equal(
    out$settings$value[out$settings$setting == "gaze_x_col"],
    "FPOGX"
  )
  expect_equal(
    out$settings$value[out$settings$setting == "gaze_y_col"],
    "FPOGY"
  )
  expect_equal(
    out$settings$value[out$settings$setting == "geometry_aoi_col"],
    "aoi"
  )
  expect_equal(
    out$settings$value[out$settings$setting == "geometry_stimulus_col"],
    "media_id"
  )
})

test_that("audit_gazepoint_aoi_margin_sensitivity ignores invalid geometry by default", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("valid", "invalid"),
    x_min = c(0.10, NA_real_),
    y_min = c(0.10, 0.10),
    x_max = c(0.30, 0.30),
    y_max = c(0.30, 0.30)
  )

  toy_gaze <- tibble::tibble(
    media_id = "stim1",
    x = 0.20,
    y = 0.20
  )

  out <- audit_gazepoint_aoi_margin_sensitivity(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    margins = c(0, 0.02),
    ignore_invalid_geometry = TRUE
  )

  expect_equal(out$overview$n_aois, 2)
  expect_equal(out$overview$n_aois_used, 1)
  expect_equal(out$sample_sensitivity$assigned_aoi[out$sample_sensitivity$margin == 0], "valid")
})

test_that("audit_gazepoint_aoi_margin_sensitivity checks invalid inputs", {
  toy_geometry <- make_test_aoi_margin_geometry()
  toy_gaze <- make_test_aoi_margin_gaze()

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = list(),
      aoi_geometry = toy_geometry
    ),
    "`gaze_data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = list()
    ),
    "`aoi_geometry` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze[0, ],
      aoi_geometry = toy_geometry
    ),
    "`gaze_data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry[0, ]
    ),
    "`aoi_geometry` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      gaze_x_col = "bad_x"
    ),
    "`gaze_x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      gaze_y_col = "bad_y"
    ),
    "`gaze_y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      margins = numeric()
    ),
    "`margins` must be a non-empty finite numeric vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      margins = c(0, NA_real_)
    ),
    "`margins` must be a non-empty finite numeric vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      max_margin_change_prop = -1
    ),
    "`max_margin_change_prop` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      max_ambiguous_prop = 2
    ),
    "`max_ambiguous_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      outside_label = ""
    ),
    "`outside_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      ambiguous_label = ""
    ),
    "`ambiguous_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      missing_label = ""
    ),
    "`missing_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_margin_sensitivity(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      ignore_invalid_geometry = NA
    ),
    "`ignore_invalid_geometry` must be TRUE or FALSE",
    fixed = TRUE
  )
})
