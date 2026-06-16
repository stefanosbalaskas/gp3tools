make_test_aoi_coding_geometry <- function() {
  tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("logo", "product"),
    x_min = c(0.10, 0.50),
    y_min = c(0.10, 0.50),
    x_max = c(0.30, 0.70),
    y_max = c(0.30, 0.70)
  )
}

make_test_aoi_coding_gaze <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    media_id = c("stim1", "stim1", "stim1", "stim1"),
    sample_id = 1:4,
    x = c(0.20, 0.60, 0.60, 0.80),
    y = c(0.20, 0.60, 0.60, 0.80),
    observed_aoi = c("logo", "product", "logo", "outside")
  )
}

test_that("audit_gazepoint_aoi_coding_matrix creates a complete coding audit", {
  toy_geometry <- make_test_aoi_coding_geometry()
  toy_gaze <- make_test_aoi_coding_gaze()

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
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
    max_mismatch_prop = 0.10
  )

  expect_s3_class(out, "gp3_aoi_coding_matrix_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "geometry_summary",
      "sample_coding",
      "coding_matrix",
      "observed_summary",
      "derived_summary",
      "flagged_samples",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$geometry_summary, "tbl_df")
  expect_s3_class(out$sample_coding, "tbl_df")
  expect_s3_class(out$coding_matrix, "tbl_df")
  expect_s3_class(out$observed_summary, "tbl_df")
  expect_s3_class(out$derived_summary, "tbl_df")
  expect_s3_class(out$flagged_samples, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_gaze_rows, 4)
  expect_equal(out$overview$n_geometry_rows, 2)
  expect_equal(out$overview$n_aois, 2)
  expect_equal(out$overview$n_aois_used, 2)
  expect_equal(out$overview$n_coded_samples, 4)
  expect_equal(out$overview$n_comparable_samples, 4)
  expect_equal(out$overview$n_mismatched_samples, 1)
  expect_equal(out$overview$mismatch_prop, 0.25)
  expect_equal(out$overview$n_ambiguous_samples, 0)
  expect_equal(out$overview$ambiguous_prop, 0)
  expect_equal(out$overview$n_missing_coordinate_samples, 0)
  expect_equal(out$overview$missing_coordinate_prop, 0)
  expect_equal(out$overview$n_flagged_samples, 1)
  expect_equal(out$overview$aoi_coding_matrix_status, "review")

  expect_equal(nrow(out$sample_coding), 4)
  expect_equal(nrow(out$flagged_samples), 1)
  expect_equal(out$flagged_samples$sample_id, 3L)
  expect_equal(out$flagged_samples$observed_aoi, "logo")
  expect_equal(out$flagged_samples$derived_aoi, "product")
  expect_equal(out$flagged_samples$aoi_coding_status, "mismatch")

  expect_equal(sum(out$coding_matrix$n_samples), 4)
  expect_true(
    any(
      out$coding_matrix$observed_aoi == "logo" &
        out$coding_matrix$derived_aoi == "product" &
        out$coding_matrix$n_samples == 1
    )
  )

  logo_observed <- out$observed_summary[
    out$observed_summary$observed_aoi == "logo",
    ,
    drop = FALSE
  ]

  product_derived <- out$derived_summary[
    out$derived_summary$derived_aoi == "product",
    ,
    drop = FALSE
  ]

  expect_equal(logo_observed$n_samples, 2)
  expect_equal(logo_observed$n_mismatches, 1)
  expect_equal(logo_observed$mismatch_prop, 0.5)

  expect_equal(product_derived$n_samples, 2)
  expect_equal(product_derived$n_mismatches, 1)
  expect_equal(product_derived$mismatch_prop, 0.5)
})

test_that("audit_gazepoint_aoi_coding_matrix reports ok for matching AOI coding", {
  toy_geometry <- make_test_aoi_coding_geometry()

  toy_gaze <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    media_id = c("stim1", "stim1", "stim1"),
    sample_id = 1:3,
    x = c(0.20, 0.60, 0.80),
    y = c(0.20, 0.60, 0.80),
    observed_aoi = c("logo", "product", "outside")
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = c("subject", "media_id", "sample_id"),
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$n_mismatched_samples, 0)
  expect_equal(out$overview$mismatch_prop, 0)
  expect_equal(out$overview$n_flagged_samples, 0)
  expect_equal(out$overview$aoi_coding_matrix_status, "ok")
  expect_true(all(out$sample_coding$aoi_coding_status == "ok"))
})

test_that("audit_gazepoint_aoi_coding_matrix standardises observed outside values", {
  toy_geometry <- make_test_aoi_coding_geometry()

  toy_gaze <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    sample_id = 1:3,
    x = c(0.80, 0.85, 0.90),
    y = c(0.80, 0.85, 0.90),
    observed_aoi = c("none", "background", "no_aoi")
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = "sample_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_true(all(out$sample_coding$observed_aoi == "outside"))
  expect_true(all(out$sample_coding$derived_aoi == "outside"))
  expect_true(all(out$sample_coding$aoi_coding_status == "ok"))
  expect_equal(out$overview$aoi_coding_matrix_status, "ok")
})

test_that("audit_gazepoint_aoi_coding_matrix flags missing observed AOI labels", {
  toy_geometry <- make_test_aoi_coding_geometry()

  toy_gaze <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    sample_id = 1:2,
    x = c(0.20, 0.60),
    y = c(0.20, 0.60),
    observed_aoi = c(NA_character_, "product")
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = "sample_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$n_coded_samples, 2)
  expect_equal(out$overview$n_comparable_samples, 1)
  expect_equal(out$overview$n_flagged_samples, 1)
  expect_equal(out$flagged_samples$sample_id, 1L)
  expect_equal(out$flagged_samples$aoi_coding_status, "observed_missing")
})

test_that("audit_gazepoint_aoi_coding_matrix detects missing coordinates", {
  toy_geometry <- make_test_aoi_coding_geometry()

  toy_gaze <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    sample_id = 1:2,
    x = c(NA_real_, 0.60),
    y = c(0.20, NA_real_),
    observed_aoi = c("logo", "product")
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
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
    max_missing_coordinate_prop = 0.20
  )

  expect_true(all(out$sample_coding$derived_aoi == "missing_coordinate"))
  expect_true(all(out$sample_coding$aoi_coding_status == "missing_coordinate"))
  expect_equal(out$overview$n_missing_coordinate_samples, 2)
  expect_equal(out$overview$missing_coordinate_prop, 1)
  expect_equal(out$overview$aoi_coding_matrix_status, "review")
})

test_that("audit_gazepoint_aoi_coding_matrix detects ambiguous derived AOI labels", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("left", "right"),
    x_min = c(0.10, 0.25),
    y_min = c(0.10, 0.10),
    x_max = c(0.40, 0.55),
    y_max = c(0.50, 0.50)
  )

  toy_gaze <- tibble::tibble(
    media_id = "stim1",
    sample_id = 1,
    x = 0.30,
    y = 0.30,
    observed_aoi = "left"
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
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
    tie_method = "ambiguous",
    max_ambiguous_prop = 0.05
  )

  expect_equal(out$sample_coding$derived_aoi, "ambiguous")
  expect_equal(out$sample_coding$n_matching_aois, 2)
  expect_equal(out$sample_coding$derived_assignment_status, "ambiguous_aoi")
  expect_equal(out$sample_coding$aoi_coding_status, "ambiguous_derived")
  expect_equal(out$overview$n_ambiguous_samples, 1)
  expect_equal(out$overview$ambiguous_prop, 1)
  expect_equal(out$overview$aoi_coding_matrix_status, "review")
})

test_that("audit_gazepoint_aoi_coding_matrix supports first-hit tie resolution", {
  toy_geometry <- tibble::tibble(
    media_id = c("stim1", "stim1"),
    aoi = c("left", "right"),
    x_min = c(0.10, 0.25),
    y_min = c(0.10, 0.10),
    x_max = c(0.40, 0.55),
    y_max = c(0.50, 0.50)
  )

  toy_gaze <- tibble::tibble(
    media_id = "stim1",
    sample_id = 1,
    x = 0.30,
    y = 0.30,
    observed_aoi = "left"
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
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
    tie_method = "first"
  )

  expect_equal(out$sample_coding$derived_aoi, "left")
  expect_equal(out$sample_coding$n_matching_aois, 2)
  expect_equal(out$sample_coding$derived_assignment_status, "multiple_aoi_resolved")
  expect_equal(out$sample_coding$aoi_coding_status, "ok")
})

test_that("audit_gazepoint_aoi_coding_matrix supports multiple stimuli", {
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
    y = c(0.20, 0.70),
    observed_aoi = c("logo", "logo")
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    sample_id_cols = c("media_id", "sample_id"),
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$n_coded_samples, 2)
  expect_equal(out$overview$n_mismatched_samples, 0)
  expect_equal(out$sample_coding$derived_aoi, c("logo", "logo"))
  expect_equal(out$overview$aoi_coding_matrix_status, "ok")
})

test_that("audit_gazepoint_aoi_coding_matrix requires gaze stimulus when geometry has multiple stimuli", {
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
    y = 0.20,
    observed_aoi = "logo"
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      observed_aoi_col = "observed_aoi",
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

test_that("audit_gazepoint_aoi_coding_matrix supports aliases and automatic detection", {
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
    AOI = "logo",
    FPOGX = 0.20,
    FPOGY = 0.20
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    gaze_stimulus_col = "MEDIA_ID",
    geometry_stimulus_col = "MEDIA_ID"
  )

  expect_equal(out$overview$aoi_coding_matrix_status, "ok")
  expect_equal(
    out$settings$value[out$settings$setting == "observed_aoi_col"],
    "aoi"
  )
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

test_that("audit_gazepoint_aoi_coding_matrix ignores invalid geometry by default", {
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
    y = 0.20,
    observed_aoi = "valid"
  )

  out <- audit_gazepoint_aoi_coding_matrix(
    gaze_data = toy_gaze,
    aoi_geometry = toy_geometry,
    observed_aoi_col = "observed_aoi",
    gaze_x_col = "x",
    gaze_y_col = "y",
    gaze_stimulus_col = "media_id",
    geometry_aoi_col = "aoi",
    geometry_stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    ignore_invalid_geometry = TRUE
  )

  expect_equal(out$overview$n_aois, 2)
  expect_equal(out$overview$n_aois_used, 1)
  expect_equal(out$sample_coding$derived_aoi, "valid")
  expect_equal(out$sample_coding$aoi_coding_status, "ok")
})

test_that("audit_gazepoint_aoi_coding_matrix checks invalid inputs", {
  toy_geometry <- make_test_aoi_coding_geometry()
  toy_gaze <- make_test_aoi_coding_gaze()

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = list(),
      aoi_geometry = toy_geometry
    ),
    "`gaze_data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = list()
    ),
    "`aoi_geometry` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze[0, ],
      aoi_geometry = toy_geometry
    ),
    "`gaze_data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry[0, ]
    ),
    "`aoi_geometry` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      observed_aoi_col = "bad_aoi"
    ),
    "`observed_aoi_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      gaze_x_col = "bad_x"
    ),
    "`gaze_x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      gaze_y_col = "bad_y"
    ),
    "`gaze_y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      outside_label = ""
    ),
    "`outside_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      ambiguous_label = ""
    ),
    "`ambiguous_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      missing_label = ""
    ),
    "`missing_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      observed_outside_values = character()
    ),
    "`observed_outside_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      max_mismatch_prop = 2
    ),
    "`max_mismatch_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      max_ambiguous_prop = 2
    ),
    "`max_ambiguous_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      max_missing_coordinate_prop = 2
    ),
    "`max_missing_coordinate_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_coding_matrix(
      gaze_data = toy_gaze,
      aoi_geometry = toy_geometry,
      ignore_invalid_geometry = NA
    ),
    "`ignore_invalid_geometry` must be TRUE or FALSE",
    fixed = TRUE
  )
})
