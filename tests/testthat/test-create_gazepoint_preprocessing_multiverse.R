test_that("create_gazepoint_preprocessing_multiverse creates pupil and AOI grids", {
  out <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = c(75, 150),
    pupil_smoothing_window_samples = c(3, 5),
    pupil_baseline_windows = list(c(0, 200), c(-200, 0)),
    pupil_artifact_padding_ms = c(0, 50),
    aoi_denominators = c("valid", "aoi_only"),
    aoi_min_denominator_samples = c(1, 5),
    label_prefix = "study1"
  )

  expect_s3_class(out, "gp3_preprocessing_multiverse")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "pupil_grid",
      "aoi_grid",
      "combined_grid",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$pupil_grid, "tbl_df")
  expect_s3_class(out$aoi_grid, "tbl_df")
  expect_s3_class(out$combined_grid, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_pupil_branches, 16)
  expect_equal(out$overview$n_aoi_branches, 4)
  expect_equal(out$overview$n_combined_branches, 64)
  expect_equal(out$overview$multiverse_status, "defined")

  expect_equal(nrow(out$pupil_grid), 16)
  expect_equal(nrow(out$aoi_grid), 4)
  expect_equal(nrow(out$combined_grid), 64)

  expect_true(all(out$pupil_grid$preprocessing_family == "pupil"))
  expect_true(all(out$aoi_grid$preprocessing_family == "aoi"))
  expect_true(all(out$pupil_grid$decision_type == "sensitivity"))
  expect_true(all(out$aoi_grid$decision_type == "sensitivity"))

  expect_true(all(out$pupil_grid$branch_status == "defined"))
  expect_true(all(out$aoi_grid$branch_status == "defined"))
  expect_true(all(out$combined_grid$branch_status == "defined"))

  expect_true(all(grepl("^study1_pupil_", out$pupil_grid$branch_id)))
  expect_true(all(grepl("^study1_aoi_", out$aoi_grid$branch_id)))
  expect_true(all(grepl("^study1_combined_", out$combined_grid$combined_branch_id)))
})

test_that("create_gazepoint_preprocessing_multiverse supports pupil-only multiverses", {
  out <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = c(100, 200),
    pupil_smoothing_window_samples = 5,
    pupil_baseline_windows = list(c(0, 200)),
    pupil_artifact_padding_ms = 0,
    include_pupil = TRUE,
    include_aoi = FALSE,
    label_prefix = "pupil_only"
  )

  expect_s3_class(out, "gp3_preprocessing_multiverse")

  expect_equal(out$overview$include_pupil, TRUE)
  expect_equal(out$overview$include_aoi, FALSE)
  expect_equal(out$overview$n_pupil_branches, 2)
  expect_equal(out$overview$n_aoi_branches, 0)
  expect_equal(out$overview$n_combined_branches, 2)

  expect_equal(nrow(out$pupil_grid), 2)
  expect_equal(nrow(out$aoi_grid), 0)
  expect_equal(nrow(out$combined_grid), 2)

  expect_true(all(is.na(out$combined_grid$aoi_branch_id)))
  expect_true(all(!is.na(out$combined_grid$pupil_branch_id)))
})

test_that("create_gazepoint_preprocessing_multiverse supports AOI-only multiverses", {
  out <- create_gazepoint_preprocessing_multiverse(
    aoi_denominators = c("valid", "all", "aoi_only"),
    aoi_min_denominator_samples = c(1, 5),
    include_pupil = FALSE,
    include_aoi = TRUE,
    label_prefix = "aoi_only"
  )

  expect_s3_class(out, "gp3_preprocessing_multiverse")

  expect_equal(out$overview$include_pupil, FALSE)
  expect_equal(out$overview$include_aoi, TRUE)
  expect_equal(out$overview$n_pupil_branches, 0)
  expect_equal(out$overview$n_aoi_branches, 6)
  expect_equal(out$overview$n_combined_branches, 6)

  expect_equal(nrow(out$pupil_grid), 0)
  expect_equal(nrow(out$aoi_grid), 6)
  expect_equal(nrow(out$combined_grid), 6)

  expect_true(all(is.na(out$combined_grid$pupil_branch_id)))
  expect_true(all(!is.na(out$combined_grid$aoi_branch_id)))
})

test_that("create_gazepoint_preprocessing_multiverse accepts a single baseline vector", {
  out <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = 100,
    pupil_smoothing_window_samples = 5,
    pupil_baseline_windows = c(0, 200),
    pupil_artifact_padding_ms = 0,
    include_pupil = TRUE,
    include_aoi = FALSE,
    label_prefix = "single_baseline"
  )

  expect_equal(nrow(out$pupil_grid), 1)
  expect_equal(out$pupil_grid$baseline_window_start_ms, 0)
  expect_equal(out$pupil_grid$baseline_window_end_ms, 200)
  expect_equal(out$pupil_grid$baseline_window_label, "0_to_200ms")
})

test_that("create_gazepoint_preprocessing_multiverse sanitises label prefixes", {
  out <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = 100,
    pupil_smoothing_window_samples = 5,
    pupil_baseline_windows = list(c(0, 200)),
    pupil_artifact_padding_ms = 0,
    include_pupil = TRUE,
    include_aoi = FALSE,
    label_prefix = "study 1 / main"
  )

  expect_true(all(grepl("^study_1_main_pupil_", out$pupil_grid$branch_id)))
  expect_true(all(grepl("^study_1_main_combined_", out$combined_grid$combined_branch_id)))
})

test_that("create_gazepoint_preprocessing_multiverse records settings", {
  out <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = c(75, 150),
    pupil_smoothing_window_samples = c(3, 5),
    pupil_baseline_windows = list(c(0, 200)),
    pupil_artifact_padding_ms = c(0, 50),
    aoi_denominators = c("valid", "aoi_only"),
    aoi_min_denominator_samples = c(1, 5),
    label_prefix = "settings_test"
  )

  expect_true("pupil_max_gap_ms" %in% out$settings$setting)
  expect_true("pupil_smoothing_window_samples" %in% out$settings$setting)
  expect_true("pupil_baseline_windows" %in% out$settings$setting)
  expect_true("aoi_denominators" %in% out$settings$setting)
  expect_true("label_prefix" %in% out$settings$setting)

  expect_equal(
    out$settings$value[out$settings$setting == "label_prefix"],
    "settings_test"
  )
})

test_that("create_gazepoint_preprocessing_multiverse checks invalid logical inputs", {
  expect_error(
    create_gazepoint_preprocessing_multiverse(
      include_pupil = FALSE,
      include_aoi = FALSE
    ),
    "At least one of `include_pupil` or `include_aoi` must be TRUE",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      include_pupil = NA
    ),
    "`include_pupil` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      include_aoi = NA
    ),
    "`include_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("create_gazepoint_preprocessing_multiverse checks invalid pupil inputs", {
  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_max_gap_ms = numeric()
    ),
    "`pupil_max_gap_ms` must be a non-empty finite numeric vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_max_gap_ms = c(100, NA)
    ),
    "`pupil_max_gap_ms` must be a non-empty finite numeric vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_max_gap_ms = 0
    ),
    "`pupil_max_gap_ms` must contain only positive values",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_smoothing_window_samples = 0
    ),
    "`pupil_smoothing_window_samples` must contain only positive values",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_artifact_padding_ms = -1
    ),
    "`pupil_artifact_padding_ms` must contain only non-negative values",
    fixed = TRUE
  )
})

test_that("create_gazepoint_preprocessing_multiverse checks invalid baseline windows", {
  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_baseline_windows = list()
    ),
    "`pupil_baseline_windows` must be a non-empty list of numeric vectors of length 2",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_baseline_windows = list(c(0, 100, 200))
    ),
    "`pupil_baseline_windows` must contain numeric vectors of length 2",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      pupil_baseline_windows = list(c(0, NA))
    ),
    "`pupil_baseline_windows` must contain numeric vectors of length 2",
    fixed = TRUE
  )
})

test_that("create_gazepoint_preprocessing_multiverse checks invalid AOI inputs", {
  expect_error(
    create_gazepoint_preprocessing_multiverse(
      aoi_denominators = character()
    ),
    "`aoi_denominators` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      aoi_denominators = c("valid", NA)
    ),
    "`aoi_denominators` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      aoi_denominators = c("valid", "")
    ),
    "`aoi_denominators` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      aoi_min_denominator_samples = 0
    ),
    "`aoi_min_denominator_samples` must contain only positive values",
    fixed = TRUE
  )
})

test_that("create_gazepoint_preprocessing_multiverse checks invalid label prefixes", {
  expect_error(
    create_gazepoint_preprocessing_multiverse(
      label_prefix = NA_character_
    ),
    "`label_prefix` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      label_prefix = c("a", "b")
    ),
    "`label_prefix` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_preprocessing_multiverse(
      label_prefix = ""
    ),
    "`label_prefix` must be a non-missing character scalar",
    fixed = TRUE
  )
})
