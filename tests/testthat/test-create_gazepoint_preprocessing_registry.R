test_that("create_gazepoint_preprocessing_registry returns expected structure", {
  registry <- create_gazepoint_preprocessing_registry()

  expect_s3_class(registry, "tbl_df")
  expect_equal(nrow(registry), 15)

  expected_cols <- c(
    "parameter",
    "value",
    "unit",
    "category",
    "description"
  )

  expect_true(all(expected_cols %in% names(registry)))

  expected_parameters <- c(
    "blink_padding_pre_ms",
    "blink_padding_post_ms",
    "max_interpolation_gap_ms",
    "smoothing_window_ms",
    "baseline_start_ms",
    "baseline_end_ms",
    "pupil_physiological_min",
    "pupil_physiological_max",
    "pupil_speed_mad_k",
    "binocular_mad_k",
    "baseline_missing_prop_threshold",
    "baseline_interpolated_prop_threshold",
    "baseline_artifact_prop_threshold",
    "overlap_trial_duration_ms",
    "overlap_event_gap_ms"
  )

  expect_equal(registry$parameter, expected_parameters)
  expect_true(is.numeric(registry$value))
  expect_false(any(is.na(registry$value)))
})

test_that("create_gazepoint_preprocessing_registry stores default values", {
  registry <- create_gazepoint_preprocessing_registry()

  get_value <- function(parameter_name) {
    registry$value[registry$parameter == parameter_name]
  }

  expect_equal(get_value("blink_padding_pre_ms"), 100)
  expect_equal(get_value("blink_padding_post_ms"), 100)
  expect_equal(get_value("max_interpolation_gap_ms"), 150)
  expect_equal(get_value("smoothing_window_ms"), 50)
  expect_equal(get_value("baseline_start_ms"), -200)
  expect_equal(get_value("baseline_end_ms"), 0)
  expect_equal(get_value("pupil_physiological_min"), 1)
  expect_equal(get_value("pupil_physiological_max"), 9)
  expect_equal(get_value("pupil_speed_mad_k"), 6)
  expect_equal(get_value("binocular_mad_k"), 6)
})

test_that("create_gazepoint_preprocessing_registry accepts custom values", {
  registry <- create_gazepoint_preprocessing_registry(
    blink_padding_pre_ms = 80,
    blink_padding_post_ms = 120,
    max_interpolation_gap_ms = 200,
    smoothing_window_ms = 75,
    baseline_start_ms = -300,
    baseline_end_ms = -50,
    pupil_physiological_min = 1.5,
    pupil_physiological_max = 8.5,
    pupil_speed_mad_k = 5,
    binocular_mad_k = 7,
    baseline_missing_prop_threshold = 0.25,
    baseline_interpolated_prop_threshold = 0.20,
    baseline_artifact_prop_threshold = 0.15,
    overlap_trial_duration_ms = 2500,
    overlap_event_gap_ms = 800
  )

  get_value <- function(parameter_name) {
    registry$value[registry$parameter == parameter_name]
  }

  expect_equal(get_value("blink_padding_pre_ms"), 80)
  expect_equal(get_value("blink_padding_post_ms"), 120)
  expect_equal(get_value("max_interpolation_gap_ms"), 200)
  expect_equal(get_value("smoothing_window_ms"), 75)
  expect_equal(get_value("baseline_start_ms"), -300)
  expect_equal(get_value("baseline_end_ms"), -50)
  expect_equal(get_value("pupil_physiological_min"), 1.5)
  expect_equal(get_value("pupil_physiological_max"), 8.5)
  expect_equal(get_value("pupil_speed_mad_k"), 5)
  expect_equal(get_value("binocular_mad_k"), 7)
  expect_equal(get_value("baseline_missing_prop_threshold"), 0.25)
  expect_equal(get_value("baseline_interpolated_prop_threshold"), 0.20)
  expect_equal(get_value("baseline_artifact_prop_threshold"), 0.15)
  expect_equal(get_value("overlap_trial_duration_ms"), 2500)
  expect_equal(get_value("overlap_event_gap_ms"), 800)
})

test_that("create_gazepoint_preprocessing_registry validates numeric scalar arguments", {
  expect_error(
    create_gazepoint_preprocessing_registry(blink_padding_pre_ms = c(1, 2)),
    "`blink_padding_pre_ms` must be a single non-missing numeric value"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(max_interpolation_gap_ms = NA_real_),
    "`max_interpolation_gap_ms` must be a single non-missing numeric value"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(smoothing_window_ms = "bad"),
    "`smoothing_window_ms` must be a single non-missing numeric value"
  )
})

test_that("create_gazepoint_preprocessing_registry validates non-negative parameters", {
  expect_error(
    create_gazepoint_preprocessing_registry(blink_padding_pre_ms = -1),
    "must be greater than or equal to 0"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(max_interpolation_gap_ms = -1),
    "must be greater than or equal to 0"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(baseline_missing_prop_threshold = -0.1),
    "must be greater than or equal to 0"
  )
})

test_that("create_gazepoint_preprocessing_registry validates proportion thresholds", {
  expect_error(
    create_gazepoint_preprocessing_registry(baseline_missing_prop_threshold = 1.1),
    "must be between 0 and 1"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(baseline_interpolated_prop_threshold = 1.1),
    "must be between 0 and 1"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(baseline_artifact_prop_threshold = 1.1),
    "must be between 0 and 1"
  )
})

test_that("create_gazepoint_preprocessing_registry validates baseline and pupil ranges", {
  expect_error(
    create_gazepoint_preprocessing_registry(
      baseline_start_ms = 0,
      baseline_end_ms = -200
    ),
    "`baseline_end_ms` must be greater than or equal to `baseline_start_ms`"
  )

  expect_error(
    create_gazepoint_preprocessing_registry(
      pupil_physiological_min = 9,
      pupil_physiological_max = 1
    ),
    "`pupil_physiological_max` must be greater than `pupil_physiological_min`"
  )
})
