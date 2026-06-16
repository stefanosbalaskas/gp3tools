make_test_eyetools_detection_data <- function() {
  set.seed(123)

  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 40),
    trial = rep(rep(1:2, each = 20), times = 2),
    time = rep(seq(0, 1900, by = 100), times = 4),
    condition = rep(c("A", "B"), each = 20, times = 2),
    stimulus = rep(c("stim_1", "stim_2"), each = 40),
    x = c(
      rep(500, 8), seq(500, 900, length.out = 4), rep(900, 8),
      rep(520, 8), seq(520, 920, length.out = 4), rep(920, 8),
      rep(540, 8), seq(540, 940, length.out = 4), rep(940, 8),
      rep(560, 8), seq(560, 960, length.out = 4), rep(960, 8)
    ) + stats::rnorm(80, 0, 3),
    y = c(
      rep(400, 8), seq(400, 700, length.out = 4), rep(700, 8),
      rep(420, 8), seq(420, 720, length.out = 4), rep(720, 8),
      rep(440, 8), seq(440, 740, length.out = 4), rep(740, 8),
      rep(460, 8), seq(460, 760, length.out = 4), rep(760, 8)
    ) + stats::rnorm(80, 0, 3)
  )
}

test_that("run_gazepoint_eyetools_fixation_detection skips cleanly when eyetools is unavailable", {
  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() FALSE
  )

  toy_data <- make_test_eyetools_detection_data()

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    condition_col = "condition",
    stimulus_col = "stimulus",
    method = "all",
    sample_rate = 10,
    progress = FALSE,
    name = "toy_eyetools"
  )

  expect_s3_class(out, "gp3_eyetools_fixation_detection")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "prepared_data",
      "fixation_dispersion",
      "fixation_vti",
      "saccades",
      "function_audit",
      "settings"
    )
  )

  expect_equal(out$overview$detector_status, "skipped_missing_package")
  expect_match(out$overview$message, "Optional package 'eyetools' is not installed", fixed = TRUE)

  expect_null(out$fixation_dispersion)
  expect_null(out$fixation_vti)
  expect_null(out$saccades)

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$prepared_data, "tbl_df")
  expect_s3_class(out$function_audit, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_true(all(c("pID", "trial", "time", "x", "y") %in% names(out$prepared_data)))
  expect_true(all(!out$function_audit$available))
})

test_that("run_gazepoint_eyetools_fixation_detection reports missing required eyetools functions", {
  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() TRUE,
    .gp3_eyetools_export_exists = function(function_name) FALSE
  )

  toy_data <- make_test_eyetools_detection_data()

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    method = "dispersion",
    progress = FALSE
  )

  expect_s3_class(out, "gp3_eyetools_fixation_detection")
  expect_equal(out$overview$detector_status, "skipped_missing_eyetools_functions")
  expect_match(out$overview$message, "missing required functions", fixed = TRUE)

  expect_null(out$fixation_dispersion)
  expect_null(out$fixation_vti)
  expect_null(out$saccades)
})

test_that("run_gazepoint_eyetools_fixation_detection runs all detector branches with mocked eyetools functions", {
  fake_dispersion <- function(data, min_dur, disp_tol, NA_tol, progress) {
    tibble::tibble(
      pID = c("S1", "S2"),
      trial = c("1", "1"),
      fix_n = c(1, 1),
      start = c(0, 0),
      end = c(500, 500),
      duration = c(500, 500),
      x = c(500, 540),
      y = c(400, 440),
      prop_NA = c(0, 0),
      min_dur = min_dur,
      disp_tol = disp_tol
    )
  }

  fake_vti <- function(data, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, smooth, progress) {
    tibble::tibble(
      pID = c("S1", "S2"),
      trialNumber = c("1", "1"),
      fix_n = c(1, 1),
      start = c(0, 0),
      end = c(600, 600),
      duration = c(600, 600),
      x = c(501, 541),
      y = c(401, 441),
      min_dur = min_dur,
      disp_tol = disp_tol
    )
  }

  fake_saccade <- function(data, sample_rate, threshold, min_dur) {
    tibble::tibble(
      pID = c("S1", "S2"),
      trial = c("1", "1"),
      sac_n = c(1, 1),
      start = c(700, 700),
      end = c(900, 900),
      duration = c(200, 200),
      origin_x = c(500, 540),
      origin_y = c(400, 440),
      terminal_x = c(900, 940),
      terminal_y = c(700, 740),
      mean_velocity = c(120, 125),
      peak_velocity = c(150, 160)
    )
  }

  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() TRUE,
    .gp3_eyetools_export_exists = function(function_name) TRUE,
    .gp3_eyetools_get_export = function(function_name) {
      if (identical(function_name, "fixation_dispersion")) {
        return(fake_dispersion)
      }

      if (identical(function_name, "fixation_VTI")) {
        return(fake_vti)
      }

      if (identical(function_name, "saccade_VTI")) {
        return(fake_saccade)
      }

      stop("Unexpected eyetools function requested.", call. = FALSE)
    }
  )

  toy_data <- make_test_eyetools_detection_data()

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    condition_col = "condition",
    stimulus_col = "stimulus",
    method = "all",
    sample_rate = 10,
    threshold = 80,
    min_dur = 200,
    min_dur_sac = 20,
    disp_tol = 80,
    progress = FALSE,
    name = "mock_eyetools"
  )

  expect_s3_class(out, "gp3_eyetools_fixation_detection")
  expect_equal(out$overview$object_name, "mock_eyetools")
  expect_equal(out$overview$detector_status, "complete")

  expect_s3_class(out$fixation_dispersion, "tbl_df")
  expect_s3_class(out$fixation_vti, "tbl_df")
  expect_s3_class(out$saccades, "tbl_df")

  expect_equal(out$overview$n_fixations_dispersion, 2)
  expect_equal(out$overview$n_fixations_vti, 2)
  expect_equal(out$overview$n_saccades, 2)

  expect_true(all(out$function_audit$available))
})

test_that("run_gazepoint_eyetools_fixation_detection records partial completion cleanly", {
  fake_dispersion <- function(data, min_dur, disp_tol, NA_tol, progress) {
    tibble::tibble(
      pID = "S1",
      trial = "1",
      fix_n = 1,
      start = 0,
      end = 500,
      duration = 500,
      x = 500,
      y = 400,
      prop_NA = 0,
      min_dur = min_dur,
      disp_tol = disp_tol
    )
  }

  fake_vti_error <- function(...) {
    stop("mock fixation_VTI error", call. = FALSE)
  }

  fake_saccade_error <- function(...) {
    stop("mock saccade_VTI error", call. = FALSE)
  }

  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() TRUE,
    .gp3_eyetools_export_exists = function(function_name) TRUE,
    .gp3_eyetools_get_export = function(function_name) {
      if (identical(function_name, "fixation_dispersion")) {
        return(fake_dispersion)
      }

      if (identical(function_name, "fixation_VTI")) {
        return(fake_vti_error)
      }

      if (identical(function_name, "saccade_VTI")) {
        return(fake_saccade_error)
      }

      stop("Unexpected eyetools function requested.", call. = FALSE)
    }
  )

  toy_data <- make_test_eyetools_detection_data()

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    method = "all",
    sample_rate = 10,
    progress = FALSE
  )

  expect_s3_class(out, "gp3_eyetools_fixation_detection")
  expect_equal(out$overview$detector_status, "partial_complete")
  expect_match(out$overview$message, "error_fixation_vti", fixed = TRUE)
  expect_match(out$overview$message, "error_saccade_vti", fixed = TRUE)

  expect_s3_class(out$fixation_dispersion, "tbl_df")
  expect_null(out$fixation_vti)
  expect_null(out$saccades)
})

test_that("run_gazepoint_eyetools_fixation_detection runs real fixation_dispersion when eyetools is installed", {
  testthat::skip_if_not_installed("eyetools")

  toy_data <- make_test_eyetools_detection_data()

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    condition_col = "condition",
    method = "dispersion",
    min_dur = 200,
    disp_tol = 80,
    progress = FALSE,
    name = "real_dispersion"
  )

  expect_s3_class(out, "gp3_eyetools_fixation_detection")
  expect_equal(out$overview$detector_status, "complete")
  expect_s3_class(out$fixation_dispersion, "tbl_df")
  expect_null(out$fixation_vti)
  expect_null(out$saccades)

  expect_true(is.na(out$overview$n_fixations_vti))
  expect_true(is.na(out$overview$n_saccades))
  expect_true(out$overview$n_fixations_dispersion >= 1)
})

test_that("run_gazepoint_eyetools_fixation_detection auto-detects common columns", {
  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() FALSE
  )

  toy_data <- make_test_eyetools_detection_data()

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    method = "dispersion",
    progress = FALSE
  )

  expect_s3_class(out, "gp3_eyetools_fixation_detection")
  expect_equal(out$overview$detector_status, "skipped_missing_package")

  expect_equal(out$settings$value[out$settings$setting == "participant_col"], "subject")
  expect_equal(out$settings$value[out$settings$setting == "trial_col"], "trial")
  expect_equal(out$settings$value[out$settings$setting == "time_col"], "time")
  expect_equal(out$settings$value[out$settings$setting == "x_col"], "x")
  expect_equal(out$settings$value[out$settings$setting == "y_col"], "y")
  expect_equal(out$settings$value[out$settings$setting == "condition_col"], "condition")
  expect_equal(out$settings$value[out$settings$setting == "stimulus_col"], "stimulus")
})

test_that("run_gazepoint_eyetools_fixation_detection drops missing gaze rows when requested", {
  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() FALSE
  )

  toy_data <- make_test_eyetools_detection_data()
  toy_data$x[1:5] <- NA_real_

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    method = "dispersion",
    drop_missing = TRUE,
    progress = FALSE
  )

  expect_equal(out$overview$n_input_rows_prepared, nrow(toy_data))
  expect_equal(out$overview$n_rows_used, nrow(toy_data) - 5)
  expect_equal(out$overview$n_rows_dropped, 5)
  expect_false(anyNA(out$prepared_data$x))
})

test_that("run_gazepoint_eyetools_fixation_detection can retain missing gaze rows", {
  testthat::local_mocked_bindings(
    .gp3_eyetools_namespace_available = function() FALSE
  )

  toy_data <- make_test_eyetools_detection_data()
  toy_data$x[1:5] <- NA_real_

  out <- run_gazepoint_eyetools_fixation_detection(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    x_col = "x",
    y_col = "y",
    method = "dispersion",
    drop_missing = FALSE,
    progress = FALSE
  )

  expect_equal(out$overview$n_input_rows_prepared, nrow(toy_data))
  expect_equal(out$overview$n_rows_used, nrow(toy_data))
  expect_equal(out$overview$n_rows_dropped, 0)
  expect_true(anyNA(out$prepared_data$x))
})

test_that("run_gazepoint_eyetools_fixation_detection checks invalid inputs", {
  toy_data <- make_test_eyetools_detection_data()

  expect_error(
    run_gazepoint_eyetools_fixation_detection(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      trial_col = "bad_trial"
    ),
    "`trial_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      x_col = "bad_x"
    ),
    "`x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      y_col = "bad_y"
    ),
    "`y_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      threshold = 0
    ),
    "`threshold` must be a finite positive number",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      min_dur = 0
    ),
    "`min_dur` must be a finite positive number",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      NA_tol = 1.1
    ),
    "`NA_tol` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      smooth = NA
    ),
    "`smooth` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      drop_missing = NA
    ),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      progress = NA
    ),
    "`progress` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("run_gazepoint_eyetools_fixation_detection errors if no valid rows remain", {
  toy_data <- make_test_eyetools_detection_data()
  toy_data$x <- NA_real_

  expect_error(
    run_gazepoint_eyetools_fixation_detection(
      toy_data,
      participant_col = "subject",
      trial_col = "trial",
      time_col = "time",
      x_col = "x",
      y_col = "y",
      drop_missing = TRUE
    ),
    "No valid rows remain after preparing eyetools input",
    fixed = TRUE
  )
})
