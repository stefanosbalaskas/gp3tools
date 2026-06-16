make_test_gazer_crosscheck_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 8),
    trial = rep(rep(1:2, each = 4), times = 2),
    time = rep(seq(0, 300, by = 100), times = 4),
    condition = rep(c("A", "B"), each = 4, times = 2),
    pupil_clean = c(
      1000, 1010, NA, 1030,
      1040, NA, 1060, 1070,
      1100, 1110, NA, 1130,
      1140, NA, 1160, 1170
    ),
    blink = is.na(pupil_clean),
    message = rep(c("start", NA, "target", "end"), times = 4)
  )
}

test_that("run_gazepoint_gazer_crosscheck prepares data and skips cleanly when gazer is unavailable", {
  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() FALSE
  )

  toy_data <- make_test_gazer_crosscheck_data()

  out <- run_gazepoint_gazer_crosscheck(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean",
    condition_col = "condition",
    message_col = "message",
    blink_col = "blink",
    name = "toy_gazer_crosscheck"
  )

  expect_s3_class(out, "gp3_gazer_crosscheck")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "prepared_data",
      "extended_data",
      "processed_data",
      "baseline_data",
      "downsampled_data",
      "function_audit",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$prepared_data, "tbl_df")
  expect_s3_class(out$function_audit, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_null(out$extended_data)
  expect_null(out$processed_data)
  expect_null(out$baseline_data)
  expect_null(out$downsampled_data)

  expect_equal(out$overview$object_name, "toy_gazer_crosscheck")
  expect_equal(out$overview$n_rows_prepared, nrow(toy_data))
  expect_equal(out$overview$n_subjects, 2)
  expect_equal(out$overview$n_trials, 4)
  expect_equal(out$overview$n_conditions, 2)
  expect_equal(out$overview$crosscheck_status, "skipped_missing_package")
  expect_match(out$overview$message, "Optional package 'gazer' is not installed", fixed = TRUE)

  expect_equal(nrow(out$function_audit), 5)
  expect_true(all(!out$function_audit$available))
  expect_true(all(out$function_audit$required[out$function_audit$function_name %in% c(
    "extend_blinks",
    "smooth_interpolate_pupil"
  )]))

  expect_equal(names(out$prepared_data), c(
    "subject",
    "trial",
    "time",
    "pupil",
    "condition",
    "message",
    "blink"
  ))

  expect_equal(sum(is.na(out$prepared_data$pupil)), 4)
  expect_equal(sum(out$prepared_data$blink), 4)
})

test_that("run_gazepoint_gazer_crosscheck auto-detects common columns", {
  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() FALSE
  )

  toy_data <- make_test_gazer_crosscheck_data()

  out <- run_gazepoint_gazer_crosscheck(toy_data)

  expect_s3_class(out, "gp3_gazer_crosscheck")
  expect_equal(out$overview$crosscheck_status, "skipped_missing_package")

  expect_equal(out$settings$value[out$settings$setting == "participant_col"], "subject")
  expect_equal(out$settings$value[out$settings$setting == "trial_col"], "trial")
  expect_equal(out$settings$value[out$settings$setting == "time_col"], "time")
  expect_equal(out$settings$value[out$settings$setting == "pupil_col"], "pupil_clean")
  expect_equal(out$settings$value[out$settings$setting == "condition_col"], "condition")
  expect_equal(out$settings$value[out$settings$setting == "message_col"], "message")
  expect_equal(out$settings$value[out$settings$setting == "blink_col"], "blink")
})

test_that("run_gazepoint_gazer_crosscheck supports all_data condition fallback", {
  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() FALSE
  )

  toy_data <- make_test_gazer_crosscheck_data() |>
    dplyr::select(-condition)

  out <- run_gazepoint_gazer_crosscheck(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean"
  )

  expect_equal(out$overview$n_conditions, 1)
  expect_equal(unique(out$prepared_data$condition), "all_data")
  expect_true(is.na(out$settings$value[out$settings$setting == "condition_col"]))
})

test_that("run_gazepoint_gazer_crosscheck can simulate successful external gazer execution", {
  fake_extend_blinks <- function(pupil, fillback, fillforward, hz) {
    pupil
  }

  fake_smooth_interpolate <- function(
    data,
    pupil,
    extendpupil,
    extendblinks,
    step.first,
    maxgap,
    type,
    hz,
    n
  ) {
    data |>
      dplyr::mutate(
        interp = dplyr::coalesce(.data[[pupil]], mean(.data[[pupil]], na.rm = TRUE)),
        gazer_processed = TRUE
      )
  }

  fake_baseline <- function(data, pupil_colnames, baseline_window) {
    data |>
      dplyr::mutate(
        baseline_corrected = .data[[pupil_colnames]] - mean(.data[[pupil_colnames]], na.rm = TRUE)
      )
  }

  fake_downsample <- function(data, bin.length, timevar, aggvars, type) {
    data |>
      dplyr::mutate(timebins = floor(.data[[timevar]] / bin.length) * bin.length) |>
      dplyr::group_by(.data$subject, .data$condition, .data$timebins) |>
      dplyr::summarise(
        pupil_downsampled = mean(.data$interp, na.rm = TRUE),
        .groups = "drop"
      )
  }

  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() TRUE,
    .gp3_gazer_export_exists = function(function_name) {
      function_name %in% c(
        "extend_blinks",
        "smooth_interpolate_pupil",
        "baseline_correction_pupil",
        "baseline_correction_pupil_msg",
        "downsample_gaze"
      )
    },
    .gp3_gazer_get_export = function(function_name) {
      switch(
        function_name,
        extend_blinks = fake_extend_blinks,
        smooth_interpolate_pupil = fake_smooth_interpolate,
        baseline_correction_pupil = fake_baseline,
        downsample_gaze = fake_downsample,
        baseline_correction_pupil_msg = function(...) list(),
        stop("Unexpected function requested.", call. = FALSE)
      )
    }
  )

  toy_data <- make_test_gazer_crosscheck_data()

  out <- run_gazepoint_gazer_crosscheck(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean",
    condition_col = "condition",
    message_col = "message",
    blink_col = "blink",
    baseline_window = c(0, 200),
    bin_length = 100,
    name = "mocked_gazer_crosscheck"
  )

  expect_s3_class(out, "gp3_gazer_crosscheck")
  expect_equal(out$overview$crosscheck_status, "complete")
  expect_match(out$overview$message, "baseline_status=baseline_window_applied", fixed = TRUE)
  expect_match(out$overview$message, "downsample_status=downsample_applied", fixed = TRUE)

  expect_s3_class(out$extended_data, "tbl_df")
  expect_s3_class(out$processed_data, "tbl_df")
  expect_s3_class(out$baseline_data, "tbl_df")
  expect_s3_class(out$downsampled_data, "tbl_df")

  expect_true("gazer_extendpupil" %in% names(out$extended_data))
  expect_true("interp" %in% names(out$processed_data))
  expect_true("baseline_corrected" %in% names(out$baseline_data))
  expect_true("pupil_downsampled" %in% names(out$downsampled_data))

  expect_true(all(out$function_audit$available))
  expect_true(all(out$function_audit$available[out$function_audit$required]))
})

test_that("run_gazepoint_gazer_crosscheck reports missing required gazer functions", {
  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() TRUE,
    .gp3_gazer_export_exists = function(function_name) {
      function_name == "extend_blinks"
    }
  )

  toy_data <- make_test_gazer_crosscheck_data()

  out <- run_gazepoint_gazer_crosscheck(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean"
  )

  expect_equal(out$overview$crosscheck_status, "skipped_missing_gazer_functions")
  expect_match(out$overview$message, "smooth_interpolate_pupil", fixed = TRUE)
  expect_null(out$extended_data)
  expect_null(out$processed_data)
})

test_that("run_gazepoint_gazer_crosscheck reports extend_blinks errors", {
  fake_extend_blinks <- function(pupil, fillback, fillforward, hz) {
    stop("mock extend error", call. = FALSE)
  }

  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() TRUE,
    .gp3_gazer_export_exists = function(function_name) {
      function_name %in% c("extend_blinks", "smooth_interpolate_pupil")
    },
    .gp3_gazer_get_export = function(function_name) {
      switch(
        function_name,
        extend_blinks = fake_extend_blinks,
        smooth_interpolate_pupil = function(...) list(),
        stop("Unexpected function requested.", call. = FALSE)
      )
    }
  )

  toy_data <- make_test_gazer_crosscheck_data()

  out <- run_gazepoint_gazer_crosscheck(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean"
  )

  expect_equal(out$overview$crosscheck_status, "error_extend_blinks")
  expect_match(out$overview$message, "mock extend error", fixed = TRUE)
  expect_null(out$extended_data)
  expect_null(out$processed_data)
})

test_that("run_gazepoint_gazer_crosscheck reports smooth interpolation errors", {
  fake_extend_blinks <- function(pupil, fillback, fillforward, hz) {
    pupil
  }

  fake_smooth_interpolate <- function(...) {
    stop("mock smooth error", call. = FALSE)
  }

  testthat::local_mocked_bindings(
    .gp3_gazer_namespace_available = function() TRUE,
    .gp3_gazer_export_exists = function(function_name) {
      function_name %in% c("extend_blinks", "smooth_interpolate_pupil")
    },
    .gp3_gazer_get_export = function(function_name) {
      switch(
        function_name,
        extend_blinks = fake_extend_blinks,
        smooth_interpolate_pupil = fake_smooth_interpolate,
        stop("Unexpected function requested.", call. = FALSE)
      )
    }
  )

  toy_data <- make_test_gazer_crosscheck_data()

  out <- run_gazepoint_gazer_crosscheck(
    toy_data,
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean"
  )

  expect_equal(out$overview$crosscheck_status, "error_smooth_interpolate")
  expect_match(out$overview$message, "mock smooth error", fixed = TRUE)
  expect_s3_class(out$extended_data, "tbl_df")
  expect_null(out$processed_data)
})

test_that("run_gazepoint_gazer_crosscheck checks invalid inputs", {
  toy_data <- make_test_gazer_crosscheck_data()

  expect_error(
    run_gazepoint_gazer_crosscheck(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      trial_col = "bad_trial"
    ),
    "`trial_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      pupil_col = "bad_pupil"
    ),
    "`pupil_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      hz = 0
    ),
    "`hz` must be a finite positive number",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      fillback = -1
    ),
    "`fillback` must be a finite non-negative number",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      smooth_n = 0
    ),
    "`smooth_n` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      baseline_window = c(0, 100, 200)
    ),
    "`baseline_window` must be a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_gazer_crosscheck(
      toy_data,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
