make_validation_master <- function() {
  tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    MEDIA_ID = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 600, 200),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", "non_aoi", "offscreen", "offscreen", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L),
    screen_width_px = rep(1000, 6),
    screen_height_px = rep(500, 6)
  )
}

test_that("validate_gazepoint_master returns expected validation structure", {
  master <- make_validation_master()

  validation <- validate_gazepoint_master(
    master,
    max_offscreen_gaze_pct = 40
  )

  expect_named(
    validation,
    c(
      "summary",
      "checks",
      "failed_checks",
      "warning_checks",
      "column_map"
    )
  )

  expect_s3_class(validation$summary, "tbl_df")
  expect_s3_class(validation$checks, "tbl_df")
  expect_s3_class(validation$failed_checks, "tbl_df")
  expect_s3_class(validation$warning_checks, "tbl_df")
  expect_s3_class(validation$column_map, "tbl_df")

  expect_equal(nrow(validation$checks), 15)
  expect_equal(validation$checks$check_id, sprintf("C%03d", 1:15))
})

test_that("validate_gazepoint_master passes a valid master table", {
  master <- make_validation_master()

  validation <- validate_gazepoint_master(
    master,
    max_offscreen_gaze_pct = 40
  )

  expect_true(validation$summary$validation_passed)
  expect_equal(validation$summary$n_checks, 15)
  expect_equal(validation$summary$n_passed, 15)
  expect_equal(validation$summary$n_failed, 0)
  expect_equal(validation$summary$n_warnings, 0)

  expect_equal(nrow(validation$failed_checks), 0)
  expect_equal(nrow(validation$warning_checks), 0)

  expect_equal(validation$summary$n_rows, 6)
  expect_equal(validation$summary$n_subjects, 2)
  expect_equal(validation$summary$n_media, 2)

  expect_equal(validation$summary$valid_sample_pct, 5 / 6 * 100, tolerance = 1e-8)
  expect_equal(validation$summary$missing_gaze_pct, 1 / 6 * 100, tolerance = 1e-8)
  expect_equal(validation$summary$missing_pupil_pct, 2 / 6 * 100, tolerance = 1e-8)
  expect_equal(validation$summary$offscreen_gaze_pct, 2 / 6 * 100, tolerance = 1e-8)

  expect_true(validation$summary$has_pupil)
  expect_true(validation$summary$has_aoi)
})

test_that("validate_gazepoint_master detects column roles", {
  master <- make_validation_master()

  validation <- validate_gazepoint_master(master)

  column_lookup <- stats::setNames(
    validation$column_map$column,
    validation$column_map$role
  )

  expect_equal(unname(column_lookup["subject"]), "subject")
  expect_equal(unname(column_lookup["media_id"]), "MEDIA_ID")
  expect_equal(unname(column_lookup["time"]), "time")
  expect_equal(unname(column_lookup["pupil"]), "mean_pupil")
  expect_equal(unname(column_lookup["aoi_current"]), "aoi_current")
  expect_equal(unname(column_lookup["aoi_count"]), "aoi_count")
})

test_that("validate_gazepoint_master detects AOI count mismatches", {
  master <- make_validation_master()

  master$aoi_count[master$aoi_current == "non_aoi"] <- 1L

  validation <- validate_gazepoint_master(master)

  expect_false(validation$summary$validation_passed)
  expect_true("C014" %in% validation$failed_checks$check_id)
  expect_equal(
    validation$failed_checks$check_name[
      validation$failed_checks$check_id == "C014"
    ],
    "AOI count matches AOI state"
  )
})

test_that("validate_gazepoint_master detects quality threshold failures", {
  master <- make_validation_master()

  validation <- validate_gazepoint_master(
    master,
    min_valid_sample_pct = 99,
    max_missing_gaze_pct = 10
  )

  expect_false(validation$summary$validation_passed)
  expect_true("C008" %in% validation$failed_checks$check_id)
  expect_true("C009" %in% validation$failed_checks$check_id)
})

test_that("validate_gazepoint_master reports optional pupil and AOI warnings", {
  master <- make_validation_master()

  master$mean_pupil <- NA_real_
  master$missing_pupil <- FALSE
  master$aoi_current <- "non_aoi"
  master$aoi_count <- 0L

  validation <- validate_gazepoint_master(master)

  expect_true(validation$summary$validation_passed)
  expect_equal(validation$summary$n_failed, 0)
  expect_true("C010" %in% validation$warning_checks$check_id)
  expect_true("C013" %in% validation$warning_checks$check_id)
})

test_that("validate_gazepoint_master can require pupil and AOI data", {
  master <- make_validation_master()

  master$mean_pupil <- NA_real_
  master$missing_pupil <- FALSE
  master$aoi_current <- "non_aoi"
  master$aoi_count <- 0L

  validation <- validate_gazepoint_master(
    master,
    require_pupil = TRUE,
    require_aoi = TRUE
  )

  expect_false(validation$summary$validation_passed)
  expect_true("C010" %in% validation$failed_checks$check_id)
  expect_true("C013" %in% validation$failed_checks$check_id)
})

test_that("validate_gazepoint_master can abort on failed validation", {
  master <- make_validation_master()

  master$aoi_count[master$aoi_current == "non_aoi"] <- 1L

  expect_error(
    validate_gazepoint_master(master, fail_on_error = TRUE),
    "`master` failed validation"
  )
})

test_that("validate_gazepoint_master handles missing required roles", {
  incomplete_master <- tibble::tibble(
    subject = "P1",
    MEDIA_ID = "M1"
  )

  validation <- validate_gazepoint_master(incomplete_master)

  expect_false(validation$summary$validation_passed)
  expect_true("C002" %in% validation$failed_checks$check_id)
})

test_that("validate_gazepoint_master errors for invalid arguments", {
  master <- make_validation_master()

  expect_error(
    validate_gazepoint_master("not a data frame"),
    "`master` must be a data frame"
  )

  expect_error(
    validate_gazepoint_master(master, min_valid_sample_pct = c(75, 80)),
    "`min_valid_sample_pct` must be a single numeric value"
  )

  expect_error(
    validate_gazepoint_master(master, max_missing_gaze_pct = c(20, 25)),
    "`max_missing_gaze_pct` must be a single numeric value"
  )

  expect_error(
    validate_gazepoint_master(master, max_missing_pupil_pct = c(40, 50)),
    "`max_missing_pupil_pct` must be a single numeric value"
  )

  expect_error(
    validate_gazepoint_master(master, max_offscreen_gaze_pct = c(20, 25)),
    "`max_offscreen_gaze_pct` must be a single numeric value"
  )

  expect_error(
    validate_gazepoint_master(master, require_pupil = c(TRUE, FALSE)),
    "`require_pupil` must be `TRUE` or `FALSE`"
  )

  expect_error(
    validate_gazepoint_master(master, require_aoi = c(TRUE, FALSE)),
    "`require_aoi` must be `TRUE` or `FALSE`"
  )

  expect_error(
    validate_gazepoint_master(master, fail_on_error = c(TRUE, FALSE)),
    "`fail_on_error` must be `TRUE` or `FALSE`"
  )
})
