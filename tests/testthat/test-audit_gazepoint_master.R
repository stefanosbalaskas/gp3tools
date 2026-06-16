test_that("audit_gazepoint_master returns expected audit tables", {
  master <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    media_id = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time_ms = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 600, 200),
    raw_x = c(0.10, NA, 0.12, -0.01, 0.20, 0.30),
    raw_y = c(0.20, NA, 0.26, 0.20, 1.20, 0.40),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", NA, "offscreen", "offscreen", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L)
  )

  audit <- audit_gazepoint_master(master)

  expect_named(
    audit,
    c(
      "overview",
      "by_subject",
      "by_media",
      "by_subject_media",
      "aoi_states",
      "pupil_summary",
      "coordinate_summary"
    )
  )

  expect_s3_class(audit$overview, "tbl_df")
  expect_s3_class(audit$by_subject, "tbl_df")
  expect_s3_class(audit$by_media, "tbl_df")
  expect_s3_class(audit$by_subject_media, "tbl_df")
  expect_s3_class(audit$aoi_states, "tbl_df")
  expect_s3_class(audit$pupil_summary, "tbl_df")
  expect_s3_class(audit$coordinate_summary, "tbl_df")
})

test_that("audit_gazepoint_master computes overview correctly", {
  master <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    media_id = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time_ms = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 600, 200),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", NA, "offscreen", "offscreen", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L)
  )

  audit <- audit_gazepoint_master(master)
  overview <- audit$overview

  expect_equal(overview$n_rows, 6)
  expect_equal(overview$n_subjects, 2)
  expect_equal(overview$n_media, 2)
  expect_equal(overview$n_subject_media, 4)

  expect_equal(overview$time_min_ms, 0)
  expect_equal(overview$time_max_ms, 20)
  expect_equal(overview$time_span_ms, 20)

  expect_equal(overview$valid_sample_pct, 5 / 6 * 100, tolerance = 1e-8)
  expect_equal(overview$missing_gaze_pct, 1 / 6 * 100, tolerance = 1e-8)
  expect_equal(overview$missing_pupil_pct, 2 / 6 * 100, tolerance = 1e-8)
  expect_equal(overview$offscreen_gaze_pct, 2 / 6 * 100, tolerance = 1e-8)

  expect_equal(overview$n_missing_gaze, 1)
  expect_equal(overview$n_missing_pupil, 2)
  expect_equal(overview$n_offscreen_gaze, 2)

  expect_true(overview$has_pupil)
  expect_true(overview$has_aoi)

  expect_equal(overview$n_aoi_samples, 2)
  expect_equal(overview$n_missing_state, 1)
  expect_equal(overview$n_offscreen_state, 2)
})

test_that("audit_gazepoint_master computes grouped summaries correctly", {
  master <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    media_id = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time_ms = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 600, 200),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", NA, "offscreen", "offscreen", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L)
  )

  audit <- audit_gazepoint_master(master)

  expect_equal(nrow(audit$by_subject), 2)
  expect_equal(nrow(audit$by_media), 2)
  expect_equal(nrow(audit$by_subject_media), 4)

  p1 <- audit$by_subject[audit$by_subject$subject == "P1", ]
  p2 <- audit$by_subject[audit$by_subject$subject == "P2", ]

  expect_equal(p1$n_rows, 3)
  expect_equal(p1$missing_gaze_pct, 1 / 3 * 100, tolerance = 1e-8)
  expect_equal(p1$offscreen_gaze_pct, 0)

  expect_equal(p2$n_rows, 3)
  expect_equal(p2$missing_gaze_pct, 0)
  expect_equal(p2$offscreen_gaze_pct, 2 / 3 * 100, tolerance = 1e-8)

  expect_equal(p1$aoi_count_sum, 1)
  expect_equal(p2$aoi_count_sum, 1)
})

test_that("audit_gazepoint_master computes AOI states correctly", {
  master <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    media_id = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time_ms = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 600, 200),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", NA, "offscreen", "offscreen", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L)
  )

  audit <- audit_gazepoint_master(master)

  states <- stats::setNames(
    audit$aoi_states$n_samples,
    audit$aoi_states$aoi_state
  )

  expect_equal(unname(states["AOI 1"]), 1)
  expect_equal(unname(states["AOI 2"]), 1)
  expect_equal(unname(states["missing"]), 1)
  expect_equal(unname(states["offscreen"]), 2)
  expect_equal(unname(states["unclassified"]), 1)

  expect_equal(sum(audit$aoi_states$n_samples), 6)
  expect_equal(sum(audit$aoi_states$prop_samples), 100, tolerance = 1e-8)
})

test_that("audit_gazepoint_master computes pupil and coordinate summaries correctly", {
  master <- tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    media_id = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time_ms = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 600, 200),
    raw_x = c(0.10, NA, 0.12, -0.01, 0.20, 0.30),
    raw_y = c(0.20, NA, 0.26, 0.20, 1.20, 0.40),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", NA, "offscreen", "offscreen", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L)
  )

  audit <- audit_gazepoint_master(master)

  expect_equal(nrow(audit$pupil_summary), 4)

  p1_m1 <- audit$pupil_summary[
    audit$pupil_summary$subject == "P1" &
      audit$pupil_summary$media_id == "M1",
  ]

  expect_equal(p1_m1$n_rows, 2)
  expect_equal(p1_m1$n_pupil_samples, 1)
  expect_equal(p1_m1$mean_pupil, 3.5)

  coord <- audit$coordinate_summary

  expect_equal(coord$x_min, -10)
  expect_equal(coord$x_max, 300)
  expect_equal(coord$y_min, 100)
  expect_equal(coord$y_max, 600)
  expect_equal(coord$raw_x_min, -0.01)
  expect_equal(coord$raw_x_max, 0.30)
  expect_equal(coord$raw_y_min, 0.20)
  expect_equal(coord$raw_y_max, 1.20)
  expect_equal(coord$n_offscreen_gaze, 2)
})

test_that("audit_gazepoint_master errors for invalid input", {
  expect_error(
    audit_gazepoint_master("not a master table"),
    "`master` must be a data frame"
  )

  incomplete_master <- tibble::tibble(
    subject = "P1",
    media_id = "M1"
  )

  expect_error(
    audit_gazepoint_master(incomplete_master),
    "`master` is missing required columns"
  )
})
