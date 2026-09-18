test_that("Gazepoint quality adapter resolves native GP3 columns conservatively", {
  d <- data.frame(
    USER = rep("P01", 4),
    MEDIA_ID = rep("T01", 4),
    TIME = c(0, .01, .02, .03),
    BPOGX = c(.50, .51, .50, .51),
    BPOGY = c(.50, .50, .50, .50),
    BPOGV = 1,
    target_x = .50,
    target_y = .50
  )

  prepared <- gp3tools:::.gp3_quality_prepare(d)
  expect_equal(prepared$source_columns$x, "BPOGX")
  expect_equal(prepared$source_columns$y, "BPOGY")
  expect_equal(prepared$source_columns$time, "TIME")
  expect_equal(prepared$coordinate_unit, "normalized")
  expect_equal(prepared$time_unit, "s")
})

test_that("generic gaze columns never trigger silent unit guessing", {
  d <- data.frame(
    timestamp_ms = c(0, 10),
    gaze_x = c(.5, .6),
    gaze_y = c(.5, .6)
  )
  expect_error(
    gp3tools:::.gp3_quality_prepare(d),
    "Coordinate unit cannot be inferred safely"
  )
  prepared <- gp3tools:::.gp3_quality_prepare(
    d,
    coordinate_unit = "normalized"
  )
  expect_equal(prepared$coordinate_unit, "normalized")
  expect_equal(prepared$time_unit, "ms")
})

test_that("mixed declared units fail explicitly", {
  d <- data.frame(
    timestamp_ms = c(0, 10),
    gaze_x = c(.5, .6),
    gaze_y = c(.5, .6),
    coordinate_unit = c("normalized", "pixels")
  )
  expect_error(
    gp3tools:::.gp3_quality_prepare(d),
    "mixed units"
  )
})

test_that("aggregation levels preserve Gazepoint identifiers", {
  d <- data.frame(
    participant_id = rep(c("P01", "P02"), each = 2),
    session_id = "S01",
    trial_id = rep(c("T01", "T02"), each = 2),
    timestamp_ms = rep(c(0, 10), 2),
    gaze_x = .5,
    gaze_y = .5,
    coordinate_unit = "normalized"
  )
  prepared <- gp3tools:::.gp3_quality_prepare(d)
  sample_groups <- gp3tools:::.gp3_quality_by(prepared, "sample")
  expect_true(".gp3_quality_sample" %in% sample_groups)

  groups <- gp3tools:::.gp3_quality_by(prepared, "trial")
  expect_equal(
    groups,
    c(".gp3_quality_participant", ".gp3_quality_session", ".gp3_quality_trial")
  )
  expect_error(
    gp3tools:::.gp3_quality_by(
      gp3tools:::.gp3_quality_prepare(
        d[c("timestamp_ms", "gaze_x", "gaze_y", "coordinate_unit")]
      ),
      "participant"
    ),
    "aggregation level is unavailable"
  )
})

test_that("GP3 geometry is used only when complete", {
  d <- data.frame(
    screen_width_px = 1920,
    screen_height_px = 1080,
    screen_width_cm = 53,
    screen_height_cm = 29.8,
    viewing_distance_cm = 60
  )
  g <- gp3tools:::.gp3_quality_geometry(d, NULL)
  expect_equal(g$screen_width_px, 1920)
  expect_equal(g$viewing_distance_cm, 60)

  incomplete <- d[c("screen_width_px", "screen_height_px")]
  expect_null(gp3tools:::.gp3_quality_geometry(incomplete, NULL))
})

test_that("Gazepoint quality wrappers delegate to eyeprocess when available", {
  testthat::skip_if_not_installed("eyeprocess")
  required <- c(
    "create_gaze_quality_report",
    "summarise_spatial_quality",
    "plot_gaze_quality_dashboard",
    "report_gaze_quality"
  )
  testthat::skip_if(!all(required %in% getNamespaceExports("eyeprocess")))

  d <- data.frame(
    participant_id = "P01",
    trial_id = rep(c("T01", "T02"), each = 4),
    MSTIMER = rep(c(0, 10, 20, 30), 2),
    BPOGX = c(.50, .51, .50, .51, .60, .61, .60, .61),
    BPOGY = .50,
    BPOGV = 1,
    target_x = c(rep(.50, 4), rep(.60, 4)),
    target_y = .50
  )

  q <- create_gazepoint_quality_report(
    d,
    level = "trial",
    nominal_sampling_hz = 100,
    thresholds = list(valid_sample_fraction = list(min = .99))
  )
  expect_s3_class(q, "gaze_quality_report")
  adapter <- attr(q, "gazepoint_quality_adapter")
  expect_equal(adapter$formula_engine, "eyeprocess")
  expect_false(adapter$automatic_exclusion)
  expect_true(all(c("accuracy_mean", "precision_rms_s2s", "bcea") %in% names(q)))

  spatial <- summarise_gazepoint_spatial_quality(d, level = "trial")
  expect_true(all(c("accuracy_mean", "precision_rms_s2s") %in% names(spatial)))

  text <- report_gazepoint_quality(q)
  expect_match(text, "Review required", fixed = TRUE)
})
