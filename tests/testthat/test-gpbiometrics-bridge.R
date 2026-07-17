test_that("gp3tools gaze output is standardized for biometrics", {
  gaze <- data.frame(
    USER_ID = c("P01", "P01"),
    MEDIA_ID = c("T01", "T01"),
    MSTIMER = c(0, 20),
    BPOGX = c(0.2, 0.3),
    BPOGY = c(0.4, 0.5),
    BPOGV = c(1, 0),
    AOI = c("claim", "evidence"),
    LPD = c(3.1, 3.2)
  )

  out <- prepare_gazepoint_gpbiometrics_bridge(gaze)

  expect_s3_class(out, "gazepoint_gpbiometrics_bridge")
  expect_equal(out$participant_id, c("P01", "P01"))
  expect_equal(out$trial_id, c("T01", "T01"))
  expect_equal(out$time_s, c(0, 0.02))
  expect_equal(out$gaze_valid, c(TRUE, FALSE))
  expect_equal(out$aoi, c("claim", "evidence"))
})

test_that("native cross-package workflow aligns nearest samples", {
  gaze <- data.frame(
    participant_id = rep("P01", 4),
    trial_id = rep("T01", 4),
    time_s = c(0, 0.02, 0.04, 0.06),
    gaze_x = c(0.2, 0.3, 0.7, 0.8),
    gaze_y = c(0.4, 0.4, 0.5, 0.5),
    aoi = c("claim", "claim", "evidence", "evidence"),
    pupil = 3.1,
    gaze_valid = TRUE
  )
  class(gaze) <- c(
    "gazepoint_gpbiometrics_bridge",
    "data.frame"
  )

  biometrics <- data.frame(
    participant_id = rep("P01", 4),
    trial_id = rep("T01", 4),
    time_s = c(0.001, 0.021, 0.041, 0.061),
    GSR = c(1, 2, 3, 4),
    HR = c(70, 71, 72, 73),
    event = c("start", "", "", "end")
  )

  out <- run_gazepoint_gpbiometrics_workflow(
    gaze,
    biometrics,
    signal_cols = c("GSR", "HR"),
    event_col = "event",
    tolerance_s = 0.005
  )

  expect_s3_class(out, "gazepoint_cross_package_workflow")
  expect_equal(out$audit$matched_rows, 4)
  expect_equal(out$audit$matched_rate, 1)
  expect_equal(out$audit$engine, "native_nearest_time")
  expect_true(all(out$synchronized$.matched))
  expect_true(all(c("GSR", "HR") %in% names(out$synchronized)))
  expect_true(nrow(out$signal_summary) > 0)
  expect_match(out$report_text, "do not directly establish")
})

test_that("cross-package adapter contract is tested without dependencies", {
  gaze <- data.frame(
    participant_id = "P01",
    trial_id = "T01",
    time_s = 0,
    gaze_x = 0.5,
    gaze_y = 0.5,
    aoi = "target",
    pupil = 3,
    gaze_valid = TRUE
  )
  class(gaze) <- c(
    "gazepoint_gpbiometrics_bridge",
    "data.frame"
  )
  biometrics <- data.frame(
    participant_id = "P01",
    trial_id = "T01",
    time_s = 0,
    GSR = 2
  )

  adapter <- function(gaze, biometrics, tolerance_s) {
    out <- gaze
    out$GSR <- biometrics$GSR
    out$.sync_diff_s <- biometrics$time_s - gaze$time_s
    out$.matched <- abs(out$.sync_diff_s) <= tolerance_s
    out
  }

  out <- run_gazepoint_gpbiometrics_workflow(
    gaze,
    biometrics,
    signal_cols = "GSR",
    tolerance_s = 0.01,
    adapter = adapter
  )

  expect_equal(out$audit$engine, "external_adapter")
  expect_equal(out$audit$matched_rows, 1)
  expect_equal(out$synchronized$GSR, 2)
})

test_that("unmatched rows can be retained or removed", {
  gaze <- data.frame(
    participant_id = rep("P01", 2),
    trial_id = rep("T01", 2),
    time_s = c(0, 1),
    gaze_x = 0.5,
    gaze_y = 0.5,
    aoi = c("A", "B"),
    pupil = 3,
    gaze_valid = TRUE
  )
  class(gaze) <- c(
    "gazepoint_gpbiometrics_bridge",
    "data.frame"
  )
  biometrics <- data.frame(
    participant_id = "P01",
    trial_id = "T01",
    time_s = 0,
    GSR = 2
  )

  retained <- run_gazepoint_gpbiometrics_workflow(
    gaze,
    biometrics,
    signal_cols = "GSR",
    tolerance_s = 0.01,
    include_unmatched = TRUE
  )
  removed <- run_gazepoint_gpbiometrics_workflow(
    gaze,
    biometrics,
    signal_cols = "GSR",
    tolerance_s = 0.01,
    include_unmatched = FALSE
  )

  expect_equal(nrow(retained$synchronized), 2)
  expect_equal(nrow(removed$synchronized), 1)
})

test_that("combined cross-package report can be written", {
  gaze <- data.frame(
    participant_id = "P01",
    trial_id = "T01",
    time_s = 0,
    gaze_x = 0.5,
    gaze_y = 0.5,
    aoi = "target",
    pupil = 3,
    gaze_valid = TRUE
  )
  class(gaze) <- c(
    "gazepoint_gpbiometrics_bridge",
    "data.frame"
  )
  biometrics <- data.frame(
    participant_id = "P01",
    trial_id = "T01",
    time_s = 0,
    GSR = 2
  )

  workflow <- run_gazepoint_gpbiometrics_workflow(
    gaze,
    biometrics,
    signal_cols = "GSR",
    tolerance_s = 0.01
  )
  path <- tempfile(fileext = ".md")
  report <- create_gazepoint_cross_package_report(
    workflow,
    output_file = path
  )

  expect_true(file.exists(path))
  expect_true(any(grepl("Alignment summary", report)))
  expect_true(any(grepl("Interpretation guardrail", report)))
})
