test_that("create_gazepoint_face_reporting_checklist handles supplied objects", {
  face_data <- data.frame(
    face_time_sec = c(0, 0.1),
    face_confidence = c(0.95, 0.94),
    face_valid = c(TRUE, TRUE),
    AU12_r = c(0.1, 0.2)
  )

  quality <- audit_gazepoint_face_quality(face_data)
  checklist <- create_gazepoint_face_reporting_checklist(
    face_data = face_data,
    quality_audit = quality
  )

  expect_s3_class(checklist, "gp3_face_reporting_checklist")
  expect_true(all(c("section", "item", "status", "evidence", "recommendation") %in%
                    names(checklist)))
  expect_true(any(checklist$status == "pass"))
  expect_true(any(checklist$section == "Interpretation"))
})


test_that("create_gazepoint_face_reporting_checklist reflects quality warnings", {
  face_data <- data.frame(
    face_time_sec = c(0, 0.1, 0.2),
    face_confidence = c(0.95, 0.10, 0.10),
    face_valid = c(TRUE, FALSE, FALSE),
    AU12_r = c(0.1, 0.2, 0.3)
  )

  quality <- audit_gazepoint_face_quality(
    face_data,
    min_valid_percent = 80,
    warning_valid_percent = 90
  )

  checklist <- create_gazepoint_face_reporting_checklist(
    face_data = face_data,
    quality_audit = quality
  )

  expect_true(any(checklist$status %in% c("fail", "review")))
})


test_that("create_gazepoint_face_reporting_checklist includes sync and window evidence", {
  gaze <- data.frame(
    participant_id = "P001",
    time_sec = c(0, 0.1),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = 1:2,
    timestamp = c(0, 0.1),
    confidence = c(0.95, 0.94),
    success = c(1, 1),
    AU12_r = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(participant_id = "participant_id"),
    gaze_time_col = "time_sec"
  )

  sync_audit <- audit_gazepoint_face_sync(synced)

  windows <- data.frame(
    participant_id = "P001",
    window = "response",
    window_start_sec = 0,
    window_end_sec = 0.2,
    stringsAsFactors = FALSE
  )

  window_summary <- summarize_gazepoint_face_windows(
    standardize_gazepoint_face_columns(face),
    windows = windows,
    group_cols = "participant_id",
    window_label_col = "window",
    measure_cols = "AU12_r"
  )

  checklist <- create_gazepoint_face_reporting_checklist(
    sync_audit = sync_audit,
    window_summary = window_summary
  )

  expect_true(any(checklist$section == "Synchronisation"))
  expect_true(any(checklist$section == "Window summaries"))
  expect_true(any(grepl("window-summary", checklist$evidence)))
})


test_that("report_gazepoint_face_qc returns markdown report", {
  face_data <- data.frame(
    face_time_sec = c(0, 0.1),
    face_confidence = c(0.95, 0.94),
    face_valid = c(TRUE, TRUE),
    AU12_r = c(0.1, 0.2)
  )

  quality <- audit_gazepoint_face_quality(face_data)

  report <- report_gazepoint_face_qc(
    face_data = face_data,
    quality_audit = quality
  )

  expect_s3_class(report, "gp3_face_qc_report")
  expect_type(report, "character")
  expect_true(any(grepl("External facial-behaviour QC report", report)))
  expect_true(any(grepl("Reporting checklist", report)))
  expect_true(any(grepl("Interpretation cautions", report)))
})


test_that("report_gazepoint_face_qc returns list report", {
  face_data <- data.frame(
    face_time_sec = c(0, 0.1),
    face_confidence = c(0.95, 0.94),
    face_valid = c(TRUE, TRUE),
    AU12_r = c(0.1, 0.2)
  )

  quality <- audit_gazepoint_face_quality(face_data)

  report <- report_gazepoint_face_qc(
    face_data = face_data,
    quality_audit = quality,
    output = "list"
  )

  expect_s3_class(report, "gp3_face_qc_report_list")
  expect_true("checklist" %in% names(report))
  expect_true("quality_overview" %in% names(report))
  expect_s3_class(report$checklist, "tbl_df")
})


test_that("report_gazepoint_face_qc can include model summary", {
  dat <- data.frame(
    AU12_r_mean = c(0.1, 0.2, 0.3, 0.4),
    rating = c(3, 4, 5, 6)
  )

  fit <- fit_gazepoint_face_window_lmm(
    dat,
    outcome = "rating",
    predictors = "AU12_r_mean"
  )

  report <- report_gazepoint_face_qc(
    multimodal_model = fit,
    output = "list"
  )

  expect_equal(report$model_summary$outcome[[1]], "rating")
  expect_equal(report$model_summary$n_rows_model[[1]], 4)
})


test_that("face reporting helpers validate inputs", {
  expect_error(
    report_gazepoint_face_qc(checklist = 1:3),
    "must be a data frame"
  )

  checklist <- create_gazepoint_face_reporting_checklist(
    include_interpretation_cautions = FALSE
  )

  report <- report_gazepoint_face_qc(
    checklist = checklist,
    output = "markdown",
    include_cautions = FALSE
  )

  expect_false(any(grepl("Interpretation cautions", report)))
})
