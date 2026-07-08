test_that("sync_gazepoint_face_data matches nearest time within participant", {
  gaze <- data.frame(
    subject_id = c("P001", "P001", "P002"),
    time_sec = c(0.000, 0.034, 0.000),
    AOI = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = c("P001", "P001", "P002"),
    frame = c(1, 2, 1),
    timestamp = c(0.000, 0.033, 0.000),
    confidence = c(0.95, 0.94, 0.93),
    success = c(1, 1, 1),
    AU12_r = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec",
    tolerance_sec = 0.010
  )

  expect_s3_class(synced, "gp3_face_sync")
  expect_equal(nrow(synced), 3)
  expect_true(all(synced$face_sync_status == "matched"))
  expect_true("face_AU12_r" %in% names(synced))
  expect_equal(synced$face_AU12_r, c(0.1, 0.2, 0.3))
})


test_that("sync_gazepoint_face_data marks outside tolerance", {
  gaze <- data.frame(
    subject_id = "P001",
    time_sec = 1.000,
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0.000,
    confidence = 0.95,
    success = 1
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec",
    tolerance_sec = 0.010
  )

  expect_equal(synced$face_sync_status[[1]], "outside_tolerance")
  expect_false(synced$face_sync_within_tolerance[[1]])
})


test_that("sync_gazepoint_face_data can drop unmatched rows", {
  gaze <- data.frame(
    subject_id = c("P001", "P002"),
    time_sec = c(0, 0),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0,
    confidence = 0.95,
    success = 1
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec",
    keep_unmatched = FALSE
  )

  expect_equal(nrow(synced), 1)
  expect_equal(synced$subject_id[[1]], "P001")
})


test_that("sync_gazepoint_face_data matches exact frames", {
  gaze <- data.frame(
    subject_id = c("P001", "P001", "P002"),
    VID_FRAME = c(10, 11, 10),
    AOI = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = c("P001", "P001", "P002"),
    frame = c(10, 11, 10),
    timestamp = c(0.1, 0.2, 0.1),
    confidence = c(0.95, 0.94, 0.93),
    success = c(1, 1, 1),
    AU04_r = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    method = "frame_exact",
    by = c(subject_id = "participant_id"),
    gaze_frame_col = "VID_FRAME"
  )

  expect_equal(nrow(synced), 3)
  expect_true(all(synced$face_sync_status == "matched"))
  expect_true("face_AU04_r" %in% names(synced))
  expect_equal(synced$face_AU04_r, c(0.1, 0.2, 0.3))
})


test_that("sync_gazepoint_face_data validates inputs and by mappings", {
  gaze <- data.frame(subject_id = "P001", time_sec = 0)
  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0,
    confidence = 0.95,
    success = 1
  )

  expect_error(
    sync_gazepoint_face_data(1:3, face),
    "must be a data frame"
  )

  expect_error(
    sync_gazepoint_face_data(gaze, 1:3),
    "must be a data frame"
  )

  expect_error(
    sync_gazepoint_face_data(gaze, face, by = c("participant_id")),
    "must be named"
  )

  expect_error(
    sync_gazepoint_face_data(
      gaze,
      face,
      by = c(missing_col = "participant_id")
    ),
    "not found"
  )

  expect_error(
    sync_gazepoint_face_data(
      gaze,
      face,
      by = c(subject_id = "missing_col")
    ),
    "not found"
  )
})


test_that("sync_gazepoint_face_data requires explicit missing time or frame columns", {
  gaze <- data.frame(subject_id = "P001", x = 0)
  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0,
    confidence = 0.95,
    success = 1
  )

  expect_error(
    sync_gazepoint_face_data(
      gaze,
      face,
      by = c(subject_id = "participant_id")
    ),
    "could not be detected"
  )

  gaze_frame <- data.frame(subject_id = "P001", x = 1)

  expect_error(
    sync_gazepoint_face_data(
      gaze_frame,
      face,
      method = "frame_exact",
      by = c(subject_id = "participant_id")
    ),
    "could not be detected"
  )
})
test_that("audit_gazepoint_face_sync summarises matched synchronisation", {
  gaze <- data.frame(
    subject_id = c("P001", "P001", "P002"),
    time_sec = c(0.000, 0.034, 0.000),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = c("P001", "P001", "P002"),
    frame = c(1, 2, 1),
    timestamp = c(0.000, 0.033, 0.000),
    confidence = c(0.95, 0.94, 0.93),
    success = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec",
    tolerance_sec = 0.010
  )

  audit <- audit_gazepoint_face_sync(
    synced,
    group_cols = "subject_id"
  )

  expect_s3_class(audit, "gp3_face_sync_audit")
  expect_s3_class(audit$overview, "tbl_df")
  expect_s3_class(audit$group_summary, "tbl_df")
  expect_s3_class(audit$issue_summary, "tbl_df")
  expect_equal(audit$overview$n_rows[[1]], 3)
  expect_equal(audit$overview$n_matched[[1]], 3)
  expect_equal(audit$overview$face_sync_audit_status[[1]], "pass")
  expect_equal(nrow(audit$group_summary), 2)
})


test_that("audit_gazepoint_face_sync warns when matches fall below threshold", {
  gaze <- data.frame(
    subject_id = c("P001", "P002"),
    time_sec = c(0, 0),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0,
    confidence = 0.95,
    success = 1
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec"
  )

  audit <- audit_gazepoint_face_sync(
    synced,
    min_matched_percent = 40,
    warning_matched_percent = 80
  )

  expect_equal(audit$overview$n_matched[[1]], 1)
  expect_equal(audit$overview$matched_percent[[1]], 50)
  expect_equal(audit$overview$face_sync_audit_status[[1]], "warn")
})


test_that("audit_gazepoint_face_sync fails when matches are too low", {
  gaze <- data.frame(
    subject_id = c("P001", "P002", "P003"),
    time_sec = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0,
    confidence = 0.95,
    success = 1
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec"
  )

  audit <- audit_gazepoint_face_sync(
    synced,
    min_matched_percent = 50,
    warning_matched_percent = 80
  )

  expect_equal(audit$overview$n_matched[[1]], 1)
  expect_equal(audit$overview$face_sync_audit_status[[1]], "fail")
})


test_that("audit_gazepoint_face_sync detects outside tolerance rows", {
  gaze <- data.frame(
    subject_id = c("P001", "P001"),
    time_sec = c(0, 1),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = 1,
    timestamp = 0,
    confidence = 0.95,
    success = 1
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec",
    tolerance_sec = 0.010
  )

  audit <- audit_gazepoint_face_sync(synced)

  expect_equal(audit$overview$n_outside_tolerance[[1]], 1)
  expect_true(any(audit$issue_summary$issue == "outside_tolerance_rows"))
})


test_that("audit_gazepoint_face_sync warns on large absolute differences", {
  gaze <- data.frame(
    subject_id = c("P001", "P001"),
    time_sec = c(0, 0.040),
    stringsAsFactors = FALSE
  )

  face <- data.frame(
    participant_id = "P001",
    frame = c(1, 2),
    timestamp = c(0, 0.000),
    confidence = c(0.95, 0.95),
    success = c(1, 1)
  )

  synced <- sync_gazepoint_face_data(
    gaze,
    face,
    by = c(subject_id = "participant_id"),
    gaze_time_col = "time_sec",
    tolerance_sec = 0.100
  )

  audit <- audit_gazepoint_face_sync(
    synced,
    max_abs_diff_sec = 0.020
  )

  expect_gt(audit$overview$max_abs_diff_sec[[1]], 0.020)
  expect_equal(audit$overview$face_sync_audit_status[[1]], "warn")
})


test_that("audit_gazepoint_face_sync validates inputs", {
  expect_error(
    audit_gazepoint_face_sync(1:3),
    "must be a data frame"
  )

  expect_error(
    audit_gazepoint_face_sync(data.frame(x = 1)),
    "does not look like synchronised face data"
  )
})
