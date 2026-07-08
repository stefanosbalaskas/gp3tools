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
