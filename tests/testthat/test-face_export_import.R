test_that("read_gazepoint_face_export reads OpenFace-style CSV", {
  tmp <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "frame, timestamp, confidence, success, gaze_0_x, pose_Tx, AU12_r, AU12_c",
      "1,0.000,0.98,1,0.10,1.5,0.20,1",
      "2,0.033,0.40,1,0.12,1.6,0.30,1"
    ),
    tmp
  )

  face <- read_gazepoint_face_export(
    tmp,
    participant_id = "P001",
    session_id = "S001"
  )

  expect_s3_class(face, "gp3_face_export")
  expect_equal(nrow(face), 2)
  expect_equal(face$gp3_face_source[[1]], "openface")
  expect_equal(face$gp3_face_participant_id[[1]], "P001")
  expect_true("AU12_r" %in% names(face))
})


test_that("read_gazepoint_face_export reads a directory of CSV files", {
  dir <- tempfile()
  dir.create(dir)

  writeLines(
    c(
      "frame,timestamp,confidence,success,AU12_r",
      "1,0.000,0.99,1,0.10"
    ),
    file.path(dir, "face_a.csv")
  )

  writeLines(
    c(
      "frame,timestamp,confidence,success,AU04_r",
      "1,0.000,0.97,1,0.20"
    ),
    file.path(dir, "face_b.csv")
  )

  face <- read_gazepoint_face_export(
    dir,
    participant_id = c("P001", "P002")
  )

  expect_equal(nrow(face), 2)
  expect_equal(sort(unique(face$gp3_face_participant_id)), c("P001", "P002"))
  expect_true("AU12_r" %in% names(face))
  expect_true("AU04_r" %in% names(face))
})


test_that("standardize_gazepoint_face_columns creates common columns", {
  dat <- data.frame(
    frame = 1:3,
    timestamp = c(0, 0.033, 0.066),
    confidence = c(0.95, 0.79, 0.90),
    success = c(1, 1, 0),
    pose_Tx = c(1, 2, 3),
    pose_Rx = c(0.1, 0.2, 0.3),
    AU12_r = c(0.2, 0.4, 0.1),
    check.names = FALSE
  )

  face <- standardize_gazepoint_face_columns(dat)

  expect_s3_class(face, "gp3_face_data")
  expect_equal(face$face_source[[1]], "openface")
  expect_equal(face$face_frame, 1:3)
  expect_equal(face$face_time_ms, c(0, 33, 66))
  expect_equal(face$face_valid, c(TRUE, FALSE, FALSE))
  expect_true("face_pose_tx" %in% names(face))
  expect_true("face_pose_rx" %in% names(face))
  expect_true("AU12_r" %in% names(face))
})


test_that("standardize_gazepoint_face_columns accepts explicit generic columns", {
  dat <- data.frame(
    subject = c("A", "A"),
    video_frame = c(10, 11),
    seconds = c(1.0, 1.1),
    score = c(0.8, 0.7),
    detected = c("yes", "no"),
    smile = c(0.2, 0.3),
    stringsAsFactors = FALSE
  )

  face <- standardize_gazepoint_face_columns(
    dat,
    source = "generic",
    participant_id_col = "subject",
    frame_col = "video_frame",
    time_col = "seconds",
    confidence_col = "score",
    success_col = "detected",
    confidence_threshold = 0.75
  )

  expect_equal(face$participant_id, c("A", "A"))
  expect_equal(face$face_frame, c(10L, 11L))
  expect_equal(face$face_valid, c(TRUE, FALSE))
})


test_that("standardize_gazepoint_face_columns can read from path", {
  tmp <- tempfile(fileext = ".csv")

  writeLines(
    c(
      "frame,timestamp,confidence,success,AU12_r",
      "1,0.000,0.98,1,0.20"
    ),
    tmp
  )

  face <- standardize_gazepoint_face_columns(tmp)

  expect_s3_class(face, "gp3_face_data")
  expect_equal(face$face_frame[[1]], 1L)
})


test_that("read_gazepoint_face_export validates paths and metadata lengths", {
  expect_error(
    read_gazepoint_face_export(tempfile()),
    "does not exist"
  )

  dir <- tempfile()
  dir.create(dir)

  writeLines(
    c("frame,timestamp", "1,0"),
    file.path(dir, "a.csv")
  )

  writeLines(
    c("frame,timestamp", "1,0"),
    file.path(dir, "b.csv")
  )

  expect_error(
    read_gazepoint_face_export(dir, participant_id = c("A", "B", "C")),
    "length 1 or the same length"
  )
})
