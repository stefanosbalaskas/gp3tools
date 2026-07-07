test_that("audit_gazepoint_face_quality audits OpenFace-style data", {
  dat <- data.frame(
    frame = 1:4,
    timestamp = c(0, 0.033, 0.066, 0.099),
    confidence = c(0.95, 0.92, 0.85, 0.40),
    success = c(1, 1, 1, 1),
    AU12_r = c(0.1, 0.2, 0.3, 0.4),
    check.names = FALSE
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    min_valid_percent = 70,
    warning_valid_percent = 90
  )

  expect_s3_class(audit, "gp3_face_quality_audit")
  expect_s3_class(audit$overview, "tbl_df")
  expect_s3_class(audit$group_summary, "tbl_df")
  expect_s3_class(audit$issue_summary, "tbl_df")
  expect_equal(audit$overview$n_rows[[1]], 4)
  expect_equal(audit$overview$n_valid[[1]], 3)
  expect_equal(audit$overview$face_quality_status[[1]], "warn")
})


test_that("audit_gazepoint_face_quality can fail low-validity data", {
  dat <- data.frame(
    frame = 1:4,
    timestamp = c(0, 0.033, 0.066, 0.099),
    confidence = c(0.95, 0.20, 0.30, 0.40),
    success = c(1, 1, 1, 1),
    check.names = FALSE
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    min_valid_percent = 60,
    warning_valid_percent = 80
  )

  expect_equal(audit$overview$n_valid[[1]], 1)
  expect_equal(audit$overview$face_quality_status[[1]], "fail")
})


test_that("audit_gazepoint_face_quality handles grouped summaries", {
  dat <- data.frame(
    participant_id = c("P001", "P001", "P002", "P002"),
    frame = c(1, 2, 1, 2),
    timestamp = c(0, 0.033, 0, 0.033),
    confidence = c(0.95, 0.90, 0.95, 0.30),
    success = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    group_cols = "participant_id",
    min_valid_percent = 70,
    warning_valid_percent = 85
  )

  expect_equal(nrow(audit$group_summary), 2)
  expect_true("participant_id" %in% names(audit$group_summary))
  expect_true("face_quality_group" %in% names(audit$group_summary))
  expect_equal(audit$overview$n_groups[[1]], 2)
})


test_that("audit_gazepoint_face_quality reports unknown validity", {
  dat <- data.frame(
    frame = 1:3,
    timestamp = c(0, 0.033, 0.066),
    smile = c(0.1, 0.2, 0.3)
  )

  audit <- audit_gazepoint_face_quality(dat)

  expect_equal(audit$overview$face_quality_status[[1]], "unknown")
  expect_true(any(audit$issue_summary$issue == "unknown_validity"))
})


test_that("audit_gazepoint_face_quality detects duplicate frames", {
  dat <- data.frame(
    frame = c(1, 1, 2, 3),
    timestamp = c(0, 0.033, 0.066, 0.099),
    confidence = c(0.95, 0.96, 0.94, 0.93),
    success = c(1, 1, 1, 1)
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    max_duplicate_frame_percent = 0
  )

  expect_gt(audit$overview$duplicate_frame_percent[[1]], 0)
  expect_equal(audit$overview$face_quality_status[[1]], "warn")
})


test_that("audit_gazepoint_face_quality checks large time gaps when requested", {
  dat <- data.frame(
    frame = 1:4,
    timestamp = c(0, 0.033, 0.066, 1.000),
    confidence = c(0.95, 0.96, 0.94, 0.93),
    success = c(1, 1, 1, 1)
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    max_time_gap_sec = 0.20
  )

  expect_gt(audit$overview$max_time_gap_sec[[1]], 0.20)
  expect_equal(audit$overview$face_quality_status[[1]], "warn")
})


test_that("summarize_gazepoint_face_quality returns overview", {
  dat <- data.frame(
    frame = 1:2,
    timestamp = c(0, 0.033),
    confidence = c(0.95, 0.96),
    success = c(1, 1)
  )

  audit <- audit_gazepoint_face_quality(dat)
  summary <- summarize_gazepoint_face_quality(audit)
  summary2 <- summarise_gazepoint_face_quality(dat)

  expect_s3_class(summary, "gp3_face_quality_summary")
  expect_equal(nrow(summary), 1)
  expect_equal(summary$face_quality_status[[1]], "pass")
  expect_equal(summary2$face_quality_status[[1]], "pass")
})


test_that("audit_gazepoint_face_quality validates inputs", {
  expect_error(
    audit_gazepoint_face_quality(data.frame()),
    "at least one row"
  )

  expect_error(
    audit_gazepoint_face_quality(1:3),
    "must be a data frame"
  )
})

test_that("plot_gazepoint_face_quality returns status plot", {
  dat <- data.frame(
    participant_id = c("P001", "P001", "P002", "P002"),
    frame = c(1, 2, 1, 2),
    timestamp = c(0, 0.033, 0, 0.033),
    confidence = c(0.95, 0.90, 0.95, 0.30),
    success = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    group_cols = "participant_id"
  )

  p <- plot_gazepoint_face_quality(audit, plot_type = "status")

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_face_quality returns validity plot", {
  dat <- data.frame(
    participant_id = c("P001", "P001", "P002", "P002"),
    frame = c(1, 2, 1, 2),
    timestamp = c(0, 0.033, 0, 0.033),
    confidence = c(0.95, 0.90, 0.95, 0.30),
    success = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  p <- plot_gazepoint_face_quality(
    dat,
    plot_type = "validity",
    group_cols = "participant_id"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_face_quality returns confidence plot", {
  dat <- data.frame(
    participant_id = c("P001", "P001", "P002", "P002"),
    frame = c(1, 2, 1, 2),
    timestamp = c(0, 0.033, 0, 0.033),
    confidence = c(0.95, 0.90, 0.95, 0.30),
    success = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    group_cols = "participant_id"
  )

  p <- plot_gazepoint_face_quality(audit, plot_type = "confidence")

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_face_quality returns time-gap plot", {
  dat <- data.frame(
    participant_id = c("P001", "P001", "P001", "P001"),
    frame = 1:4,
    timestamp = c(0, 0.033, 0.066, 1.000),
    confidence = c(0.95, 0.96, 0.94, 0.93),
    success = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  audit <- audit_gazepoint_face_quality(
    dat,
    group_cols = "participant_id",
    max_time_gap_sec = 0.20
  )

  p <- plot_gazepoint_face_quality(audit, plot_type = "time_gaps")

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_face_quality validates plot type and group column", {
  dat <- data.frame(
    frame = 1:2,
    timestamp = c(0, 0.033),
    confidence = c(0.95, 0.96),
    success = c(1, 1)
  )

  audit <- audit_gazepoint_face_quality(dat)

  expect_error(
    plot_gazepoint_face_quality(audit, plot_type = "bad"),
    "should be one of"
  )

  expect_error(
    plot_gazepoint_face_quality(
      audit,
      plot_type = "validity",
      group_col = "missing_col"
    ),
    "not found"
  )
})
