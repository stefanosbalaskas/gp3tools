test_that("summarize_gazepoint_face_windows uses a separate window table", {
  face <- data.frame(
    participant_id = c("P001", "P001", "P001", "P002", "P002", "P002"),
    face_time_sec = c(0.00, 0.05, 0.10, 0.00, 0.05, 0.10),
    face_confidence = c(0.95, 0.94, 0.93, 0.96, 0.95, 0.94),
    face_valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    AU12_r = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4),
    AU04_r = c(0.5, 0.6, 0.7, 0.4, 0.5, 0.6),
    stringsAsFactors = FALSE
  )

  windows <- data.frame(
    participant_id = rep(c("P001", "P002"), each = 2),
    window = rep(c("baseline", "response"), times = 2),
    window_start_sec = c(0.00, 0.05, 0.00, 0.05),
    window_end_sec = c(0.05, 0.15, 0.05, 0.15),
    stringsAsFactors = FALSE
  )

  out <- summarize_gazepoint_face_windows(
    face,
    windows = windows,
    group_cols = "participant_id",
    window_label_col = "window"
  )

  expect_s3_class(out, "gp3_face_window_summary")
  expect_equal(nrow(out), 4)
  expect_true("AU12_r_mean" %in% names(out))
  expect_true("AU04_r_mean" %in% names(out))

  p1_base <- out[
    out$participant_id == "P001" & out$face_window_label == "baseline",
  ]

  expect_equal(p1_base$n_rows[[1]], 2)
  expect_equal(p1_base$AU12_r_mean[[1]], 0.15)
})


test_that("summarize_gazepoint_face_windows filters invalid rows when requested", {
  face <- data.frame(
    participant_id = "P001",
    face_time_sec = c(0.00, 0.05, 0.10),
    face_confidence = c(0.95, 0.94, 0.93),
    face_valid = c(TRUE, FALSE, TRUE),
    AU12_r = c(0.1, 99, 0.3),
    stringsAsFactors = FALSE
  )

  windows <- data.frame(
    participant_id = "P001",
    window = "all",
    window_start_sec = 0,
    window_end_sec = 0.15,
    stringsAsFactors = FALSE
  )

  out <- summarize_gazepoint_face_windows(
    face,
    windows = windows,
    group_cols = "participant_id",
    window_label_col = "window",
    measure_cols = "AU12_r",
    require_valid = TRUE
  )

  expect_equal(out$n_rows[[1]], 3)
  expect_equal(out$n_used[[1]], 2)
  expect_equal(out$valid_percent[[1]], 100 * 2 / 3)
  expect_equal(out$AU12_r_mean[[1]], 0.2)
})


test_that("summarize_gazepoint_face_windows keeps empty windows", {
  face <- data.frame(
    participant_id = "P001",
    face_time_sec = c(0.00, 0.05),
    face_valid = c(TRUE, TRUE),
    AU12_r = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  windows <- data.frame(
    participant_id = c("P001", "P001"),
    window = c("observed", "empty"),
    window_start_sec = c(0.00, 1.00),
    window_end_sec = c(0.10, 1.10),
    stringsAsFactors = FALSE
  )

  out <- summarize_gazepoint_face_windows(
    face,
    windows = windows,
    group_cols = "participant_id",
    window_label_col = "window"
  )

  expect_equal(nrow(out), 2)
  expect_equal(out$n_rows[out$face_window_label == "empty"], 0)
  expect_true(is.na(out$AU12_r_mean[out$face_window_label == "empty"]))
})


test_that("summarize_gazepoint_face_windows summarises labelled data", {
  dat <- data.frame(
    participant_id = c("P001", "P001", "P001", "P001"),
    window = c("baseline", "baseline", "response", "response"),
    face_time_sec = c(0.00, 0.05, 0.10, 0.15),
    face_valid = c(TRUE, TRUE, TRUE, TRUE),
    AU12_r = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )

  out <- summarize_gazepoint_face_windows(
    dat,
    group_cols = "participant_id",
    window_label_col = "window",
    measure_cols = "AU12_r"
  )

  expect_equal(nrow(out), 2)
  expect_equal(
    out$AU12_r_mean[out$face_window_label == "baseline"],
    0.15
  )
  expect_equal(
    out$AU12_r_mean[out$face_window_label == "response"],
    0.35
  )
})


test_that("summarize_gazepoint_face_reactivity computes baseline response difference", {
  face <- data.frame(
    participant_id = c("P001", "P001", "P001", "P001"),
    face_time_sec = c(0.00, 0.05, 0.10, 0.15),
    face_valid = c(TRUE, TRUE, TRUE, TRUE),
    AU12_r = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )

  windows <- data.frame(
    participant_id = c("P001", "P001"),
    window = c("baseline", "response"),
    window_start_sec = c(0.00, 0.10),
    window_end_sec = c(0.05, 0.20),
    stringsAsFactors = FALSE
  )

  summary <- summarize_gazepoint_face_windows(
    face,
    windows = windows,
    group_cols = "participant_id",
    window_label_col = "window",
    measure_cols = "AU12_r"
  )

  reactivity <- summarize_gazepoint_face_reactivity(
    summary,
    baseline_window = "baseline",
    response_window = "response",
    group_cols = "participant_id",
    measure_cols = "AU12_r"
  )

  expect_s3_class(reactivity, "gp3_face_reactivity_summary")
  expect_equal(nrow(reactivity), 1)
  expect_equal(reactivity$measure[[1]], "AU12_r")
  expect_equal(reactivity$baseline_value[[1]], 0.15)
  expect_equal(reactivity$response_value[[1]], 0.35)
  expect_equal(reactivity$reactivity[[1]], 0.20)
})


test_that("summarize_gazepoint_face_reactivity supports median statistic", {
  summary <- data.frame(
    participant_id = "P001",
    face_window_label = c("baseline", "response"),
    AU12_r_mean = c(0.1, 0.3),
    AU12_r_median = c(0.2, 0.5),
    stringsAsFactors = FALSE
  )

  out <- summarize_gazepoint_face_reactivity(
    summary,
    baseline_window = "baseline",
    response_window = "response",
    group_cols = "participant_id",
    measure_cols = "AU12_r",
    statistic = "median"
  )

  expect_equal(out$baseline_value[[1]], 0.2)
  expect_equal(out$response_value[[1]], 0.5)
  expect_equal(out$reactivity[[1]], 0.3)
})


test_that("face window summary helpers validate inputs", {
  face <- data.frame(
    participant_id = "P001",
    face_time_sec = 0,
    AU12_r = 0.1
  )

  windows <- data.frame(
    participant_id = "P001",
    window_start_sec = 0,
    window_end_sec = 1
  )

  expect_error(
    summarize_gazepoint_face_windows(1:3),
    "must be a data frame"
  )

  expect_error(
    summarize_gazepoint_face_windows(face, windows = 1:3),
    "must be a data frame"
  )

  expect_error(
    summarize_gazepoint_face_windows(
      face,
      windows = windows,
      group_cols = "missing"
    ),
    "Grouping column"
  )

  expect_error(
    summarize_gazepoint_face_windows(
      face,
      windows = windows,
      measure_cols = "missing"
    ),
    "Measure column"
  )
})


test_that("face reactivity helper validates inputs", {
  summary <- data.frame(
    face_window_label = c("baseline", "response"),
    AU12_r_mean = c(0.1, 0.2)
  )

  expect_error(
    summarize_gazepoint_face_reactivity(
      1:3,
      baseline_window = "baseline",
      response_window = "response"
    ),
    "must be a window-summary data frame"
  )

  expect_error(
    summarize_gazepoint_face_reactivity(
      data.frame(AU12_r_mean = 1),
      baseline_window = "baseline",
      response_window = "response"
    ),
    "window_col"
  )

  expect_error(
    summarize_gazepoint_face_reactivity(
      summary,
      baseline_window = "missing",
      response_window = "response"
    ),
    "No baseline-window"
  )

  expect_error(
    summarize_gazepoint_face_reactivity(
      summary,
      baseline_window = "baseline",
      response_window = "response",
      measure_cols = "missing"
    ),
    "Reactivity measure"
  )
})
