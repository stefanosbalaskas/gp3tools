test_that("segment_gazepoint_task_phases assigns phases from windows", {
  x <- data.frame(time_ms = c(0, 250, 500, 750, 1000, 1250, 2000))
  windows <- data.frame(
    phase = c("baseline", "stimulus", "response"),
    start = c(0, 500, 1000),
    end = c(500, 1000, 2000)
  )

  out <- segment_gazepoint_task_phases(
    x,
    time_col = "time_ms",
    phase_windows = windows,
    keep_window_metadata = TRUE
  )

  expect_equal(
    out$task_phase,
    c("baseline", "baseline", "stimulus", "stimulus", "response", "response", "outside")
  )
  expect_equal(sum(out$.gp3_phase_assigned), 6)
  expect_true(".gp3_phase_window_start" %in% names(out))
  expect_true(".gp3_phase_window_end" %in% names(out))
})


test_that("segment_gazepoint_task_phases supports inclusive upper boundaries", {
  x <- data.frame(time_ms = c(0, 500, 1000))
  windows <- data.frame(
    phase = c("baseline", "stimulus"),
    start = c(0, 500),
    end = c(500, 1000)
  )

  out <- segment_gazepoint_task_phases(
    x,
    time_col = "time_ms",
    phase_windows = windows,
    include_upper = TRUE,
    outside_label = NA_character_
  )

  expect_equal(out$task_phase, c("baseline", "baseline", "stimulus"))
})


test_that("segment_gazepoint_task_phases validates phase windows", {
  x <- data.frame(time_ms = 1:3)

  expect_error(
    segment_gazepoint_task_phases(
      x,
      time_col = "missing",
      phase_windows = data.frame(phase = "a", start = 0, end = 1)
    ),
    "missing required column"
  )

  expect_error(
    segment_gazepoint_task_phases(
      x,
      time_col = "time_ms",
      phase_windows = data.frame(phase = "a", start = 1, end = 1)
    ),
    "greater than"
  )

  expect_error(
    segment_gazepoint_task_phases(
      x,
      time_col = "time_ms",
      phase_windows = data.frame(phase = "", start = 0, end = 1)
    ),
    "Phase labels"
  )
})


test_that("summarize_gazepoint_phase_coverage summarizes timing and completeness", {
  x <- data.frame(
    subject = rep(c("S1", "S2"), each = 4),
    time_ms = rep(c(0, 250, 750, 1250), times = 2),
    value = c(1, NA, 3, 4, 1, 2, NA, 4)
  )

  windows <- data.frame(
    phase = c("baseline", "stimulus"),
    start = c(0, 500),
    end = c(500, 1500)
  )

  segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)

  out <- summarize_gazepoint_phase_coverage(
    segmented,
    phase_col = "task_phase",
    group_cols = "subject",
    time_col = "time_ms",
    value_cols = "value"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4)
  expect_true(all(c(
    "group_id",
    "phase",
    "n_rows",
    "n_finite_time",
    "min_time",
    "max_time",
    "time_span",
    "n_complete_value_rows",
    "complete_value_rate"
  ) %in% names(out)))

  s1_baseline <- out[out$group_id == "S1" & out$phase == "baseline", ]
  expect_equal(s1_baseline$n_rows, 2)
  expect_equal(s1_baseline$n_complete_value_rows, 1)
  expect_equal(s1_baseline$complete_value_rate, 0.5)
})


test_that("summarise_gazepoint_phase_coverage is an alias", {
  x <- data.frame(task_phase = c("a", "a", "b"), value = c(1, NA, 3))

  us <- summarize_gazepoint_phase_coverage(x, value_cols = "value")
  uk <- summarise_gazepoint_phase_coverage(x, value_cols = "value")

  expect_equal(us, uk)
})


test_that("report_gazepoint_phase_coverage returns cautious report text", {
  x <- data.frame(
    time_ms = c(0, 250, 750, 1250),
    value = c(1, NA, 3, 4)
  )

  windows <- data.frame(
    phase = c("baseline", "stimulus"),
    start = c(0, 500),
    end = c(500, 1500)
  )

  segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)

  out <- report_gazepoint_phase_coverage(
    segmented,
    phase_col = "task_phase",
    time_col = "time_ms",
    value_cols = "value"
  )

  expect_type(out, "list")
  expect_s3_class(out$summary, "data.frame")
  expect_s3_class(out$overall, "data.frame")
  expect_s3_class(out$phase_totals, "data.frame")
  expect_match(out$report_text, "Task-phase coverage")
  expect_match(out$report_text, "do not by themselves define exclusion decisions")
})


test_that("report_gazepoint_phase_coverage accepts summary input and validates digits", {
  x <- data.frame(task_phase = c("a", "a", "b"), value = c(1, NA, 3))
  summary <- summarize_gazepoint_phase_coverage(x, value_cols = "value")

  out <- report_gazepoint_phase_coverage(summary)

  expect_type(out, "list")
  expect_equal(out$overall$n_phases, 2)

  expect_error(
    report_gazepoint_phase_coverage(summary, digits = -1),
    "digits"
  )
})

test_that("plot_gazepoint_phase_timeline returns ggplot with timing information", {
  x <- data.frame(
    subject = rep(c("S1", "S2"), each = 4),
    time_ms = rep(c(0, 250, 750, 1250), times = 2)
  )

  windows <- data.frame(
    phase = c("baseline", "stimulus"),
    start = c(0, 500),
    end = c(500, 1500)
  )

  segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)

  p <- plot_gazepoint_phase_timeline(
    segmented,
    group_cols = "subject",
    time_col = "time_ms",
    title = "Task-phase timeline"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_phase_timeline returns fallback count plot without timing", {
  x <- data.frame(
    task_phase = c("baseline", "baseline", "stimulus", "response"),
    subject = c("S1", "S1", "S1", "S1")
  )

  p <- plot_gazepoint_phase_timeline(
    x,
    group_cols = "subject"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_phase_timeline accepts summary input", {
  x <- data.frame(
    task_phase = c("baseline", "baseline", "stimulus"),
    value = c(1, NA, 3)
  )

  summary <- summarize_gazepoint_phase_coverage(
    x,
    value_cols = "value"
  )

  p <- plot_gazepoint_phase_timeline(summary)

  expect_s3_class(p, "ggplot")
})
