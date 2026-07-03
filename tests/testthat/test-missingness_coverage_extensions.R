test_that("summarize_gazepoint_missingness summarizes selected columns", {
  x <- data.frame(
    condition = rep(c("A", "B"), each = 4),
    pupil = c(1, NA, 3, 4, NA, NA, 7, 8),
    gaze_x = c(1, 2, NA, 4, 5, 6, 7, 8)
  )

  out <- summarize_gazepoint_missingness(
    x,
    cols = c("pupil", "gaze_x"),
    group_cols = "condition"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4)
  expect_true(all(c(
    "group_id",
    "variable",
    "n_rows",
    "n_missing",
    "n_observed",
    "missing_rate",
    "observed_rate"
  ) %in% names(out)))

  a_pupil <- out[out$group_id == "A" & out$variable == "pupil", ]
  b_pupil <- out[out$group_id == "B" & out$variable == "pupil", ]

  expect_equal(a_pupil$n_missing, 1)
  expect_equal(a_pupil$missing_rate, 0.25)
  expect_equal(b_pupil$n_missing, 2)
  expect_equal(b_pupil$missing_rate, 0.50)
})


test_that("summarise_gazepoint_missingness is an alias", {
  x <- data.frame(a = c(1, NA, 3))

  us <- summarize_gazepoint_missingness(x)
  uk <- summarise_gazepoint_missingness(x)

  expect_equal(us, uk)
})


test_that("summarize_gazepoint_missingness validates inputs", {
  x <- data.frame(a = 1:3)

  expect_error(
    summarize_gazepoint_missingness(x, cols = "missing"),
    "missing required column"
  )

  expect_error(
    summarize_gazepoint_missingness(x, group_cols = "missing"),
    "missing required column"
  )

  expect_error(
    summarize_gazepoint_missingness(x, cols = character()),
    "at least one column"
  )
})


test_that("plot_gazepoint_missingness_profile returns ggplot objects from raw data", {
  x <- data.frame(
    condition = rep(c("A", "B"), each = 4),
    pupil = c(1, NA, 3, 4, NA, NA, 7, 8),
    gaze_x = c(1, 2, NA, 4, 5, 6, 7, 8)
  )

  p_bar <- plot_gazepoint_missingness_profile(
    x,
    cols = c("pupil", "gaze_x"),
    group_cols = "condition",
    plot_type = "bar"
  )

  p_tile <- plot_gazepoint_missingness_profile(
    x,
    cols = c("pupil", "gaze_x"),
    group_cols = "condition",
    plot_type = "tile"
  )

  expect_s3_class(p_bar, "ggplot")
  expect_s3_class(p_tile, "ggplot")
})


test_that("plot_gazepoint_missingness_profile accepts summary input", {
  x <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 1))
  summary <- summarize_gazepoint_missingness(x)

  p <- plot_gazepoint_missingness_profile(summary)

  expect_s3_class(p, "ggplot")
})


test_that("report_gazepoint_missingness returns summary and cautious text", {
  x <- data.frame(
    pupil = c(1, NA, 3, NA),
    gaze_x = c(1, 2, 3, 4)
  )

  out <- report_gazepoint_missingness(
    x,
    cols = c("pupil", "gaze_x"),
    digits = 1
  )

  expect_type(out, "list")
  expect_s3_class(out$summary, "data.frame")
  expect_s3_class(out$overall, "data.frame")
  expect_s3_class(out$variable_summary, "data.frame")
  expect_match(out$report_text, "overall cell-level missingness rate")
  expect_match(out$report_text, "do not by themselves define exclusion decisions")
  expect_equal(out$overall$n_variables, 2)
  expect_equal(out$overall$total_missing, 2)
})


test_that("report_gazepoint_missingness accepts summary input and validates arguments", {
  x <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 1))
  summary <- summarize_gazepoint_missingness(x)

  out <- report_gazepoint_missingness(summary, max_variables = 1)

  expect_type(out, "list")
  expect_equal(nrow(out$variable_summary), 2)

  expect_error(
    report_gazepoint_missingness(x, digits = -1),
    "digits"
  )

  expect_error(
    report_gazepoint_missingness(x, max_variables = 0),
    "positive integer"
  )
})
