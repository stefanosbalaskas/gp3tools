make_test_pupil_timecourse_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 8),
    condition = rep(c("A", "B"), each = 4, times = 2),
    time = rep(c(0, 100, 200, 300), times = 4),
    pupil_smoothed = c(
      1.0, 1.2, 1.4, 1.6,
      1.5, 1.7, 1.9, 2.1,
      1.1, 1.3, 1.5, 1.7,
      1.6, 1.8, 2.0, 2.2
    )
  )
}

test_that("plot_gazepoint_pupil_timecourse returns ggplot object", {
  x <- make_test_pupil_timecourse_data()

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = "condition",
    bin_width_ms = 100
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_timecourse auto-detects pupil column", {
  x <- make_test_pupil_timecourse_data()

  p <- plot_gazepoint_pupil_timecourse(
    x,
    time_col = "time",
    condition_col = "condition",
    bin_width_ms = 100
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_timecourse supports missing condition column", {
  x <- make_test_pupil_timecourse_data() |>
    dplyr::select(-condition)

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = "condition",
    bin_width_ms = 100
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_timecourse handles all-missing condition values", {
  x <- make_test_pupil_timecourse_data() |>
    dplyr::mutate(condition = NA_character_)

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = "condition",
    bin_width_ms = 100
  )

  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  expect_true(length(built$data) >= 1L)
})

test_that("plot_gazepoint_pupil_timecourse supports faceting", {
  x <- make_test_pupil_timecourse_data()

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = "condition",
    facet_cols = "subject",
    bin_width_ms = 100
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_timecourse supports condition_col = NULL", {
  x <- make_test_pupil_timecourse_data()

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = NULL,
    bin_width_ms = 100
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_timecourse builds summary data correctly", {
  x <- make_test_pupil_timecourse_data()

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = "condition",
    bin_width_ms = 100
  )

  built <- ggplot2::ggplot_build(p)

  expect_true(length(built$data) >= 2L)
  expect_true(nrow(built$data[[2]]) > 0L)
})

test_that("plot_gazepoint_pupil_timecourse respects min_samples", {
  x <- make_test_pupil_timecourse_data()

  p <- plot_gazepoint_pupil_timecourse(
    x,
    pupil_col = "pupil_smoothed",
    time_col = "time",
    condition_col = "condition",
    bin_width_ms = 100,
    min_samples = 2
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_timecourse errors when no valid samples remain", {
  x <- make_test_pupil_timecourse_data() |>
    dplyr::mutate(pupil_smoothed = NA_real_)

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      pupil_col = "pupil_smoothed",
      time_col = "time",
      condition_col = "condition",
      bin_width_ms = 100
    ),
    "No valid pupil/time samples available to plot after filtering"
  )
})

test_that("plot_gazepoint_pupil_timecourse errors when pupil column cannot be detected", {
  x <- make_test_pupil_timecourse_data() |>
    dplyr::rename(not_pupil = pupil_smoothed)

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      pupil_col = NULL,
      time_col = "time",
      condition_col = "condition"
    ),
    "Could not automatically detect a pupil column"
  )
})

test_that("plot_gazepoint_pupil_timecourse errors when required columns are missing", {
  x <- make_test_pupil_timecourse_data()

  expect_error(
    plot_gazepoint_pupil_timecourse(
      dplyr::select(x, -time),
      pupil_col = "pupil_smoothed",
      time_col = "time"
    ),
    "Missing required columns"
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      pupil_col = "missing_pupil",
      time_col = "time"
    ),
    "Missing required columns"
  )
})

test_that("plot_gazepoint_pupil_timecourse errors for invalid inputs", {
  x <- make_test_pupil_timecourse_data()

  expect_error(
    plot_gazepoint_pupil_timecourse("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      facet_cols = c("subject", "subject")
    ),
    "`facet_cols` must be NULL or a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      bin_width_ms = 0
    ),
    "`bin_width_ms` must be greater than 0",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      ci_level = 1
    ),
    "`ci_level` must be greater than 0 and less than 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      min_samples = 0
    ),
    "`min_samples` must be at least 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_timecourse(
      x,
      band_alpha = 2
    ),
    "`band_alpha` must be between 0 and 1",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_pupil_timecourse works with real pipeline object when available", {
  if (exists("smoothed_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "smoothed_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "time",
      "pupil_smoothed"
    )

    if (all(required_cols %in% names(real_data))) {
      p <- plot_gazepoint_pupil_timecourse(
        real_data,
        pupil_col = "pupil_smoothed",
        time_col = "time",
        condition_col = "condition",
        bin_width_ms = 100,
        min_samples = 1
      )

      expect_s3_class(p, "ggplot")
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
