make_test_pupil_status_data <- function() {
  tibble::tibble(
    subject = rep("S1", 8),
    trial_global = rep("S1_T1", 8),
    condition = rep("A", 8),
    time = seq(0, 700, by = 100),
    pupil_smoothed = c(1.0, 1.1, NA, 1.3, 1.4, NA, 1.6, 1.7),
    pupil_interpolation_status = c(
      "observed",
      "observed",
      "missing_edge_gap",
      "interpolated",
      "observed",
      "missing_long_gap",
      "observed",
      "observed"
    ),
    pupil_was_interpolated = c(
      FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
    ),
    pupil_artifact_flag = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE
    )
  )
}

test_that("plot_gazepoint_pupil_status returns ggplot object for timeline plot", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "timeline",
    group_cols = c("subject", "trial_global")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status returns ggplot object for summary plot", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "summary",
    group_cols = c("subject", "trial_global")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status supports faceted timeline plots", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "timeline",
    group_cols = "trial_global",
    facet_cols = "subject"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status supports faceted summary plots", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "summary",
    group_cols = "trial_global",
    facet_cols = "subject"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status handles missing pupil values", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "summary",
    group_cols = "subject",
    pupil_col = "pupil_smoothed"
  )

  built <- ggplot2::ggplot_build(p)

  expect_true(length(built$data) >= 1L)
})

test_that("plot_gazepoint_pupil_status supports artifact reason column", {
  x <- make_test_pupil_status_data() |>
    dplyr::select(-pupil_artifact_flag) |>
    dplyr::mutate(
      pupil_artifact_reason = c(
        "valid",
        "valid",
        "valid",
        "valid",
        "valid",
        "blink",
        "valid",
        "valid"
      )
    )

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "summary",
    group_cols = "subject",
    artifact_reason_col = "pupil_artifact_reason"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status works without status column", {
  x <- make_test_pupil_status_data() |>
    dplyr::select(-pupil_interpolation_status)

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "summary",
    group_cols = "subject",
    pupil_col = "pupil_smoothed",
    interpolated_col = "pupil_was_interpolated",
    artifact_col = "pupil_artifact_flag"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status auto-detects pupil and artifact columns", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "summary",
    group_cols = "subject"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_status supports max_points thinning", {
  x <- make_test_pupil_status_data()

  p <- plot_gazepoint_pupil_status(
    x,
    plot_type = "timeline",
    group_cols = "trial_global",
    max_points = 3
  )

  built <- ggplot2::ggplot_build(p)

  expect_true(nrow(built$data[[1]]) <= 3L)
})

test_that("plot_gazepoint_pupil_status errors for invalid inputs", {
  x <- make_test_pupil_status_data()

  expect_error(
    plot_gazepoint_pupil_status("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_status(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_status(
      x,
      facet_cols = c("subject", "subject")
    ),
    "`facet_cols` must be NULL or a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_status(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_status(
      x,
      max_points = 0
    ),
    "`max_points` must be at least 1",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_pupil_status errors when required columns are missing", {
  x <- make_test_pupil_status_data()

  expect_error(
    plot_gazepoint_pupil_status(
      dplyr::select(x, -time),
      group_cols = "subject"
    ),
    "Missing required columns"
  )

  expect_error(
    plot_gazepoint_pupil_status(
      x,
      group_cols = "missing_subject"
    ),
    "Missing required columns"
  )
})

test_that("plot_gazepoint_pupil_status works with real pipeline object when available", {
  if (exists("smoothed_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "smoothed_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "subject",
      "trial_global",
      "time",
      "pupil_smoothed"
    )

    if (all(required_cols %in% names(real_data))) {
      p <- plot_gazepoint_pupil_status(
        real_data,
        plot_type = "summary",
        group_cols = "subject"
      )

      expect_s3_class(p, "ggplot")
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
