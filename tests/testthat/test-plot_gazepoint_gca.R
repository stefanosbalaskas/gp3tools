make_test_gca_plot_data <- function() {
  base <- expand.grid(
    subject = paste0("S", 1:4),
    condition = c("A", "B"),
    gca_time = seq(0, 900, by = 100),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  base <- tibble::as_tibble(base) |>
    dplyr::arrange(subject, condition, gca_time)

  base |>
    dplyr::mutate(
      gca_pupil =
        0.20 +
        ifelse(condition == "B", 0.15, 0) +
        sin(gca_time / 250) * 0.20 +
        as.numeric(factor(subject)) * 0.02,
      gca_fitted =
        0.20 +
        ifelse(condition == "B", 0.15, 0) +
        sin(gca_time / 250) * 0.18
    )
}

make_test_gca_plot_model <- function() {
  x <- make_test_gca_plot_data()

  out <- list(
    data = x,
    model = NULL,
    model_status = "ok"
  )

  class(out) <- c("gp3_gca_model", "list")

  out
}

test_that("plot_gazepoint_gca plots a gp3_gca_model object", {
  testthat::skip_if_not_installed("ggplot2")

  model <- make_test_gca_plot_model()

  p <- plot_gazepoint_gca(model)

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca plots a data frame directly", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data()

  p <- plot_gazepoint_gca(x)

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca supports observed-only plots", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data() |>
    dplyr::select(-gca_fitted)

  p <- plot_gazepoint_gca(
    x,
    show_observed = TRUE,
    show_fitted = FALSE
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca supports fitted-only plots", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data()

  p <- plot_gazepoint_gca(
    x,
    show_observed = FALSE,
    show_fitted = TRUE
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca supports unsummarised row-level plotting", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data()

  p <- plot_gazepoint_gca(
    x,
    summarise = FALSE
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca supports subject-level observed trajectories", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data()

  p <- plot_gazepoint_gca(
    x,
    show_subjects = TRUE
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca works without a condition column", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data() |>
    dplyr::select(-condition)

  p <- plot_gazepoint_gca(
    x,
    condition_col = NULL
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca supports custom column names", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data() |>
    dplyr::rename(
      time_ms = gca_time,
      pupil_observed = gca_pupil,
      pupil_predicted = gca_fitted,
      group = condition,
      participant = subject
    )

  p <- plot_gazepoint_gca(
    x,
    time_col = "time_ms",
    observed_col = "pupil_observed",
    fitted_col = "pupil_predicted",
    condition_col = "group",
    subject_col = "participant"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_gca supports a custom title", {
  testthat::skip_if_not_installed("ggplot2")

  x <- make_test_gca_plot_data()

  p <- plot_gazepoint_gca(
    x,
    title = "Custom GCA plot"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Custom GCA plot")
})

test_that("plot_gazepoint_gca errors for invalid model input", {
  expect_error(
    plot_gazepoint_gca("not a model"),
    "`model` must be a `gp3_gca_model` object or a data frame when `data = NULL`.",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors when data argument is invalid", {
  model <- make_test_gca_plot_model()

  expect_error(
    plot_gazepoint_gca(
      model,
      data = "not a data frame"
    ),
    "`data` must be NULL or a data frame.",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors when both observed and fitted are disabled", {
  x <- make_test_gca_plot_data()

  expect_error(
    plot_gazepoint_gca(
      x,
      show_observed = FALSE,
      show_fitted = FALSE
    ),
    "At least one of `show_observed` or `show_fitted` must be TRUE.",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors for invalid scalar arguments", {
  x <- make_test_gca_plot_data()

  expect_error(
    plot_gazepoint_gca(x, time_col = NA_character_),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, observed_col = NA_character_),
    "`observed_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, fitted_col = NA_character_),
    "`fitted_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, condition_col = NA_character_),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, subject_col = NA_character_),
    "`subject_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, summarise = NA),
    "`summarise` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, show_observed = NA),
    "`show_observed` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, show_fitted = NA),
    "`show_fitted` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, show_subjects = NA),
    "`show_subjects` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, interval = NA),
    "`interval` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors for invalid plotting numeric arguments", {
  x <- make_test_gca_plot_data()

  expect_error(
    plot_gazepoint_gca(x, point_size = 0),
    "`point_size` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, line_width = 0),
    "`line_width` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, alpha = -0.1),
    "`alpha` must be a finite numeric scalar in [0, 1]",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, alpha = 1.1),
    "`alpha` must be a finite numeric scalar in [0, 1]",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(x, title = NA_character_),
    "`title` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors when required columns are missing", {
  x <- make_test_gca_plot_data()

  expect_error(
    plot_gazepoint_gca(
      dplyr::select(x, -gca_time)
    ),
    "Missing required columns: gca_time",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(
      dplyr::select(x, -gca_pupil)
    ),
    "Missing required columns: gca_pupil",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_gca(
      dplyr::select(x, -subject),
      show_subjects = TRUE
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors when fitted values are unavailable", {
  x <- make_test_gca_plot_data() |>
    dplyr::select(-gca_fitted)

  expect_error(
    plot_gazepoint_gca(
      x,
      show_fitted = TRUE
    ),
    "Missing required columns: gca_fitted",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca errors when no rows remain after filtering", {
  x <- make_test_gca_plot_data() |>
    dplyr::mutate(
      gca_time = NA_real_,
      gca_pupil = NA_real_,
      gca_fitted = NA_real_
    )

  expect_error(
    plot_gazepoint_gca(x),
    "No rows remain after removing missing plotting values.",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_gca works with real pupil_gca_fit object when available", {
  testthat::skip_if_not_installed("ggplot2")

  if (exists("pupil_gca_fit", envir = .GlobalEnv, inherits = TRUE)) {
    real_fit <- get("pupil_gca_fit", envir = .GlobalEnv, inherits = TRUE)

    if (inherits(real_fit, "gp3_gca_model")) {
      p <- plot_gazepoint_gca(real_fit)

      expect_s3_class(p, "ggplot")
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
