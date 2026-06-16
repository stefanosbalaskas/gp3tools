make_test_aoi_gamm_plot_data <- function(two_conditions = TRUE) {
  subjects <- paste0("S", 1:8)
  conditions <- if (two_conditions) c("control", "treatment") else "control"
  trials <- paste0("T", 1:3)
  times <- seq(0, 950, by = 50)

  dat <- expand.grid(
    subject = subjects,
    condition = conditions,
    trial_global = trials,
    time = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat <- tibble::as_tibble(dat)

  subject_num <- as.numeric(factor(dat$subject))
  time_scaled <- dat$time / max(dat$time)

  base_prob <- stats::plogis(-2 + 2 * sin(time_scaled * pi))

  treatment_boost <- ifelse(
    dat$condition == "treatment" & dat$time >= 300 & dat$time <= 650,
    0.25,
    0
  )

  subject_shift <- (subject_num - mean(subject_num)) * 0.015

  prob <- pmin(
    pmax(base_prob + treatment_boost + subject_shift, 0.02),
    0.98
  )

  dat$target_aoi <- prob > 0.35

  prepare_gazepoint_aoi_gamm_data(
    dat,
    outcome_col = "target_aoi",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 950),
    bin_size_ms = 50,
    min_denominator_samples = 1,
    outcome_label = "target_aoi"
  )
}

make_test_aoi_gamm_plot_fit <- function(two_conditions = TRUE) {
  dat <- make_test_aoi_gamm_plot_data(two_conditions = two_conditions)

  fit_gazepoint_aoi_gamm(
    dat,
    include_condition = TRUE,
    condition_smooths = TRUE,
    random_subject = TRUE,
    random_subject_time = FALSE,
    time_k = 6
  )
}

test_that("plot_gazepoint_aoi_gamm returns a ggplot for single-condition fits", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = 25,
    include_observed = TRUE,
    include_fitted = TRUE,
    show_ci = TRUE
  )

  expect_true(inherits(p, "ggplot"))

  prediction_data <- attr(p, "gp3_aoi_gamm_prediction_data")
  observed_data <- attr(p, "gp3_aoi_gamm_observed_data")
  plot_settings <- attr(p, "gp3_aoi_gamm_plot_settings")

  expect_s3_class(prediction_data, "tbl_df")
  expect_s3_class(observed_data, "tbl_df")
  expect_true(is.list(plot_settings))

  expect_equal(nrow(prediction_data), 25)
  expect_true(all(prediction_data$fit >= 0))
  expect_true(all(prediction_data$fit <= 1))
  expect_true(all(prediction_data$conf_low >= 0))
  expect_true(all(prediction_data$conf_high <= 1))

  expect_true(all(observed_data$proportion >= 0))
  expect_true(all(observed_data$proportion <= 1))

  expect_true(plot_settings$include_observed)
  expect_true(plot_settings$include_fitted)
  expect_true(plot_settings$show_ci)
})

test_that("plot_gazepoint_aoi_gamm returns a ggplot for two-condition fits", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = TRUE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = 25,
    include_observed = TRUE,
    include_fitted = TRUE,
    show_ci = TRUE
  )

  expect_true(inherits(p, "ggplot"))

  prediction_data <- attr(p, "gp3_aoi_gamm_prediction_data")
  observed_data <- attr(p, "gp3_aoi_gamm_observed_data")

  expect_equal(
    sort(unique(as.character(prediction_data$condition))),
    c("control", "treatment")
  )

  expect_equal(nrow(prediction_data), 2 * 25)

  expect_equal(
    sort(unique(as.character(observed_data$condition))),
    c("control", "treatment")
  )

  expect_equal(p$labels$colour, "Condition")
  expect_equal(p$labels$fill, "Condition")
})

test_that("plot_gazepoint_aoi_gamm supports fitted-only plots", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = 20,
    include_observed = FALSE,
    include_fitted = TRUE,
    show_ci = TRUE
  )

  expect_true(inherits(p, "ggplot"))
  expect_false(attr(p, "gp3_aoi_gamm_plot_settings")$include_observed)
  expect_true(attr(p, "gp3_aoi_gamm_plot_settings")$include_fitted)
  expect_equal(nrow(attr(p, "gp3_aoi_gamm_prediction_data")), 20)
})

test_that("plot_gazepoint_aoi_gamm supports observed-only plots", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    include_observed = TRUE,
    include_fitted = FALSE
  )

  expect_true(inherits(p, "ggplot"))
  expect_true(attr(p, "gp3_aoi_gamm_plot_settings")$include_observed)
  expect_false(attr(p, "gp3_aoi_gamm_plot_settings")$include_fitted)
  expect_equal(nrow(attr(p, "gp3_aoi_gamm_prediction_data")), 0)
  expect_gt(nrow(attr(p, "gp3_aoi_gamm_observed_data")), 0)
})

test_that("plot_gazepoint_aoi_gamm supports observed time bins as prediction grid", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = NULL,
    include_observed = TRUE,
    include_fitted = TRUE
  )

  prediction_data <- attr(p, "gp3_aoi_gamm_prediction_data")
  observed_bins <- sort(unique(fit$fit_data$.gp3_aoi_gamm_time_bin))

  expect_true(inherits(p, "ggplot"))
  expect_equal(sort(unique(prediction_data$time_bin)), observed_bins)
  expect_null(attr(p, "gp3_aoi_gamm_plot_settings")$n_time_points)
})

test_that("plot_gazepoint_aoi_gamm supports custom labels and y limits", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = 20,
    title = "Custom title",
    subtitle = "Custom subtitle",
    x_label = "Custom time",
    y_label = "Custom AOI probability",
    y_limits = c(0, 0.8)
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$labels$title, "Custom title")
  expect_equal(p$labels$subtitle, "Custom subtitle")
  expect_equal(p$labels$x, "Custom time")
  expect_equal(p$labels$y, "Custom AOI probability")
  expect_equal(attr(p, "gp3_aoi_gamm_plot_settings")$y_limits, c(0, 0.8))
})

test_that("plot_gazepoint_aoi_gamm can hide confidence intervals", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = 20,
    show_ci = FALSE
  )

  expect_true(inherits(p, "ggplot"))
  expect_false(attr(p, "gp3_aoi_gamm_plot_settings")$show_ci)
})

test_that("plot_gazepoint_aoi_gamm can include subject random effects", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  p <- plot_gazepoint_aoi_gamm(
    fit,
    n_time_points = 20,
    exclude_random_effects = FALSE
  )

  expect_true(inherits(p, "ggplot"))
  expect_false(attr(p, "gp3_aoi_gamm_plot_settings")$exclude_random_effects)
})

test_that("plot_gazepoint_aoi_gamm checks fit object structure", {
  expect_error(
    plot_gazepoint_aoi_gamm(data.frame(x = 1)),
    "`fit` is missing required element(s)",
    fixed = TRUE
  )

  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)
  fit$fit_data <- NULL

  expect_error(
    plot_gazepoint_aoi_gamm(fit),
    "`fit` is missing required element(s): fit_data",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_gamm rejects unsuccessful model fits", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)
  fit$model_status <- "error"
  fit["model"] <- list(NULL)

  expect_error(
    plot_gazepoint_aoi_gamm(fit),
    "`fit` does not contain a successfully fitted AOI-GAMM model",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_gamm checks fit_data columns", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)
  fit$fit_data$.gp3_aoi_gamm_success <- NULL

  expect_error(
    plot_gazepoint_aoi_gamm(fit),
    "`fit$fit_data` is missing required column(s): .gp3_aoi_gamm_success",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_gamm requires at least one plot layer", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  expect_error(
    plot_gazepoint_aoi_gamm(
      fit,
      include_observed = FALSE,
      include_fitted = FALSE
    ),
    "At least one of `include_observed` or `include_fitted` must be TRUE",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_gamm checks scalar arguments", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  expect_error(
    plot_gazepoint_aoi_gamm(fit, n_time_points = 0),
    "`n_time_points` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, include_observed = NA),
    "`include_observed` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, include_fitted = NA),
    "`include_fitted` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, show_ci = NA),
    "`show_ci` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, ci_level = 1),
    "`ci_level` must be between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, exclude_random_effects = NA),
    "`exclude_random_effects` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, point_size = 0),
    "`point_size` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, point_alpha = -0.1),
    "`point_alpha` must be a non-negative finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, point_alpha = 1.1),
    "`point_alpha` must be between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, ribbon_alpha = -0.1),
    "`ribbon_alpha` must be a non-negative finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, ribbon_alpha = 1.1),
    "`ribbon_alpha` must be between 0 and 1",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_gamm checks label and limit arguments", {
  fit <- make_test_aoi_gamm_plot_fit(two_conditions = FALSE)

  expect_error(
    plot_gazepoint_aoi_gamm(fit, title = NA_character_),
    "`title` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, subtitle = NA_character_),
    "`subtitle` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, x_label = ""),
    "`x_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, y_label = ""),
    "`y_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_gamm(fit, y_limits = c(1, 0)),
    "`y_limits` must be NULL or a finite numeric vector of length 2",
    fixed = TRUE
  )
})
