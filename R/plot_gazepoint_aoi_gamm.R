#' Plot AOI time-course GAMM results
#'
#' Plot observed AOI target-looking proportions and fitted GAMM trajectories
#' from a model returned by `fit_gazepoint_aoi_gamm()`.
#'
#' The plot supports single-condition fallback models and multi-condition
#' AOI time-course GAMMs. By default, fitted trajectories are population-level
#' predictions with subject random-effect smooths excluded.
#'
#' @param fit A result object returned by `fit_gazepoint_aoi_gamm()`.
#' @param n_time_points Number of time points used for the fitted prediction
#'   grid. If `NULL`, the observed time bins are used.
#' @param include_observed Logical. If `TRUE`, plot observed binned
#'   proportions.
#' @param include_fitted Logical. If `TRUE`, plot fitted GAMM trajectories.
#' @param show_ci Logical. If `TRUE`, plot fitted confidence intervals.
#' @param ci_level Confidence level for fitted intervals.
#' @param exclude_random_effects Logical. If `TRUE`, exclude subject
#'   random-effect smooths from fitted predictions.
#' @param observed_summary Character. Currently `"pooled"` pools successes and
#'   denominators by condition and time.
#' @param point_size Size of observed points.
#' @param point_alpha Transparency for observed points.
#' @param line_width Width of fitted trajectory lines.
#' @param ribbon_alpha Transparency for fitted confidence ribbons.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#'
#' @return A `ggplot` object with prediction and observed data stored as
#'   attributes.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_aoi_gamm <- function(
    fit,
    n_time_points = 100,
    include_observed = TRUE,
    include_fitted = TRUE,
    show_ci = TRUE,
    ci_level = 0.95,
    exclude_random_effects = TRUE,
    observed_summary = c("pooled"),
    point_size = 1.8,
    point_alpha = 0.65,
    line_width = 0.8,
    ribbon_alpha = 0.15,
    title = NULL,
    subtitle = NULL,
    x_label = "Time (ms)",
    y_label = "Target AOI looking probability",
    y_limits = c(0, 1)
) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop(
      "Package `mgcv` is required to plot AOI GAMM predictions.",
      call. = FALSE
    )
  }

  if (!is.list(fit)) {
    stop("`fit` must be an AOI-GAMM fit object.", call. = FALSE)
  }

  required_elements <- c(
    "model",
    "fit_data",
    "model_status",
    "condition_status",
    "formula_text"
  )

  missing_elements <- setdiff(required_elements, names(fit))

  if (length(missing_elements) > 0L) {
    stop(
      "`fit` is missing required element(s): ",
      paste(missing_elements, collapse = ", "),
      call. = FALSE
    )
  }

  if (!identical(fit$model_status, "ok") || is.null(fit$model)) {
    stop(
      "`fit` does not contain a successfully fitted AOI-GAMM model.",
      call. = FALSE
    )
  }

  observed_summary <- match.arg(observed_summary)

  check_logical_scalar <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  check_positive_numeric <- function(x, arg, allow_null = FALSE) {
    if (is.null(x) && allow_null) {
      return(invisible(TRUE))
    }


    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x <= 0) {
      stop("`", arg, "` must be a positive finite numeric scalar.",
           call. = FALSE)
    }

    invisible(TRUE)


  }

  check_nonnegative_numeric <- function(x, arg) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x < 0) {
      stop(
        "`", arg, "` must be a non-negative finite numeric scalar.",
        call. = FALSE
      )
    }


    invisible(TRUE)


  }

  check_character_scalar <- function(x, arg, allow_null = FALSE) {
    if (is.null(x) && allow_null) {
      return(invisible(TRUE))
    }


    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop("`", arg, "` must be a non-missing character scalar.",
           call. = FALSE)
    }

    invisible(TRUE)


  }

  check_positive_numeric(n_time_points, "n_time_points", allow_null = TRUE)
  check_logical_scalar(include_observed, "include_observed")
  check_logical_scalar(include_fitted, "include_fitted")
  check_logical_scalar(show_ci, "show_ci")
  check_logical_scalar(exclude_random_effects, "exclude_random_effects")
  check_positive_numeric(ci_level, "ci_level")

  if (ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be between 0 and 1.", call. = FALSE)
  }

  check_positive_numeric(point_size, "point_size")
  check_nonnegative_numeric(point_alpha, "point_alpha")
  check_positive_numeric(line_width, "line_width")
  check_nonnegative_numeric(ribbon_alpha, "ribbon_alpha")

  if (point_alpha > 1) {
    stop("`point_alpha` must be between 0 and 1.", call. = FALSE)
  }

  if (ribbon_alpha > 1) {
    stop("`ribbon_alpha` must be between 0 and 1.", call. = FALSE)
  }

  check_character_scalar(title, "title", allow_null = TRUE)
  check_character_scalar(subtitle, "subtitle", allow_null = TRUE)
  check_character_scalar(x_label, "x_label")
  check_character_scalar(y_label, "y_label")

  if (!is.null(y_limits)) {
    if (!is.numeric(y_limits) ||
        length(y_limits) != 2L ||
        any(is.na(y_limits)) ||
        any(!is.finite(y_limits)) ||
        y_limits[[1L]] >= y_limits[[2L]]) {
      stop(
        "`y_limits` must be NULL or a finite numeric vector of length 2.",
        call. = FALSE
      )
    }
  }

  if (!include_observed && !include_fitted) {
    stop(
      "At least one of `include_observed` or `include_fitted` must be TRUE.",
      call. = FALSE
    )
  }

  fit_data <- tibble::as_tibble(fit$fit_data)

  required_fit_cols <- c(
    ".gp3_aoi_gamm_subject",
    ".gp3_aoi_gamm_condition",
    ".gp3_aoi_gamm_time_bin",
    ".gp3_aoi_gamm_success",
    ".gp3_aoi_gamm_failure",
    ".gp3_aoi_gamm_denominator"
  )

  missing_fit_cols <- setdiff(required_fit_cols, names(fit_data))

  if (length(missing_fit_cols) > 0L) {
    stop(
      "`fit$fit_data` is missing required column(s): ",
      paste(missing_fit_cols, collapse = ", "),
      call. = FALSE
    )
  }

  fit_data$.gp3_aoi_gamm_subject <- factor(
    fit_data$.gp3_aoi_gamm_subject
  )
  fit_data$.gp3_aoi_gamm_condition <- factor(
    fit_data$.gp3_aoi_gamm_condition
  )
  fit_data$.gp3_aoi_gamm_time_bin <- suppressWarnings(
    as.numeric(fit_data$.gp3_aoi_gamm_time_bin)
  )
  fit_data$.gp3_aoi_gamm_success <- suppressWarnings(
    as.numeric(fit_data$.gp3_aoi_gamm_success)
  )
  fit_data$.gp3_aoi_gamm_failure <- suppressWarnings(
    as.numeric(fit_data$.gp3_aoi_gamm_failure)
  )
  fit_data$.gp3_aoi_gamm_denominator <- suppressWarnings(
    as.numeric(fit_data$.gp3_aoi_gamm_denominator)
  )

  fit_data <- fit_data[
    is.finite(fit_data$.gp3_aoi_gamm_time_bin) &
      is.finite(fit_data$.gp3_aoi_gamm_success) &
      is.finite(fit_data$.gp3_aoi_gamm_failure) &
      is.finite(fit_data$.gp3_aoi_gamm_denominator) &
      fit_data$.gp3_aoi_gamm_denominator > 0,
    ,
    drop = FALSE
  ]

  if (nrow(fit_data) == 0L) {
    stop(
      "`fit$fit_data` does not contain any valid rows for plotting.",
      call. = FALSE
    )
  }

  condition_levels <- levels(fit_data$.gp3_aoi_gamm_condition)
  condition_levels <- condition_levels[!is.na(condition_levels)]

  if (length(condition_levels) == 0L) {
    condition_levels <- sort(unique(as.character(
      fit_data$.gp3_aoi_gamm_condition
    )))
  }

  subject_levels <- levels(fit_data$.gp3_aoi_gamm_subject)
  subject_levels <- subject_levels[!is.na(subject_levels)]

  if (length(subject_levels) == 0L) {
    subject_levels <- sort(unique(as.character(
      fit_data$.gp3_aoi_gamm_subject
    )))
  }

  observed_data <- fit_data |>
    dplyr::group_by(
      .data[[".gp3_aoi_gamm_condition"]],
      .data[[".gp3_aoi_gamm_time_bin"]]
    ) |>
    dplyr::summarise(
      success = sum(.data[[".gp3_aoi_gamm_success"]], na.rm = TRUE),
      failure = sum(.data[[".gp3_aoi_gamm_failure"]], na.rm = TRUE),
      denominator = sum(.data[[".gp3_aoi_gamm_denominator"]], na.rm = TRUE),
      n_subjects = dplyr::n_distinct(.data[[".gp3_aoi_gamm_subject"]]),
      .groups = "drop"
    )

  observed_data$proportion <- dplyr::if_else(
    observed_data$denominator > 0,
    observed_data$success / observed_data$denominator,
    NA_real_
  )

  observed_data <- observed_data[
    is.finite(observed_data$proportion),
    ,
    drop = FALSE
  ]

  names(observed_data)[
    names(observed_data) == ".gp3_aoi_gamm_condition"
  ] <- "condition"

  names(observed_data)[
    names(observed_data) == ".gp3_aoi_gamm_time_bin"
  ] <- "time_bin"

  observed_data$condition <- factor(
    observed_data$condition,
    levels = condition_levels
  )

  time_min <- min(fit_data$.gp3_aoi_gamm_time_bin, na.rm = TRUE)
  time_max <- max(fit_data$.gp3_aoi_gamm_time_bin, na.rm = TRUE)

  if (is.null(n_time_points)) {
    time_grid <- sort(unique(fit_data$.gp3_aoi_gamm_time_bin))
  } else {
    n_time_points <- as.integer(n_time_points)
    n_time_points <- max(2L, n_time_points)


    if (time_min == time_max) {
      time_grid <- time_min
    } else {
      time_grid <- seq(time_min, time_max, length.out = n_time_points)
    }


  }

  prediction_data <- tibble::tibble()

  if (include_fitted) {
    prediction_data <- expand.grid(
      .gp3_aoi_gamm_condition = condition_levels,
      .gp3_aoi_gamm_time_bin = time_grid,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )


    prediction_data <- tibble::as_tibble(prediction_data)

    prediction_data$.gp3_aoi_gamm_condition <- factor(
      prediction_data$.gp3_aoi_gamm_condition,
      levels = condition_levels
    )

    prediction_data$.gp3_aoi_gamm_subject <- factor(
      subject_levels[[1L]],
      levels = subject_levels
    )

    random_exclude <- character()

    if (exclude_random_effects &&
        !is.null(fit$model$smooth) &&
        length(fit$model$smooth) > 0L) {
      smooth_labels <- vapply(
        fit$model$smooth,
        function(x) x$label,
        character(1)
      )

      random_exclude <- smooth_labels[
        grepl(".gp3_aoi_gamm_subject", smooth_labels, fixed = TRUE)
      ]
    }

    prediction <- tryCatch(
      stats::predict(
        fit$model,
        newdata = prediction_data,
        type = "link",
        se.fit = TRUE,
        exclude = random_exclude
      ),
      error = function(e) {
        stop(
          "Could not create AOI-GAMM predictions: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    z_value <- stats::qnorm(1 - ((1 - ci_level) / 2))

    prediction_data$fit_link <- as.numeric(prediction$fit)
    prediction_data$se_link <- as.numeric(prediction$se.fit)
    prediction_data$fit <- fit$model$family$linkinv(prediction_data$fit_link)
    prediction_data$conf_low <- fit$model$family$linkinv(
      prediction_data$fit_link - z_value * prediction_data$se_link
    )
    prediction_data$conf_high <- fit$model$family$linkinv(
      prediction_data$fit_link + z_value * prediction_data$se_link
    )

    names(prediction_data)[
      names(prediction_data) == ".gp3_aoi_gamm_condition"
    ] <- "condition"

    names(prediction_data)[
      names(prediction_data) == ".gp3_aoi_gamm_time_bin"
    ] <- "time_bin"

    prediction_data$condition <- factor(
      prediction_data$condition,
      levels = condition_levels
    )


  }

  has_multiple_conditions <- length(condition_levels) >= 2L

  if (is.null(title)) {
    title <- "AOI time-course GAMM"
  }

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Model status: ",
      fit$model_status,
      "; condition status: ",
      fit$condition_status
    )
  }

  p <- ggplot2::ggplot()

  if (include_fitted && show_ci && nrow(prediction_data) > 0L) {
    if (has_multiple_conditions) {
      p <- p +
        ggplot2::geom_ribbon(
          data = prediction_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            ymin = .data[["conf_low"]],
            ymax = .data[["conf_high"]],
            fill = .data[["condition"]],
            group = .data[["condition"]]
          ),
          alpha = ribbon_alpha
        )
    } else {
      p <- p +
        ggplot2::geom_ribbon(
          data = prediction_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            ymin = .data[["conf_low"]],
            ymax = .data[["conf_high"]]
          ),
          alpha = ribbon_alpha
        )
    }
  }

  if (include_observed && nrow(observed_data) > 0L) {
    if (has_multiple_conditions) {
      p <- p +
        ggplot2::geom_point(
          data = observed_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            y = .data[["proportion"]],
            colour = .data[["condition"]]
          ),
          size = point_size,
          alpha = point_alpha
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = observed_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            y = .data[["proportion"]]
          ),
          size = point_size,
          alpha = point_alpha
        )
    }
  }

  if (include_fitted && nrow(prediction_data) > 0L) {
    if (has_multiple_conditions) {
      p <- p +
        ggplot2::geom_line(
          data = prediction_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            y = .data[["fit"]],
            colour = .data[["condition"]],
            group = .data[["condition"]]
          ),
          linewidth = line_width
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          data = prediction_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            y = .data[["fit"]]
          ),
          linewidth = line_width
        )
    }
  }

  plot_labels <- list(
    title = title,
    subtitle = subtitle,
    x = x_label,
    y = y_label
  )

  if (has_multiple_conditions) {
    plot_labels$colour <- "Condition"
    plot_labels$fill <- "Condition"
  }

  p <- p +
    do.call(ggplot2::labs, plot_labels) +
    ggplot2::theme_minimal()

  if (!is.null(y_limits)) {
    p <- p +
      ggplot2::coord_cartesian(ylim = y_limits)
  }

  attr(p, "gp3_aoi_gamm_prediction_data") <- prediction_data
  attr(p, "gp3_aoi_gamm_observed_data") <- observed_data
  attr(p, "gp3_aoi_gamm_plot_settings") <- list(
    n_time_points = n_time_points,
    include_observed = include_observed,
    include_fitted = include_fitted,
    show_ci = show_ci,
    ci_level = ci_level,
    exclude_random_effects = exclude_random_effects,
    observed_summary = observed_summary,
    point_size = point_size,
    point_alpha = point_alpha,
    line_width = line_width,
    ribbon_alpha = ribbon_alpha,
    y_limits = y_limits
  )

  p
}
