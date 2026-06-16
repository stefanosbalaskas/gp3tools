#' Plot observed summaries and model-implied predictions
#'
#' Create a reporting plot that overlays observed outcome summaries with
#' fitted/model-predicted trajectories. The helper is intentionally generic and
#' can be used with linear models, GLMs, mixed models, GAMMs, GCA-style models,
#' AOI GLMMs, and pupil LMMs when the fitted object supports `predict()`.
#'
#' @param data A data frame containing the observed data.
#' @param model Optional fitted model object. If supplied, predictions are
#'   computed using `predict()`.
#' @param x_col Column used on the x-axis, usually time or time bin.
#' @param outcome_col Observed outcome column.
#' @param condition_col Optional condition column used for colour/grouping.
#' @param group_cols Optional additional grouping columns for observed and
#'   predicted trajectories.
#' @param facet_cols Optional columns used for faceting.
#' @param newdata Optional prediction grid. If `NULL`, predictions are computed
#'   on `data` and then summarised by x/group/facet.
#' @param prediction_type Prediction scale passed to `predict()`. Common values
#'   are `"response"` and `"link"`.
#' @param include_random_effects Logical. For `lme4` mixed models, `FALSE`
#'   requests population-level predictions via `re.form = NA`; `TRUE` includes
#'   conditional random effects where possible.
#' @param observed_summary_function Summary for observed outcomes. Options are
#'   `"mean"` and `"median"`.
#' @param ci Confidence level for observed and prediction intervals when
#'   standard errors are available.
#' @param show_observed Logical. Plot observed summaries.
#' @param show_observed_ci Logical. Plot observed normal-approximation intervals.
#' @param show_predictions Logical. Plot model predictions when `model` is
#'   supplied.
#' @param show_prediction_ci Logical. Plot prediction intervals when standard
#'   errors are available from `predict()`.
#' @param point_alpha Alpha value for observed points.
#' @param line_width Line width for prediction trajectories.
#' @param name Character label stored in plot attributes.
#'
#' @return A `ggplot` object with attributes containing the observed summary,
#'   prediction summary, overview, and settings.
#' @export
plot_gazepoint_model_predictions <- function(
    data,
    model = NULL,
    x_col,
    outcome_col,
    condition_col = NULL,
    group_cols = NULL,
    facet_cols = NULL,
    newdata = NULL,
    prediction_type = c("response", "link"),
    include_random_effects = FALSE,
    observed_summary_function = c("mean", "median"),
    ci = 0.95,
    show_observed = TRUE,
    show_observed_ci = TRUE,
    show_predictions = TRUE,
    show_prediction_ci = TRUE,
    point_alpha = 0.55,
    line_width = 1,
    name = "gazepoint_model_predictions"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  prediction_type <- match.arg(prediction_type)
  observed_summary_function <- match.arg(observed_summary_function)

  .gp3_pred_check_col(x_col, names(data), "x_col")
  .gp3_pred_check_col(outcome_col, names(data), "outcome_col")

  if (!is.null(condition_col)) {
    .gp3_pred_check_col(condition_col, names(data), "condition_col")
  }

  .gp3_pred_check_cols(group_cols, names(data), "group_cols")
  .gp3_pred_check_cols(facet_cols, names(data), "facet_cols")

  .gp3_pred_check_logical(include_random_effects, "include_random_effects")
  .gp3_pred_check_logical(show_observed, "show_observed")
  .gp3_pred_check_logical(show_observed_ci, "show_observed_ci")
  .gp3_pred_check_logical(show_predictions, "show_predictions")
  .gp3_pred_check_logical(show_prediction_ci, "show_prediction_ci")
  .gp3_pred_check_ci(ci)
  .gp3_pred_check_nonnegative_number(point_alpha, "point_alpha")
  .gp3_pred_check_positive_number(line_width, "line_width")
  .gp3_pred_check_label(name, "name")

  plot_group_cols <- unique(c(condition_col, group_cols))
  facet_cols <- unique(facet_cols)

  observed_summary <- .gp3_pred_observed_summary(
    data = data,
    x_col = x_col,
    outcome_col = outcome_col,
    plot_group_cols = plot_group_cols,
    facet_cols = facet_cols,
    observed_summary_function = observed_summary_function,
    ci = ci
  )

  prediction_summary <- NULL
  prediction_status <- "not_requested"

  if (!is.null(model) && isTRUE(show_predictions)) {
    prediction_data <- if (is.null(newdata)) {
      tibble::as_tibble(data)
    } else {
      if (!is.data.frame(newdata)) {
        stop("`newdata` must be NULL or a data frame.", call. = FALSE)
      }

      if (nrow(newdata) == 0L) {
        stop("`newdata` must contain at least one row.", call. = FALSE)
      }

      tibble::as_tibble(newdata)
    }

    .gp3_pred_check_col(x_col, names(prediction_data), "x_col in prediction data")
    .gp3_pred_check_cols(plot_group_cols, names(prediction_data), "group columns in prediction data")
    .gp3_pred_check_cols(facet_cols, names(prediction_data), "facet columns in prediction data")

    prediction_result <- .gp3_pred_predict_model(
      model = model,
      newdata = prediction_data,
      prediction_type = prediction_type,
      include_random_effects = include_random_effects,
      ci = ci
    )

    if (inherits(prediction_result, "gp3_prediction_error")) {
      prediction_status <- "prediction_error"
      prediction_summary <- tibble::tibble()
      prediction_error <- prediction_result$error
    } else {
      prediction_status <- prediction_result$status
      prediction_error <- NA_character_

      prediction_summary <- .gp3_pred_prediction_summary(
        prediction_data = prediction_data,
        prediction_result = prediction_result$predictions,
        x_col = x_col,
        plot_group_cols = plot_group_cols,
        facet_cols = facet_cols
      )
    }
  } else if (is.null(model)) {
    prediction_status <- "no_model_supplied"
    prediction_error <- NA_character_
  } else {
    prediction_status <- "predictions_hidden"
    prediction_error <- NA_character_
  }

  plot <- .gp3_pred_build_plot(
    observed_summary = observed_summary,
    prediction_summary = prediction_summary,
    x_col = x_col,
    outcome_col = outcome_col,
    plot_group_cols = plot_group_cols,
    facet_cols = facet_cols,
    show_observed = show_observed,
    show_observed_ci = show_observed_ci,
    show_predictions = show_predictions,
    show_prediction_ci = show_prediction_ci,
    point_alpha = point_alpha,
    line_width = line_width
  )

  overview <- tibble::tibble(
    object_name = name,
    plot_type = "model_predictions",
    prediction_status = prediction_status,
    prediction_error = prediction_error,
    x_col = x_col,
    outcome_col = outcome_col,
    condition_col = .gp3_pred_collapse_nullable(condition_col),
    group_cols = .gp3_pred_collapse_nullable(group_cols),
    facet_cols = .gp3_pred_collapse_nullable(facet_cols),
    n_input_rows = nrow(data),
    n_observed_summary_rows = nrow(observed_summary),
    n_prediction_summary_rows = if (is.null(prediction_summary)) 0L else nrow(prediction_summary),
    prediction_type = prediction_type,
    include_random_effects = include_random_effects,
    observed_summary_function = observed_summary_function,
    ci = ci
  )

  settings <- tibble::tibble(
    setting = c(
      "x_col",
      "outcome_col",
      "condition_col",
      "group_cols",
      "facet_cols",
      "newdata",
      "prediction_type",
      "include_random_effects",
      "observed_summary_function",
      "ci",
      "show_observed",
      "show_observed_ci",
      "show_predictions",
      "show_prediction_ci",
      "point_alpha",
      "line_width",
      "name"
    ),
    value = c(
      x_col,
      outcome_col,
      .gp3_pred_collapse_nullable(condition_col),
      .gp3_pred_collapse_nullable(group_cols),
      .gp3_pred_collapse_nullable(facet_cols),
      if (is.null(newdata)) "data" else "supplied_newdata",
      prediction_type,
      as.character(include_random_effects),
      observed_summary_function,
      as.character(ci),
      as.character(show_observed),
      as.character(show_observed_ci),
      as.character(show_predictions),
      as.character(show_prediction_ci),
      as.character(point_alpha),
      as.character(line_width),
      name
    )
  )

  attr(plot, "gp3_model_prediction_overview") <- overview
  attr(plot, "gp3_model_prediction_observed_summary") <- observed_summary
  attr(plot, "gp3_model_prediction_prediction_summary") <- prediction_summary
  attr(plot, "gp3_model_prediction_settings") <- settings

  class(plot) <- c("gp3_model_prediction_plot", class(plot))

  plot
}

.gp3_pred_observed_summary <- function(
    data,
    x_col,
    outcome_col,
    plot_group_cols,
    facet_cols,
    observed_summary_function,
    ci
) {
  summary_fun <- switch(
    observed_summary_function,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) stats::median(x, na.rm = TRUE)
  )

  z <- stats::qnorm(1 - (1 - ci) / 2)

  data2 <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_x = suppressWarnings(as.numeric(.data[[x_col]])),
      .gp3_outcome = suppressWarnings(as.numeric(.data[[outcome_col]]))
    ) |>
    dplyr::filter(
      is.finite(.data$.gp3_x),
      is.finite(.data$.gp3_outcome)
    )

  data2 <- .gp3_pred_add_plot_group(data2, plot_group_cols)

  group_vars <- c(".gp3_x", ".gp3_plot_group", facet_cols)

  data2 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      observed = summary_fun(.data$.gp3_outcome),
      observed_sd = stats::sd(.data$.gp3_outcome, na.rm = TRUE),
      observed_n = dplyr::n(),
      observed_se = .data$observed_sd / sqrt(.data$observed_n),
      observed_lower = .data$observed - z * .data$observed_se,
      observed_upper = .data$observed + z * .data$observed_se,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$.gp3_plot_group, .data$.gp3_x)
}

.gp3_pred_prediction_summary <- function(
    prediction_data,
    prediction_result,
    x_col,
    plot_group_cols,
    facet_cols
) {
  pred_data <- tibble::as_tibble(prediction_data) |>
    dplyr::mutate(
      .gp3_x = suppressWarnings(as.numeric(.data[[x_col]])),
      .gp3_prediction = prediction_result$fit,
      .gp3_prediction_se = prediction_result$se,
      .gp3_prediction_lower = prediction_result$lower,
      .gp3_prediction_upper = prediction_result$upper
    ) |>
    dplyr::filter(
      is.finite(.data$.gp3_x),
      is.finite(.data$.gp3_prediction)
    )

  pred_data <- .gp3_pred_add_plot_group(pred_data, plot_group_cols)

  group_vars <- c(".gp3_x", ".gp3_plot_group", facet_cols)

  pred_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      predicted = mean(.data$.gp3_prediction, na.rm = TRUE),
      predicted_lower = if (all(is.na(.data$.gp3_prediction_lower))) {
        NA_real_
      } else {
        mean(.data$.gp3_prediction_lower, na.rm = TRUE)
      },
      predicted_upper = if (all(is.na(.data$.gp3_prediction_upper))) {
        NA_real_
      } else {
        mean(.data$.gp3_prediction_upper, na.rm = TRUE)
      },
      prediction_n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$.gp3_plot_group, .data$.gp3_x)
}

.gp3_pred_predict_model <- function(
    model,
    newdata,
    prediction_type,
    include_random_effects,
    ci
) {
  z <- stats::qnorm(1 - (1 - ci) / 2)

  if (inherits(model, "merMod")) {
    pred <- tryCatch(
      stats::predict(
        model,
        newdata = newdata,
        type = prediction_type,
        re.form = if (isTRUE(include_random_effects)) NULL else NA
      ),
      error = function(e) .gp3_pred_error(conditionMessage(e))
    )

    if (inherits(pred, "gp3_prediction_error")) {
      return(pred)
    }

    fit <- as.numeric(pred)

    return(list(
      status = "complete_without_prediction_se",
      predictions = tibble::tibble(
        fit = fit,
        se = NA_real_,
        lower = NA_real_,
        upper = NA_real_
      )
    ))
  }

  pred <- tryCatch(
    stats::predict(
      model,
      newdata = newdata,
      type = prediction_type,
      se.fit = TRUE
    ),
    error = function(e) .gp3_pred_error(conditionMessage(e))
  )

  if (!inherits(pred, "gp3_prediction_error")) {
    if (is.list(pred) && !is.null(pred$fit)) {
      fit <- as.numeric(pred$fit)
      se <- if (!is.null(pred$se.fit)) as.numeric(pred$se.fit) else rep(NA_real_, length(fit))

      return(list(
        status = if (all(is.na(se))) "complete_without_prediction_se" else "complete",
        predictions = tibble::tibble(
          fit = fit,
          se = se,
          lower = ifelse(is.na(se), NA_real_, fit - z * se),
          upper = ifelse(is.na(se), NA_real_, fit + z * se)
        )
      ))
    }

    fit <- as.numeric(pred)

    return(list(
      status = "complete_without_prediction_se",
      predictions = tibble::tibble(
        fit = fit,
        se = NA_real_,
        lower = NA_real_,
        upper = NA_real_
      )
    ))
  }

  pred_no_se <- tryCatch(
    stats::predict(
      model,
      newdata = newdata,
      type = prediction_type
    ),
    error = function(e) .gp3_pred_error(conditionMessage(e))
  )

  if (inherits(pred_no_se, "gp3_prediction_error")) {
    return(pred_no_se)
  }

  fit <- as.numeric(pred_no_se)

  list(
    status = "complete_without_prediction_se",
    predictions = tibble::tibble(
      fit = fit,
      se = NA_real_,
      lower = NA_real_,
      upper = NA_real_
    )
  )
}

.gp3_pred_build_plot <- function(
    observed_summary,
    prediction_summary,
    x_col,
    outcome_col,
    plot_group_cols,
    facet_cols,
    show_observed,
    show_observed_ci,
    show_predictions,
    show_prediction_ci,
    point_alpha,
    line_width
) {
  p <- ggplot2::ggplot()

  if (isTRUE(show_prediction_ci) && !is.null(prediction_summary) && nrow(prediction_summary) > 0L) {
    if (any(is.finite(prediction_summary$predicted_lower)) && any(is.finite(prediction_summary$predicted_upper))) {
      p <- p +
        ggplot2::geom_ribbon(
          data = prediction_summary,
          ggplot2::aes(
            x = .data$.gp3_x,
            ymin = .data$predicted_lower,
            ymax = .data$predicted_upper,
            fill = .data$.gp3_plot_group,
            group = .data$.gp3_plot_group
          ),
          alpha = 0.15,
          colour = NA
        )
    }
  }

  if (isTRUE(show_observed) && isTRUE(show_observed_ci) && nrow(observed_summary) > 0L) {
    p <- p +
      ggplot2::geom_errorbar(
        data = observed_summary,
        ggplot2::aes(
          x = .data$.gp3_x,
          ymin = .data$observed_lower,
          ymax = .data$observed_upper,
          colour = .data$.gp3_plot_group,
          group = .data$.gp3_plot_group
        ),
        alpha = 0.25,
        width = 0
      )
  }

  if (isTRUE(show_observed) && nrow(observed_summary) > 0L) {
    p <- p +
      ggplot2::geom_point(
        data = observed_summary,
        ggplot2::aes(
          x = .data$.gp3_x,
          y = .data$observed,
          colour = .data$.gp3_plot_group,
          group = .data$.gp3_plot_group
        ),
        alpha = point_alpha
      ) +
      ggplot2::geom_line(
        data = observed_summary,
        ggplot2::aes(
          x = .data$.gp3_x,
          y = .data$observed,
          colour = .data$.gp3_plot_group,
          group = .data$.gp3_plot_group
        ),
        alpha = 0.35,
        linewidth = 0.5
      )
  }

  if (isTRUE(show_predictions) && !is.null(prediction_summary) && nrow(prediction_summary) > 0L) {
    p <- p +
      ggplot2::geom_line(
        data = prediction_summary,
        ggplot2::aes(
          x = .data$.gp3_x,
          y = .data$predicted,
          colour = .data$.gp3_plot_group,
          group = .data$.gp3_plot_group
        ),
        linewidth = line_width
      )
  }

  p <- p +
    ggplot2::labs(
      x = x_col,
      y = outcome_col,
      colour = .gp3_pred_legend_title(plot_group_cols),
      fill = .gp3_pred_legend_title(plot_group_cols)
    ) +
    ggplot2::theme_minimal()

  if (length(facet_cols) > 0L) {
    p <- p +
      ggplot2::facet_wrap(stats::as.formula(
        paste("~", paste(facet_cols, collapse = " + "))
      ))
  }

  p
}

.gp3_pred_add_plot_group <- function(data, plot_group_cols) {
  if (length(plot_group_cols) == 0L) {
    data$.gp3_plot_group <- "All observations"
    return(data)
  }

  data$.gp3_plot_group <- as.character(
    do.call(
      interaction,
      c(data[plot_group_cols], list(drop = TRUE, sep = " | "))
    )
  )

  data
}

.gp3_pred_legend_title <- function(plot_group_cols) {
  if (length(plot_group_cols) == 0L) {
    return("Series")
  }

  paste(plot_group_cols, collapse = " / ")
}

.gp3_pred_error <- function(message) {
  structure(
    list(error = .gp3_pred_clean_error_message(message)),
    class = "gp3_prediction_error"
  )
}

.gp3_pred_clean_error_message <- function(x) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.gp3_pred_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pred_check_cols <- function(cols, names_data, arg) {
  if (is.null(cols)) {
    return(invisible(TRUE))
  }

  if (!is.character(cols) || anyNA(cols) || any(!nzchar(cols))) {
    stop("`", arg, "` must be NULL or a character vector of column names.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop(
      "`", arg, "` contains column(s) not present in the relevant data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_pred_check_ci <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0 || x >= 1) {
    stop("`ci` must be a finite number between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pred_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pred_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a positive finite number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pred_check_nonnegative_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`", arg, "` must be a finite non-negative number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pred_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pred_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
