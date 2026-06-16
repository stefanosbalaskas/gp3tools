#' Plot observed and fitted Growth Curve Analysis trajectories
#'
#' Plot observed and fitted pupil trajectories from a `gp3_gca_model` object.
#' The function aggregates observed and fitted values by condition and time, and
#' returns a `ggplot2` object.
#'
#' @param model A fitted object returned by `fit_gazepoint_gca()`, or a data
#'   frame containing observed and fitted values.
#' @param data Optional data frame. If `NULL` and `model` is a
#'   `gp3_gca_model`, the model data are used.
#' @param time_col Name of the time column.
#' @param observed_col Name of the observed outcome column.
#' @param fitted_col Name of the fitted-value column. If unavailable and
#'   `model` is a `gp3_gca_model`, fitted values are computed from the model.
#' @param condition_col Name of the condition column.
#' @param subject_col Optional subject column, used only when `show_subjects =
#'   TRUE`.
#' @param summarise Logical. If `TRUE`, plot mean trajectories by condition and
#'   time. If `FALSE`, plot row-level values.
#' @param show_observed Logical. If `TRUE`, include observed trajectory.
#' @param show_fitted Logical. If `TRUE`, include fitted trajectory.
#' @param show_subjects Logical. If `TRUE`, add faint subject-level observed
#'   trajectories when a subject column is available.
#' @param interval Logical. If `TRUE`, add a standard-error ribbon around the
#'   observed mean trajectory when `summarise = TRUE`.
#' @param title Optional plot title.
#' @param point_size Point size for observed means.
#' @param line_width Line width for trajectories.
#' @param alpha Alpha value for observed points/lines.
#'
#' @return A `ggplot2` object.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_gca <- function(
    model,
    data = NULL,
    time_col = "gca_time",
    observed_col = "gca_pupil",
    fitted_col = "gca_fitted",
    condition_col = "condition",
    subject_col = "subject",
    summarise = TRUE,
    show_observed = TRUE,
    show_fitted = TRUE,
    show_subjects = FALSE,
    interval = TRUE,
    title = NULL,
    point_size = 1.6,
    line_width = 0.8,
    alpha = 0.75
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package `ggplot2` is required to plot GCA trajectories.",
      call. = FALSE
    )
  }

  valid_column <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_optional_column <- function(x, arg) {
    if (!is.null(x) &&
        (!is.character(x) ||
         length(x) != 1L ||
         is.na(x) ||
         !nzchar(x))) {
      stop(
        "`", arg, "` must be NULL or a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_column(time_col, "time_col")
  valid_column(observed_col, "observed_col")
  valid_column(fitted_col, "fitted_col")
  valid_optional_column(condition_col, "condition_col")
  valid_optional_column(subject_col, "subject_col")

  for (arg_name in c("summarise", "show_observed", "show_fitted",
                     "show_subjects", "interval")) {
    arg_value <- get(arg_name)

    if (!is.logical(arg_value) ||
        length(arg_value) != 1L ||
        is.na(arg_value)) {
      stop(
        "`", arg_name, "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }

  if (!show_observed && !show_fitted) {
    stop(
      "At least one of `show_observed` or `show_fitted` must be TRUE.",
      call. = FALSE
    )
  }

  if (!is.numeric(point_size) ||
      length(point_size) != 1L ||
      is.na(point_size) ||
      !is.finite(point_size) ||
      point_size <= 0) {
    stop("`point_size` must be a positive finite numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(line_width) ||
      length(line_width) != 1L ||
      is.na(line_width) ||
      !is.finite(line_width) ||
      line_width <= 0) {
    stop("`line_width` must be a positive finite numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1) {
    stop("`alpha` must be a finite numeric scalar in [0, 1].", call. = FALSE)
  }

  is_gca_model <- inherits(model, "gp3_gca_model")

  if (is.null(data)) {
    if (!is_gca_model) {
      if (!is.data.frame(model)) {
        stop(
          "`model` must be a `gp3_gca_model` object or a data frame when `data = NULL`.",
          call. = FALSE
        )
      }

      plot_data <- tibble::as_tibble(model)
    } else {
      if (is.null(model$data)) {
        stop(
          "`model$data` is missing, so plotting data could not be found.",
          call. = FALSE
        )
      }

      plot_data <- tibble::as_tibble(model$data)
    }
  } else {
    if (!is.data.frame(data)) {
      stop("`data` must be NULL or a data frame.", call. = FALSE)
    }

    plot_data <- tibble::as_tibble(data)
  }

  required_cols <- c(time_col, observed_col)

  if (!is.null(condition_col) && condition_col %in% names(plot_data)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (!is.null(subject_col) && show_subjects) {
    required_cols <- c(required_cols, subject_col)
  }

  missing_cols <- setdiff(unique(required_cols), names(plot_data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!fitted_col %in% names(plot_data)) {
    if (is_gca_model && !is.null(model$model)) {
      plot_data[[fitted_col]] <- as.numeric(
        stats::predict(model$model, newdata = plot_data, allow.new.levels = TRUE)
      )
    } else if (show_fitted) {
      stop(
        "Missing required columns: ",
        fitted_col,
        call. = FALSE
      )
    } else {
      plot_data[[fitted_col]] <- NA_real_
    }
  }

  plot_data$.gp3_plot_time <- suppressWarnings(as.numeric(plot_data[[time_col]]))
  plot_data$.gp3_plot_observed <- suppressWarnings(as.numeric(plot_data[[observed_col]]))
  plot_data$.gp3_plot_fitted <- suppressWarnings(as.numeric(plot_data[[fitted_col]]))

  if (!is.null(condition_col) && condition_col %in% names(plot_data)) {
    condition_values <- as.character(plot_data[[condition_col]])
    condition_values <- trimws(condition_values)
    condition_values[
      is.na(condition_values) |
        !nzchar(condition_values)
    ] <- "all_data"

    plot_data$.gp3_plot_condition <- condition_values
  } else {
    plot_data$.gp3_plot_condition <- "all_data"
  }

  if (!is.null(subject_col) && subject_col %in% names(plot_data)) {
    plot_data$.gp3_plot_subject <- as.character(plot_data[[subject_col]])
  } else {
    plot_data$.gp3_plot_subject <- NA_character_
  }

  plot_data <- plot_data |>
    dplyr::filter(
      is.finite(.data[[".gp3_plot_time"]]),
      is.finite(.data[[".gp3_plot_observed"]]) |
        is.finite(.data[[".gp3_plot_fitted"]])
    )

  if (nrow(plot_data) == 0L) {
    stop(
      "No rows remain after removing missing plotting values.",
      call. = FALSE
    )
  }

  if (summarise) {
    summary_data <- plot_data |>
      dplyr::group_by(
        .data[[".gp3_plot_condition"]],
        .data[[".gp3_plot_time"]]
      ) |>
      dplyr::summarise(
        observed = mean(.data[[".gp3_plot_observed"]], na.rm = TRUE),
        fitted = mean(.data[[".gp3_plot_fitted"]], na.rm = TRUE),
        observed_sd = stats::sd(.data[[".gp3_plot_observed"]], na.rm = TRUE),
        n = sum(is.finite(.data[[".gp3_plot_observed"]])),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        observed_se = dplyr::if_else(
          .data[["n"]] > 1,
          .data[["observed_sd"]] / sqrt(.data[["n"]]),
          NA_real_
        ),
        observed_lower = .data[["observed"]] - .data[["observed_se"]],
        observed_upper = .data[["observed"]] + .data[["observed_se"]]
      )
  } else {
    summary_data <- plot_data |>
      dplyr::transmute(
        .gp3_plot_condition = .data[[".gp3_plot_condition"]],
        .gp3_plot_time = .data[[".gp3_plot_time"]],
        observed = .data[[".gp3_plot_observed"]],
        fitted = .data[[".gp3_plot_fitted"]],
        observed_lower = NA_real_,
        observed_upper = NA_real_
      )
  }

  p <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(
      x = .data[[".gp3_plot_time"]],
      colour = .data[[".gp3_plot_condition"]],
      group = .data[[".gp3_plot_condition"]]
    )
  )

  if (show_subjects &&
      !is.null(subject_col) &&
      subject_col %in% names(plot_data)) {
    subject_data <- plot_data |>
      dplyr::filter(is.finite(.data[[".gp3_plot_observed"]]))

    p <- p +
      ggplot2::geom_line(
        data = subject_data,
        ggplot2::aes(
          x = .data[[".gp3_plot_time"]],
          y = .data[[".gp3_plot_observed"]],
          group = interaction(
            .data[[".gp3_plot_subject"]],
            .data[[".gp3_plot_condition"]],
            drop = TRUE
          ),
          colour = .data[[".gp3_plot_condition"]]
        ),
        linewidth = line_width * 0.35,
        alpha = 0.18,
        inherit.aes = FALSE
      )
  }

  if (summarise && interval && show_observed) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          y = .data[["observed"]],
          ymin = .data[["observed_lower"]],
          ymax = .data[["observed_upper"]],
          fill = .data[[".gp3_plot_condition"]]
        ),
        alpha = 0.12,
        colour = NA
      )
  }

  if (show_observed) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(y = .data[["observed"]]),
        size = point_size,
        alpha = alpha
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[["observed"]]),
        linewidth = line_width,
        alpha = alpha
      )
  }

  if (show_fitted) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(
          y = .data[["fitted"]],
          linetype = "Fitted"
        ),
        linewidth = line_width,
        alpha = 1
      )
  }

  plot_title <- if (is.null(title)) {
    "Growth Curve Analysis trajectory"
  } else {
    title
  }

  if (!is.character(plot_title) ||
      length(plot_title) != 1L ||
      is.na(plot_title) ||
      !nzchar(plot_title)) {
    stop(
      "`title` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  p +
    ggplot2::labs(
      title = plot_title,
      x = "Time",
      y = "Pupil",
      colour = "Condition",
      fill = "Condition",
      linetype = NULL
    ) +
    ggplot2::theme_minimal()
}
