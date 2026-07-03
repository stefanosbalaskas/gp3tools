#' Plot a Gazepoint-style time series
#'
#' Creates a compact line plot for pupil, gaze, AOI, or other time-varying
#' Gazepoint-derived measures. The helper is intentionally descriptive: it does
#' not smooth, model, or infer effects unless the user has already prepared the
#' plotted values.
#'
#' @param data A data frame.
#' @param time_col Character name of the time column.
#' @param value_col Character name of the value column.
#' @param group_cols Optional character vector of grouping columns used to draw
#'   separate trajectories.
#' @param colour_col Optional character name of a column used for colour.
#' @param facet_col Optional character name of a column used for faceting.
#' @param alpha Line opacity.
#' @param linewidth Line width.
#' @param title Optional plot title.
#' @param x_label,y_label Optional axis labels.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' plot_gazepoint_time_series(
#'   x,
#'   time_col = "time_bin",
#'   value_col = "pupil",
#'   group_cols = c("subject", "trial"),
#'   colour_col = "condition"
#' )
plot_gazepoint_time_series <- function(data,
                                       time_col,
                                       value_col,
                                       group_cols = NULL,
                                       colour_col = NULL,
                                       facet_col = NULL,
                                       alpha = 0.55,
                                       linewidth = 0.4,
                                       title = NULL,
                                       x_label = NULL,
                                       y_label = NULL) {
  .gp3_require_data_frame(data, "data")

  required_cols <- c(time_col, value_col, group_cols, colour_col, facet_col)
  required_cols <- required_cols[!is.na(required_cols) & nzchar(required_cols)]
  .gp3_require_columns(data, required_cols, "data")

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(linewidth) || length(linewidth) != 1L || is.na(linewidth) || linewidth <= 0) {
    stop("`linewidth` must be a positive numeric value.", call. = FALSE)
  }

  plot_data <- data
  plot_data$.gp3_x <- plot_data[[time_col]]
  plot_data$.gp3_y <- suppressWarnings(as.numeric(plot_data[[value_col]]))

  if (is.null(group_cols) || length(group_cols) == 0L) {
    plot_data$.gp3_group <- "all"
  } else {
    plot_data$.gp3_group <- interaction(plot_data[group_cols], drop = TRUE, lex.order = TRUE)
  }

  if (is.null(colour_col)) {
    plot_data$.gp3_label <- "series"
  } else {
    plot_data$.gp3_label <- as.factor(plot_data[[colour_col]])
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .gp3_x,
      y = .gp3_y,
      group = .gp3_group,
      colour = .gp3_label
    )
  ) +
    ggplot2::geom_line(alpha = alpha, linewidth = linewidth, na.rm = TRUE) +
    ggplot2::labs(
      title = title,
      x = if (is.null(x_label)) time_col else x_label,
      y = if (is.null(y_label)) value_col else y_label,
      colour = if (is.null(colour_col)) NULL else colour_col
    ) +
    ggplot2::theme_minimal()

  if (is.null(colour_col)) {
    p <- p + ggplot2::guides(colour = "none")
  }

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)))
  }

  p
}


#' Plot multiple Gazepoint scanpaths
#'
#' Creates a descriptive multi-scanpath plot from gaze coordinates. This helper
#' is intended for visual quality review, participant/trial inspection, and
#' documentation examples. It should not be interpreted as an inferential
#' scanpath-comparison method.
#'
#' @param data A data frame.
#' @param x_col,y_col Character names of gaze coordinate columns.
#' @param order_col Optional column used to order gaze samples before plotting.
#' @param group_cols Optional character vector used to define separate paths.
#' @param colour_col Optional column used for colour.
#' @param facet_col Optional column used for faceting.
#' @param screen_width,screen_height Optional screen dimensions. If supplied,
#'   axis limits are set to the screen bounds.
#' @param reverse_y If `TRUE`, reverses the y-axis so the origin is displayed at
#'   the top-left, matching common screen-coordinate conventions.
#' @param show_points If `TRUE`, sample points are added on top of paths.
#' @param alpha Line opacity.
#' @param linewidth Line width.
#' @param point_size Point size when `show_points = TRUE`.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' plot_gazepoint_scanpaths(
#'   x,
#'   x_col = "gaze_x",
#'   y_col = "gaze_y",
#'   order_col = "time_bin",
#'   group_cols = c("subject", "trial"),
#'   colour_col = "condition"
#' )
plot_gazepoint_scanpaths <- function(data,
                                     x_col,
                                     y_col,
                                     order_col = NULL,
                                     group_cols = NULL,
                                     colour_col = NULL,
                                     facet_col = NULL,
                                     screen_width = NULL,
                                     screen_height = NULL,
                                     reverse_y = TRUE,
                                     show_points = TRUE,
                                     alpha = 0.45,
                                     linewidth = 0.4,
                                     point_size = 0.7,
                                     title = NULL) {
  .gp3_require_data_frame(data, "data")

  required_cols <- c(x_col, y_col, order_col, group_cols, colour_col, facet_col)
  required_cols <- required_cols[!is.na(required_cols) & nzchar(required_cols)]
  .gp3_require_columns(data, required_cols, "data")

  .gp3_require_plot_scalar(alpha, "alpha", lower = 0, upper = 1, allow_zero = TRUE)
  .gp3_require_plot_scalar(linewidth, "linewidth", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(point_size, "point_size", lower = 0, allow_zero = FALSE)

  plot_data <- data
  plot_data$.gp3_x <- suppressWarnings(as.numeric(plot_data[[x_col]]))
  plot_data$.gp3_y <- suppressWarnings(as.numeric(plot_data[[y_col]]))

  if (is.null(group_cols) || length(group_cols) == 0L) {
    plot_data$.gp3_group <- "all"
  } else {
    plot_data$.gp3_group <- interaction(plot_data[group_cols], drop = TRUE, lex.order = TRUE)
  }

  if (!is.null(order_col)) {
    plot_data <- plot_data[order(plot_data$.gp3_group, plot_data[[order_col]]), , drop = FALSE]
  }

  if (is.null(colour_col)) {
    plot_data$.gp3_label <- "scanpath"
  } else {
    plot_data$.gp3_label <- as.factor(plot_data[[colour_col]])
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .gp3_x,
      y = .gp3_y,
      group = .gp3_group,
      colour = .gp3_label
    )
  ) +
    ggplot2::geom_path(alpha = alpha, linewidth = linewidth, na.rm = TRUE) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = title,
      x = x_col,
      y = y_col,
      colour = if (is.null(colour_col)) NULL else colour_col
    ) +
    ggplot2::theme_minimal()

  if (isTRUE(show_points)) {
    p <- p + ggplot2::geom_point(alpha = alpha, size = point_size, na.rm = TRUE)
  }

  if (!is.null(screen_width)) {
    .gp3_require_plot_scalar(screen_width, "screen_width", lower = 0, allow_zero = FALSE)
    p <- p + ggplot2::xlim(0, screen_width)
  }

  if (!is.null(screen_height)) {
    .gp3_require_plot_scalar(screen_height, "screen_height", lower = 0, allow_zero = FALSE)

    if (isTRUE(reverse_y)) {
      p <- p + ggplot2::scale_y_reverse(limits = c(screen_height, 0))
    } else {
      p <- p + ggplot2::ylim(0, screen_height)
    }
  } else if (isTRUE(reverse_y)) {
    p <- p + ggplot2::scale_y_reverse()
  }

  if (is.null(colour_col)) {
    p <- p + ggplot2::guides(colour = "none")
  }

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)))
  }

  p
}


.gp3_require_plot_scalar <- function(x,
                                     arg,
                                     lower = NULL,
                                     upper = NULL,
                                     allow_zero = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single numeric value.", call. = FALSE)
  }

  if (!is.null(lower)) {
    if (isTRUE(allow_zero)) {
      bad_lower <- x < lower
    } else {
      bad_lower <- x <= lower
    }

    if (bad_lower) {
      stop("`", arg, "` is outside the allowed range.", call. = FALSE)
    }
  }

  if (!is.null(upper) && x > upper) {
    stop("`", arg, "` is outside the allowed range.", call. = FALSE)
  }

  invisible(TRUE)
}
