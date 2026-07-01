#' Compute basic saccade metrics from fixation coordinates
#'
#' Compute between-fixation displacement metrics from ordered fixation
#' coordinates. The function does not perform raw event detection; it assumes
#' that fixation-level coordinates are already available, for example from
#' Gazepoint fixation exports.
#'
#' @param data A fixation-level data frame.
#' @param x_col Name of the horizontal fixation-coordinate column.
#' @param y_col Name of the vertical fixation-coordinate column.
#' @param group_cols Optional columns defining independent scanpaths, such as
#'   participant and trial.
#' @param time_col Optional column used to order fixations and compute
#'   inter-fixation time differences.
#' @param start_time_col Optional fixation-start column.
#' @param end_time_col Optional fixation-end column. If both start and end
#'   columns are supplied, saccade duration is computed as the next fixation
#'   start minus the current fixation end.
#' @param distance_scale Multiplicative scale applied to coordinate distances.
#' @param drop_missing Should rows with missing coordinates be dropped?
#'
#' @return A data frame with one row per between-fixation movement.
#' @export
compute_gazepoint_saccade_metrics <- function(data,
                                             x_col,
                                             y_col,
                                             group_cols = NULL,
                                             time_col = NULL,
                                             start_time_col = NULL,
                                             end_time_col = NULL,
                                             distance_scale = 1,
                                             drop_missing = TRUE) {
  .gp3_ext_check_data(data)
  x_col <- .gp3_ext_check_scalar_string(x_col, "x_col")
  y_col <- .gp3_ext_check_scalar_string(y_col, "y_col")
  if (!is.null(group_cols)) {
    group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
  }
  if (!is.null(time_col)) {
    time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  }
  if (!is.null(start_time_col)) {
    start_time_col <- .gp3_ext_check_scalar_string(start_time_col, "start_time_col")
  }
  if (!is.null(end_time_col)) {
    end_time_col <- .gp3_ext_check_scalar_string(end_time_col, "end_time_col")
  }
  .gp3_ext_check_columns(data, c(x_col, y_col, group_cols, time_col, start_time_col, end_time_col))
  if (!is.numeric(distance_scale) || length(distance_scale) != 1L ||
      is.na(distance_scale) || distance_scale <= 0) {
    stop("distance_scale must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) {
    stop("x_col and y_col must identify numeric coordinate columns.", call. = FALSE)
  }

  groups <- .gp3_ext_split_groups(data, group_cols)
  rows <- lapply(groups, function(group_data) {
    group_data <- .gp3_ext_order_data(group_data, time_col)
    if (isTRUE(drop_missing)) {
      keep <- !is.na(group_data[[x_col]]) & !is.na(group_data[[y_col]])
      group_data <- group_data[keep, , drop = FALSE]
    }
    n_fix <- nrow(group_data)
    if (n_fix < 2L) {
      return(NULL)
    }
    x <- as.numeric(group_data[[x_col]])
    y <- as.numeric(group_data[[y_col]])
    dx <- x[-1L] - x[-n_fix]
    dy <- y[-1L] - y[-n_fix]
    amp <- sqrt(dx^2 + dy^2) * distance_scale
    angle_rad <- atan2(dy, dx)
    angle_deg <- angle_rad * 180 / pi

    time_delta <- rep(NA_real_, n_fix - 1L)
    time_kind <- rep(NA_character_, n_fix - 1L)
    if (!is.null(start_time_col) && !is.null(end_time_col)) {
      start_time <- as.numeric(group_data[[start_time_col]])
      end_time <- as.numeric(group_data[[end_time_col]])
      time_delta <- start_time[-1L] - end_time[-n_fix]
      time_kind <- rep("next_start_minus_current_end", n_fix - 1L)
    } else if (!is.null(time_col)) {
      time_value <- as.numeric(group_data[[time_col]])
      time_delta <- time_value[-1L] - time_value[-n_fix]
      time_kind <- rep("successive_time_difference", n_fix - 1L)
    }
    speed <- ifelse(!is.na(time_delta) & time_delta > 0, amp / time_delta, NA_real_)

    metrics <- data.frame(
      saccade_index = seq_len(n_fix - 1L),
      from_fixation_index = seq_len(n_fix - 1L),
      to_fixation_index = seq_len(n_fix - 1L) + 1L,
      from_x = x[-n_fix],
      from_y = y[-n_fix],
      to_x = x[-1L],
      to_y = y[-1L],
      dx = dx,
      dy = dy,
      saccade_amplitude = amp,
      saccade_angle_rad = angle_rad,
      saccade_angle_deg = angle_deg,
      time_delta = time_delta,
      time_delta_kind = time_kind,
      saccade_speed = speed,
      n_fixations = n_fix,
      saccade_status = "ok",
      stringsAsFactors = FALSE
    )
    group_values <- .gp3_ext_group_values(group_data, group_cols)
    if (ncol(group_values) > 0L) {
      group_values <- group_values[rep(1L, nrow(metrics)), , drop = FALSE]
    }
    cbind(group_values, metrics)
  })
  out <- .gp3_ext_bind_rows(rows)
  rownames(out) <- NULL
  out
}

#' Plot a fixation scanpath
#'
#' Plot fixation coordinates connected in temporal order. This is a static
#' ggplot2 scanpath helper for fixation-level Gazepoint exports. It does not
#' require a stimulus image, but the returned plot can be extended by users
#' with additional ggplot2 layers.
#'
#' @param data A fixation-level data frame.
#' @param x_col Name of the horizontal fixation-coordinate column.
#' @param y_col Name of the vertical fixation-coordinate column.
#' @param group_cols Optional columns defining scanpaths.
#' @param time_col Optional column used to order fixations.
#' @param fixation_index_col Optional fixation index column used for labels.
#' @param label_col Optional label column. If supplied, labels use this column.
#' @param point_size Size of fixation points.
#' @param line_width Width of connecting path lines.
#' @param show_order Should fixation order labels be drawn?
#' @param add_arrows Should arrows be added to scanpath lines?
#' @param reverse_y Should the y-axis be reversed for screen-coordinate plots?
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_gazepoint_scanpath <- function(data,
                                   x_col,
                                   y_col,
                                   group_cols = NULL,
                                   time_col = NULL,
                                   fixation_index_col = NULL,
                                   label_col = NULL,
                                   point_size = 2,
                                   line_width = 0.4,
                                   show_order = TRUE,
                                   add_arrows = TRUE,
                                   reverse_y = FALSE,
                                   title = NULL) {
  .gp3_ext_check_data(data)
  x_col <- .gp3_ext_check_scalar_string(x_col, "x_col")
  y_col <- .gp3_ext_check_scalar_string(y_col, "y_col")
  if (!is.null(group_cols)) {
    group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
  }
  if (!is.null(time_col)) {
    time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  }
  if (!is.null(fixation_index_col)) {
    fixation_index_col <- .gp3_ext_check_scalar_string(fixation_index_col, "fixation_index_col")
  }
  if (!is.null(label_col)) {
    label_col <- .gp3_ext_check_scalar_string(label_col, "label_col")
  }
  .gp3_ext_check_columns(data, c(x_col, y_col, group_cols, time_col, fixation_index_col, label_col))
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]])) {
    stop("x_col and y_col must identify numeric coordinate columns.", call. = FALSE)
  }

  d <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]), , drop = FALSE]
  d <- .gp3_ext_order_data(d, time_col)
  if (nrow(d) == 0L) {
    stop("No non-missing fixation coordinates are available for plotting.", call. = FALSE)
  }
  d$.gp3_x <- as.numeric(d[[x_col]])
  d$.gp3_y <- as.numeric(d[[y_col]])
  d$.gp3_group <- if (is.null(group_cols)) {
    "all"
  } else {
    apply(d[, group_cols, drop = FALSE], 1L, function(z) paste(paste(group_cols, z, sep = "="), collapse = "|"))
  }
  d$.gp3_order <- ave(seq_len(nrow(d)), d$.gp3_group, FUN = seq_along)
  if (!is.null(label_col)) {
    d$.gp3_label <- as.character(d[[label_col]])
  } else if (!is.null(fixation_index_col)) {
    d$.gp3_label <- as.character(d[[fixation_index_col]])
  } else {
    d$.gp3_label <- as.character(d$.gp3_order)
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .gp3_x, y = .gp3_y))
  if (isTRUE(add_arrows)) {
    p <- p + ggplot2::geom_path(
      ggplot2::aes(group = .gp3_group),
      linewidth = line_width,
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")
    )
  } else {
    p <- p + ggplot2::geom_path(ggplot2::aes(group = .gp3_group), linewidth = line_width)
  }
  p <- p + ggplot2::geom_point(size = point_size)
  if (isTRUE(show_order)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .gp3_label),
      vjust = -0.7,
      size = 3
    )
  }
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = title,
      x = x_col,
      y = y_col
    ) +
    ggplot2::theme_minimal()
  if (isTRUE(reverse_y)) {
    p <- p + ggplot2::scale_y_reverse()
  }
  p
}
