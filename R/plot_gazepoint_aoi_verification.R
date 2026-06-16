#' Plot AOI geometry for visual verification
#'
#' Create a visual verification plot of AOI rectangles, with optional gaze
#' samples overlaid.
#'
#' @param aoi_geometry A data frame containing AOI geometry definitions.
#' @param gaze_data Optional data frame containing gaze samples to overlay.
#' @param geometry_aoi_col AOI label/name column in `aoi_geometry`.
#' @param geometry_stimulus_col Optional stimulus/media column in
#'   `aoi_geometry`.
#' @param x_min_col Optional AOI left/x-min column.
#' @param y_min_col Optional AOI top/y-min column.
#' @param x_max_col Optional AOI right/x-max column.
#' @param y_max_col Optional AOI bottom/y-max column.
#' @param x_col Optional AOI left/x column used with `width_col`.
#' @param y_col Optional AOI top/y column used with `height_col`.
#' @param width_col Optional AOI width column.
#' @param height_col Optional AOI height column.
#' @param gaze_x_col Optional gaze x-coordinate column.
#' @param gaze_y_col Optional gaze y-coordinate column.
#' @param gaze_stimulus_col Optional gaze stimulus/media column.
#' @param screen_x_range Numeric length-2 vector defining the screen x range.
#' @param screen_y_range Numeric length-2 vector defining the screen y range.
#' @param facet_by_stimulus Logical. If `TRUE`, facet by stimulus/media when a
#'   stimulus column is available.
#' @param show_labels Logical. If `TRUE`, draw AOI labels at AOI centres.
#' @param show_gaze Logical. If `TRUE`, overlay gaze samples when `gaze_data` is
#'   supplied.
#' @param invert_y Logical. If `TRUE`, reverse the y-axis so screen origin is at
#'   the top-left.
#' @param point_alpha Alpha transparency for gaze points.
#' @param point_size Size of gaze points.
#' @param line_width Width of AOI rectangle borders.
#' @param label_size Size of AOI labels.
#'
#' @return A `ggplot` object.
#' @export
plot_gazepoint_aoi_verification <- function(
    aoi_geometry,
    gaze_data = NULL,
    geometry_aoi_col = NULL,
    geometry_stimulus_col = NULL,
    x_min_col = NULL,
    y_min_col = NULL,
    x_max_col = NULL,
    y_max_col = NULL,
    x_col = NULL,
    y_col = NULL,
    width_col = NULL,
    height_col = NULL,
    gaze_x_col = NULL,
    gaze_y_col = NULL,
    gaze_stimulus_col = NULL,
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1),
    facet_by_stimulus = TRUE,
    show_labels = TRUE,
    show_gaze = TRUE,
    invert_y = TRUE,
    point_alpha = 0.35,
    point_size = 1.2,
    line_width = 0.8,
    label_size = 3
) {
  if (!is.data.frame(aoi_geometry)) {
    stop("`aoi_geometry` must be a data frame.", call. = FALSE)
  }

  if (nrow(aoi_geometry) == 0L) {
    stop("`aoi_geometry` must contain at least one row.", call. = FALSE)
  }

  if (!is.null(gaze_data) && !is.data.frame(gaze_data)) {
    stop("`gaze_data` must be NULL or a data frame.", call. = FALSE)
  }

  .gp3_aoi_verification_check_logical_scalar(
    facet_by_stimulus,
    "facet_by_stimulus"
  )
  .gp3_aoi_verification_check_logical_scalar(show_labels, "show_labels")
  .gp3_aoi_verification_check_logical_scalar(show_gaze, "show_gaze")
  .gp3_aoi_verification_check_logical_scalar(invert_y, "invert_y")

  .gp3_aoi_verification_check_nonnegative_numeric(
    point_alpha,
    "point_alpha"
  )
  .gp3_aoi_verification_check_nonnegative_numeric(
    point_size,
    "point_size"
  )
  .gp3_aoi_verification_check_nonnegative_numeric(
    line_width,
    "line_width"
  )
  .gp3_aoi_verification_check_nonnegative_numeric(
    label_size,
    "label_size"
  )

  if (point_alpha > 1) {
    stop("`point_alpha` must be between 0 and 1.", call. = FALSE)
  }

  geometry_audit <- audit_gazepoint_aoi_geometry(
    data = aoi_geometry,
    aoi_col = geometry_aoi_col,
    stimulus_col = geometry_stimulus_col,
    x_min_col = x_min_col,
    y_min_col = y_min_col,
    x_max_col = x_max_col,
    y_max_col = y_max_col,
    x_col = x_col,
    y_col = y_col,
    width_col = width_col,
    height_col = height_col,
    screen_x_range = screen_x_range,
    screen_y_range = screen_y_range,
    require_within_screen = FALSE
  )

  geometry_summary <- geometry_audit$geometry_summary

  resolved_geometry_aoi_col <- geometry_audit$settings$value[
    geometry_audit$settings$setting == "aoi_col"
  ]

  resolved_geometry_stimulus_col <- geometry_audit$settings$value[
    geometry_audit$settings$setting == "stimulus_col"
  ]

  if (is.na(resolved_geometry_stimulus_col) ||
      !nzchar(resolved_geometry_stimulus_col)) {
    resolved_geometry_stimulus_col <- NULL
  }

  geometry_plot <- .gp3_aoi_verification_prepare_geometry(
    geometry_summary = geometry_summary,
    aoi_col = resolved_geometry_aoi_col,
    stimulus_col = resolved_geometry_stimulus_col
  )

  gaze_plot <- NULL

  if (isTRUE(show_gaze) && !is.null(gaze_data) && nrow(gaze_data) > 0L) {
    gaze_plot <- .gp3_aoi_verification_prepare_gaze(
      gaze_data = gaze_data,
      gaze_x_col = gaze_x_col,
      gaze_y_col = gaze_y_col,
      gaze_stimulus_col = gaze_stimulus_col,
      geometry_stimulus_col = resolved_geometry_stimulus_col
    )
  }

  p <- ggplot2::ggplot()

  if (!is.null(gaze_plot)) {
    p <- p +
      ggplot2::geom_point(
        data = gaze_plot,
        ggplot2::aes(
          x = .data$gaze_x,
          y = .data$gaze_y
        ),
        alpha = point_alpha,
        size = point_size
      )
  }

  p <- p +
    ggplot2::geom_rect(
      data = geometry_plot,
      ggplot2::aes(
        xmin = .data$x_min,
        ymin = .data$y_min,
        xmax = .data$x_max,
        ymax = .data$y_max,
        colour = .data$aoi_label
      ),
      fill = NA,
      linewidth = line_width
    )

  if (isTRUE(show_labels)) {
    p <- p +
      ggplot2::geom_text(
        data = geometry_plot,
        ggplot2::aes(
          x = .data$center_x,
          y = .data$center_y,
          label = .data$aoi_label,
          colour = .data$aoi_label
        ),
        size = label_size,
        show.legend = FALSE
      )
  }

  if (isTRUE(facet_by_stimulus) &&
      "stimulus_label" %in% names(geometry_plot) &&
      length(unique(geometry_plot$stimulus_label)) > 1L) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(.data$stimulus_label)
      )
  }

  p <- p +
    ggplot2::coord_fixed(
      xlim = screen_x_range,
      ylim = screen_y_range,
      expand = FALSE
    ) +
    ggplot2::labs(
      title = "AOI verification plot",
      x = "X coordinate",
      y = "Y coordinate",
      colour = "AOI"
    ) +
    ggplot2::theme_minimal()

  if (isTRUE(invert_y)) {
    p <- p +
      ggplot2::scale_y_reverse(
        limits = rev(screen_y_range)
      )
  }

  p
}

.gp3_aoi_verification_prepare_geometry <- function(
    geometry_summary,
    aoi_col,
    stimulus_col
) {
  out <- tibble::tibble(
    aoi_label = as.character(geometry_summary[[aoi_col]]),
    x_min = geometry_summary$x_min,
    y_min = geometry_summary$y_min,
    x_max = geometry_summary$x_max,
    y_max = geometry_summary$y_max,
    center_x = geometry_summary$center_x,
    center_y = geometry_summary$center_y,
    aoi_geometry_status = geometry_summary$aoi_geometry_status
  )

  if (!is.null(stimulus_col) && stimulus_col %in% names(geometry_summary)) {
    out$stimulus_label <- as.character(geometry_summary[[stimulus_col]])
  }

  out
}

.gp3_aoi_verification_prepare_gaze <- function(
    gaze_data,
    gaze_x_col,
    gaze_y_col,
    gaze_stimulus_col,
    geometry_stimulus_col
) {
  gaze_data <- .gp3_aoi_verification_standardise_gaze_aliases(gaze_data)

  gaze_x_col <- .gp3_aoi_verification_resolve_or_detect_col(
    col = gaze_x_col,
    names_data = names(gaze_data),
    arg = "gaze_x_col",
    candidates = c("x", "X", "gaze_x", "gaze_x_norm", "FPOGX", "BPOGX"),
    required = TRUE
  )

  gaze_y_col <- .gp3_aoi_verification_resolve_or_detect_col(
    col = gaze_y_col,
    names_data = names(gaze_data),
    arg = "gaze_y_col",
    candidates = c("y", "Y", "gaze_y", "gaze_y_norm", "FPOGY", "BPOGY"),
    required = TRUE
  )

  gaze_stimulus_col <- .gp3_aoi_verification_resolve_or_detect_col(
    col = gaze_stimulus_col,
    names_data = names(gaze_data),
    arg = "gaze_stimulus_col",
    candidates = c("media_id", "MEDIA_ID", "stimulus", "stimulus_id"),
    required = FALSE
  )

  out <- tibble::tibble(
    gaze_x = suppressWarnings(as.numeric(gaze_data[[gaze_x_col]])),
    gaze_y = suppressWarnings(as.numeric(gaze_data[[gaze_y_col]]))
  )

  if (!is.null(gaze_stimulus_col) && !is.null(geometry_stimulus_col)) {
    out$stimulus_label <- as.character(gaze_data[[gaze_stimulus_col]])
  }

  out
}

.gp3_aoi_verification_standardise_gaze_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  data
}

.gp3_aoi_verification_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_aoi_verification_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_aoi_verification_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    if (found[[1]] == "MEDIA_ID" && "media_id" %in% names_data) {
      return("media_id")
    }

    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_aoi_verification_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_verification_check_nonnegative_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0) {
    stop("`", arg, "` must be a non-negative numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}
