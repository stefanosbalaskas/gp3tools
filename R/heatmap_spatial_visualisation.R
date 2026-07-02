#' Prepare gaze or fixation coordinates for heatmap plotting
#'
#' `prepare_gazepoint_heatmap_data()` standardises gaze or fixation
#' coordinates for spatial heatmap visualisation. It supports normalised
#' Gazepoint-style coordinates in the range 0--1 and pixel coordinates.
#'
#' @param data A data frame containing gaze or fixation coordinates.
#' @param x_col,y_col Character strings giving the x and y coordinate columns.
#' @param weight_col Optional character string giving a non-negative weight
#'   column, such as fixation duration. If `NULL`, each point receives equal
#'   weight.
#' @param display_width,display_height Display width and height in pixels. For
#'   normalised coordinates, these values are used to convert coordinates to
#'   pixel space. If omitted for normalised coordinates, a unit display is used.
#'   If omitted for pixel coordinates, bounds are inferred from the observed
#'   coordinates.
#' @param coordinate_space One of `"auto"`, `"normalized"`, or `"pixel"`.
#'   `"auto"` treats coordinates as normalised only when all finite x and y
#'   values fall between 0 and 1.
#' @return A data frame with the original retained rows and standardised
#'   columns `.gp3_x_px`, `.gp3_y_px`, and `.gp3_weight`.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   x = c(0.20, 0.25, 0.75),
#'   y = c(0.30, 0.35, 0.60),
#'   duration = c(120, 200, 80)
#' )
#'
#' prepare_gazepoint_heatmap_data(
#'   gaze,
#'   x_col = "x",
#'   y_col = "y",
#'   weight_col = "duration",
#'   display_width = 1920,
#'   display_height = 1080
#' )
prepare_gazepoint_heatmap_data <- function(data,
                                           x_col,
                                           y_col,
                                           weight_col = NULL,
                                           display_width = NULL,
                                           display_height = NULL,
                                           coordinate_space = c("auto", "normalized", "pixel")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  x_col <- .gp3_heatmap_scalar_string(x_col, "x_col")
  y_col <- .gp3_heatmap_scalar_string(y_col, "y_col")
  weight_col <- .gp3_heatmap_scalar_string(weight_col, "weight_col", allow_null = TRUE)

  if (!x_col %in% names(data)) {
    stop("`x_col` was not found in `data`.", call. = FALSE)
  }
  if (!y_col %in% names(data)) {
    stop("`y_col` was not found in `data`.", call. = FALSE)
  }
  if (!is.null(weight_col) && !weight_col %in% names(data)) {
    stop("`weight_col` was not found in `data`.", call. = FALSE)
  }

  coordinate_space <- match.arg(coordinate_space)

  x <- suppressWarnings(as.numeric(data[[x_col]]))
  y <- suppressWarnings(as.numeric(data[[y_col]]))

  if (is.null(weight_col)) {
    weight <- rep(1, nrow(data))
  } else {
    weight <- suppressWarnings(as.numeric(data[[weight_col]]))
  }

  base_valid <- is.finite(x) & is.finite(y)

  if (!any(base_valid)) {
    stop("No finite gaze coordinates were found.", call. = FALSE)
  }

  if (coordinate_space == "auto") {
    x_valid <- x[base_valid]
    y_valid <- y[base_valid]

    is_normalized <- all(x_valid >= 0 & x_valid <= 1) &&
      all(y_valid >= 0 & y_valid <= 1)

    coordinate_space <- if (is_normalized) "normalized" else "pixel"
  }

  if (coordinate_space == "normalized") {
    display_width <- .gp3_heatmap_positive_number(
      display_width %||% 1,
      "display_width"
    )
    display_height <- .gp3_heatmap_positive_number(
      display_height %||% 1,
      "display_height"
    )

    x_px <- x * display_width
    y_px <- y * display_height

    coordinate_valid <- x >= 0 & x <= 1 & y >= 0 & y <= 1
  } else {
    if (is.null(display_width)) {
      display_width <- max(1, ceiling(max(x[base_valid], na.rm = TRUE)))
    }
    if (is.null(display_height)) {
      display_height <- max(1, ceiling(max(y[base_valid], na.rm = TRUE)))
    }

    display_width <- .gp3_heatmap_positive_number(display_width, "display_width")
    display_height <- .gp3_heatmap_positive_number(display_height, "display_height")

    x_px <- x
    y_px <- y

    coordinate_valid <- x >= 0 & x <= display_width &
      y >= 0 & y <= display_height
  }

  weight_valid <- is.finite(weight) & weight >= 0
  keep <- base_valid & coordinate_valid & weight_valid

  if (!any(keep)) {
    stop("No valid heatmap points remained after coordinate and weight checks.", call. = FALSE)
  }

  out <- data[keep, , drop = FALSE]
  out$.gp3_x_source <- x[keep]
  out$.gp3_y_source <- y[keep]
  out$.gp3_x_px <- x_px[keep]
  out$.gp3_y_px <- y_px[keep]
  out$.gp3_weight <- weight[keep]
  out$.gp3_coordinate_space <- coordinate_space
  out$.gp3_display_width <- display_width
  out$.gp3_display_height <- display_height

  class(out) <- unique(c("gp3_heatmap_data", class(out)))

  out
}


#' Plot a Gazepoint gaze or fixation heatmap
#'
#' `plot_gazepoint_heatmap()` creates a binned spatial heatmap from gaze or
#' fixation coordinates. Points may optionally be weighted by duration.
#'
#' @param data A data frame or an object returned by
#'   `prepare_gazepoint_heatmap_data()`.
#' @param x_col,y_col Character strings giving x and y coordinate columns. These
#'   may be omitted when `data` is already prepared by
#'   `prepare_gazepoint_heatmap_data()`.
#' @param weight_col Optional non-negative weight column, such as fixation
#'   duration.
#' @param display_width,display_height Display width and height in pixels.
#' @param coordinate_space One of `"auto"`, `"normalized"`, or `"pixel"`.
#' @param bins Number of bins. Either one integer or two integers for x and y.
#' @param alpha Heatmap layer transparency.
#' @param normalize Logical. If `TRUE`, bin intensities are scaled to the range
#'   0--1.
#' @param show_points Logical. If `TRUE`, raw points are added over the heatmap.
#' @param show_legend Logical. If `TRUE`, the fill legend is shown.
#' @return A ggplot object.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   x = c(0.20, 0.25, 0.27, 0.70, 0.75),
#'   y = c(0.30, 0.32, 0.34, 0.55, 0.60),
#'   duration = c(120, 180, 160, 90, 100)
#' )
#'
#' plot_gazepoint_heatmap(
#'   gaze,
#'   x_col = "x",
#'   y_col = "y",
#'   weight_col = "duration",
#'   display_width = 1920,
#'   display_height = 1080,
#'   bins = 20
#' )
plot_gazepoint_heatmap <- function(data,
                                   x_col = NULL,
                                   y_col = NULL,
                                   weight_col = NULL,
                                   display_width = NULL,
                                   display_height = NULL,
                                   coordinate_space = c("auto", "normalized", "pixel"),
                                   bins = 60,
                                   alpha = 0.85,
                                   normalize = TRUE,
                                   show_points = FALSE,
                                   show_legend = TRUE) {
  prepared <- .gp3_heatmap_prepare_or_use(
    data = data,
    x_col = x_col,
    y_col = y_col,
    weight_col = weight_col,
    display_width = display_width,
    display_height = display_height,
    coordinate_space = coordinate_space
  )

  grid <- .gp3_heatmap_grid(prepared, bins = bins, normalize = normalize)

  p <- ggplot2::ggplot(
    grid,
    ggplot2::aes(
      x = .gp3_x_tile,
      y = .gp3_y_tile,
      fill = .gp3_intensity
    )
  ) +
    ggplot2::geom_tile(
      width = attr(grid, "tile_width"),
      height = attr(grid, "tile_height"),
      alpha = .gp3_heatmap_alpha(alpha, "alpha"),
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, attr(grid, "display_width")),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_reverse(
      limits = c(attr(grid, "display_height"), 0),
      expand = c(0, 0)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = "X position",
      y = "Y position",
      fill = if (normalize) "Relative intensity" else "Weighted count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  if (isTRUE(show_points)) {
    p <- p +
      ggplot2::geom_point(
        data = prepared,
        ggplot2::aes(x = .gp3_x_px, y = .gp3_y_px),
        inherit.aes = FALSE,
        alpha = 0.25,
        size = 0.6
      )
  }

  if (!isTRUE(show_legend)) {
    p <- p + ggplot2::guides(fill = "none")
  }

  p
}


#' Plot a Gazepoint heatmap over a background image
#'
#' `plot_gazepoint_heatmap_overlay()` overlays a binned gaze or fixation
#' heatmap on a PNG background image, such as a stimulus screenshot. The `png`
#' package is used only when this helper is called.
#'
#' @param data A data frame or prepared heatmap data.
#' @param background_image Path to a PNG background image.
#' @param x_col,y_col Character strings giving x and y coordinate columns.
#' @param weight_col Optional non-negative weight column.
#' @param display_width,display_height Display width and height in pixels. If
#'   omitted, the PNG image dimensions are used.
#' @param coordinate_space One of `"auto"`, `"normalized"`, or `"pixel"`.
#' @param bins Number of heatmap bins. Either one integer or two integers.
#' @param heatmap_alpha Heatmap layer transparency.
#' @param background_alpha Background image transparency.
#' @param normalize Logical. If `TRUE`, bin intensities are scaled to 0--1.
#' @param show_legend Logical. If `TRUE`, the fill legend is shown.
#' @return A ggplot object.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   x = c(0.20, 0.25, 0.70),
#'   y = c(0.30, 0.35, 0.60),
#'   duration = c(120, 200, 80)
#' )
#'
#' if (requireNamespace("png", quietly = TRUE)) {
#'   bg <- tempfile(fileext = ".png")
#'   img <- array(1, dim = c(200, 300, 3))
#'   png::writePNG(img, bg)
#'
#'   plot_gazepoint_heatmap_overlay(
#'     gaze,
#'     background_image = bg,
#'     x_col = "x",
#'     y_col = "y",
#'     weight_col = "duration"
#'   )
#' }
plot_gazepoint_heatmap_overlay <- function(data,
                                           background_image,
                                           x_col = NULL,
                                           y_col = NULL,
                                           weight_col = NULL,
                                           display_width = NULL,
                                           display_height = NULL,
                                           coordinate_space = c("auto", "normalized", "pixel"),
                                           bins = 60,
                                           heatmap_alpha = 0.70,
                                           background_alpha = 1,
                                           normalize = TRUE,
                                           show_legend = TRUE) {
  background_image <- .gp3_heatmap_scalar_string(background_image, "background_image")

  if (!file.exists(background_image)) {
    stop("`background_image` does not exist.", call. = FALSE)
  }

  bg <- .gp3_heatmap_read_png(
    background_image,
    alpha = .gp3_heatmap_alpha(background_alpha, "background_alpha")
  )

  if (is.null(display_width)) {
    display_width <- bg$width
  }
  if (is.null(display_height)) {
    display_height <- bg$height
  }

  prepared <- .gp3_heatmap_prepare_or_use(
    data = data,
    x_col = x_col,
    y_col = y_col,
    weight_col = weight_col,
    display_width = display_width,
    display_height = display_height,
    coordinate_space = coordinate_space
  )

  grid <- .gp3_heatmap_grid(prepared, bins = bins, normalize = normalize)

  p <- ggplot2::ggplot() +
    ggplot2::annotation_raster(
      bg$raster,
      xmin = 0,
      xmax = attr(grid, "display_width"),
      ymin = 0,
      ymax = attr(grid, "display_height"),
      interpolate = TRUE
    ) +
    ggplot2::geom_tile(
      data = grid,
      ggplot2::aes(
        x = .gp3_x_tile,
        y = .gp3_y_tile,
        fill = .gp3_intensity
      ),
      width = attr(grid, "tile_width"),
      height = attr(grid, "tile_height"),
      alpha = .gp3_heatmap_alpha(heatmap_alpha, "heatmap_alpha"),
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, attr(grid, "display_width")),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_reverse(
      limits = c(attr(grid, "display_height"), 0),
      expand = c(0, 0)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = "X position",
      y = "Y position",
      fill = if (normalize) "Relative intensity" else "Weighted count"
    ) +
    ggplot2::theme_void()

  if (isTRUE(show_legend)) {
    p <- p +
      ggplot2::theme(
        legend.position = "right",
        legend.title = ggplot2::element_text(),
        legend.text = ggplot2::element_text()
      )
  } else {
    p <- p + ggplot2::guides(fill = "none")
  }

  p
}


#' Export a Gazepoint heatmap plot to PNG
#'
#' `export_gazepoint_heatmap_png()` saves a ggplot heatmap to a PNG file.
#'
#' @param plot A ggplot object, usually returned by
#'   `plot_gazepoint_heatmap()` or `plot_gazepoint_heatmap_overlay()`.
#' @param filename Output file path.
#' @param width,height Plot size passed to `ggplot2::ggsave()`.
#' @param units Units passed to `ggplot2::ggsave()`.
#' @param dpi Resolution passed to `ggplot2::ggsave()`.
#' @param create_dir Logical. If `TRUE`, the output directory is created when
#'   needed.
#' @param ... Additional arguments passed to `ggplot2::ggsave()`.
#' @return Invisibly returns the output path.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   x = c(0.20, 0.25, 0.70),
#'   y = c(0.30, 0.35, 0.60)
#' )
#'
#' p <- plot_gazepoint_heatmap(
#'   gaze,
#'   x_col = "x",
#'   y_col = "y",
#'   bins = 10
#' )
#'
#' out <- tempfile(fileext = ".png")
#' export_gazepoint_heatmap_png(p, out, width = 4, height = 3)
export_gazepoint_heatmap_png <- function(plot,
                                         filename,
                                         width = 8,
                                         height = 5,
                                         units = "in",
                                         dpi = 300,
                                         create_dir = TRUE,
                                         ...) {
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object.", call. = FALSE)
  }

  filename <- .gp3_heatmap_scalar_string(filename, "filename")

  out_dir <- dirname(filename)
  if (isTRUE(create_dir) && nzchar(out_dir) && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = FALSE,
    ...
  )

  invisible(normalizePath(filename, winslash = "/", mustWork = FALSE))
}


.gp3_heatmap_prepare_or_use <- function(data,
                                        x_col,
                                        y_col,
                                        weight_col,
                                        display_width,
                                        display_height,
                                        coordinate_space) {
  if (.gp3_heatmap_is_prepared(data) && is.null(x_col) && is.null(y_col)) {
    return(data)
  }

  if (is.null(x_col) || is.null(y_col)) {
    stop(
      "`x_col` and `y_col` are required unless `data` was created by ",
      "`prepare_gazepoint_heatmap_data()`.",
      call. = FALSE
    )
  }

  prepare_gazepoint_heatmap_data(
    data = data,
    x_col = x_col,
    y_col = y_col,
    weight_col = weight_col,
    display_width = display_width,
    display_height = display_height,
    coordinate_space = coordinate_space
  )
}


.gp3_heatmap_is_prepared <- function(data) {
  is.data.frame(data) &&
    all(c(
      ".gp3_x_px",
      ".gp3_y_px",
      ".gp3_weight",
      ".gp3_display_width",
      ".gp3_display_height"
    ) %in% names(data))
}


.gp3_heatmap_grid <- function(prepared, bins, normalize) {
  bins <- .gp3_heatmap_bins(bins)

  display_width <- unique(prepared$.gp3_display_width)
  display_height <- unique(prepared$.gp3_display_height)

  display_width <- .gp3_heatmap_positive_number(display_width[1], "display_width")
  display_height <- .gp3_heatmap_positive_number(display_height[1], "display_height")

  x_bin <- pmin(
    pmax(floor(prepared$.gp3_x_px / display_width * bins[1]) + 1L, 1L),
    bins[1]
  )
  y_bin <- pmin(
    pmax(floor(prepared$.gp3_y_px / display_height * bins[2]) + 1L, 1L),
    bins[2]
  )

  grid <- stats::aggregate(
    prepared$.gp3_weight,
    by = list(.gp3_x_bin = x_bin, .gp3_y_bin = y_bin),
    FUN = sum,
    na.rm = TRUE
  )

  names(grid)[names(grid) == "x"] <- ".gp3_intensity"

  grid$.gp3_x_tile <- (grid$.gp3_x_bin - 0.5) * display_width / bins[1]
  grid$.gp3_y_tile <- (grid$.gp3_y_bin - 0.5) * display_height / bins[2]

  if (isTRUE(normalize)) {
    max_intensity <- max(grid$.gp3_intensity, na.rm = TRUE)
    if (is.finite(max_intensity) && max_intensity > 0) {
      grid$.gp3_intensity <- grid$.gp3_intensity / max_intensity
    }
  }

  attr(grid, "display_width") <- display_width
  attr(grid, "display_height") <- display_height
  attr(grid, "tile_width") <- display_width / bins[1]
  attr(grid, "tile_height") <- display_height / bins[2]

  grid
}


.gp3_heatmap_bins <- function(bins) {
  if (!is.numeric(bins) || !length(bins) %in% c(1L, 2L)) {
    stop("`bins` must be one positive number or two positive numbers.", call. = FALSE)
  }

  bins <- as.integer(round(bins))

  if (length(bins) == 1L) {
    bins <- rep(bins, 2L)
  }

  if (any(!is.finite(bins)) || any(bins < 1L)) {
    stop("`bins` must contain positive finite values.", call. = FALSE)
  }

  bins
}


.gp3_heatmap_scalar_string <- function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }

  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a non-empty character string.", call. = FALSE)
  }

  x
}


.gp3_heatmap_positive_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be a positive finite number.", call. = FALSE)
  }

  x
}


.gp3_heatmap_alpha <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x > 1) {
    stop("`", name, "` must be a finite number between 0 and 1.", call. = FALSE)
  }

  x
}


.gp3_heatmap_read_png <- function(path, alpha = 1) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop(
      "The `png` package is required for background-image overlays. ",
      "Install it with install.packages(\"png\").",
      call. = FALSE
    )
  }

  img <- png::readPNG(path)

  if (length(dim(img)) == 2L) {
    h <- dim(img)[1]
    w <- dim(img)[2]

    img_rgb <- array(NA_real_, dim = c(h, w, 3L))
    img_rgb[, , 1L] <- img
    img_rgb[, , 2L] <- img
    img_rgb[, , 3L] <- img

    img <- img_rgb
  }

  if (length(dim(img)) != 3L || !dim(img)[3] %in% c(3L, 4L)) {
    stop("`background_image` must be a valid grayscale, RGB, or RGBA PNG file.", call. = FALSE)
  }

  h <- dim(img)[1]
  w <- dim(img)[2]

  if (dim(img)[3] == 3L) {
    img_rgba <- array(1, dim = c(h, w, 4L))
    img_rgba[, , 1:3] <- img
    img_rgba[, , 4L] <- alpha
    img <- img_rgba
  } else {
    img[, , 4L] <- img[, , 4L] * alpha
  }

  list(
    raster = grDevices::as.raster(img),
    width = w,
    height = h
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
