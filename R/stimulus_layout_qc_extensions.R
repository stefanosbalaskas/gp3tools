#' Audit AOI coverage against screen bounds
#'
#' Checks rectangular AOIs against declared screen or stimulus dimensions. The
#' helper reports missing geometry, invalid rectangle order, off-screen AOIs,
#' raw AOI area, clipped on-screen area, and screen-coverage rates. Coverage
#' summaries are descriptive and do not correct for AOI overlap.
#'
#' @param data A data frame containing AOI geometry.
#' @param screen_width,screen_height Numeric screen or stimulus dimensions.
#' @param aoi_col Optional AOI identifier column.
#' @param x_min_col,x_max_col,y_min_col,y_max_col Character names of rectangle
#'   boundary columns.
#' @param margin Numeric tolerance around screen bounds.
#'
#' @return A list with AOI-level summary, overall summary, and settings.
#' @export
#'
#' @examples
#' aoi <- data.frame(
#'   aoi = c("left", "right"),
#'   x_min = c(100, 1200),
#'   x_max = c(500, 1700),
#'   y_min = c(100, 100),
#'   y_max = c(400, 400)
#' )
#' audit_gazepoint_aoi_screen_coverage(aoi, 1920, 1080, aoi_col = "aoi")
audit_gazepoint_aoi_screen_coverage <- function(data,
                                                screen_width,
                                                screen_height,
                                                aoi_col = NULL,
                                                x_min_col = "x_min",
                                                x_max_col = "x_max",
                                                y_min_col = "y_min",
                                                y_max_col = "y_max",
                                                margin = 0) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, c(aoi_col, x_min_col, x_max_col, y_min_col, y_max_col), "data")
  .gp3_require_plot_scalar(screen_width, "screen_width", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(screen_height, "screen_height", lower = 0, allow_zero = FALSE)

  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) || margin < 0) {
    stop("`margin` must be a single non-negative numeric value.", call. = FALSE)
  }

  x_min <- suppressWarnings(as.numeric(data[[x_min_col]]))
  x_max <- suppressWarnings(as.numeric(data[[x_max_col]]))
  y_min <- suppressWarnings(as.numeric(data[[y_min_col]]))
  y_max <- suppressWarnings(as.numeric(data[[y_max_col]]))

  if (is.null(aoi_col)) {
    aoi_id <- paste0("AOI_", seq_len(nrow(data)))
  } else {
    aoi_id <- as.character(data[[aoi_col]])
  }

  missing_geometry <- !is.finite(x_min) | !is.finite(x_max) |
    !is.finite(y_min) | !is.finite(y_max)

  invalid_rectangle <- !missing_geometry & (x_max <= x_min | y_max <= y_min)

  offscreen_left <- !missing_geometry & x_min < -margin
  offscreen_right <- !missing_geometry & x_max > screen_width + margin
  offscreen_top <- !missing_geometry & y_min < -margin
  offscreen_bottom <- !missing_geometry & y_max > screen_height + margin
  outside_screen <- offscreen_left | offscreen_right | offscreen_top | offscreen_bottom

  width <- ifelse(missing_geometry | invalid_rectangle, NA_real_, x_max - x_min)
  height <- ifelse(missing_geometry | invalid_rectangle, NA_real_, y_max - y_min)
  raw_area <- width * height

  clipped_x_min <- pmax(0, pmin(screen_width, x_min))
  clipped_x_max <- pmax(0, pmin(screen_width, x_max))
  clipped_y_min <- pmax(0, pmin(screen_height, y_min))
  clipped_y_max <- pmax(0, pmin(screen_height, y_max))

  clipped_width <- pmax(0, clipped_x_max - clipped_x_min)
  clipped_height <- pmax(0, clipped_y_max - clipped_y_min)
  clipped_area <- clipped_width * clipped_height
  clipped_area[missing_geometry | invalid_rectangle] <- NA_real_

  screen_area <- screen_width * screen_height

  aoi_summary <- data.frame(
    aoi_id = aoi_id,
    x_min = x_min,
    x_max = x_max,
    y_min = y_min,
    y_max = y_max,
    width = width,
    height = height,
    raw_area = raw_area,
    clipped_area = clipped_area,
    raw_screen_coverage = raw_area / screen_area,
    clipped_screen_coverage = clipped_area / screen_area,
    missing_geometry = missing_geometry,
    invalid_rectangle = invalid_rectangle,
    outside_screen = outside_screen,
    offscreen_left = offscreen_left,
    offscreen_right = offscreen_right,
    offscreen_top = offscreen_top,
    offscreen_bottom = offscreen_bottom,
    stringsAsFactors = FALSE
  )

  overall_summary <- data.frame(
    n_aois = nrow(aoi_summary),
    n_missing_geometry = sum(aoi_summary$missing_geometry),
    n_invalid_rectangles = sum(aoi_summary$invalid_rectangle),
    n_outside_screen = sum(aoi_summary$outside_screen),
    total_raw_area = sum(aoi_summary$raw_area, na.rm = TRUE),
    total_clipped_area = sum(aoi_summary$clipped_area, na.rm = TRUE),
    total_raw_screen_coverage = sum(aoi_summary$raw_screen_coverage, na.rm = TRUE),
    total_clipped_screen_coverage = sum(aoi_summary$clipped_screen_coverage, na.rm = TRUE),
    coverage_note = "Coverage sums are descriptive and do not correct for AOI overlap.",
    stringsAsFactors = FALSE
  )

  list(
    aoi_summary = aoi_summary,
    overall_summary = overall_summary,
    settings = list(
      screen_width = screen_width,
      screen_height = screen_height,
      aoi_col = aoi_col,
      x_min_col = x_min_col,
      x_max_col = x_max_col,
      y_min_col = y_min_col,
      y_max_col = y_max_col,
      margin = margin
    )
  )
}


#' Summarize gaze-coordinate coverage over a screen grid
#'
#' Summarizes how much of the screen area is represented by valid gaze
#' coordinates. The helper reports valid-coordinate rates, coordinate ranges,
#' and the proportion of occupied grid cells. It is intended for quality review
#' and documentation, not for inference about attention.
#'
#' @param data A data frame.
#' @param x_col,y_col Character names of gaze-coordinate columns.
#' @param screen_width,screen_height Numeric screen or stimulus dimensions.
#' @param group_cols Optional grouping columns.
#' @param grid_n_x,grid_n_y Number of grid cells along the x and y dimensions.
#' @param include_out_of_bounds If `TRUE`, finite out-of-bounds coordinates are
#'   included in range summaries but not in grid-occupancy calculations.
#'
#' @return A data frame with one row per group.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' summarize_gazepoint_coordinate_coverage(
#'   x,
#'   x_col = "gaze_x",
#'   y_col = "gaze_y",
#'   screen_width = 1920,
#'   screen_height = 1080,
#'   group_cols = "condition"
#' )
summarize_gazepoint_coordinate_coverage <- function(data,
                                                    x_col,
                                                    y_col,
                                                    screen_width,
                                                    screen_height,
                                                    group_cols = NULL,
                                                    grid_n_x = 10,
                                                    grid_n_y = 10,
                                                    include_out_of_bounds = FALSE) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, c(x_col, y_col, group_cols), "data")
  .gp3_require_plot_scalar(screen_width, "screen_width", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(screen_height, "screen_height", lower = 0, allow_zero = FALSE)
  .gp3_require_positive_integer(grid_n_x, "grid_n_x")
  .gp3_require_positive_integer(grid_n_y, "grid_n_y")

  x <- suppressWarnings(as.numeric(data[[x_col]]))
  y <- suppressWarnings(as.numeric(data[[y_col]]))

  finite_coordinate <- is.finite(x) & is.finite(y)
  inside_screen <- finite_coordinate & x >= 0 & x <= screen_width & y >= 0 & y <= screen_height
  valid_for_range <- finite_coordinate

  if (!isTRUE(include_out_of_bounds)) {
    valid_for_range <- inside_screen
  }

  if (is.null(group_cols) || length(group_cols) == 0L) {
    group_id <- rep("all", nrow(data))
  } else {
    group_id <- as.character(interaction(data[group_cols], drop = TRUE, lex.order = TRUE))
  }

  split_ids <- split(seq_len(nrow(data)), group_id)

  out <- lapply(names(split_ids), function(id) {
    rows <- split_ids[[id]]
    range_rows <- rows[valid_for_range[rows]]
    grid_rows <- rows[inside_screen[rows]]

    occupied_cells <- .gp3_count_occupied_coordinate_cells(
      x = x[grid_rows],
      y = y[grid_rows],
      screen_width = screen_width,
      screen_height = screen_height,
      grid_n_x = grid_n_x,
      grid_n_y = grid_n_y
    )

    data.frame(
      group_id = id,
      n_rows = length(rows),
      n_finite_coordinates = sum(finite_coordinate[rows]),
      n_inside_screen = sum(inside_screen[rows]),
      finite_coordinate_rate = mean(finite_coordinate[rows]),
      inside_screen_rate = mean(inside_screen[rows]),
      x_min = .gp3_safe_min(x[range_rows]),
      x_max = .gp3_safe_max(x[range_rows]),
      y_min = .gp3_safe_min(y[range_rows]),
      y_max = .gp3_safe_max(y[range_rows]),
      x_mean = .gp3_safe_mean(x[range_rows]),
      y_mean = .gp3_safe_mean(y[range_rows]),
      occupied_grid_cells = occupied_cells,
      total_grid_cells = grid_n_x * grid_n_y,
      occupied_grid_rate = occupied_cells / (grid_n_x * grid_n_y),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, out)
  row.names(result) <- NULL
  result
}


.gp3_count_occupied_coordinate_cells <- function(x,
                                                 y,
                                                 screen_width,
                                                 screen_height,
                                                 grid_n_x,
                                                 grid_n_y) {
  if (length(x) == 0L) {
    return(0L)
  }

  x_bin <- ceiling((x / screen_width) * grid_n_x)
  y_bin <- ceiling((y / screen_height) * grid_n_y)

  x_bin[x_bin < 1L] <- 1L
  y_bin[y_bin < 1L] <- 1L
  x_bin[x_bin > grid_n_x] <- grid_n_x
  y_bin[y_bin > grid_n_y] <- grid_n_y

  length(unique(paste(x_bin, y_bin, sep = "_")))
}


.gp3_safe_min <- function(x) {
  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}


.gp3_safe_max <- function(x) {
  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}


.gp3_safe_mean <- function(x) {
  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}
