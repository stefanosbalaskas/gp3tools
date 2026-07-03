#' Audit Gazepoint gaze coordinates against screen bounds
#'
#' Checks whether gaze coordinates are missing, equal to `(0, 0)`, or outside
#' the expected screen/stimulus bounds. The helper is intended for transparent
#' quality-control reporting, not for automatic exclusion decisions.
#'
#' @param data A data frame.
#' @param x_col,y_col Character names of gaze-coordinate columns.
#' @param screen_width,screen_height Numeric screen or stimulus dimensions.
#' @param group_cols Optional grouping columns for group-level summaries.
#' @param margin Numeric tolerance around the screen bounds. A positive value
#'   allows coordinates slightly outside the nominal screen area.
#' @param treat_zero_zero_as_out_of_bounds If `TRUE`, `(0, 0)` coordinates are
#'   flagged separately and counted as invalid.
#'
#' @return A list with row-level flags, group-level summary, overall summary,
#'   and settings.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' audit_gazepoint_screen_bounds(x, "gaze_x", "gaze_y", 1920, 1080)
audit_gazepoint_screen_bounds <- function(data,
                                          x_col,
                                          y_col,
                                          screen_width,
                                          screen_height,
                                          group_cols = NULL,
                                          margin = 0,
                                          treat_zero_zero_as_out_of_bounds = TRUE) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, c(x_col, y_col, group_cols), "data")
  .gp3_require_plot_scalar(screen_width, "screen_width", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(screen_height, "screen_height", lower = 0, allow_zero = FALSE)

  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) || margin < 0) {
    stop("`margin` must be a single non-negative numeric value.", call. = FALSE)
  }

  x <- suppressWarnings(as.numeric(data[[x_col]]))
  y <- suppressWarnings(as.numeric(data[[y_col]]))

  missing_coordinate <- !is.finite(x) | !is.finite(y)
  zero_zero <- !missing_coordinate & x == 0 & y == 0

  outside_x <- !missing_coordinate & (x < -margin | x > screen_width + margin)
  outside_y <- !missing_coordinate & (y < -margin | y > screen_height + margin)
  outside_bounds <- outside_x | outside_y

  invalid_coordinate <- missing_coordinate | outside_bounds
  if (isTRUE(treat_zero_zero_as_out_of_bounds)) {
    invalid_coordinate <- invalid_coordinate | zero_zero
  }

  row_flags <- data.frame(
    row_id = seq_len(nrow(data)),
    x = x,
    y = y,
    missing_coordinate = missing_coordinate,
    zero_zero = zero_zero,
    outside_x = outside_x,
    outside_y = outside_y,
    outside_bounds = outside_bounds,
    invalid_coordinate = invalid_coordinate,
    stringsAsFactors = FALSE
  )

  if (!is.null(group_cols) && length(group_cols) > 0L) {
    group_id <- interaction(data[group_cols], drop = TRUE, lex.order = TRUE)
    row_flags$.gp3_group_id <- as.character(group_id)
  } else {
    row_flags$.gp3_group_id <- "all"
  }

  group_summary <- .gp3_summarize_screen_bounds(row_flags)

  overall_summary <- data.frame(
    n_rows = nrow(row_flags),
    n_missing_coordinate = sum(row_flags$missing_coordinate),
    n_zero_zero = sum(row_flags$zero_zero),
    n_outside_bounds = sum(row_flags$outside_bounds),
    n_invalid_coordinate = sum(row_flags$invalid_coordinate),
    missing_coordinate_rate = mean(row_flags$missing_coordinate),
    zero_zero_rate = mean(row_flags$zero_zero),
    outside_bounds_rate = mean(row_flags$outside_bounds),
    invalid_coordinate_rate = mean(row_flags$invalid_coordinate),
    stringsAsFactors = FALSE
  )

  list(
    row_flags = row_flags,
    group_summary = group_summary,
    overall_summary = overall_summary,
    settings = list(
      x_col = x_col,
      y_col = y_col,
      screen_width = screen_width,
      screen_height = screen_height,
      group_cols = group_cols,
      margin = margin,
      treat_zero_zero_as_out_of_bounds = treat_zero_zero_as_out_of_bounds
    )
  )
}


#' Harmonize Gazepoint screen coordinates across resolutions
#'
#' Rescales gaze coordinates from one screen or stimulus resolution to another.
#' This is a deterministic coordinate transformation for harmonizing exports
#' before plotting, AOI checks, or descriptive summaries. It does not recalibrate
#' gaze data or correct measurement error.
#'
#' @param data A data frame.
#' @param x_col,y_col Character names of source coordinate columns.
#' @param from_width,from_height Original screen or stimulus dimensions.
#' @param to_width,to_height Target screen or stimulus dimensions.
#' @param output_x_col,output_y_col Names of the rescaled output columns.
#' @param keep_original If `TRUE`, original coordinate columns are retained.
#'   If `FALSE`, the original columns are removed when output column names differ.
#'
#' @return A copy of `data` with harmonized coordinate columns.
#' @export
#'
#' @examples
#' x <- data.frame(gaze_x = c(0, 960, 1920), gaze_y = c(0, 540, 1080))
#' harmonize_gazepoint_screen_coordinates(
#'   x,
#'   x_col = "gaze_x",
#'   y_col = "gaze_y",
#'   from_width = 1920,
#'   from_height = 1080,
#'   to_width = 1280,
#'   to_height = 720
#' )
harmonize_gazepoint_screen_coordinates <- function(data,
                                                   x_col,
                                                   y_col,
                                                   from_width,
                                                   from_height,
                                                   to_width,
                                                   to_height,
                                                   output_x_col = "gaze_x_harmonized",
                                                   output_y_col = "gaze_y_harmonized",
                                                   keep_original = TRUE) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, c(x_col, y_col), "data")

  .gp3_require_plot_scalar(from_width, "from_width", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(from_height, "from_height", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(to_width, "to_width", lower = 0, allow_zero = FALSE)
  .gp3_require_plot_scalar(to_height, "to_height", lower = 0, allow_zero = FALSE)

  if (!is.character(output_x_col) || length(output_x_col) != 1L || !nzchar(output_x_col)) {
    stop("`output_x_col` must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.character(output_y_col) || length(output_y_col) != 1L || !nzchar(output_y_col)) {
    stop("`output_y_col` must be a single non-empty character string.", call. = FALSE)
  }

  x <- suppressWarnings(as.numeric(data[[x_col]]))
  y <- suppressWarnings(as.numeric(data[[y_col]]))

  out <- data
  out[[output_x_col]] <- x * (to_width / from_width)
  out[[output_y_col]] <- y * (to_height / from_height)

  if (!isTRUE(keep_original)) {
    remove_cols <- setdiff(c(x_col, y_col), c(output_x_col, output_y_col))
    out[remove_cols] <- NULL
  }

  attr(out, "gp3_screen_harmonization") <- list(
    x_col = x_col,
    y_col = y_col,
    from_width = from_width,
    from_height = from_height,
    to_width = to_width,
    to_height = to_height,
    output_x_col = output_x_col,
    output_y_col = output_y_col,
    x_scale = to_width / from_width,
    y_scale = to_height / from_height
  )

  out
}


.gp3_summarize_screen_bounds <- function(row_flags) {
  split_flags <- split(row_flags, row_flags$.gp3_group_id)

  summaries <- lapply(names(split_flags), function(group_id) {
    x <- split_flags[[group_id]]

    data.frame(
      group_id = group_id,
      n_rows = nrow(x),
      n_missing_coordinate = sum(x$missing_coordinate),
      n_zero_zero = sum(x$zero_zero),
      n_outside_bounds = sum(x$outside_bounds),
      n_invalid_coordinate = sum(x$invalid_coordinate),
      missing_coordinate_rate = mean(x$missing_coordinate),
      zero_zero_rate = mean(x$zero_zero),
      outside_bounds_rate = mean(x$outside_bounds),
      invalid_coordinate_rate = mean(x$invalid_coordinate),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summaries)
}
