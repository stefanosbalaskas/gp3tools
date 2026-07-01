
#' Plot an AOI timeline
#'
#' Creates a scarf-style timeline plot showing the current AOI over time for
#' each subject, trial, or user-defined row grouping.
#'
#' @param data A data frame containing AOI observations.
#' @param aoi_col Character scalar. Column containing AOI labels.
#' @param time_col Character scalar. Column containing time values.
#' @param y_col Optional character scalar used directly as the y-axis row.
#' @param subject_col Optional subject column used to construct the y-axis row.
#' @param trial_col Optional trial column used to construct the y-axis row.
#' @param group_cols Optional character vector used to construct the y-axis row
#'   when `y_col` is not supplied.
#' @param include_missing Logical. If `TRUE`, missing or empty AOI labels are
#'   retained as `missing_label`; otherwise they are removed.
#' @param missing_label Character scalar used when `include_missing = TRUE`.
#' @param sample_width Optional numeric tile width. If omitted, a median time
#'   difference is estimated.
#' @param title Optional plot title.
#' @param x_label X-axis label.
#' @param y_label Optional y-axis label.
#' @param aoi_label Fill legend label.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   subject = rep(c("S01", "S02"), each = 4),
#'   trial = "T01",
#'   time = rep(1:4, 2),
#'   AOI = c("A", "A", "B", "C", "A", "B", "B", "C")
#' )
#'
#' plot_gazepoint_aoi_timeline(
#'   dat,
#'   aoi_col = "AOI",
#'   time_col = "time",
#'   subject_col = "subject",
#'   trial_col = "trial"
#' )
plot_gazepoint_aoi_timeline <- function(data,
                                        aoi_col,
                                        time_col,
                                        y_col = NULL,
                                        subject_col = NULL,
                                        trial_col = NULL,
                                        group_cols = NULL,
                                        include_missing = FALSE,
                                        missing_label = "missing",
                                        sample_width = NULL,
                                        title = NULL,
                                        x_label = "Time",
                                        y_label = NULL,
                                        aoi_label = "AOI") {
  .gp3_sequence_check_data(data)
  .gp3_sequence_check_scalar_string(aoi_col, "aoi_col")
  .gp3_sequence_check_scalar_string(time_col, "time_col")
  .gp3_sequence_check_scalar_string(y_col, "y_col", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(subject_col, "subject_col", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(trial_col, "trial_col", allow_null = TRUE)
  .gp3_sequence_check_character_vector(group_cols, "group_cols", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(missing_label, "missing_label")
  .gp3_sequence_check_scalar_string(title, "title", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(x_label, "x_label")
  .gp3_sequence_check_scalar_string(y_label, "y_label", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(aoi_label, "aoi_label")

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for this plot.", call. = FALSE)
  }

  if (!is.logical(include_missing) || length(include_missing) != 1L ||
      is.na(include_missing)) {
    stop("`include_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(sample_width) &&
      (!is.numeric(sample_width) || length(sample_width) != 1L ||
       is.na(sample_width) || !is.finite(sample_width) || sample_width <= 0)) {
    stop("`sample_width` must be NULL or a positive finite numeric scalar.",
         call. = FALSE)
  }

  .gp3_sequence_check_columns(
    data,
    c(aoi_col, time_col, y_col, subject_col, trial_col, group_cols)
  )

  dat <- data

  dat$.gp3_aoi <- as.character(dat[[aoi_col]])
  missing <- is.na(dat$.gp3_aoi) | !nzchar(trimws(dat$.gp3_aoi))

  if (isTRUE(include_missing)) {
    dat$.gp3_aoi[missing] <- missing_label
  } else {
    dat <- dat[!missing, , drop = FALSE]
  }

  dat$.gp3_time <- suppressWarnings(as.numeric(dat[[time_col]]))
  dat <- dat[is.finite(dat$.gp3_time), , drop = FALSE]

  if (!nrow(dat)) {
    stop("No valid AOI/time rows are available for plotting.", call. = FALSE)
  }

  if (!is.null(y_col)) {
    dat$.gp3_timeline_y <- as.character(dat[[y_col]])
  } else {
    y_parts <- c(subject_col, trial_col, group_cols)

    if (length(y_parts) > 0L) {
      dat$.gp3_timeline_y <- apply(dat[y_parts], 1L, function(row) {
        paste(row, collapse = " | ")
      })
    } else {
      dat$.gp3_timeline_y <- ".all"
    }
  }

  dat$.gp3_timeline_y[is.na(dat$.gp3_timeline_y) |
                        !nzchar(dat$.gp3_timeline_y)] <- "<NA>"

  if (is.null(sample_width)) {
    diffs <- unlist(lapply(
      split(dat$.gp3_time, dat$.gp3_timeline_y, drop = TRUE),
      function(x) {
        x <- sort(unique(x))
        diff(x)
      }
    ))

    diffs <- diffs[is.finite(diffs) & diffs > 0]
    sample_width <- if (length(diffs)) stats::median(diffs) else 1
  }

  if (is.null(y_label)) {
    y_label <- if (!is.null(y_col)) y_col else "Subject / trial"
  }

  dat$.gp3_timeline_y <- factor(
    dat$.gp3_timeline_y,
    levels = rev(unique(dat$.gp3_timeline_y))
  )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$.gp3_time,
      y = .data$.gp3_timeline_y,
      fill = .data$.gp3_aoi
    )
  ) +
    ggplot2::geom_tile(width = sample_width, height = 0.9) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = aoi_label
    ) +
    ggplot2::theme_minimal()
}
