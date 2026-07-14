#' Summarise gaze or pupil measures in sliding time windows
#'
#' Creates overlapping or non-overlapping time windows and calculates selected
#' summary statistics for numeric gaze or pupil columns.
#'
#' @param et_data A sample-level data frame.
#' @param window_size Window width.
#' @param step Distance between consecutive window starts.
#' @param summary_stats Statistics to calculate. Supported values are
#'   `"mean"`, `"sd"`, `"median"`, `"min"`, `"max"`, `"sum"`, and
#'   `"valid_prop"`.
#' @param by Grouping columns defining independent time series.
#' @param condition_col Optional condition column appended to `by`.
#' @param value_cols Numeric columns to summarise. When `NULL`, common gaze and
#'   pupil columns are detected.
#' @param ts_col Timestamp column.
#' @param window_unit Unit used by `window_size` and `step`.
#' @param time_unit Unit of the timestamp column.
#' @param include_partial Include a final window that is shorter than
#'   `window_size`.
#'
#' @return A tibble with one row per group and time window.
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   USER_ID = "P01",
#'   TIME = seq(0, 0.99, by = 0.01),
#'   mean_pupil = sin(seq(0, 2 * pi, length.out = 100))
#' )
#' analyze_gazepoint_window(
#'   pupil,
#'   window_size = 100,
#'   step = 50,
#'   value_cols = "mean_pupil"
#' )
analyze_gazepoint_window <- function(
  et_data,
  window_size = 50,
  step = 10,
  summary_stats = c("mean", "sd"),
  by = "USER_ID",
  condition_col = NULL,
  value_cols = NULL,
  ts_col = "TIME",
  window_unit = c("milliseconds", "seconds", "native"),
  time_unit = c("auto", "seconds", "milliseconds"),
  include_partial = FALSE
) {
  .gp3_hp_assert_data_frame(et_data, "et_data")
  window_unit <- match.arg(window_unit)
  time_unit <- match.arg(time_unit)
  by <- unique(c(by, condition_col))

  .gp3_hp_assert_columns(et_data, unique(c(by, ts_col)), "et_data")

  if (is.null(value_cols)) {
    candidates <- c(
      "FPOGX", "FPOGY", "x", "y", "mean_pupil", "pupil",
      "pupil_clean", "pupil_smoothed", "LPupil", "RPupil",
      "LPD", "RPD", "LPMM", "RPMM"
    )
    detected <- candidates[candidates %in% names(et_data)]
    value_cols <- detected[
      vapply(et_data[detected], is.numeric, logical(1))
    ]
  }

  if (!length(value_cols)) {
    stop(
      "No numeric `value_cols` were supplied or detected.",
      call. = FALSE
    )
  }
  .gp3_hp_assert_columns(et_data, value_cols, "et_data")

  non_numeric <- value_cols[
    !vapply(et_data[value_cols], is.numeric, logical(1))
  ]
  if (length(non_numeric)) {
    stop(
      sprintf(
        "`value_cols` must be numeric. Non-numeric: %s.",
        paste(non_numeric, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  supported_stats <- c(
    "mean", "sd", "median", "min", "max", "sum", "valid_prop"
  )
  summary_stats <- unique(summary_stats)
  unsupported <- setdiff(summary_stats, supported_stats)
  if (length(unsupported)) {
    stop(
      sprintf(
        "Unsupported `summary_stats`: %s.",
        paste(unsupported, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (arg in c("window_size", "step")) {
    value <- get(arg, inherits = FALSE)
    if (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value <= 0) {
      stop(sprintf("`%s` must be one finite positive number.", arg),
           call. = FALSE)
    }
  }

  groups <- .gp3_hp_split_indices(et_data, by)
  rows <- list()
  counter <- 0L

  for (idx in groups) {
    ord <- order(et_data[[ts_col]][idx], na.last = TRUE)
    gi <- idx[ord]
    time_raw <- suppressWarnings(as.numeric(et_data[[ts_col]][gi]))
    time_info <- .gp3_hp_time_info(time_raw, time_unit)
    time_sec <- time_raw * time_info$to_seconds

    finite_time <- is.finite(time_sec)
    if (!any(finite_time)) next

    window_sec <- .gp3_hp_window_to_seconds(
      window_size,
      window_unit,
      time_info$to_seconds
    )
    step_sec <- .gp3_hp_window_to_seconds(
      step,
      window_unit,
      time_info$to_seconds
    )

    min_time <- min(time_sec[finite_time])
    max_time <- max(time_sec[finite_time])

    if (isTRUE(include_partial)) {
      starts <- seq(min_time, max_time, by = step_sec)
    } else {
      final_start <- max_time - window_sec
      if (final_start < min_time) next
      starts <- seq(min_time, final_start, by = step_sec)
    }

    for (start_sec in starts) {
      end_sec <- start_sec + window_sec
      in_window <- finite_time &
        time_sec >= start_sec &
        if (isTRUE(include_partial) && end_sec > max_time) {
          time_sec <= max_time
        } else {
          time_sec < end_sec
        }

      window_idx <- gi[in_window]
      if (!length(window_idx)) next

      row <- et_data[window_idx[1L], by, drop = FALSE]
      row$window_start <- start_sec / time_info$to_seconds
      row$window_end <- min(end_sec, max_time) / time_info$to_seconds
      row$window_mid <- (
        start_sec + min(end_sec, max_time)
      ) / 2 / time_info$to_seconds
      row$window_size <- window_size
      row$window_step <- step
      row$window_unit <- window_unit
      row$n_samples <- length(window_idx)

      for (column in value_cols) {
        values <- suppressWarnings(
          as.numeric(et_data[[column]][window_idx])
        )
        finite <- is.finite(values)

        for (stat in summary_stats) {
          output_name <- paste(column, stat, sep = "_")
          row[[output_name]] <- .gp3_hp_summary_stat(
            values,
            finite,
            stat
          )
        }
      }

      counter <- counter + 1L
      rows[[counter]] <- row
    }
  }

  output <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    template <- et_data[0, by, drop = FALSE]
    template$window_start <- numeric()
    template$window_end <- numeric()
    template$window_mid <- numeric()
    template$window_size <- numeric()
    template$window_step <- numeric()
    template$window_unit <- character()
    template$n_samples <- integer()

    for (column in value_cols) {
      for (stat in summary_stats) {
        template[[paste(column, stat, sep = "_")]] <- numeric()
      }
    }
    template
  }

  rownames(output) <- NULL
  output <- tibble::as_tibble(output)
  class(output) <- c("gp3_window_summary", class(output))
  output
}


#' Add rectangular AOI membership to gaze data
#'
#' Labels gaze samples using one or more rectangular AOI definitions.
#'
#' @param master_df A sample-level gaze data frame.
#' @param aoi_defs AOI definition data frame. Recognised aliases include
#'   `name`/`aoi_name`, `L`/`left`/`xmin`, `R`/`right`/`xmax`,
#'   `T`/`top`/`ymin`, and `B`/`bottom`/`ymax`.
#' @param x_col,y_col Gaze-coordinate columns.
#' @param aoi_name Optional AOI name selecting one row from `aoi_defs`.
#' @param output Add logical AOI columns, a single label column, or both.
#' @param prefix Prefix for logical AOI columns.
#' @param label_col Name of the single AOI-label column.
#' @param outside_label Label for samples outside all AOIs.
#' @param overlap Overlap handling for the label column.
#' @param include_overlap_count Add the number of AOIs containing each sample.
#'
#' @return The input data with AOI membership columns.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   FPOGX = c(0.2, 0.5, 0.8),
#'   FPOGY = c(0.2, 0.5, 0.8)
#' )
#' defs <- data.frame(
#'   name = c("top_left", "bottom_right"),
#'   L = c(0, 0.6),
#'   R = c(0.4, 1),
#'   T = c(0, 0.6),
#'   B = c(0.4, 1)
#' )
#' add_gazepoint_aoi(gaze, defs, output = "both")
add_gazepoint_aoi <- function(
  master_df,
  aoi_defs,
  x_col = "FPOGX",
  y_col = "FPOGY",
  aoi_name = NULL,
  output = c("logical", "label", "both"),
  prefix = "aoi_",
  label_col = "aoi_current",
  outside_label = "outside",
  overlap = c("first", "last", "error"),
  include_overlap_count = TRUE
) {
  .gp3_hp_assert_data_frame(master_df, "master_df")
  .gp3_hp_assert_data_frame(aoi_defs, "aoi_defs")
  output <- match.arg(output)
  overlap <- match.arg(overlap)
  .gp3_hp_assert_columns(master_df, c(x_col, y_col), "master_df")

  aliases <- list(
    name = c("name", "aoi_name", "AOI", "aoi", "label"),
    left = c("L", "left", "xmin", "x_min"),
    right = c("R", "right", "xmax", "x_max"),
    top = c("T", "top", "ymin", "y_min"),
    bottom = c("B", "bottom", "ymax", "y_max")
  )

  resolved <- vapply(
    aliases,
    function(options) {
      hit <- options[options %in% names(aoi_defs)]
      if (length(hit)) hit[1L] else NA_character_
    },
    character(1)
  )

  missing_defs <- names(resolved)[is.na(resolved)]
  if (length(missing_defs)) {
    stop(
      sprintf(
        "Could not resolve AOI definition fields: %s.",
        paste(missing_defs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  defs <- data.frame(
    name = as.character(aoi_defs[[resolved["name"]]]),
    left = suppressWarnings(as.numeric(aoi_defs[[resolved["left"]]])),
    right = suppressWarnings(as.numeric(aoi_defs[[resolved["right"]]])),
    top = suppressWarnings(as.numeric(aoi_defs[[resolved["top"]]])),
    bottom = suppressWarnings(as.numeric(aoi_defs[[resolved["bottom"]]])),
    stringsAsFactors = FALSE
  )

  if (!is.null(aoi_name)) {
    defs <- defs[defs$name %in% aoi_name, , drop = FALSE]
    if (!nrow(defs)) {
      stop("`aoi_name` did not match any AOI definition.",
           call. = FALSE)
    }
  }

  if (!nrow(defs)) {
    stop("`aoi_defs` must contain at least one AOI.", call. = FALSE)
  }
  if (anyDuplicated(defs$name)) {
    stop("AOI names must be unique.", call. = FALSE)
  }
  if (any(!is.finite(as.matrix(defs[-1L])))) {
    stop("AOI boundaries must be finite numeric values.",
         call. = FALSE)
  }

  left <- pmin(defs$left, defs$right)
  right <- pmax(defs$left, defs$right)
  top <- pmin(defs$top, defs$bottom)
  bottom <- pmax(defs$top, defs$bottom)

  x <- suppressWarnings(as.numeric(master_df[[x_col]]))
  y <- suppressWarnings(as.numeric(master_df[[y_col]]))
  valid_xy <- is.finite(x) & is.finite(y)

  membership <- vapply(
    seq_len(nrow(defs)),
    function(i) {
      valid_xy &
        x >= left[i] & x <= right[i] &
        y >= top[i] & y <= bottom[i]
    },
    logical(nrow(master_df))
  )

  if (nrow(defs) == 1L) {
    membership <- matrix(
      membership,
      ncol = 1L,
      dimnames = list(NULL, defs$name)
    )
  } else {
    colnames(membership) <- defs$name
  }

  overlap_count <- rowSums(membership)
  if (overlap == "error" && any(overlap_count > 1L)) {
    stop(
      sprintf(
        "%d sample(s) fall inside overlapping AOIs.",
        sum(overlap_count > 1L)
      ),
      call. = FALSE
    )
  }

  output_data <- master_df

  if (output %in% c("logical", "both")) {
    logical_names <- paste0(
      prefix,
      make.names(defs$name, unique = TRUE)
    )
    for (i in seq_len(ncol(membership))) {
      output_data[[logical_names[i]]] <- membership[, i]
    }
  }

  if (output %in% c("label", "both")) {
    labels <- rep(outside_label, nrow(master_df))
    labels[!valid_xy] <- NA_character_

    for (i in seq_len(ncol(membership))) {
      hit <- membership[, i]
      if (overlap == "last") {
        labels[hit] <- defs$name[i]
      } else {
        labels[hit & labels == outside_label] <- defs$name[i]
      }
    }
    output_data[[label_col]] <- labels
  }

  if (isTRUE(include_overlap_count)) {
    output_data$aoi_overlap_count <- overlap_count
  }

  attr(output_data, "gazepoint_aoi_definitions") <- defs
  .gp3_hp_restore_class(output_data, master_df)
}
