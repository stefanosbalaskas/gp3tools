#' Offline gaze recalibration using known target coordinates
#'
#' Apply an offline drift-correction shift to Gazepoint gaze coordinates using
#' known fixation/check-target coordinates. For each group, the helper estimates
#' the horizontal and vertical gaze offset from valid target samples and applies
#' the correction to all gaze samples in the same group.
#'
#' This helper is useful only when known target coordinates are available, for
#' example from calibration checks, fixation targets, validation targets, or
#' drift-check trials.
#'
#' @param data A data frame containing gaze and target coordinates.
#' @param x_col Horizontal gaze coordinate column.
#' @param y_col Vertical gaze coordinate column.
#' @param target_x_col Known horizontal target coordinate column.
#' @param target_y_col Known vertical target coordinate column.
#' @param time_col Optional time column used only for stable ordering.
#' @param grouping_cols Optional grouping columns used to estimate one correction
#'   per participant, trial, block, stimulus, or other unit.
#' @param calibration_col Optional column identifying rows to use for estimating
#'   the correction.
#' @param calibration_value Optional value in `calibration_col` identifying
#'   calibration/check rows. If `calibration_col` is supplied and
#'   `calibration_value = NULL`, logical `TRUE` rows are used.
#' @param method Shift estimator. `"median_shift"` uses median target-minus-gaze
#'   offsets; `"mean_shift"` uses mean offsets.
#' @param min_valid_points Minimum valid target/gaze pairs required per group.
#' @param max_shift Optional maximum Euclidean correction shift. If exceeded,
#'   the shift is reported but not applied.
#' @param output_x_col Corrected horizontal gaze output column.
#' @param output_y_col Corrected vertical gaze output column.
#' @param dx_col Estimated horizontal correction column.
#' @param dy_col Estimated vertical correction column.
#' @param shift_col Estimated Euclidean shift-distance column.
#' @param error_before_col Row-wise gaze-to-target error before correction.
#' @param error_after_col Row-wise gaze-to-target error after correction.
#' @param status_col Row-level recalibration status column.
#' @param overwrite Logical. If `FALSE`, existing output columns are protected.
#' @param name Character label stored in object attributes.
#'
#' @return A tibble with recalibrated gaze columns and recalibration attributes.
#' @export
recalibrate_gazepoint_gaze <- function(
    data,
    x_col,
    y_col,
    target_x_col,
    target_y_col,
    time_col = NULL,
    grouping_cols = NULL,
    calibration_col = NULL,
    calibration_value = NULL,
    method = c("median_shift", "mean_shift"),
    min_valid_points = 3L,
    max_shift = NULL,
    output_x_col = "gaze_x_recalibrated",
    output_y_col = "gaze_y_recalibrated",
    dx_col = "gaze_recalibration_dx",
    dy_col = "gaze_recalibration_dy",
    shift_col = "gaze_recalibration_shift",
    error_before_col = "gaze_error_before_recalibration",
    error_after_col = "gaze_error_after_recalibration",
    status_col = "gaze_recalibration_status",
    overwrite = FALSE,
    name = "gazepoint_gaze_recalibration"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  method <- match.arg(method)

  .gp3_recal_check_col(x_col, names(data), "x_col")
  .gp3_recal_check_col(y_col, names(data), "y_col")
  .gp3_recal_check_col(target_x_col, names(data), "target_x_col")
  .gp3_recal_check_col(target_y_col, names(data), "target_y_col")

  if (!is.null(time_col)) {
    .gp3_recal_check_col(time_col, names(data), "time_col")
  }

  .gp3_recal_check_cols(grouping_cols, names(data), "grouping_cols")

  if (!is.null(calibration_col)) {
    .gp3_recal_check_col(calibration_col, names(data), "calibration_col")
  }

  if (!is.null(calibration_value)) {
    if (length(calibration_value) != 1L || is.na(calibration_value)) {
      stop("`calibration_value` must be NULL or a non-missing scalar.", call. = FALSE)
    }
  }

  .gp3_recal_check_positive_integer(min_valid_points, "min_valid_points")
  .gp3_recal_check_nullable_positive_number(max_shift, "max_shift")
  .gp3_recal_check_logical(overwrite, "overwrite")
  .gp3_recal_check_label(name, "name")

  output_cols <- c(
    output_x_col,
    output_y_col,
    dx_col,
    dy_col,
    shift_col,
    error_before_col,
    error_after_col,
    status_col
  )

  vapply(
    output_cols,
    .gp3_recal_check_output_name,
    logical(1),
    arg = "output column"
  )

  if (anyDuplicated(output_cols)) {
    stop("Output column names must be unique.", call. = FALSE)
  }

  if (!isTRUE(overwrite)) {
    existing <- intersect(output_cols, names(data))

    if (length(existing) > 0L) {
      stop(
        "Output column(s) already exist in `data`: ",
        paste(existing, collapse = ", "),
        ". Use `overwrite = TRUE` to replace them.",
        call. = FALSE
      )
    }
  }

  prepared <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_recal_row_id = seq_len(dplyr::n()),
      .gp3_gaze_x = suppressWarnings(as.numeric(.data[[x_col]])),
      .gp3_gaze_y = suppressWarnings(as.numeric(.data[[y_col]])),
      .gp3_target_x = suppressWarnings(as.numeric(.data[[target_x_col]])),
      .gp3_target_y = suppressWarnings(as.numeric(.data[[target_y_col]]))
    )

  if (!is.null(time_col)) {
    prepared <- prepared |>
      dplyr::mutate(
        .gp3_recal_time = suppressWarnings(as.numeric(.data[[time_col]]))
      )

    if (any(!is.finite(prepared$.gp3_recal_time))) {
      stop("`time_col` must be numeric or coercible to finite numeric values.", call. = FALSE)
    }
  } else {
    prepared <- prepared |>
      dplyr::mutate(
        .gp3_recal_time = .data$.gp3_recal_row_id
      )
  }

  if (!is.null(calibration_col)) {
    calibration_values <- prepared[[calibration_col]]

    if (is.null(calibration_value)) {
      prepared$.gp3_recal_calibration_row <- calibration_values %in% TRUE
    } else {
      prepared$.gp3_recal_calibration_row <- calibration_values == calibration_value
      prepared$.gp3_recal_calibration_row[is.na(prepared$.gp3_recal_calibration_row)] <- FALSE
    }
  } else {
    prepared$.gp3_recal_calibration_row <- TRUE
  }

  if (length(grouping_cols) > 0L) {
    grouping_data <- prepared[, grouping_cols, drop = FALSE]

    prepared$.gp3_recal_group <- as.character(
      do.call(
        interaction,
        c(grouping_data, list(drop = TRUE, sep = "||"))
      )
    )
  } else {
    prepared$.gp3_recal_group <- "all_rows"
  }

  prepared <- prepared |>
    dplyr::arrange(
      .data$.gp3_recal_group,
      .data$.gp3_recal_time,
      .data$.gp3_recal_row_id
    )

  split_data <- split(prepared, prepared$.gp3_recal_group)

  recalibrated <- lapply(
    split_data,
    .gp3_recalibrate_one_group,
    method = method,
    min_valid_points = min_valid_points,
    max_shift = max_shift
  )

  recalibrated <- dplyr::bind_rows(recalibrated) |>
    dplyr::arrange(.data$.gp3_recal_row_id)

  out <- tibble::as_tibble(data)

  out[[output_x_col]] <- recalibrated$.gp3_recalibrated_x
  out[[output_y_col]] <- recalibrated$.gp3_recalibrated_y
  out[[dx_col]] <- recalibrated$.gp3_recal_dx
  out[[dy_col]] <- recalibrated$.gp3_recal_dy
  out[[shift_col]] <- recalibrated$.gp3_recal_shift
  out[[error_before_col]] <- recalibrated$.gp3_error_before
  out[[error_after_col]] <- recalibrated$.gp3_error_after
  out[[status_col]] <- recalibrated$.gp3_recal_status

  group_summary <- recalibrated |>
    dplyr::distinct(
      .data$.gp3_recal_group,
      .data$.gp3_group_status,
      .data$.gp3_n_calibration_rows,
      .data$.gp3_n_valid_calibration_rows,
      .data$.gp3_recal_dx,
      .data$.gp3_recal_dy,
      .data$.gp3_recal_shift,
      .data$.gp3_shift_applied
    ) |>
    dplyr::rename(
      group = ".gp3_recal_group",
      group_status = ".gp3_group_status",
      n_calibration_rows = ".gp3_n_calibration_rows",
      n_valid_calibration_rows = ".gp3_n_valid_calibration_rows",
      dx = ".gp3_recal_dx",
      dy = ".gp3_recal_dy",
      shift = ".gp3_recal_shift",
      shift_applied = ".gp3_shift_applied"
    ) |>
    dplyr::arrange(.data$group)

  status_summary <- tibble::tibble(
    status = out[[status_col]]
  ) |>
    dplyr::count(.data$status, name = "n") |>
    dplyr::arrange(.data$status)

  complete_group_status <- group_summary$group_status == "complete"

  overview <- tibble::tibble(
    object_name = name,
    recalibration_method = method,
    x_col = x_col,
    y_col = y_col,
    target_x_col = target_x_col,
    target_y_col = target_y_col,
    time_col = .gp3_recal_collapse_nullable(time_col),
    grouping_cols = .gp3_recal_collapse_nullable(grouping_cols),
    calibration_col = .gp3_recal_collapse_nullable(calibration_col),
    calibration_value = .gp3_recal_collapse_nullable(calibration_value),
    n_input_rows = nrow(data),
    n_groups = nrow(group_summary),
    n_complete_groups = sum(complete_group_status),
    n_problem_groups = sum(!complete_group_status),
    n_recalibrated_rows = sum(out[[status_col]] == "complete"),
    n_problem_rows = sum(out[[status_col]] != "complete"),
    min_valid_points = min_valid_points,
    max_shift = if (is.null(max_shift)) NA_real_ else max_shift,
    mean_shift = if (all(is.na(group_summary$shift))) {
      NA_real_
    } else {
      mean(group_summary$shift, na.rm = TRUE)
    },
    max_observed_shift = if (all(is.na(group_summary$shift))) {
      NA_real_
    } else {
      max(group_summary$shift, na.rm = TRUE)
    }
  )

  settings <- tibble::tibble(
    setting = c(
      "x_col",
      "y_col",
      "target_x_col",
      "target_y_col",
      "time_col",
      "grouping_cols",
      "calibration_col",
      "calibration_value",
      "method",
      "min_valid_points",
      "max_shift",
      "output_x_col",
      "output_y_col",
      "dx_col",
      "dy_col",
      "shift_col",
      "error_before_col",
      "error_after_col",
      "status_col",
      "overwrite",
      "name"
    ),
    value = c(
      x_col,
      y_col,
      target_x_col,
      target_y_col,
      .gp3_recal_collapse_nullable(time_col),
      .gp3_recal_collapse_nullable(grouping_cols),
      .gp3_recal_collapse_nullable(calibration_col),
      .gp3_recal_collapse_nullable(calibration_value),
      method,
      as.character(min_valid_points),
      if (is.null(max_shift)) NA_character_ else as.character(max_shift),
      output_x_col,
      output_y_col,
      dx_col,
      dy_col,
      shift_col,
      error_before_col,
      error_after_col,
      status_col,
      as.character(overwrite),
      name
    )
  )

  attr(out, "gp3_gaze_recalibration_overview") <- overview
  attr(out, "gp3_gaze_recalibration_group_summary") <- group_summary
  attr(out, "gp3_gaze_recalibration_status_summary") <- status_summary
  attr(out, "gp3_gaze_recalibration_settings") <- settings

  class(out) <- c("gp3_gaze_recalibrated_data", class(out))

  out
}

.gp3_recalibrate_one_group <- function(
    df,
    method,
    min_valid_points,
    max_shift
) {
  calibration_rows <- df$.gp3_recal_calibration_row &
    is.finite(df$.gp3_gaze_x) &
    is.finite(df$.gp3_gaze_y) &
    is.finite(df$.gp3_target_x) &
    is.finite(df$.gp3_target_y)

  n_calibration_rows <- sum(df$.gp3_recal_calibration_row, na.rm = TRUE)
  n_valid_calibration_rows <- sum(calibration_rows, na.rm = TRUE)

  dx <- NA_real_
  dy <- NA_real_
  shift <- NA_real_
  group_status <- "complete"
  shift_applied <- FALSE

  if (n_valid_calibration_rows < min_valid_points) {
    group_status <- "insufficient_valid_targets"
  } else {
    residual_x <- df$.gp3_target_x[calibration_rows] - df$.gp3_gaze_x[calibration_rows]
    residual_y <- df$.gp3_target_y[calibration_rows] - df$.gp3_gaze_y[calibration_rows]

    if (method == "median_shift") {
      dx <- stats::median(residual_x, na.rm = TRUE)
      dy <- stats::median(residual_y, na.rm = TRUE)
    } else {
      dx <- mean(residual_x, na.rm = TRUE)
      dy <- mean(residual_y, na.rm = TRUE)
    }

    shift <- sqrt(dx^2 + dy^2)

    if (!is.null(max_shift) && is.finite(shift) && shift > max_shift) {
      group_status <- "shift_exceeds_max"
      shift_applied <- FALSE
    } else {
      group_status <- "complete"
      shift_applied <- TRUE
    }
  }

  recalibrated_x <- df$.gp3_gaze_x
  recalibrated_y <- df$.gp3_gaze_y

  if (isTRUE(shift_applied)) {
    recalibrated_x <- df$.gp3_gaze_x + dx
    recalibrated_y <- df$.gp3_gaze_y + dy
  }

  finite_gaze <- is.finite(df$.gp3_gaze_x) & is.finite(df$.gp3_gaze_y)
  finite_target <- is.finite(df$.gp3_target_x) & is.finite(df$.gp3_target_y)
  finite_error <- finite_gaze & finite_target

  error_before <- rep(NA_real_, nrow(df))
  error_after <- rep(NA_real_, nrow(df))

  error_before[finite_error] <- sqrt(
    (df$.gp3_gaze_x[finite_error] - df$.gp3_target_x[finite_error])^2 +
      (df$.gp3_gaze_y[finite_error] - df$.gp3_target_y[finite_error])^2
  )

  error_after[finite_error] <- sqrt(
    (recalibrated_x[finite_error] - df$.gp3_target_x[finite_error])^2 +
      (recalibrated_y[finite_error] - df$.gp3_target_y[finite_error])^2
  )

  row_status <- rep(group_status, nrow(df))
  row_status[!finite_gaze] <- "missing_or_nonfinite_gaze"

  df$.gp3_recalibrated_x <- recalibrated_x
  df$.gp3_recalibrated_y <- recalibrated_y
  df$.gp3_recal_dx <- dx
  df$.gp3_recal_dy <- dy
  df$.gp3_recal_shift <- shift
  df$.gp3_error_before <- error_before
  df$.gp3_error_after <- error_after
  df$.gp3_recal_status <- row_status
  df$.gp3_group_status <- group_status
  df$.gp3_n_calibration_rows <- n_calibration_rows
  df$.gp3_n_valid_calibration_rows <- n_valid_calibration_rows
  df$.gp3_shift_applied <- shift_applied

  df
}

.gp3_recal_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_recal_check_cols <- function(cols, names_data, arg) {
  if (is.null(cols)) {
    return(invisible(TRUE))
  }

  if (!is.character(cols) || anyNA(cols) || any(!nzchar(cols))) {
    stop("`", arg, "` must be NULL or a character vector of column names.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop(
      "`", arg, "` contains column(s) not present in `data`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_recal_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_recal_check_nullable_positive_number <- function(x, arg) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be NULL or a positive finite number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_recal_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_recal_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_recal_check_output_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("Each ", arg, " must be a non-missing character scalar.", call. = FALSE)
  }

  TRUE
}

.gp3_recal_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
