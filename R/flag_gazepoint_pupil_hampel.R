#' Flag pupil artifacts with a Hampel filter
#'
#' Apply a rolling Hampel filter to a Gazepoint pupil column. The helper computes
#' a rolling median and median absolute deviation (MAD) within a centred sample
#' window, then flags pupil samples whose absolute deviation from the local
#' median exceeds `k * MAD`.
#'
#' This helper is intended as an optional sensitivity/artifact-flagging branch.
#' It complements existing pupil artifact checks and should not automatically
#' replace confirmatory preprocessing decisions.
#'
#' @param data A data frame containing pupil observations.
#' @param pupil_col Pupil column to screen.
#' @param time_col Optional time column used to order samples within groups.
#' @param grouping_cols Optional grouping columns, for example participant and
#'   trial.
#' @param window_size_samples Odd integer rolling-window size in samples.
#' @param k Hampel threshold multiplier.
#' @param min_valid_samples Minimum finite pupil samples required inside a
#'   rolling window.
#' @param scale_mad Scaling factor applied to MAD. The default `1.4826` makes MAD
#'   comparable to the standard deviation under normality.
#' @param flag_col Name of the logical Hampel-flag output column.
#' @param median_col Name of the rolling median output column.
#' @param mad_col Name of the rolling MAD output column.
#' @param threshold_col Name of the rolling threshold output column.
#' @param corrected_col Optional name of a corrected pupil column. If supplied,
#'   flagged samples are replaced with the local rolling median.
#' @param status_col Name of the row-level Hampel status column.
#' @param overwrite Logical. If `FALSE`, the function errors when output columns
#'   already exist.
#' @param name Character label stored in object attributes.
#'
#' @return A tibble with Hampel-filter columns added. The object has class
#'   `gp3_pupil_hampel_flags`.
#' @export
flag_gazepoint_pupil_hampel <- function(
    data,
    pupil_col,
    time_col = NULL,
    grouping_cols = NULL,
    window_size_samples = 7L,
    k = 3,
    min_valid_samples = 3L,
    scale_mad = 1.4826,
    flag_col = "pupil_hampel_outlier",
    median_col = "pupil_hampel_median",
    mad_col = "pupil_hampel_mad",
    threshold_col = "pupil_hampel_threshold",
    corrected_col = NULL,
    status_col = "pupil_hampel_status",
    overwrite = FALSE,
    name = "gazepoint_pupil_hampel"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_hampel_check_col(pupil_col, names(data), "pupil_col")

  if (!is.null(time_col)) {
    .gp3_hampel_check_col(time_col, names(data), "time_col")
  }

  .gp3_hampel_check_cols(grouping_cols, names(data), "grouping_cols")
  .gp3_hampel_check_odd_positive_integer(window_size_samples, "window_size_samples")
  .gp3_hampel_check_positive_number(k, "k")
  .gp3_hampel_check_positive_integer(min_valid_samples, "min_valid_samples")
  .gp3_hampel_check_positive_number(scale_mad, "scale_mad")
  if (min_valid_samples > window_size_samples) {
    stop("`min_valid_samples` must be less than or equal to `window_size_samples`.", call. = FALSE)
  }
  .gp3_hampel_check_logical(overwrite, "overwrite")
  .gp3_hampel_check_label(name, "name")

  output_cols <- c(
    flag_col,
    median_col,
    mad_col,
    threshold_col,
    status_col
  )

  if (!is.null(corrected_col)) {
    output_cols <- c(output_cols, corrected_col)
  }

  vapply(
    output_cols,
    .gp3_hampel_check_output_name,
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
      .gp3_hampel_row_id = seq_len(dplyr::n()),
      .gp3_hampel_pupil = suppressWarnings(as.numeric(.data[[pupil_col]]))
    )

  if (!is.null(time_col)) {
    prepared <- prepared |>
      dplyr::mutate(
        .gp3_hampel_time = suppressWarnings(as.numeric(.data[[time_col]]))
      )

    if (any(!is.finite(prepared$.gp3_hampel_time))) {
      stop("`time_col` must be numeric or coercible to finite numeric values.", call. = FALSE)
    }
  } else {
    prepared <- prepared |>
      dplyr::mutate(
        .gp3_hampel_time = .data$.gp3_hampel_row_id
      )
  }

  if (length(grouping_cols) > 0L) {
    grouping_data <- prepared[, grouping_cols, drop = FALSE]

    prepared$.gp3_hampel_group <- as.character(
      do.call(
        interaction,
        c(grouping_data, list(drop = TRUE, sep = "||"))
      )
    )
  } else {
    prepared$.gp3_hampel_group <- "all_rows"
  }

  prepared <- prepared |>
    dplyr::arrange(
      .data$.gp3_hampel_group,
      .data$.gp3_hampel_time,
      .data$.gp3_hampel_row_id
    )

  split_data <- split(prepared, prepared$.gp3_hampel_group)

  filtered <- lapply(
    split_data,
    .gp3_hampel_filter_one_group,
    window_size_samples = window_size_samples,
    k = k,
    min_valid_samples = min_valid_samples,
    scale_mad = scale_mad
  )

  filtered <- dplyr::bind_rows(filtered) |>
    dplyr::arrange(.data$.gp3_hampel_row_id)

  out <- tibble::as_tibble(data)

  out[[median_col]] <- filtered$.gp3_hampel_median
  out[[mad_col]] <- filtered$.gp3_hampel_mad
  out[[threshold_col]] <- filtered$.gp3_hampel_threshold
  out[[flag_col]] <- filtered$.gp3_hampel_flag
  out[[status_col]] <- filtered$.gp3_hampel_status

  if (!is.null(corrected_col)) {
    corrected <- suppressWarnings(as.numeric(data[[pupil_col]]))
    replaceable <- filtered$.gp3_hampel_flag &
      is.finite(filtered$.gp3_hampel_median)

    corrected[replaceable] <- filtered$.gp3_hampel_median[replaceable]
    out[[corrected_col]] <- corrected
  }

  status_summary <- tibble::tibble(
    status = out[[status_col]]
  ) |>
    dplyr::count(.data$status, name = "n") |>
    dplyr::arrange(.data$status)

  n_flagged <- sum(out[[flag_col]], na.rm = TRUE)

  overview <- tibble::tibble(
    object_name = name,
    filter = "hampel",
    pupil_col = pupil_col,
    time_col = .gp3_hampel_collapse_nullable(time_col),
    grouping_cols = .gp3_hampel_collapse_nullable(grouping_cols),
    n_input_rows = nrow(data),
    n_groups = dplyr::n_distinct(filtered$.gp3_hampel_group),
    window_size_samples = window_size_samples,
    k = k,
    min_valid_samples = min_valid_samples,
    scale_mad = scale_mad,
    n_flagged = n_flagged,
    flagged_proportion = n_flagged / nrow(data),
    n_complete = sum(out[[status_col]] %in% c("complete", "complete_zero_mad")),
    n_problem_rows = sum(!out[[status_col]] %in% c("complete", "complete_zero_mad"))
  )

  settings <- tibble::tibble(
    setting = c(
      "pupil_col",
      "time_col",
      "grouping_cols",
      "window_size_samples",
      "k",
      "min_valid_samples",
      "scale_mad",
      "flag_col",
      "median_col",
      "mad_col",
      "threshold_col",
      "corrected_col",
      "status_col",
      "overwrite",
      "name"
    ),
    value = c(
      pupil_col,
      .gp3_hampel_collapse_nullable(time_col),
      .gp3_hampel_collapse_nullable(grouping_cols),
      as.character(window_size_samples),
      as.character(k),
      as.character(min_valid_samples),
      as.character(scale_mad),
      flag_col,
      median_col,
      mad_col,
      threshold_col,
      .gp3_hampel_collapse_nullable(corrected_col),
      status_col,
      as.character(overwrite),
      name
    )
  )

  attr(out, "gp3_hampel_overview") <- overview
  attr(out, "gp3_hampel_status_summary") <- status_summary
  attr(out, "gp3_hampel_settings") <- settings

  class(out) <- c("gp3_pupil_hampel_flags", class(out))

  out
}

.gp3_hampel_filter_one_group <- function(
    df,
    window_size_samples,
    k,
    min_valid_samples,
    scale_mad
) {
  n <- nrow(df)
  half_window <- floor(window_size_samples / 2)

  rolling_median <- rep(NA_real_, n)
  rolling_mad <- rep(NA_real_, n)
  rolling_threshold <- rep(NA_real_, n)
  flag <- rep(FALSE, n)
  status <- rep("complete", n)

  x <- df$.gp3_hampel_pupil

  for (i in seq_len(n)) {
    if (!is.finite(x[[i]])) {
      status[[i]] <- "missing_or_nonfinite_pupil"
      next
    }

    start_i <- max(1L, i - half_window)
    end_i <- min(n, i + half_window)

    window_values <- x[start_i:end_i]
    valid_values <- window_values[is.finite(window_values)]

    if (length(valid_values) < min_valid_samples) {
      status[[i]] <- "insufficient_valid_window"
      next
    }

    med <- stats::median(valid_values, na.rm = TRUE)
    mad_raw <- stats::median(abs(valid_values - med), na.rm = TRUE)
    mad_scaled <- mad_raw * scale_mad
    threshold <- k * mad_scaled

    rolling_median[[i]] <- med
    rolling_mad[[i]] <- mad_scaled
    rolling_threshold[[i]] <- threshold

    deviation <- abs(x[[i]] - med)

    if (isTRUE(all.equal(threshold, 0))) {
      flag[[i]] <- deviation > 0
      status[[i]] <- "complete_zero_mad"
    } else {
      flag[[i]] <- deviation > threshold
      status[[i]] <- "complete"
    }
  }

  df$.gp3_hampel_median <- rolling_median
  df$.gp3_hampel_mad <- rolling_mad
  df$.gp3_hampel_threshold <- rolling_threshold
  df$.gp3_hampel_flag <- flag
  df$.gp3_hampel_status <- status

  df
}

.gp3_hampel_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hampel_check_cols <- function(cols, names_data, arg) {
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

.gp3_hampel_check_odd_positive_integer <- function(x, arg) {
  .gp3_hampel_check_positive_integer(x, arg)

  if (x %% 2 == 0) {
    stop("`", arg, "` must be an odd positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hampel_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hampel_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a positive finite number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hampel_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hampel_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hampel_check_output_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("Each ", arg, " must be a non-missing character scalar.", call. = FALSE)
  }

  TRUE
}

.gp3_hampel_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
