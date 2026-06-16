#' Interpolate Gazepoint pupil data using PCHIP
#'
#' Apply shape-preserving piecewise cubic Hermite interpolation to short internal
#' gaps in Gazepoint pupil time series. This helper is intended as a sensitivity
#' branch alongside the default linear interpolation workflow. It does not
#' overwrite the original pupil column.
#'
#' @param data A data frame containing pupil time-series data.
#' @param pupil_col Name of the pupil column to interpolate. If `NULL`, common
#'   processed pupil columns are detected automatically.
#' @param time_col Name of the time column. If `NULL`, common time columns are
#'   detected automatically.
#' @param grouping_cols Optional character vector of grouping columns. If `NULL`,
#'   common participant/trial/media grouping columns are detected automatically.
#'   Use `character(0)` to interpolate the full data as one sequence.
#' @param max_gap_ms Maximum internal gap duration, in milliseconds, eligible for
#'   interpolation. If both `max_gap_ms` and `max_gap_samples` are supplied, both
#'   criteria must be satisfied.
#' @param max_gap_samples Maximum number of consecutive missing samples eligible
#'   for interpolation.
#' @param min_valid_points Minimum number of valid non-missing points required
#'   within a group before PCHIP interpolation is attempted.
#' @param output_col Name of the interpolated pupil output column.
#' @param flag_col Name of the logical flag column indicating samples filled by
#'   PCHIP interpolation.
#' @param status_col Name of the interpolation-status column.
#'
#' @return A tibble with PCHIP interpolation columns added.
#' @export
interpolate_gazepoint_pupil_pchip <- function(
    data,
    pupil_col = NULL,
    time_col = NULL,
    grouping_cols = NULL,
    max_gap_ms = 500,
    max_gap_samples = NULL,
    min_valid_points = 3,
    output_col = "pupil_interpolated_pchip",
    flag_col = "interpolated_pupil_pchip",
    status_col = "pchip_interpolation_status"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop(
      "Package 'pracma' is required for PCHIP interpolation. ",
      "Install it with install.packages('pracma').",
      call. = FALSE
    )
  }

  .gp3_pchip_check_positive_integer(min_valid_points, "min_valid_points")
  .gp3_pchip_check_label(output_col, "output_col")
  .gp3_pchip_check_label(flag_col, "flag_col")
  .gp3_pchip_check_label(status_col, "status_col")

  if (!is.null(max_gap_ms)) {
    .gp3_pchip_check_positive_number(max_gap_ms, "max_gap_ms")
  }

  if (!is.null(max_gap_samples)) {
    .gp3_pchip_check_positive_integer(max_gap_samples, "max_gap_samples")
  }

  names_data <- names(data)

  pupil_col <- .gp3_pchip_resolve_or_detect_col(
    col = pupil_col,
    names_data = names_data,
    arg = "pupil_col",
    candidates = c(
      "pupil_clean",
      "pupil_for_preprocessing",
      "pupil_raw",
      "mean_pupil",
      "pupil",
      "PUPIL",
      "BPOPD",
      "LPOPD",
      "RPOPD",
      "LPD",
      "RPD"
    ),
    required = TRUE
  )

  time_col <- .gp3_pchip_resolve_or_detect_col(
    col = time_col,
    names_data = names_data,
    arg = "time_col",
    candidates = c(
      "time",
      "time_ms",
      "timestamp",
      "TIMESTAMP",
      "TIME",
      "TIME_TICK",
      "sample_index",
      "CNT"
    ),
    required = TRUE
  )

  if (is.null(grouping_cols)) {
    grouping_cols <- intersect(
      c(
        "subject",
        "participant",
        "participant_id",
        "USER_FILE",
        "recording_id",
        "trial_global",
        "trial",
        "trial_id",
        "media_id",
        "MEDIA_ID"
      ),
      names_data
    )

    grouping_cols <- setdiff(grouping_cols, c(pupil_col, time_col))
  } else {
    grouping_cols <- .gp3_pchip_resolve_cols_allow_empty(
      grouping_cols,
      names_data,
      "grouping_cols"
    )
  }

  out <- tibble::as_tibble(data)
  out$.gp3_pchip_row_id <- seq_len(nrow(out))

  group_key <- .gp3_pchip_group_key(out, grouping_cols)
  split_data <- split(out, group_key)

  interpolated <- lapply(split_data, function(x) {
    .gp3_pchip_interpolate_group(
      group_data = x,
      pupil_col = pupil_col,
      time_col = time_col,
      max_gap_ms = max_gap_ms,
      max_gap_samples = max_gap_samples,
      min_valid_points = min_valid_points,
      output_col = output_col,
      flag_col = flag_col,
      status_col = status_col
    )
  })

  out <- dplyr::bind_rows(interpolated)
  out <- out[order(out$.gp3_pchip_row_id), , drop = FALSE]
  out$.gp3_pchip_row_id <- NULL

  settings <- tibble::tibble(
    setting = c(
      "pupil_col",
      "time_col",
      "grouping_cols",
      "max_gap_ms",
      "max_gap_samples",
      "min_valid_points",
      "output_col",
      "flag_col",
      "status_col"
    ),
    value = c(
      pupil_col,
      time_col,
      .gp3_pchip_collapse_nullable(grouping_cols),
      .gp3_pchip_collapse_nullable(max_gap_ms),
      .gp3_pchip_collapse_nullable(max_gap_samples),
      as.character(min_valid_points),
      output_col,
      flag_col,
      status_col
    )
  )

  attr(out, "gp3_pchip_settings") <- settings
  attr(out, "gp3_pchip_input_col") <- pupil_col
  attr(out, "gp3_pchip_time_col") <- time_col
  attr(out, "gp3_pchip_output_col") <- output_col

  class(out) <- unique(c("gp3_pupil_pchip_interpolation", class(out)))

  out
}

.gp3_pchip_interpolate_group <- function(
    group_data,
    pupil_col,
    time_col,
    max_gap_ms,
    max_gap_samples,
    min_valid_points,
    output_col,
    flag_col,
    status_col
) {
  group_data <- group_data[order(group_data[[time_col]], group_data$.gp3_pchip_row_id), , drop = FALSE]

  pupil <- suppressWarnings(as.numeric(group_data[[pupil_col]]))
  time <- suppressWarnings(as.numeric(group_data[[time_col]]))

  output <- pupil
  interpolated_flag <- rep(FALSE, length(pupil))
  status <- rep("observed", length(pupil))

  value_missing <- is.na(pupil) | !is.finite(pupil)
  time_missing <- is.na(time) | !is.finite(time)

  status[value_missing] <- "missing_unfilled"
  status[time_missing] <- "missing_time"

  gap_info <- .gp3_pchip_gap_info(
    value_missing = value_missing,
    time = time,
    max_gap_ms = max_gap_ms,
    max_gap_samples = max_gap_samples
  )

  valid <- !value_missing & !time_missing

  valid_data <- tibble::tibble(
    time = time[valid],
    pupil = pupil[valid]
  )

  valid_data <- valid_data |>
    dplyr::group_by(.data$time) |>
    dplyr::summarise(
      pupil = mean(.data$pupil, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$time)

  if (nrow(valid_data) < min_valid_points) {
    status[value_missing & !time_missing] <- "missing_insufficient_valid_points"

    group_data[[output_col]] <- output
    group_data[[flag_col]] <- interpolated_flag
    group_data$pchip_gap_id <- gap_info$gap_id
    group_data$pchip_gap_n_samples <- gap_info$gap_n_samples
    group_data$pchip_gap_duration_ms <- gap_info$gap_duration_ms
    group_data$pchip_gap_within_limit <- gap_info$gap_within_limit
    group_data[[status_col]] <- status

    return(group_data)
  }

  internal_missing <- value_missing &
    !time_missing &
    time >= min(valid_data$time, na.rm = TRUE) &
    time <= max(valid_data$time, na.rm = TRUE)

  eligible_missing <- internal_missing & gap_info$gap_within_limit

  status[value_missing & !time_missing & !internal_missing] <- "missing_leading_or_trailing_gap"
  status[internal_missing & !gap_info$gap_within_limit] <- "missing_long_gap"

  if (any(eligible_missing, na.rm = TRUE)) {
    pchip_values <- tryCatch(
      pracma::pchip(
        xi = valid_data$time,
        yi = valid_data$pupil,
        x = time[eligible_missing]
      ),
      error = function(e) rep(NA_real_, sum(eligible_missing, na.rm = TRUE))
    )

    output[eligible_missing] <- pchip_values

    successfully_filled <- eligible_missing & !is.na(output) & is.finite(output)

    interpolated_flag[successfully_filled] <- TRUE
    status[successfully_filled] <- "interpolated_pchip"

    failed_fill <- eligible_missing & !successfully_filled
    status[failed_fill] <- "missing_pchip_failed"
  }

  group_data[[output_col]] <- output
  group_data[[flag_col]] <- interpolated_flag
  group_data$pchip_gap_id <- gap_info$gap_id
  group_data$pchip_gap_n_samples <- gap_info$gap_n_samples
  group_data$pchip_gap_duration_ms <- gap_info$gap_duration_ms
  group_data$pchip_gap_within_limit <- gap_info$gap_within_limit
  group_data[[status_col]] <- status

  group_data
}

.gp3_pchip_gap_info <- function(
    value_missing,
    time,
    max_gap_ms,
    max_gap_samples
) {
  n <- length(value_missing)

  gap_id <- rep(NA_integer_, n)
  gap_n_samples <- rep(NA_integer_, n)
  gap_duration_ms <- rep(NA_real_, n)
  gap_within_limit <- rep(FALSE, n)

  if (!any(value_missing, na.rm = TRUE)) {
    return(
      list(
        gap_id = gap_id,
        gap_n_samples = gap_n_samples,
        gap_duration_ms = gap_duration_ms,
        gap_within_limit = gap_within_limit
      )
    )
  }

  missing_start <- value_missing & !dplyr::lag(value_missing, default = FALSE)
  running_gap_id <- cumsum(missing_start)
  gap_id[value_missing] <- running_gap_id[value_missing]

  finite_time <- time[is.finite(time)]
  positive_steps <- diff(sort(unique(finite_time)))
  positive_steps <- positive_steps[is.finite(positive_steps) & positive_steps > 0]

  sample_step <- if (length(positive_steps) == 0L) {
    NA_real_
  } else {
    stats::median(positive_steps, na.rm = TRUE)
  }

  for (id in unique(gap_id[!is.na(gap_id)])) {
    idx <- which(gap_id == id)
    gap_n <- length(idx)

    gap_duration <- if (all(is.na(time[idx])) || !is.finite(sample_step)) {
      NA_real_
    } else {
      max(time[idx], na.rm = TRUE) - min(time[idx], na.rm = TRUE) + sample_step
    }

    within_samples <- if (is.null(max_gap_samples)) {
      TRUE
    } else {
      gap_n <= max_gap_samples
    }

    within_ms <- if (is.null(max_gap_ms) || is.na(gap_duration)) {
      TRUE
    } else {
      gap_duration <= max_gap_ms
    }

    gap_n_samples[idx] <- gap_n
    gap_duration_ms[idx] <- gap_duration
    gap_within_limit[idx] <- within_samples && within_ms
  }

  list(
    gap_id = gap_id,
    gap_n_samples = gap_n_samples,
    gap_duration_ms = gap_duration_ms,
    gap_within_limit = gap_within_limit
  )
}

.gp3_pchip_group_key <- function(data, grouping_cols) {
  if (length(grouping_cols) == 0L) {
    return(rep("all_data", nrow(data)))
  }

  key_data <- data[grouping_cols]

  key_data[] <- lapply(key_data, function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr) | !nzchar(x_chr)] <- "missing"
    x_chr
  })

  do.call(paste, c(key_data, sep = "||"))
}

.gp3_pchip_resolve_cols_allow_empty <- function(cols, names_data, arg) {
  if (!is.character(cols) || anyNA(cols)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }

  if (length(cols) == 0L) {
    return(character(0))
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_pchip_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_pchip_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_pchip_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_pchip_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pchip_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a finite positive number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pchip_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  if (x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pchip_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
