#' Interpolate short missing gaps in Gazepoint pupil data
#'
#' Performs linear interpolation over short internal gaps in Gazepoint pupil
#' data. This function is intended to be used after [flag_gazepoint_pupil()] or
#' [flag_gazepoint_pupil_artifacts()]. When available, `pupil_clean` is used as
#' the preferred default input column, followed by `pupil_for_preprocessing`.
#' Leading gaps, trailing gaps, long gaps, non-finite time values, and groups
#' with too few valid pupil samples are not interpolated.
#'
#' @param data A Gazepoint master table, preferably after [flag_gazepoint_pupil()]
#'   or [flag_gazepoint_pupil_artifacts()].
#' @param pupil_col Optional name of the pupil column to interpolate. If `NULL`,
#'   the function detects one of `pupil_clean`, `pupil_for_preprocessing`,
#'   `mean_pupil`, `pupil`, `pupil_raw`, `left_pupil`, or `right_pupil`.
#' @param time_col Optional name of the time column. If `NULL`, the function
#'   detects one of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.
#' @param group_cols Character vector of grouping columns used to keep
#'   interpolation within independent time series. Defaults to
#'   `c("subject", "media_id")` using internally standardised names. Use
#'   `character(0)` for global interpolation.
#' @param max_gap_ms Maximum duration, in milliseconds, of a gap that may be
#'   interpolated. The duration is measured between the valid samples immediately
#'   before and after the gap. Defaults to `150`.
#' @param max_gap_samples Maximum number of consecutive missing samples that may
#'   be interpolated. Defaults to `Inf`.
#' @param min_valid_points Minimum number of valid samples required within a
#'   group before interpolation is attempted. Defaults to `2`.
#'
#' @return A tibble containing the original data plus interpolation columns.
#'
#' @examples
#' \dontrun{
#' flagged <- flag_gazepoint_pupil(master)
#'
#' interpolated <- interpolate_gazepoint_pupil(flagged)
#'
#' dplyr::count(interpolated, pupil_interpolation_status)
#'
#' artifact_flagged <- flag_gazepoint_pupil_artifacts(master)
#'
#' artifact_interpolated <- interpolate_gazepoint_pupil(artifact_flagged)
#'
#' dplyr::count(artifact_interpolated, pupil_interpolation_status)
#' }
#'
#' @importFrom rlang .data
#'
#' @export
interpolate_gazepoint_pupil <- function(
    data,
    pupil_col = NULL,
    time_col = NULL,
    group_cols = c("subject", "media_id"),
    max_gap_ms = 150,
    max_gap_samples = Inf,
    min_valid_points = 2
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  if (!is.null(pupil_col) && (!is.character(pupil_col) || length(pupil_col) != 1)) {
    rlang::abort("`pupil_col` must be `NULL` or a single character string.")
  }

  if (!is.null(time_col) && (!is.character(time_col) || length(time_col) != 1)) {
    rlang::abort("`time_col` must be `NULL` or a single character string.")
  }

  if (!is.character(group_cols)) {
    rlang::abort("`group_cols` must be a character vector.")
  }

  if (!is.numeric(max_gap_ms) || length(max_gap_ms) != 1) {
    rlang::abort("`max_gap_ms` must be a single numeric value.")
  }

  if (!is.numeric(max_gap_samples) || length(max_gap_samples) != 1) {
    rlang::abort("`max_gap_samples` must be a single numeric value.")
  }

  if (!is.numeric(min_valid_points) || length(min_valid_points) != 1) {
    rlang::abort("`min_valid_points` must be a single numeric value.")
  }

  if (max_gap_ms < 0) {
    rlang::abort("`max_gap_ms` must be greater than or equal to 0.")
  }

  if (max_gap_samples < 0) {
    rlang::abort("`max_gap_samples` must be greater than or equal to 0.")
  }

  if (min_valid_points < 2) {
    rlang::abort("`min_valid_points` must be greater than or equal to 2.")
  }

  detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0) {
      return(NA_character_)
    }

    found[[1]]
  }

  subject_source <- detect_col(c("subject", "pID", "participant"))
  media_source <- detect_col(c("media_id", "MEDIA_ID"))
  trial_source <- detect_col(c("trial"))
  trial_global_source <- detect_col(c("trial_global"))

  if (is.null(pupil_col)) {
    pupil_source <- detect_col(c(
      "pupil_clean",
      "pupil_for_preprocessing",
      "mean_pupil",
      "pupil",
      "pupil_raw",
      "left_pupil",
      "right_pupil"
    ))
  } else {
    pupil_source <- pupil_col
  }

  if (is.null(time_col)) {
    time_source <- detect_col(c("time_ms", "time", "time_orig", "time_orig_ms"))
  } else {
    time_source <- time_col
  }

  if (is.na(pupil_source) || !pupil_source %in% names(data)) {
    rlang::abort("No pupil column was found.")
  }

  if (is.na(time_source) || !time_source %in% names(data)) {
    rlang::abort("No time column was found.")
  }

  available_group_roles <- c(
    subject = subject_source,
    media_id = media_source,
    trial = trial_source,
    trial_global = trial_global_source
  )

  allowed_group_cols <- names(available_group_roles)

  invalid_group_cols <- setdiff(group_cols, allowed_group_cols)

  if (length(invalid_group_cols) > 0) {
    rlang::abort(
      paste0(
        "`group_cols` can only contain: ",
        paste(allowed_group_cols, collapse = ", ")
      )
    )
  }

  missing_group_cols <- group_cols[
    is.na(available_group_roles[group_cols])
  ]

  if (length(missing_group_cols) > 0) {
    rlang::abort(
      paste0(
        "The following grouping column role(s) were requested but not found: ",
        paste(missing_group_cols, collapse = ", ")
      )
    )
  }

  work <- tibble::tibble(
    row_id = seq_len(nrow(data)),
    subject = if (!is.na(subject_source)) {
      as.character(data[[subject_source]])
    } else {
      NA_character_
    },
    media_id = if (!is.na(media_source)) {
      as.character(data[[media_source]])
    } else {
      NA_character_
    },
    trial = if (!is.na(trial_source)) {
      as.character(data[[trial_source]])
    } else {
      NA_character_
    },
    trial_global = if (!is.na(trial_global_source)) {
      as.character(data[[trial_global_source]])
    } else {
      NA_character_
    },
    time_ms = suppressWarnings(as.numeric(data[[time_source]])),
    pupil_input_value = suppressWarnings(as.numeric(data[[pupil_source]]))
  )

  interpolate_group <- function(group_data) {
    group_data <- group_data |>
      dplyr::arrange(.data$time_ms, .data$row_id)

    n <- nrow(group_data)

    pupil_input <- group_data$pupil_input_value
    time_ms <- group_data$time_ms

    pupil_observed <- is.finite(pupil_input)
    time_valid <- is.finite(time_ms)
    endpoint_valid <- pupil_observed & time_valid
    needs_interpolation <- !pupil_observed

    pupil_after <- pupil_input
    was_interpolated <- rep(FALSE, n)
    interpolation_status <- rep("observed", n)

    interpolation_status[needs_interpolation] <- "missing_unfilled"
    interpolation_status[needs_interpolation & !time_valid] <- "missing_no_time"

    gap_id <- rep(NA_integer_, n)
    gap_n_samples <- rep(NA_integer_, n)
    gap_duration_ms <- rep(NA_real_, n)

    if (sum(endpoint_valid, na.rm = TRUE) < min_valid_points) {
      interpolation_status[needs_interpolation & time_valid] <-
        "missing_insufficient_valid"

      return(
        tibble::tibble(
          row_id = group_data$row_id,
          pupil_interpolated = pupil_after,
          pupil_was_interpolated = was_interpolated,
          pupil_interpolation_status = interpolation_status,
          pupil_gap_id = gap_id,
          pupil_gap_n_samples = gap_n_samples,
          pupil_gap_duration_ms = gap_duration_ms
        )
      )
    }

    if (!any(needs_interpolation, na.rm = TRUE)) {
      return(
        tibble::tibble(
          row_id = group_data$row_id,
          pupil_interpolated = pupil_after,
          pupil_was_interpolated = was_interpolated,
          pupil_interpolation_status = interpolation_status,
          pupil_gap_id = gap_id,
          pupil_gap_n_samples = gap_n_samples,
          pupil_gap_duration_ms = gap_duration_ms
        )
      )
    }

    runs <- rle(needs_interpolation)
    run_ends <- cumsum(runs$lengths)
    run_starts <- run_ends - runs$lengths + 1

    missing_run_counter <- 0L

    valid_times <- time_ms[endpoint_valid]
    valid_pupils <- pupil_input[endpoint_valid]

    for (i in seq_along(runs$values)) {
      if (!isTRUE(runs$values[[i]])) {
        next
      }

      missing_run_counter <- missing_run_counter + 1L

      idx <- seq.int(run_starts[[i]], run_ends[[i]])

      gap_id[idx] <- missing_run_counter
      gap_n_samples[idx] <- length(idx)

      previous_valid <- which(endpoint_valid & seq_len(n) < min(idx))
      next_valid <- which(endpoint_valid & seq_len(n) > max(idx))

      if (length(previous_valid) == 0 || length(next_valid) == 0) {
        interpolation_status[idx[time_valid[idx]]] <- "missing_edge_gap"
        next
      }

      left_idx <- max(previous_valid)
      right_idx <- min(next_valid)

      current_gap_duration <- time_ms[[right_idx]] - time_ms[[left_idx]]
      gap_duration_ms[idx] <- current_gap_duration

      if (
        !is.finite(current_gap_duration) ||
        current_gap_duration > max_gap_ms ||
        length(idx) > max_gap_samples
      ) {
        interpolation_status[idx[time_valid[idx]]] <- "missing_long_gap"
        next
      }

      interpolated_values <- stats::approx(
        x = valid_times,
        y = valid_pupils,
        xout = time_ms[idx],
        method = "linear",
        rule = 1,
        ties = mean
      )$y

      fillable <- is.finite(time_ms[idx]) & is.finite(interpolated_values)

      if (any(fillable)) {
        fill_idx <- idx[fillable]

        pupil_after[fill_idx] <- interpolated_values[fillable]
        was_interpolated[fill_idx] <- TRUE
        interpolation_status[fill_idx] <- "interpolated"
      }

      if (any(!fillable)) {
        interpolation_status[idx[!fillable & time_valid[idx]]] <-
          "missing_unfilled"
      }
    }

    tibble::tibble(
      row_id = group_data$row_id,
      pupil_interpolated = pupil_after,
      pupil_was_interpolated = was_interpolated,
      pupil_interpolation_status = interpolation_status,
      pupil_gap_id = gap_id,
      pupil_gap_n_samples = gap_n_samples,
      pupil_gap_duration_ms = gap_duration_ms
    )
  }

  interpolated <- if (length(group_cols) == 0) {
    interpolate_group(work)
  } else {
    work |>
      dplyr::group_by(!!!rlang::syms(group_cols)) |>
      dplyr::group_modify(~ interpolate_group(.x)) |>
      dplyr::ungroup()
  }

  interpolated <- interpolated |>
    dplyr::arrange(.data$row_id) |>
    dplyr::mutate(
      pupil_interp_pupil_column = pupil_source,
      pupil_interp_time_column = time_source,
      pupil_interp_max_gap_ms = max_gap_ms,
      pupil_interp_max_gap_samples = max_gap_samples,
      pupil_interp_min_valid_points = min_valid_points
    )

  output_cols <- c(
    "pupil_interpolated",
    "pupil_was_interpolated",
    "pupil_interpolation_status",
    "pupil_gap_id",
    "pupil_gap_n_samples",
    "pupil_gap_duration_ms",
    "pupil_interp_pupil_column",
    "pupil_interp_time_column",
    "pupil_interp_max_gap_ms",
    "pupil_interp_max_gap_samples",
    "pupil_interp_min_valid_points"
  )

  original <- tibble::as_tibble(data)
  original[intersect(names(original), output_cols)] <- NULL

  dplyr::bind_cols(
    original,
    interpolated |>
      dplyr::select(dplyr::all_of(output_cols))
  )
}
