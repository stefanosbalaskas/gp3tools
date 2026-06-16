#' Smooth Gazepoint pupil data
#'
#' Applies sample-based rolling smoothing to a Gazepoint pupil time series,
#' typically after [flag_gazepoint_pupil()], [interpolate_gazepoint_pupil()],
#' and optionally [baseline_correct_gazepoint_pupil()]. The function preserves
#' the original pupil column and adds smoothed-output columns.
#'
#' @param data A Gazepoint master table, preferably after pupil preprocessing.
#' @param pupil_col Optional name of the pupil column to smooth. If `NULL`, the
#'   function detects one of `pupil_baseline_corrected`,
#'   `pupil_baseline_percent_change`, `pupil_interpolated`,
#'   `pupil_for_preprocessing`, `mean_pupil`, `pupil`, `pupil_raw`,
#'   `left_pupil`, or `right_pupil`.
#' @param time_col Optional name of the time column. If `NULL`, the function
#'   detects one of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.
#' @param group_cols Character vector of grouping columns used to keep smoothing
#'   within independent time series. Defaults to `c("subject", "media_id")`.
#'   Columns such as `"trial"` or `"trial_global"` can be added when available.
#'   Use `character(0)` for global smoothing.
#' @param window_samples Number of samples in the rolling smoothing window.
#'   Defaults to `5`.
#' @param method Smoothing statistic. One of `"mean"` or `"median"`. Defaults
#'   to `"mean"`.
#' @param align Window alignment. One of `"center"`, `"right"`, or `"left"`.
#'   Defaults to `"center"`.
#' @param min_points Minimum number of finite values required inside a window to
#'   return a smoothed value. Defaults to `1`.
#' @param preserve_missing Logical. If `TRUE`, rows with missing/non-finite input
#'   remain missing in `pupil_smoothed`. Defaults to `TRUE`.
#'
#' @return A tibble containing the original data plus pupil-smoothing columns.
#'
#' @examples
#' \dontrun{
#' smoothed <- smooth_gazepoint_pupil(
#'   baseline_corrected,
#'   pupil_col = "pupil_baseline_corrected",
#'   window_samples = 5,
#'   method = "mean"
#' )
#'
#' dplyr::count(smoothed, pupil_smoothing_status)
#' }
#'
#' @importFrom rlang .data
#'
#' @export
smooth_gazepoint_pupil <- function(
    data,
    pupil_col = NULL,
    time_col = NULL,
    group_cols = c("subject", "media_id"),
    window_samples = 5,
    method = c("mean", "median"),
    align = c("center", "right", "left"),
    min_points = 1,
    preserve_missing = TRUE
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

  if (!is.numeric(window_samples) || length(window_samples) != 1) {
    rlang::abort("`window_samples` must be a single numeric value.")
  }

  if (!is.numeric(min_points) || length(min_points) != 1) {
    rlang::abort("`min_points` must be a single numeric value.")
  }

  if (!is.logical(preserve_missing) || length(preserve_missing) != 1) {
    rlang::abort("`preserve_missing` must be `TRUE` or `FALSE`.")
  }

  if (window_samples < 1) {
    rlang::abort("`window_samples` must be greater than or equal to 1.")
  }

  if (min_points < 1) {
    rlang::abort("`min_points` must be greater than or equal to 1.")
  }

  window_samples <- as.integer(window_samples)
  min_points <- as.integer(min_points)

  if (min_points > window_samples) {
    rlang::abort("`min_points` must be less than or equal to `window_samples`.")
  }

  method <- match.arg(method)
  align <- match.arg(align)

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
      "pupil_baseline_corrected",
      "pupil_baseline_percent_change",
      "pupil_interpolated",
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

  role_sources <- c(
    subject = subject_source,
    media_id = media_source,
    trial = trial_source,
    trial_global = trial_global_source
  )

  standard_group_roles <- names(role_sources)

  missing_group_roles <- group_cols[
    group_cols %in% standard_group_roles &
      is.na(role_sources[group_cols])
  ]

  if (length(missing_group_roles) > 0) {
    rlang::abort(
      paste0(
        "The following grouping column role(s) were requested but not found: ",
        paste(missing_group_roles, collapse = ", ")
      )
    )
  }

  non_role_group_cols <- setdiff(group_cols, standard_group_roles)
  missing_non_role_group_cols <- setdiff(non_role_group_cols, names(data))

  if (length(missing_non_role_group_cols) > 0) {
    rlang::abort(
      paste0(
        "The following grouping column(s) were requested but not found: ",
        paste(missing_non_role_group_cols, collapse = ", ")
      )
    )
  }

  work <- tibble::tibble(
    row_id = seq_len(nrow(data)),
    subject = if (!is.na(subject_source)) as.character(data[[subject_source]]) else NA_character_,
    media_id = if (!is.na(media_source)) as.character(data[[media_source]]) else NA_character_,
    trial = if (!is.na(trial_source)) as.character(data[[trial_source]]) else NA_character_,
    trial_global = if (!is.na(trial_global_source)) as.character(data[[trial_global_source]]) else NA_character_,
    time_ms = suppressWarnings(as.numeric(data[[time_source]])),
    pupil_input_value = suppressWarnings(as.numeric(data[[pupil_source]]))
  )

  if (length(non_role_group_cols) > 0) {
    for (col in non_role_group_cols) {
      work[[col]] <- data[[col]]
    }
  }

  smooth_vector <- function(x) {
    n <- length(x)
    x <- suppressWarnings(as.numeric(x))

    smoothed <- rep(NA_real_, n)
    window_n <- rep(NA_integer_, n)
    status <- rep("insufficient_window", n)

    input_finite <- is.finite(x)
    status[!input_finite] <- "missing_input"

    for (i in seq_len(n)) {
      if (identical(align, "center")) {
        before <- floor((window_samples - 1) / 2)
        after <- window_samples - 1 - before
        start <- max(1L, i - before)
        end <- min(n, i + after)
      } else if (identical(align, "right")) {
        start <- max(1L, i - window_samples + 1L)
        end <- i
      } else {
        start <- i
        end <- min(n, i + window_samples - 1L)
      }

      values <- x[start:end]
      values <- values[is.finite(values)]
      window_n[[i]] <- length(values)

      if (isTRUE(preserve_missing) && !input_finite[[i]]) {
        next
      }

      if (length(values) < min_points) {
        next
      }

      smoothed[[i]] <- if (identical(method, "median")) {
        stats::median(values, na.rm = TRUE)
      } else {
        mean(values, na.rm = TRUE)
      }

      status[[i]] <- "smoothed"
    }

    tibble::tibble(
      pupil_smoothed = smoothed,
      pupil_smoothing_status = status,
      pupil_smoothing_window_n = window_n
    )
  }

  smoothed <- if (length(group_cols) == 0) {
    dplyr::bind_cols(
      work["row_id"],
      smooth_vector(work$pupil_input_value)
    )
  } else {
    work |>
      dplyr::group_by(!!!rlang::syms(group_cols)) |>
      dplyr::group_modify(~ {
        result <- smooth_vector(.x$pupil_input_value)
        dplyr::bind_cols(.x["row_id"], result)
      }) |>
      dplyr::ungroup()
  }

  smoothed <- smoothed |>
    dplyr::arrange(.data$row_id) |>
    dplyr::mutate(
      pupil_smoothing_input_column = pupil_source,
      pupil_smoothing_time_column = time_source,
      pupil_smoothing_method = method,
      pupil_smoothing_align = align,
      pupil_smoothing_window_samples = window_samples,
      pupil_smoothing_min_points = min_points,
      pupil_smoothing_preserve_missing = preserve_missing
    )

  output_cols <- c(
    "pupil_smoothed",
    "pupil_smoothing_status",
    "pupil_smoothing_window_n",
    "pupil_smoothing_input_column",
    "pupil_smoothing_time_column",
    "pupil_smoothing_method",
    "pupil_smoothing_align",
    "pupil_smoothing_window_samples",
    "pupil_smoothing_min_points",
    "pupil_smoothing_preserve_missing"
  )

  original <- tibble::as_tibble(data)
  original[intersect(names(original), output_cols)] <- NULL

  dplyr::bind_cols(
    original,
    smoothed |>
      dplyr::select(dplyr::all_of(output_cols))
  )
}
