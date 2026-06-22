#' Baseline-correct Gazepoint pupil data
#'
#' Computes baseline-corrected pupil columns from a Gazepoint pupil time series,
#' typically after [flag_gazepoint_pupil()] and [interpolate_gazepoint_pupil()].
#' Baselines can be defined either by a time window, such as `c(-200, 0)`, or by
#' a user-supplied logical baseline/pre-stimulus flag column.
#'
#' @param data A Gazepoint master table, preferably after
#'   [interpolate_gazepoint_pupil()].
#' @param pupil_col Optional name of the pupil column to baseline-correct. If
#'   `NULL`, the function detects one of `pupil_interpolated`,
#'   `pupil_for_preprocessing`, `mean_pupil`, `pupil`, `pupil_raw`,
#'   `left_pupil`, or `right_pupil`.
#' @param time_col Optional name of the main time column. If `NULL`, the
#'   function detects one of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.
#' @param baseline_time_col Optional name of the time column used for selecting
#'   baseline samples. If `NULL`, the function detects relative-time columns
#'   first, then falls back to `time_col`.
#' @param baseline_window Numeric vector of length two giving the baseline
#'   window in milliseconds. Defaults to `c(-200, 0)`. This can also be set to
#'   post-onset or early-window values such as `c(0, 200)` when no pre-stimulus
#'   period is available.
#' @param baseline_flag_col Optional logical column identifying baseline samples.
#'   If supplied, this takes priority over `baseline_window`.
#' @param group_cols Character vector of grouping columns used to compute one
#'   baseline per independent time series. Defaults to `c("subject",
#'   "media_id")`. Columns such as `"trial"` or `"trial_global"` can be added
#'   when available. Use `character(0)` for one global baseline.
#' @param baseline_method Baseline statistic. One of `"mean"` or `"median"`.
#'   Defaults to `"mean"`.
#' @param min_baseline_samples Minimum number of valid baseline samples required
#'   to compute a baseline. Defaults to `1`.
#'
#' @return A tibble containing the original data plus baseline-correction
#'   columns.
#'
#' @examples
#' \donttest{
#' master <- gazepoint_example_master
#' flagged <- flag_gazepoint_pupil(master)
#' interpolated <- interpolate_gazepoint_pupil(flagged)
#'
#' corrected <- baseline_correct_gazepoint_pupil(
#'   interpolated,
#'   baseline_window = c(-200, 0)
#' )
#'
#' corrected <- baseline_correct_gazepoint_pupil(
#'   interpolated,
#'   baseline_window = c(0, 200)
#' )
#' }
#'
#' @importFrom rlang .data
#'
#' @export
baseline_correct_gazepoint_pupil <- function(
    data,
    pupil_col = NULL,
    time_col = NULL,
    baseline_time_col = NULL,
    baseline_window = c(-200, 0),
    baseline_flag_col = NULL,
    group_cols = c("subject", "media_id"),
    baseline_method = c("mean", "median"),
    min_baseline_samples = 1
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

  if (
    !is.null(baseline_time_col) &&
    (!is.character(baseline_time_col) || length(baseline_time_col) != 1)
  ) {
    rlang::abort("`baseline_time_col` must be `NULL` or a single character string.")
  }

  if (
    !is.null(baseline_flag_col) &&
    (!is.character(baseline_flag_col) || length(baseline_flag_col) != 1)
  ) {
    rlang::abort("`baseline_flag_col` must be `NULL` or a single character string.")
  }

  if (!is.character(group_cols)) {
    rlang::abort("`group_cols` must be a character vector.")
  }

  if (!is.numeric(min_baseline_samples) || length(min_baseline_samples) != 1) {
    rlang::abort("`min_baseline_samples` must be a single numeric value.")
  }

  if (min_baseline_samples < 1) {
    rlang::abort("`min_baseline_samples` must be greater than or equal to 1.")
  }

  baseline_method <- match.arg(baseline_method)

  if (is.null(baseline_flag_col)) {
    if (!is.numeric(baseline_window) || length(baseline_window) != 2) {
      rlang::abort("`baseline_window` must be a numeric vector of length 2.")
    }

    if (any(is.na(baseline_window))) {
      rlang::abort("`baseline_window` must not contain missing values.")
    }

    if (baseline_window[[2]] < baseline_window[[1]]) {
      rlang::abort("`baseline_window[2]` must be greater than or equal to `baseline_window[1]`.")
    }
  } else {
    if (!is.null(baseline_window)) {
      if (!is.numeric(baseline_window) || length(baseline_window) != 2) {
        rlang::abort("`baseline_window` must be `NULL` or a numeric vector of length 2.")
      }
    }
  }

  baseline_window_start <- if (is.null(baseline_window)) NA_real_ else baseline_window[[1]]
  baseline_window_end <- if (is.null(baseline_window)) NA_real_ else baseline_window[[2]]

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

  if (is.null(baseline_time_col)) {
    baseline_time_source <- detect_col(c(
      "time_relative_ms",
      "relative_time_ms",
      "event_time_ms",
      "time_ms",
      "time",
      "time_orig",
      "time_orig_ms"
    ))
  } else {
    baseline_time_source <- baseline_time_col
  }

  if (is.na(pupil_source) || !pupil_source %in% names(data)) {
    rlang::abort("No pupil column was found.")
  }

  if (is.na(time_source) || !time_source %in% names(data)) {
    rlang::abort("No time column was found.")
  }

  if (is.na(baseline_time_source) || !baseline_time_source %in% names(data)) {
    rlang::abort("No baseline-time column was found.")
  }

  if (!is.null(baseline_flag_col) && !baseline_flag_col %in% names(data)) {
    rlang::abort("`baseline_flag_col` was not found in `data`.")
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
    baseline_time_ms = suppressWarnings(as.numeric(data[[baseline_time_source]])),
    pupil_input_value = suppressWarnings(as.numeric(data[[pupil_source]]))
  )

  if (length(non_role_group_cols) > 0) {
    for (col in non_role_group_cols) {
      work[[col]] <- data[[col]]
    }
  }

  work <- work |>
    dplyr::mutate(
      baseline_candidate = if (!is.null(baseline_flag_col)) {
        as.logical(data[[baseline_flag_col]]) &
          is.finite(.data$pupil_input_value)
      } else {
        is.finite(.data$pupil_input_value) &
          is.finite(.data$baseline_time_ms) &
          .data$baseline_time_ms >= baseline_window_start &
          .data$baseline_time_ms <= baseline_window_end
      }
    )

  safe_baseline_value <- function(x, method) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    if (identical(method, "median")) {
      return(stats::median(x, na.rm = TRUE))
    }

    mean(x, na.rm = TRUE)
  }

  safe_sd <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) < 2) {
      return(NA_real_)
    }

    stats::sd(x, na.rm = TRUE)
  }

  safe_min <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    min(x)
  }

  safe_max <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    max(x)
  }

  summarise_baseline <- function(df) {
    baseline_values <- df$pupil_input_value[df$baseline_candidate]
    baseline_times <- df$baseline_time_ms[df$baseline_candidate]

    tibble::tibble(
      pupil_baseline_n = sum(df$baseline_candidate, na.rm = TRUE),
      pupil_baseline_value = safe_baseline_value(
        baseline_values,
        baseline_method
      ),
      pupil_baseline_sd = safe_sd(baseline_values),
      pupil_baseline_time_min = safe_min(baseline_times),
      pupil_baseline_time_max = safe_max(baseline_times)
    )
  }

  if (length(group_cols) == 0) {
    baseline_summary <- summarise_baseline(work)

    work <- dplyr::bind_cols(
      work,
      baseline_summary[rep(1, nrow(work)), ]
    )
  } else {
    baseline_summary <- work |>
      dplyr::group_by(!!!rlang::syms(group_cols)) |>
      dplyr::group_modify(~ summarise_baseline(.x)) |>
      dplyr::ungroup()

    work <- work |>
      dplyr::left_join(
        baseline_summary,
        by = group_cols
      )
  }

  work <- work |>
    dplyr::mutate(
      pupil_baseline_available = .data$pupil_baseline_n >= min_baseline_samples &
        is.finite(.data$pupil_baseline_value),
      pupil_baseline_corrected = dplyr::if_else(
        .data$pupil_baseline_available & is.finite(.data$pupil_input_value),
        .data$pupil_input_value - .data$pupil_baseline_value,
        NA_real_
      ),
      pupil_baseline_percent_change = dplyr::if_else(
        .data$pupil_baseline_available &
          is.finite(.data$pupil_input_value) &
          .data$pupil_baseline_value != 0,
        ((.data$pupil_input_value - .data$pupil_baseline_value) /
           .data$pupil_baseline_value) * 100,
        NA_real_
      ),
      pupil_baseline_ratio = dplyr::if_else(
        .data$pupil_baseline_available &
          is.finite(.data$pupil_input_value) &
          .data$pupil_baseline_value != 0,
        .data$pupil_input_value / .data$pupil_baseline_value,
        NA_real_
      ),
      pupil_baseline_z = dplyr::if_else(
        .data$pupil_baseline_available &
          is.finite(.data$pupil_input_value) &
          is.finite(.data$pupil_baseline_sd) &
          .data$pupil_baseline_sd > 0,
        (.data$pupil_input_value - .data$pupil_baseline_value) /
          .data$pupil_baseline_sd,
        NA_real_
      ),
      pupil_baseline_status = dplyr::case_when(
        !is.finite(.data$pupil_input_value) ~ "missing_pupil",
        !.data$pupil_baseline_available ~ "no_baseline",
        TRUE ~ "corrected"
      ),
      pupil_baseline_used = .data$baseline_candidate,
      pupil_baseline_pupil_column = pupil_source,
      pupil_baseline_time_column = baseline_time_source,
      pupil_baseline_flag_column = if (is.null(baseline_flag_col)) {
        NA_character_
      } else {
        baseline_flag_col
      },
      pupil_baseline_window_start = baseline_window_start,
      pupil_baseline_window_end = baseline_window_end,
      pupil_baseline_method = baseline_method,
      pupil_baseline_min_samples = min_baseline_samples
    ) |>
    dplyr::arrange(.data$row_id)

  output_cols <- c(
    "pupil_baseline_value",
    "pupil_baseline_sd",
    "pupil_baseline_n",
    "pupil_baseline_available",
    "pupil_baseline_time_min",
    "pupil_baseline_time_max",
    "pupil_baseline_used",
    "pupil_baseline_corrected",
    "pupil_baseline_percent_change",
    "pupil_baseline_ratio",
    "pupil_baseline_z",
    "pupil_baseline_status",
    "pupil_baseline_pupil_column",
    "pupil_baseline_time_column",
    "pupil_baseline_flag_column",
    "pupil_baseline_window_start",
    "pupil_baseline_window_end",
    "pupil_baseline_method",
    "pupil_baseline_min_samples"
  )

  original <- tibble::as_tibble(data)
  original[intersect(names(original), output_cols)] <- NULL

  dplyr::bind_cols(
    original,
    work |>
      dplyr::select(dplyr::all_of(output_cols))
  )
}
