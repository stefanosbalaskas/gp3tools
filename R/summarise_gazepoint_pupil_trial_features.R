#' Summarise Gazepoint pupil trial-level features
#'
#' Convert sample-level Gazepoint pupil time series into trial-level pupil
#' features for statistical modelling.
#'
#' The function summarises one row per trial or other user-defined grouping.
#' It computes mean pupil, peak pupil, time-to-peak, AUC, early/middle/late
#' window means, valid-sample percentage, interpolation percentage, artifact
#' percentage, and missingness summaries.
#'
#' @param data A Gazepoint pupil data frame.
#' @param group_cols Character vector of grouping columns. The default is
#'   `c("subject", "trial_global")`.
#' @param pupil_col Name of the processed pupil column to summarise. If `NULL`,
#'   the function tries `pupil_smoothed`, `pupil_baseline_corrected`,
#'   `pupil_baseline_percent_change`, `pupil_interpolated`, `pupil_clean`,
#'   and `pupil`.
#' @param time_col Name of the time column.
#' @param interpolated_col Optional logical interpolation flag column.
#' @param artifact_col Optional artifact flag column. If `NULL`, the function
#'   tries to detect `pupil_artifact_flag`, `pupil_flag_invalid`, or
#'   `artifact_flag`.
#' @param artifact_reason_col Optional artifact-reason column. If `NULL`, the
#'   function tries to detect `pupil_artifact_reason`, `pupil_flag_reason`, or
#'   `artifact_reason`.
#' @param early_window Numeric vector of length 2 defining the early window in
#'   milliseconds.
#' @param middle_window Numeric vector of length 2 defining the middle window in
#'   milliseconds.
#' @param late_window Numeric vector of length 2 defining the late window in
#'   milliseconds.
#' @param min_valid_samples Minimum number of valid pupil samples required for
#'   a trial to be labelled `"ok"`.
#'
#' @return A tibble with one row per trial/group.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_pupil_trial_features <- function(
    data,
    group_cols = c("subject", "trial_global"),
    pupil_col = NULL,
    time_col = "time",
    interpolated_col = "pupil_was_interpolated",
    artifact_col = NULL,
    artifact_reason_col = NULL,
    early_window = c(0, 500),
    middle_window = c(500, 1500),
    late_window = c(1500, 3000),
    min_valid_samples = 1
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(group_cols) ||
      any(is.na(group_cols)) ||
      any(!nzchar(group_cols)) ||
      anyDuplicated(group_cols)) {
    stop(
      "`group_cols` must be a character vector of unique column names.",
      call. = FALSE
    )
  }

  valid_optional_column <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          nzchar(x)
      )
  }

  optional_column_args <- list(
    pupil_col = pupil_col,
    interpolated_col = interpolated_col,
    artifact_col = artifact_col,
    artifact_reason_col = artifact_reason_col
  )

  valid_optional_args <- vapply(
    optional_column_args,
    valid_optional_column,
    logical(1)
  )

  if (any(!valid_optional_args)) {
    stop(
      "Optional column-name arguments must be NULL or non-missing character scalars: ",
      paste(names(valid_optional_args)[!valid_optional_args], collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(time_col) ||
      length(time_col) != 1L ||
      is.na(time_col) ||
      !nzchar(time_col)) {
    stop("`time_col` must be a non-missing character scalar.", call. = FALSE)
  }

  check_window <- function(x, arg_name) {
    if (!is.numeric(x) ||
        length(x) != 2L ||
        any(is.na(x)) ||
        any(!is.finite(x)) ||
        x[[2]] <= x[[1]]) {
      stop(
        "`", arg_name, "` must be a finite numeric vector of length 2, ",
        "with the second value greater than the first.",
        call. = FALSE
      )
    }
  }

  check_window(early_window, "early_window")
  check_window(middle_window, "middle_window")
  check_window(late_window, "late_window")

  if (!is.numeric(min_valid_samples) ||
      length(min_valid_samples) != 1L ||
      is.na(min_valid_samples) ||
      !is.finite(min_valid_samples)) {
    stop("`min_valid_samples` must be a finite numeric scalar.", call. = FALSE)
  }

  auto_detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0L) {
      return(NULL)
    }

    found[[1]]
  }

  if (is.null(pupil_col)) {
    pupil_col <- auto_detect_col(
      c(
        "pupil_smoothed",
        "pupil_baseline_corrected",
        "pupil_baseline_percent_change",
        "pupil_interpolated",
        "pupil_clean",
        "pupil"
      )
    )
  }

  if (is.null(pupil_col)) {
    stop(
      "Could not automatically detect a pupil column. Please provide `pupil_col`.",
      call. = FALSE
    )
  }

  if (is.null(artifact_col)) {
    artifact_col <- auto_detect_col(
      c("pupil_artifact_flag", "pupil_flag_invalid", "artifact_flag")
    )
  }

  if (is.null(artifact_reason_col)) {
    artifact_reason_col <- auto_detect_col(
      c("pupil_artifact_reason", "pupil_flag_reason", "artifact_reason")
    )
  }

  required_cols <- unique(c(group_cols, pupil_col, time_col))

  if (!is.null(interpolated_col) && interpolated_col %in% names(data)) {
    required_cols <- unique(c(required_cols, interpolated_col))
  }

  if (!is.null(artifact_col) && artifact_col %in% names(data)) {
    required_cols <- unique(c(required_cols, artifact_col))
  }

  if (!is.null(artifact_reason_col) && artifact_reason_col %in% names(data)) {
    required_cols <- unique(c(required_cols, artifact_reason_col))
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  as_logical_flag <- function(x) {
    if (is.logical(x)) {
      return(dplyr::coalesce(x, FALSE))
    }

    if (is.numeric(x) || is.integer(x)) {
      return(!is.na(x) & x != 0)
    }

    x_chr <- tolower(trimws(as.character(x)))

    x_chr %in% c("true", "t", "1", "yes", "y")
  }

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  sd_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    stats::sd(x)
  }

  min_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    min(x)
  }

  max_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x)
  }

  auc_or_na <- function(y, time) {
    y <- as_numeric_safe(y)
    time <- as_numeric_safe(time)

    ok <- !is.na(y) & !is.na(time)

    y <- y[ok]
    time <- time[ok]

    if (length(y) < 2L || length(unique(time)) < 2L) {
      return(NA_real_)
    }

    ord <- order(time)

    y <- y[ord]
    time <- time[ord]

    sum(diff(time) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
  }

  peak_or_na <- function(y) {
    y <- as_numeric_safe(y)
    y <- y[!is.na(y)]

    if (length(y) == 0L) {
      return(NA_real_)
    }

    max(y)
  }

  peak_time_or_na <- function(y, time) {
    y <- as_numeric_safe(y)
    time <- as_numeric_safe(time)

    ok <- !is.na(y) & !is.na(time)

    y <- y[ok]
    time <- time[ok]

    if (length(y) == 0L) {
      return(NA_real_)
    }

    time[which.max(y)][[1]]
  }

  mean_in_window <- function(y, time, window) {
    y <- as_numeric_safe(y)
    time <- as_numeric_safe(time)

    ok <- !is.na(y) &
      !is.na(time) &
      time >= window[[1]] &
      time < window[[2]]

    if (!any(ok)) {
      return(NA_real_)
    }

    mean(y[ok])
  }

  count_valid_in_window <- function(y, time, window) {
    y <- as_numeric_safe(y)
    time <- as_numeric_safe(time)

    sum(
      !is.na(y) &
        !is.na(time) &
        time >= window[[1]] &
        time < window[[2]],
      na.rm = TRUE
    )
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_trial_pupil = as_numeric_safe(.data[[pupil_col]]),
      .gp3_trial_time = as_numeric_safe(.data[[time_col]])
    )

  if (!is.null(interpolated_col) && interpolated_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_trial_interpolated =
          as_logical_flag(.data[[interpolated_col]])
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_trial_interpolated = NA
      )
  }

  if (!is.null(artifact_col) && artifact_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_trial_artifact =
          as_logical_flag(.data[[artifact_col]])
      )
  } else if (!is.null(artifact_reason_col) &&
             artifact_reason_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_trial_artifact =
          !is.na(.data[[artifact_reason_col]]) &
          as.character(.data[[artifact_reason_col]]) != "valid" &
          as.character(.data[[artifact_reason_col]]) != ""
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_trial_artifact = NA
      )
  }

  out <- working |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_samples = dplyr::n(),

      n_valid_pupil = sum(
        !is.na(.data[[".gp3_trial_pupil"]]) &
          !is.na(.data[[".gp3_trial_time"]]),
        na.rm = TRUE
      ),

      n_missing_pupil = sum(
        is.na(.data[[".gp3_trial_pupil"]]) |
          is.na(.data[[".gp3_trial_time"]]),
        na.rm = TRUE
      ),

      n_interpolated_samples = if (all(is.na(.data[[".gp3_trial_interpolated"]]))) {
        NA_integer_
      } else {
        sum(.data[[".gp3_trial_interpolated"]], na.rm = TRUE)
      },

      n_artifact_samples = if (all(is.na(.data[[".gp3_trial_artifact"]]))) {
        NA_integer_
      } else {
        sum(.data[[".gp3_trial_artifact"]], na.rm = TRUE)
      },

      time_min = min_or_na(.data[[".gp3_trial_time"]]),
      time_max = max_or_na(.data[[".gp3_trial_time"]]),
      time_span_ms = .data$time_max - .data$time_min,

      mean_pupil = mean_or_na(.data[[".gp3_trial_pupil"]]),
      sd_pupil = sd_or_na(.data[[".gp3_trial_pupil"]]),
      peak_pupil = peak_or_na(.data[[".gp3_trial_pupil"]]),

      peak_time_ms = peak_time_or_na(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]]
      ),

      pupil_auc = auc_or_na(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]]
      ),

      early_mean_pupil = mean_in_window(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]],
        early_window
      ),

      middle_mean_pupil = mean_in_window(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]],
        middle_window
      ),

      late_mean_pupil = mean_in_window(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]],
        late_window
      ),

      n_valid_early = count_valid_in_window(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]],
        early_window
      ),

      n_valid_middle = count_valid_in_window(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]],
        middle_window
      ),

      n_valid_late = count_valid_in_window(
        .data[[".gp3_trial_pupil"]],
        .data[[".gp3_trial_time"]],
        late_window
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      time_to_peak_ms = .data$peak_time_ms - .data$time_min,

      valid_sample_pct = dplyr::if_else(
        .data$n_samples > 0L,
        100 * .data$n_valid_pupil / .data$n_samples,
        NA_real_
      ),

      missing_sample_pct = dplyr::if_else(
        .data$n_samples > 0L,
        100 * .data$n_missing_pupil / .data$n_samples,
        NA_real_
      ),

      interpolation_pct = dplyr::if_else(
        .data$n_samples > 0L & !is.na(.data$n_interpolated_samples),
        100 * .data$n_interpolated_samples / .data$n_samples,
        NA_real_
      ),

      artifact_pct = dplyr::if_else(
        .data$n_samples > 0L & !is.na(.data$n_artifact_samples),
        100 * .data$n_artifact_samples / .data$n_samples,
        NA_real_
      ),

      early_window_start_ms = early_window[[1]],
      early_window_end_ms = early_window[[2]],
      middle_window_start_ms = middle_window[[1]],
      middle_window_end_ms = middle_window[[2]],
      late_window_start_ms = late_window[[1]],
      late_window_end_ms = late_window[[2]],

      pupil_feature_status = dplyr::case_when(
        .data$n_valid_pupil < min_valid_samples ~ "insufficient_valid_samples",
        is.na(.data$mean_pupil) ~ "missing_pupil",
        TRUE ~ "ok"
      ),

      pupil_feature_pupil_column = pupil_col,
      pupil_feature_time_column = time_col
    )

  output_cols <- c(
    "n_samples",
    "n_valid_pupil",
    "n_missing_pupil",
    "valid_sample_pct",
    "missing_sample_pct",
    "n_interpolated_samples",
    "interpolation_pct",
    "n_artifact_samples",
    "artifact_pct",
    "time_min",
    "time_max",
    "time_span_ms",
    "mean_pupil",
    "sd_pupil",
    "peak_pupil",
    "peak_time_ms",
    "time_to_peak_ms",
    "pupil_auc",
    "early_mean_pupil",
    "middle_mean_pupil",
    "late_mean_pupil",
    "n_valid_early",
    "n_valid_middle",
    "n_valid_late",
    "early_window_start_ms",
    "early_window_end_ms",
    "middle_window_start_ms",
    "middle_window_end_ms",
    "late_window_start_ms",
    "late_window_end_ms",
    "pupil_feature_status",
    "pupil_feature_pupil_column",
    "pupil_feature_time_column"
  )

  out |>
    dplyr::select(dplyr::all_of(c(group_cols, output_cols)))
}
