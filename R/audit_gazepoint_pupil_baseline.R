#' Audit Gazepoint pupil baseline quality
#'
#' Summarise baseline quality after [baseline_correct_gazepoint_pupil()].
#'
#' The function reports baseline-row counts, valid/missing baseline samples,
#' interpolated baseline samples, artifact-flagged baseline samples,
#' no-baseline cases, and low-quality baseline flags by subject, media, trial,
#' condition, or any selected grouping variables.
#'
#' @param data A data frame returned by `baseline_correct_gazepoint_pupil()`
#'   or a later pupil-preprocessing step.
#' @param group_cols Character vector of grouping columns. Use `character(0)`
#'   for an overall audit.
#' @param time_col Name of the time column.
#' @param pupil_col Name of the pupil column used to evaluate missingness.
#' @param baseline_n_col Name of the baseline valid-sample count column.
#' @param baseline_status_col Name of the baseline-status column.
#' @param baseline_available_col Name of the baseline-availability column.
#' @param baseline_used_col Name of the logical column indicating whether a
#'   row used a baseline value.
#' @param baseline_window_start_col Name of the baseline-window start column.
#' @param baseline_window_end_col Name of the baseline-window end column.
#' @param baseline_flag_col Optional logical column identifying baseline rows.
#'   If `NULL`, baseline rows are detected from the time column and baseline
#'   window start/end columns.
#' @param interpolated_col Name of the logical interpolation flag column.
#' @param artifact_col Optional artifact flag column. If `NULL`, the function
#'   tries to detect `pupil_artifact_flag`, `pupil_flag_invalid`, or
#'   `artifact_flag`.
#' @param artifact_reason_col Optional artifact-reason column. If `NULL`, the
#'   function tries to detect `pupil_artifact_reason`, `pupil_flag_reason`, or
#'   `artifact_reason`.
#' @param min_baseline_samples Minimum acceptable number of valid baseline
#'   samples before a group is flagged as low quality.
#' @param max_missing_pct Maximum acceptable percentage of missing baseline
#'   samples.
#' @param max_interpolated_pct Maximum acceptable percentage of interpolated
#'   baseline samples.
#' @param max_artifact_pct Maximum acceptable percentage of artifact-flagged
#'   baseline samples.
#'
#' @return A tibble with one row per group.
#'
#' @export
#' @importFrom rlang .data
audit_gazepoint_pupil_baseline <- function(
    data,
    group_cols = c("subject", "media_id"),
    time_col = "time",
    pupil_col = "pupil_interpolated",
    baseline_n_col = "pupil_baseline_n",
    baseline_status_col = "pupil_baseline_status",
    baseline_available_col = "pupil_baseline_available",
    baseline_used_col = "pupil_baseline_used",
    baseline_window_start_col = "pupil_baseline_window_start",
    baseline_window_end_col = "pupil_baseline_window_end",
    baseline_flag_col = NULL,
    interpolated_col = "pupil_was_interpolated",
    artifact_col = NULL,
    artifact_reason_col = NULL,
    min_baseline_samples = 1,
    max_missing_pct = 50,
    max_interpolated_pct = 50,
    max_artifact_pct = 50
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

  scalar_column_args <- c(
    time_col = time_col,
    pupil_col = pupil_col,
    baseline_n_col = baseline_n_col,
    baseline_status_col = baseline_status_col,
    baseline_available_col = baseline_available_col,
    baseline_used_col = baseline_used_col,
    baseline_window_start_col = baseline_window_start_col,
    baseline_window_end_col = baseline_window_end_col,
    interpolated_col = interpolated_col
  )

  valid_scalar_column_arg <- vapply(
    scalar_column_args,
    function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(x)
    },
    logical(1)
  )

  if (any(!valid_scalar_column_arg)) {
    stop(
      "Column-name arguments must be non-missing character scalars: ",
      paste(names(scalar_column_args)[!valid_scalar_column_arg], collapse = ", "),
      call. = FALSE
    )
  }

  optional_column_args <- c(
    baseline_flag_col = baseline_flag_col,
    artifact_col = artifact_col,
    artifact_reason_col = artifact_reason_col
  )

  supplied_optional_args <- !vapply(
    optional_column_args,
    is.null,
    logical(1)
  )

  valid_optional_arg <- vapply(
    optional_column_args[supplied_optional_args],
    function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(x)
    },
    logical(1)
  )

  if (length(valid_optional_arg) > 0L && any(!valid_optional_arg)) {
    stop(
      "Optional column-name arguments must be NULL or non-missing character scalars: ",
      paste(names(valid_optional_arg)[!valid_optional_arg], collapse = ", "),
      call. = FALSE
    )
  }

  numeric_args <- c(
    min_baseline_samples = min_baseline_samples,
    max_missing_pct = max_missing_pct,
    max_interpolated_pct = max_interpolated_pct,
    max_artifact_pct = max_artifact_pct
  )

  valid_numeric_arg <- vapply(
    numeric_args,
    function(x) {
      is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        is.finite(x)
    },
    logical(1)
  )

  if (any(!valid_numeric_arg)) {
    stop(
      "Quality-threshold arguments must be finite numeric scalars: ",
      paste(names(numeric_args)[!valid_numeric_arg], collapse = ", "),
      call. = FALSE
    )
  }

  auto_detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0L) {
      return(NULL)
    }

    found[[1]]
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

  required_cols <- unique(c(
    group_cols,
    unname(scalar_column_args)
  ))

  if (!is.null(baseline_flag_col)) {
    required_cols <- unique(c(required_cols, baseline_flag_col))
  }

  if (!is.null(artifact_col)) {
    required_cols <- unique(c(required_cols, artifact_col))
  }

  if (!is.null(artifact_reason_col)) {
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

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  first_or_na <- function(x) {
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA)
    }

    x[[1]]
  }

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
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

  group_data <- function(x) {
    if (length(group_cols) > 0L) {
      dplyr::group_by(x, dplyr::across(dplyr::all_of(group_cols)))
    } else {
      x
    }
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_baseline_time = as_numeric_safe(.data[[time_col]]),
      .gp3_baseline_pupil = .data[[pupil_col]],
      .gp3_baseline_n = as_numeric_safe(.data[[baseline_n_col]]),
      .gp3_baseline_status = as.character(.data[[baseline_status_col]]),
      .gp3_baseline_available = as_logical_flag(.data[[baseline_available_col]]),
      .gp3_baseline_used = as_logical_flag(.data[[baseline_used_col]]),
      .gp3_baseline_window_start =
        as_numeric_safe(.data[[baseline_window_start_col]]),
      .gp3_baseline_window_end =
        as_numeric_safe(.data[[baseline_window_end_col]]),
      .gp3_baseline_interpolated =
        as_logical_flag(.data[[interpolated_col]])
    )

  if (!is.null(baseline_flag_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_is_baseline_row = as_logical_flag(.data[[baseline_flag_col]])
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_is_baseline_row =
          !is.na(.data[[".gp3_baseline_time"]]) &
          !is.na(.data[[".gp3_baseline_window_start"]]) &
          !is.na(.data[[".gp3_baseline_window_end"]]) &
          .data[[".gp3_baseline_time"]] >= .data[[".gp3_baseline_window_start"]] &
          .data[[".gp3_baseline_time"]] <= .data[[".gp3_baseline_window_end"]]
      )
  }

  if (!is.null(artifact_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_baseline_artifact =
          as_logical_flag(.data[[artifact_col]])
      )
  } else if (!is.null(artifact_reason_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_baseline_artifact =
          !is.na(.data[[artifact_reason_col]]) &
          as.character(.data[[artifact_reason_col]]) != "valid" &
          as.character(.data[[artifact_reason_col]]) != ""
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_baseline_artifact = NA
      )
  }

  out <- working |>
    group_data() |>
    dplyr::summarise(
      n_rows = dplyr::n(),

      n_baseline_rows = sum(
        .data[[".gp3_is_baseline_row"]],
        na.rm = TRUE
      ),

      n_baseline_valid_samples = sum(
        .data[[".gp3_is_baseline_row"]] &
          !is.na(.data[[".gp3_baseline_pupil"]]),
        na.rm = TRUE
      ),

      n_baseline_missing_samples = sum(
        .data[[".gp3_is_baseline_row"]] &
          is.na(.data[[".gp3_baseline_pupil"]]),
        na.rm = TRUE
      ),

      n_baseline_interpolated_samples = sum(
        .data[[".gp3_is_baseline_row"]] &
          .data[[".gp3_baseline_interpolated"]],
        na.rm = TRUE
      ),

      n_baseline_artifact_samples = sum(
        .data[[".gp3_is_baseline_row"]] &
          .data[[".gp3_baseline_artifact"]],
        na.rm = TRUE
      ),

      baseline_n_min = min_or_na(.data[[".gp3_baseline_n"]]),
      baseline_n_mean = mean_or_na(.data[[".gp3_baseline_n"]]),
      baseline_n_max = max_or_na(.data[[".gp3_baseline_n"]]),

      baseline_status = as.character(first_or_na(.data[[".gp3_baseline_status"]])),
      baseline_available = first_or_na(.data[[".gp3_baseline_available"]]),
      baseline_used = first_or_na(.data[[".gp3_baseline_used"]]),

      n_no_baseline_rows = sum(
        .data[[".gp3_baseline_status"]] == "no_baseline",
        na.rm = TRUE
      ),

      n_missing_pupil_baseline_rows = sum(
        .data[[".gp3_baseline_status"]] == "missing_pupil",
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      baseline_missing_pct = dplyr::if_else(
        .data$n_baseline_rows > 0L,
        100 * .data$n_baseline_missing_samples / .data$n_baseline_rows,
        NA_real_
      ),

      baseline_interpolated_pct = dplyr::if_else(
        .data$n_baseline_rows > 0L,
        100 * .data$n_baseline_interpolated_samples / .data$n_baseline_rows,
        NA_real_
      ),

      baseline_artifact_pct = dplyr::if_else(
        .data$n_baseline_rows > 0L,
        100 * .data$n_baseline_artifact_samples / .data$n_baseline_rows,
        NA_real_
      ),

      no_baseline_case =
        .data$baseline_status == "no_baseline" |
        !.data$baseline_available |
        .data$baseline_n_max < min_baseline_samples,

      low_quality_baseline_flag =
        .data$no_baseline_case |
        is.na(.data$baseline_n_max) |
        .data$baseline_n_max < min_baseline_samples |
        dplyr::coalesce(.data$baseline_missing_pct > max_missing_pct, FALSE) |
        dplyr::coalesce(
          .data$baseline_interpolated_pct > max_interpolated_pct,
          FALSE
        ) |
        dplyr::coalesce(.data$baseline_artifact_pct > max_artifact_pct, FALSE),

      baseline_quality_reason = dplyr::case_when(
        .data$no_baseline_case ~ "no_baseline",
        is.na(.data$baseline_n_max) ~ "missing_baseline_n",
        .data$baseline_n_max < min_baseline_samples ~ "too_few_baseline_samples",
        dplyr::coalesce(.data$baseline_missing_pct > max_missing_pct, FALSE) ~
          "high_baseline_missing_pct",
        dplyr::coalesce(
          .data$baseline_interpolated_pct > max_interpolated_pct,
          FALSE
        ) ~ "high_baseline_interpolated_pct",
        dplyr::coalesce(.data$baseline_artifact_pct > max_artifact_pct, FALSE) ~
          "high_baseline_artifact_pct",
        TRUE ~ "ok"
      )
    )

  output_cols <- c(
    "n_rows",
    "n_baseline_rows",
    "n_baseline_valid_samples",
    "n_baseline_missing_samples",
    "n_baseline_interpolated_samples",
    "n_baseline_artifact_samples",
    "baseline_missing_pct",
    "baseline_interpolated_pct",
    "baseline_artifact_pct",
    "baseline_n_min",
    "baseline_n_mean",
    "baseline_n_max",
    "baseline_status",
    "baseline_available",
    "baseline_used",
    "n_no_baseline_rows",
    "n_missing_pupil_baseline_rows",
    "no_baseline_case",
    "low_quality_baseline_flag",
    "baseline_quality_reason"
  )

  out |>
    dplyr::select(dplyr::all_of(c(group_cols, output_cols)))
}
