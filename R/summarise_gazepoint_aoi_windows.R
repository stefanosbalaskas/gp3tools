#' Summarise Gazepoint AOI samples within predefined time windows
#'
#' Summarise sample-level AOI states into predefined analysis windows. This is
#' intended for confirmatory AOI window modelling, especially binomial
#' target-looking models where target samples are modelled relative to a
#' denominator such as all valid window samples.
#'
#' @param data A Gazepoint master table or sample-level gaze data.
#' @param windows Numeric breakpoints, for example `c(0, 500, 1000, 2000)`,
#'   or a data frame with window labels and start/end columns.
#' @param time_col Name of the time column.
#' @param aoi_col Optional AOI-state column. If `NULL`, the function attempts to
#'   detect `aoi_current`, `AOI`, or `aoi_state`.
#' @param subject_col Name of the subject column.
#' @param condition_col Optional condition column.
#' @param group_cols Additional grouping columns. Defaults to subject, condition,
#'   media, trial-global, and trial columns when available.
#' @param target_aoi_values Character vector identifying target AOIs.
#' @param distractor_aoi_values Character vector identifying distractor AOIs.
#' @param non_aoi_values Character vector identifying background/non-AOI states.
#' @param window_label_col Window label column when `windows` is a data frame.
#' @param window_start_col Window start column when `windows` is a data frame.
#' @param window_end_col Window end column when `windows` is a data frame.
#' @param include_right_endpoint Logical. If `TRUE`, include the right endpoint
#'   of each window.
#' @param missing_condition_label Label used when condition is missing.
#' @param missing_aoi_label Label used when AOI state is missing.
#'
#' @return A tibble with one row per group and AOI window.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_aoi_windows <- function(
    data,
    windows,
    time_col = "time",
    aoi_col = NULL,
    subject_col = "subject",
    condition_col = "condition",
    group_cols = NULL,
    target_aoi_values = NULL,
    distractor_aoi_values = NULL,
    non_aoi_values = c(
      "non_aoi",
      "none",
      "background",
      "outside",
      "outside_aoi",
      "missing",
      "missing_aoi"
    ),
    window_label_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    include_right_endpoint = FALSE,
    missing_condition_label = "all_data",
    missing_aoi_label = "missing_aoi"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_column <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_optional_column <- function(x, arg) {
    if (!is.null(x) &&
        (!is.character(x) ||
         length(x) != 1L ||
         is.na(x) ||
         !nzchar(x))) {
      stop(
        "`", arg, "` must be NULL or a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_column(time_col, "time_col")
  valid_optional_column(aoi_col, "aoi_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_column(window_label_col, "window_label_col")
  valid_column(window_start_col, "window_start_col")
  valid_column(window_end_col, "window_end_col")

  if (!is.null(group_cols) &&
      (!is.character(group_cols) ||
       any(is.na(group_cols)) ||
       any(!nzchar(group_cols)))) {
    stop(
      "`group_cols` must be NULL or a character vector of column names.",
      call. = FALSE
    )
  }

  if (!is.null(target_aoi_values) &&
      (!is.character(target_aoi_values) ||
       any(is.na(target_aoi_values)))) {
    stop(
      "`target_aoi_values` must be NULL or a character vector.",
      call. = FALSE
    )
  }

  if (!is.null(distractor_aoi_values) &&
      (!is.character(distractor_aoi_values) ||
       any(is.na(distractor_aoi_values)))) {
    stop(
      "`distractor_aoi_values` must be NULL or a character vector.",
      call. = FALSE
    )
  }

  if (!is.character(non_aoi_values) || any(is.na(non_aoi_values))) {
    stop(
      "`non_aoi_values` must be a character vector.",
      call. = FALSE
    )
  }

  if (!is.logical(include_right_endpoint) ||
      length(include_right_endpoint) != 1L ||
      is.na(include_right_endpoint)) {
    stop(
      "`include_right_endpoint` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.character(missing_condition_label) ||
      length(missing_condition_label) != 1L ||
      is.na(missing_condition_label) ||
      !nzchar(missing_condition_label)) {
    stop(
      "`missing_condition_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.character(missing_aoi_label) ||
      length(missing_aoi_label) != 1L ||
      is.na(missing_aoi_label) ||
      !nzchar(missing_aoi_label)) {
    stop(
      "`missing_aoi_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  detect_first_existing <- function(candidates, names_x) {
    candidates[candidates %in% names_x][1]
  }

  if (is.null(aoi_col)) {
    aoi_col <- detect_first_existing(
      c("aoi_current", "AOI", "aoi_state"),
      names(dat)
    )

    if (is.na(aoi_col) || length(aoi_col) == 0L) {
      stop(
        "Could not detect an AOI column. Please provide `aoi_col`.",
        call. = FALSE
      )
    }
  }

  required_cols <- c(time_col, aoi_col, subject_col)

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (!is.null(group_cols)) {
    required_cols <- c(required_cols, group_cols)
  }

  missing_cols <- setdiff(unique(required_cols), names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(group_cols)) {
    default_group_cols <- c(
      subject_col,
      condition_col,
      "MEDIA_ID",
      "trial_global",
      "trial"
    )

    group_cols <- unique(default_group_cols[default_group_cols %in% names(dat)])

    if (!subject_col %in% group_cols) {
      group_cols <- c(subject_col, group_cols)
    }
  }

  if (!is.null(condition_col)) {
    if (!condition_col %in% names(dat)) {
      dat[[condition_col]] <- missing_condition_label
    }

    if (!condition_col %in% group_cols) {
      group_cols <- c(group_cols, condition_col)
    }
  }

  if (length(group_cols) == 0L) {
    stop(
      "No grouping columns could be detected. Please provide `group_cols`.",
      call. = FALSE
    )
  }

  if (length(group_cols) == 0L) {
    stop(
      "No grouping columns could be detected. Please provide `group_cols`.",
      call. = FALSE
    )
  }

  make_window_table <- function(windows) {
    if (is.numeric(windows)) {
      if (length(windows) < 2L ||
          any(is.na(windows)) ||
          any(!is.finite(windows))) {
        stop(
          "`windows` must contain at least two finite numeric breakpoints.",
          call. = FALSE
        )
      }

      windows <- sort(unique(windows))

      if (length(windows) < 2L) {
        stop(
          "`windows` must contain at least two distinct breakpoints.",
          call. = FALSE
        )
      }

      starts <- utils::head(windows, -1L)
      ends <- utils::tail(windows, -1L)

      tibble::tibble(
        window_label = paste0(starts, "_", ends, "ms"),
        window_start_ms = starts,
        window_end_ms = ends
      )
    } else if (is.data.frame(windows)) {
      win <- tibble::as_tibble(windows)

      needed <- c(window_label_col, window_start_col, window_end_col)
      missing_win_cols <- setdiff(needed, names(win))

      if (length(missing_win_cols) > 0L) {
        stop(
          "Missing required window columns: ",
          paste(missing_win_cols, collapse = ", "),
          call. = FALSE
        )
      }

      tibble::tibble(
        window_label = as.character(win[[window_label_col]]),
        window_start_ms = suppressWarnings(as.numeric(win[[window_start_col]])),
        window_end_ms = suppressWarnings(as.numeric(win[[window_end_col]]))
      )
    } else {
      stop(
        "`windows` must be a numeric vector or a data frame.",
        call. = FALSE
      )
    }
  }

  window_table <- make_window_table(windows)

  if (any(is.na(window_table$window_label)) ||
      any(!nzchar(window_table$window_label))) {
    stop(
      "Window labels must be non-missing and non-empty.",
      call. = FALSE
    )
  }

  if (any(!is.finite(window_table$window_start_ms)) ||
      any(!is.finite(window_table$window_end_ms))) {
    stop(
      "Window start and end values must be finite.",
      call. = FALSE
    )
  }

  if (any(window_table$window_end_ms <= window_table$window_start_ms)) {
    stop(
      "Each AOI window must have `window_end_ms` greater than `window_start_ms`.",
      call. = FALSE
    )
  }

  dat$.gp3_time <- suppressWarnings(as.numeric(dat[[time_col]]))
  dat$.gp3_aoi <- as.character(dat[[aoi_col]])

  dat$.gp3_aoi[
    is.na(dat$.gp3_aoi) |
      !nzchar(trimws(dat$.gp3_aoi))
  ] <- missing_aoi_label

  dat$.gp3_aoi <- trimws(dat$.gp3_aoi)

  dat$.gp3_subject <- as.character(dat[[subject_col]])
  dat$.gp3_subject[
    is.na(dat$.gp3_subject) |
      !nzchar(trimws(dat$.gp3_subject))
  ] <- "unknown_subject"

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    dat$.gp3_condition <- as.character(dat[[condition_col]])
    dat$.gp3_condition <- trimws(dat$.gp3_condition)
    dat$.gp3_condition[
      is.na(dat$.gp3_condition) |
        !nzchar(dat$.gp3_condition)
    ] <- missing_condition_label
  } else {
    dat$.gp3_condition <- missing_condition_label
  }

  dat[[subject_col]] <- dat$.gp3_subject

  if (!is.null(condition_col)) {
    dat[[condition_col]] <- dat$.gp3_condition
  }

  dat <- dat |>
    dplyr::filter(is.finite(.data[[".gp3_time"]]))

  if (nrow(dat) == 0L) {
    stop(
      "No rows contain finite time values.",
      call. = FALSE
    )
  }

  assign_windows <- function(x_time, win) {
    window_index <- rep(NA_integer_, length(x_time))

    for (i in seq_len(nrow(win))) {
      if (include_right_endpoint) {
        in_window <- x_time >= win$window_start_ms[[i]] &
          x_time <= win$window_end_ms[[i]]
      } else {
        in_window <- x_time >= win$window_start_ms[[i]] &
          x_time < win$window_end_ms[[i]]
      }

      window_index[is.na(window_index) & in_window] <- i
    }

    window_index
  }

  dat$.gp3_window_index <- assign_windows(dat$.gp3_time, window_table)

  dat <- dat |>
    dplyr::filter(!is.na(.data[[".gp3_window_index"]]))

  if (nrow(dat) == 0L) {
    stop(
      "No rows fall inside the supplied AOI windows.",
      call. = FALSE
    )
  }

  dat$window_label <- window_table$window_label[dat$.gp3_window_index]
  dat$window_start_ms <- window_table$window_start_ms[dat$.gp3_window_index]
  dat$window_end_ms <- window_table$window_end_ms[dat$.gp3_window_index]

  target_values <- if (is.null(target_aoi_values)) character(0) else target_aoi_values
  distractor_values <- if (is.null(distractor_aoi_values)) character(0) else distractor_aoi_values

  dat$.gp3_is_target <- dat$.gp3_aoi %in% target_values
  dat$.gp3_is_distractor <- dat$.gp3_aoi %in% distractor_values
  dat$.gp3_is_non_aoi <- dat$.gp3_aoi %in% non_aoi_values
  dat$.gp3_is_missing_aoi <- dat$.gp3_aoi == missing_aoi_label |
    dat$.gp3_aoi %in% c("missing", "missing_aoi")

  dat$.gp3_is_other_aoi <- !dat$.gp3_is_target &
    !dat$.gp3_is_distractor &
    !dat$.gp3_is_non_aoi &
    !dat$.gp3_is_missing_aoi

  summary <- dat |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[["window_label"]],
      .data[["window_start_ms"]],
      .data[["window_end_ms"]]
    ) |>
    dplyr::summarise(
      n_window_samples = dplyr::n(),
      n_target_samples = sum(.data[[".gp3_is_target"]], na.rm = TRUE),
      n_distractor_samples = sum(.data[[".gp3_is_distractor"]], na.rm = TRUE),
      n_non_aoi_samples = sum(.data[[".gp3_is_non_aoi"]], na.rm = TRUE),
      n_missing_aoi_samples = sum(.data[[".gp3_is_missing_aoi"]], na.rm = TRUE),
      n_other_aoi_samples = sum(.data[[".gp3_is_other_aoi"]], na.rm = TRUE),
      n_unique_aoi_states = dplyr::n_distinct(.data[[".gp3_aoi"]]),
      first_aoi_state = dplyr::first(.data[[".gp3_aoi"]]),
      last_aoi_state = dplyr::last(.data[[".gp3_aoi"]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_aoi_samples = .data[["n_target_samples"]] +
        .data[["n_distractor_samples"]] +
        .data[["n_other_aoi_samples"]],
      n_valid_denominator_samples = .data[["n_window_samples"]] -
        .data[["n_missing_aoi_samples"]],
      target_sample_prop_all = dplyr::if_else(
        .data[["n_window_samples"]] > 0,
        .data[["n_target_samples"]] / .data[["n_window_samples"]],
        NA_real_
      ),
      target_sample_prop_valid = dplyr::if_else(
        .data[["n_valid_denominator_samples"]] > 0,
        .data[["n_target_samples"]] / .data[["n_valid_denominator_samples"]],
        NA_real_
      ),
      target_sample_prop_aoi = dplyr::if_else(
        .data[["n_aoi_samples"]] > 0,
        .data[["n_target_samples"]] / .data[["n_aoi_samples"]],
        NA_real_
      ),
      distractor_sample_prop_all = dplyr::if_else(
        .data[["n_window_samples"]] > 0,
        .data[["n_distractor_samples"]] / .data[["n_window_samples"]],
        NA_real_
      ),
      valid_denominator_prop = dplyr::if_else(
        .data[["n_window_samples"]] > 0,
        .data[["n_valid_denominator_samples"]] / .data[["n_window_samples"]],
        NA_real_
      ),
      target_aoi_defined = length(target_values) > 0L,
      distractor_aoi_defined = length(distractor_values) > 0L,
      aoi_window_status = dplyr::case_when(
        !.data[["target_aoi_defined"]] ~ "no_target_aoi_defined",
        .data[["n_valid_denominator_samples"]] == 0 ~ "zero_valid_denominator",
        .data[["n_target_samples"]] == 0 ~ "target_not_observed",
        .data[["n_target_samples"]] == .data[["n_valid_denominator_samples"]] ~ "target_only",
        TRUE ~ "ok"
      )
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[["window_start_ms"]]
    )

  class(summary) <- c("gp3_aoi_window_summary", class(summary))

  attr(summary, "settings") <- list(
    time_col = time_col,
    aoi_col = aoi_col,
    subject_col = subject_col,
    condition_col = condition_col,
    group_cols = group_cols,
    target_aoi_values = target_aoi_values,
    distractor_aoi_values = distractor_aoi_values,
    non_aoi_values = non_aoi_values,
    include_right_endpoint = include_right_endpoint,
    missing_condition_label = missing_condition_label,
    missing_aoi_label = missing_aoi_label
  )

  summary
}
