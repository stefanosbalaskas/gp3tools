#' Audit Gazepoint pupil-response overlap risk
#'
#' Check whether event-related pupil-response windows may overlap.
#'
#' This function is designed as a deconvolution/readiness gate. It checks
#' whether events within the same trial are too close together and whether
#' their response windows overlap. If no usable event-time values are found,
#' the function returns a clean audit with `overlap_assessment_status =
#' "no_usable_event_times"`.
#'
#' @param data A Gazepoint sample-level data frame.
#' @param group_cols Character vector of grouping columns, usually `"subject"`.
#' @param trial_col Name of the trial identifier column.
#' @param time_col Name of the within-trial time column.
#' @param event_time_cols Character vector of event-time columns, in ms.
#' @param window_start_ms Response-window start relative to each event, in ms.
#' @param window_end_ms Response-window end relative to each event, in ms.
#' @param min_event_gap_ms Minimum acceptable event-to-event gap in ms.
#' @param exclude_col Optional logical exclusion column.
#' @param include_excluded Logical. If `FALSE`, rows marked by `exclude_col`
#'   are removed before the audit when that column exists.
#'
#' @return A named list containing `events`, `event_gaps`, `by_trial`, and
#'   `summary` tibbles.
#'
#' @export
#' @importFrom rlang .data
audit_gazepoint_pupil_overlap_risk <- function(
    data,
    group_cols = "subject",
    trial_col = "trial_global",
    time_col = "time",
    event_time_cols = c(
      "stimulus_onset_time",
      "target_onset_time",
      "response_time"
    ),
    window_start_ms = 0,
    window_end_ms = 2000,
    min_event_gap_ms = 1000,
    exclude_col = "excluded_trial",
    include_excluded = FALSE
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
    trial_col = trial_col,
    time_col = time_col
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

  if (!is.character(event_time_cols) ||
      length(event_time_cols) == 0L ||
      any(is.na(event_time_cols)) ||
      any(!nzchar(event_time_cols)) ||
      anyDuplicated(event_time_cols)) {
    stop(
      "`event_time_cols` must be a non-empty character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.null(exclude_col) &&
      (
        !is.character(exclude_col) ||
        length(exclude_col) != 1L ||
        is.na(exclude_col) ||
        !nzchar(exclude_col)
      )) {
    stop(
      "`exclude_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  numeric_args <- c(
    window_start_ms = window_start_ms,
    window_end_ms = window_end_ms,
    min_event_gap_ms = min_event_gap_ms
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
      "Window/gap arguments must be finite numeric scalars: ",
      paste(names(numeric_args)[!valid_numeric_arg], collapse = ", "),
      call. = FALSE
    )
  }

  if (window_end_ms <= window_start_ms) {
    stop(
      "`window_end_ms` must be greater than `window_start_ms`.",
      call. = FALSE
    )
  }

  required_cols <- unique(c(
    group_cols,
    trial_col,
    time_col,
    event_time_cols
  ))

  if (!is.null(exclude_col) && exclude_col %in% names(data)) {
    required_cols <- unique(c(required_cols, exclude_col))
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

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  empty_key_tibble <- function(reference_data, cols) {
    out <- lapply(cols, function(col) reference_data[[col]][0])
    names(out) <- cols
    tibble::as_tibble(out)
  }

  working <- tibble::as_tibble(data)

  if (!is.null(exclude_col) &&
      exclude_col %in% names(working) &&
      !include_excluded) {
    working <- working |>
      dplyr::filter(!as_logical_flag(.data[[exclude_col]]))
  }

  key_cols <- unique(c(group_cols, trial_col))

  by_trial_base <- working |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      trial_time_min = min_or_na(.data[[time_col]]),
      trial_time_mean = mean_or_na(.data[[time_col]]),
      trial_time_max = max_or_na(.data[[time_col]]),
      trial_time_range_ms = .data$trial_time_max - .data$trial_time_min,
      .groups = "drop"
    )

  event_list <- lapply(
    event_time_cols,
    function(event_col) {
      event_data <- tibble::as_tibble(
        working[, key_cols, drop = FALSE]
      ) |>
        dplyr::mutate(
          event_name = event_col,
          event_time_ms = as_numeric_safe(working[[event_col]])
        ) |>
        dplyr::filter(!is.na(.data$event_time_ms)) |>
        dplyr::distinct()

      event_data
    }
  )

  events <- dplyr::bind_rows(event_list)

  if (nrow(events) > 0L) {
    events <- events |>
      dplyr::mutate(
        response_window_start_ms = .data$event_time_ms + window_start_ms,
        response_window_end_ms = .data$event_time_ms + window_end_ms,
        response_window_duration_ms =
          .data$response_window_end_ms - .data$response_window_start_ms
      ) |>
      dplyr::arrange(
        dplyr::across(dplyr::all_of(key_cols)),
        .data$event_time_ms,
        .data$event_name
      )
  } else {
    events <- empty_key_tibble(working, key_cols) |>
      dplyr::mutate(
        event_name = character(),
        event_time_ms = numeric(),
        response_window_start_ms = numeric(),
        response_window_end_ms = numeric(),
        response_window_duration_ms = numeric()
      )
  }

  if (nrow(events) > 0L) {
    event_gaps <- events |>
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::mutate(
        previous_event_name = dplyr::lag(.data$event_name),
        previous_event_time_ms = dplyr::lag(.data$event_time_ms),
        previous_response_window_end_ms =
          dplyr::lag(.data$response_window_end_ms),

        event_gap_ms =
          .data$event_time_ms - .data$previous_event_time_ms,

        short_event_gap =
          !is.na(.data$event_gap_ms) &
          .data$event_gap_ms < min_event_gap_ms,

        response_window_overlap =
          !is.na(.data$previous_response_window_end_ms) &
          .data$response_window_start_ms <
          .data$previous_response_window_end_ms,

        overlap_amount_ms = dplyr::if_else(
          .data$response_window_overlap,
          .data$previous_response_window_end_ms -
            .data$response_window_start_ms,
          0
        ),

        event_gap_status = dplyr::case_when(
          is.na(.data$previous_event_time_ms) ~ "first_event",
          .data$response_window_overlap & .data$short_event_gap ~
            "overlap_and_short_gap",
          .data$response_window_overlap ~ "overlapping_response_window",
          .data$short_event_gap ~ "short_event_gap",
          TRUE ~ "ok"
        )
      ) |>
      dplyr::ungroup()
  } else {
    event_gaps <- empty_key_tibble(working, key_cols) |>
      dplyr::mutate(
        event_name = character(),
        event_time_ms = numeric(),
        response_window_start_ms = numeric(),
        response_window_end_ms = numeric(),
        response_window_duration_ms = numeric(),
        previous_event_name = character(),
        previous_event_time_ms = numeric(),
        previous_response_window_end_ms = numeric(),
        event_gap_ms = numeric(),
        short_event_gap = logical(),
        response_window_overlap = logical(),
        overlap_amount_ms = numeric(),
        event_gap_status = character()
      )
  }

  if (nrow(event_gaps) > 0L) {
    event_summary <- event_gaps |>
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
      dplyr::summarise(
        n_events = dplyr::n(),

        n_event_gaps = sum(
          !is.na(.data$event_gap_ms),
          na.rm = TRUE
        ),

        min_event_gap_observed_ms = min_or_na(.data$event_gap_ms),
        mean_event_gap_observed_ms = mean_or_na(.data$event_gap_ms),
        max_event_gap_observed_ms = max_or_na(.data$event_gap_ms),

        n_short_event_gaps = sum(
          .data$short_event_gap,
          na.rm = TRUE
        ),

        n_overlapping_response_windows = sum(
          .data$response_window_overlap,
          na.rm = TRUE
        ),

        max_overlap_amount_ms = max_or_na(.data$overlap_amount_ms),

        .groups = "drop"
      ) |>
      dplyr::mutate(
        overlap_risk_warning =
          .data$n_short_event_gaps > 0L |
          .data$n_overlapping_response_windows > 0L,

        overlap_risk_reason = dplyr::case_when(
          .data$n_short_event_gaps > 0L &
            .data$n_overlapping_response_windows > 0L ~
            "short_event_gap;overlapping_response_window",
          .data$n_short_event_gaps > 0L ~ "short_event_gap",
          .data$n_overlapping_response_windows > 0L ~
            "overlapping_response_window",
          TRUE ~ "ok"
        )
      )
  } else {
    event_summary <- empty_key_tibble(working, key_cols) |>
      dplyr::mutate(
        n_events = integer(),
        n_event_gaps = integer(),
        min_event_gap_observed_ms = numeric(),
        mean_event_gap_observed_ms = numeric(),
        max_event_gap_observed_ms = numeric(),
        n_short_event_gaps = integer(),
        n_overlapping_response_windows = integer(),
        max_overlap_amount_ms = numeric(),
        overlap_risk_warning = logical(),
        overlap_risk_reason = character()
      )
  }

  by_trial <- by_trial_base |>
    dplyr::left_join(event_summary, by = key_cols) |>
    dplyr::mutate(
      n_events = dplyr::coalesce(.data$n_events, 0L),
      n_event_gaps = dplyr::coalesce(.data$n_event_gaps, 0L),
      n_short_event_gaps = dplyr::coalesce(.data$n_short_event_gaps, 0L),
      n_overlapping_response_windows =
        dplyr::coalesce(.data$n_overlapping_response_windows, 0L),
      overlap_risk_warning =
        dplyr::coalesce(.data$overlap_risk_warning, FALSE),
      overlap_risk_reason =
        dplyr::coalesce(
          .data$overlap_risk_reason,
          "no_usable_event_times"
        )
    )

  n_trials <- nrow(by_trial)
  n_events <- nrow(events)
  n_trials_with_events <- sum(by_trial$n_events > 0L, na.rm = TRUE)
  n_trials_with_short_event_gaps <- sum(
    by_trial$n_short_event_gaps > 0L,
    na.rm = TRUE
  )
  n_trials_with_overlapping_windows <- sum(
    by_trial$n_overlapping_response_windows > 0L,
    na.rm = TRUE
  )
  n_overlap_risk_trials <- sum(
    by_trial$overlap_risk_warning,
    na.rm = TRUE
  )

  overlap_assessment_status <- dplyr::case_when(
    n_events == 0L ~ "no_usable_event_times",
    n_overlap_risk_trials > 0L ~ "possible_overlap_risk",
    TRUE ~ "ok"
  )

  summary <- tibble::tibble(
    n_trials = n_trials,
    n_events = n_events,
    n_trials_with_events = n_trials_with_events,
    n_trials_without_events = n_trials - n_trials_with_events,
    n_trials_with_short_event_gaps = n_trials_with_short_event_gaps,
    n_trials_with_overlapping_windows = n_trials_with_overlapping_windows,
    n_overlap_risk_trials = n_overlap_risk_trials,
    pct_overlap_risk_trials = dplyr::if_else(
      n_trials > 0L,
      100 * n_overlap_risk_trials / n_trials,
      NA_real_
    ),
    window_start_ms = window_start_ms,
    window_end_ms = window_end_ms,
    response_window_duration_ms = window_end_ms - window_start_ms,
    min_event_gap_ms = min_event_gap_ms,
    overlap_assessment_status = overlap_assessment_status
  )

  list(
    events = events,
    event_gaps = event_gaps,
    by_trial = by_trial,
    summary = summary
  )
}
