#' Summarise Gazepoint pupil responses within time windows
#'
#' Aggregates processed Gazepoint pupil data into user-defined analysis windows,
#' typically after [flag_gazepoint_pupil()], [interpolate_gazepoint_pupil()],
#' [baseline_correct_gazepoint_pupil()], and [smooth_gazepoint_pupil()].
#' The function can summarise raw, interpolated, baseline-corrected,
#' percent-change, or smoothed pupil columns.
#'
#' @param data A Gazepoint master table or processed pupil table.
#' @param pupil_col Optional name of the pupil column to summarise. If `NULL`,
#'   the function detects one of `pupil_smoothed`,
#'   `pupil_baseline_corrected`, `pupil_baseline_percent_change`,
#'   `pupil_interpolated`, `pupil_for_preprocessing`, `mean_pupil`,
#'   `pupil`, `pupil_raw`, `left_pupil`, or `right_pupil`.
#' @param time_col Optional name of the time column used for assigning samples
#'   to windows. If `NULL`, the function detects one of
#'   `time_relative_ms`, `relative_time_ms`, `event_time_ms`, `time_ms`,
#'   `time`, `time_orig`, or `time_orig_ms`.
#' @param windows Window definitions. Either a numeric vector of breakpoints,
#'   such as `c(0, 500, 1000, 2000)`, or a data frame with window start and end
#'   columns. Supported names include `window_start_ms`, `window_start`,
#'   `start_ms`, or `start`, and `window_end_ms`, `window_end`, `end_ms`, or
#'   `end`. A `window_label` or `label` column is optional.
#' @param group_cols Character vector of grouping columns. Standard roles such
#'   as `"subject"`, `"media_id"`, `"trial"`, and `"trial_global"` are
#'   internally standardised when available. Other columns, such as
#'   `"condition"` or `"AOI"`, can also be used if present in `data`. Use
#'   `character(0)` for overall window summaries.
#' @param include_window_end Logical. If `FALSE`, windows are left-closed and
#'   right-open: `[start, end)`. If `TRUE`, the end point is included:
#'   `[start, end]`. Defaults to `FALSE`.
#' @param min_valid_samples Minimum number of finite pupil samples required for
#'   a window to be labelled `"valid"`. Defaults to `1`.
#'
#' @return A tibble with one row per group-by-window combination present in the
#'   data.
#'
#' @examples
#' \donttest{
#' pupil_data <- tibble::tibble(
#'   subject = rep("P1", 6),
#'   media_id = rep("M1", 6),
#'   time = c(0, 250, 500, 750, 1000, 1250),
#'   pupil_smoothed = c(0.00, 0.05, 0.10, 0.08, 0.03, 0.00)
#' )
#'
#' pupil_windows <- summarise_gazepoint_pupil_windows(
#'   pupil_data,
#'   pupil_col = "pupil_smoothed",
#'   time_col = "time",
#'   windows = c(0, 500, 1000, 1500),
#'   group_cols = c("subject", "media_id")
#' )
#'
#' pupil_windows
#' }
#' @importFrom rlang .data
#'
#' @export
summarise_gazepoint_pupil_windows <- function(
    data,
    pupil_col = NULL,
    time_col = NULL,
    windows = c(0, 500, 1000, 2000),
    group_cols = c("subject", "media_id"),
    include_window_end = FALSE,
    min_valid_samples = 1
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

  if (!is.logical(include_window_end) || length(include_window_end) != 1) {
    rlang::abort("`include_window_end` must be `TRUE` or `FALSE`.")
  }

  if (!is.numeric(min_valid_samples) || length(min_valid_samples) != 1) {
    rlang::abort("`min_valid_samples` must be a single numeric value.")
  }

  if (min_valid_samples < 1) {
    rlang::abort("`min_valid_samples` must be greater than or equal to 1.")
  }

  min_valid_samples <- as.integer(min_valid_samples)

  detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0) {
      return(NA_character_)
    }

    found[[1]]
  }

  detect_from <- function(df, candidates) {
    found <- candidates[candidates %in% names(df)]

    if (length(found) == 0) {
      return(NA_character_)
    }

    found[[1]]
  }

  format_window_number <- function(x) {
    format(x, trim = TRUE, scientific = FALSE)
  }

  normalise_windows <- function(windows) {
    if (is.numeric(windows)) {
      if (length(windows) < 2) {
        rlang::abort("Numeric `windows` must contain at least two breakpoints.")
      }

      if (any(is.na(windows))) {
        rlang::abort("Numeric `windows` must not contain missing values.")
      }

      if (is.unsorted(windows, strictly = TRUE)) {
        rlang::abort("Numeric `windows` must be strictly increasing.")
      }

      starts <- utils::head(windows, -1)
      ends <- utils::tail(windows, -1)

      return(
        tibble::tibble(
          window_label = paste0(
            format_window_number(starts),
            "_",
            format_window_number(ends),
            "ms"
          ),
          window_start_ms = starts,
          window_end_ms = ends
        )
      )
    }

    if (is.data.frame(windows)) {
      start_col <- detect_from(
        windows,
        c("window_start_ms", "window_start", "start_ms", "start")
      )

      end_col <- detect_from(
        windows,
        c("window_end_ms", "window_end", "end_ms", "end")
      )

      label_col <- detect_from(
        windows,
        c("window_label", "label", "window")
      )

      if (is.na(start_col)) {
        rlang::abort("Window data must contain a start column.")
      }

      if (is.na(end_col)) {
        rlang::abort("Window data must contain an end column.")
      }

      starts <- suppressWarnings(as.numeric(windows[[start_col]]))
      ends <- suppressWarnings(as.numeric(windows[[end_col]]))

      if (any(is.na(starts)) || any(is.na(ends))) {
        rlang::abort("Window start and end values must be numeric and non-missing.")
      }

      labels <- if (!is.na(label_col)) {
        as.character(windows[[label_col]])
      } else {
        paste0(
          format_window_number(starts),
          "_",
          format_window_number(ends),
          "ms"
        )
      }

      return(
        tibble::tibble(
          window_label = labels,
          window_start_ms = starts,
          window_end_ms = ends
        )
      )
    }

    rlang::abort("`windows` must be either a numeric vector or a data frame.")
  }

  window_tbl <- normalise_windows(windows)

  if (nrow(window_tbl) == 0) {
    rlang::abort("`windows` must define at least one window.")
  }

  if (any(window_tbl$window_end_ms < window_tbl$window_start_ms)) {
    rlang::abort("Each window end must be greater than or equal to its start.")
  }

  if (any(is.na(window_tbl$window_label)) || any(window_tbl$window_label == "")) {
    rlang::abort("Window labels must not be missing or empty.")
  }

  subject_source <- detect_col(c("subject", "pID", "participant"))
  media_source <- detect_col(c("media_id", "MEDIA_ID"))
  trial_source <- detect_col(c("trial"))
  trial_global_source <- detect_col(c("trial_global"))

  if (is.null(pupil_col)) {
    pupil_source <- detect_col(c(
      "pupil_smoothed",
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
    time_source <- detect_col(c(
      "time_relative_ms",
      "relative_time_ms",
      "event_time_ms",
      "time_ms",
      "time",
      "time_orig",
      "time_orig_ms"
    ))
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
    pupil_value = suppressWarnings(as.numeric(data[[pupil_source]]))
  )

  if (length(non_role_group_cols) > 0) {
    for (col in non_role_group_cols) {
      work[[col]] <- data[[col]]
    }
  }

  work <- work |>
    dplyr::filter(is.finite(.data$time_ms))

  assign_window <- function(df, window_row) {
    window_start <- window_row$window_start_ms[[1]]
    window_end <- window_row$window_end_ms[[1]]

    in_window <- if (isTRUE(include_window_end)) {
      df$time_ms >= window_start & df$time_ms <= window_end
    } else {
      df$time_ms >= window_start & df$time_ms < window_end
    }

    selected <- df[in_window, , drop = FALSE]

    if (nrow(selected) == 0) {
      return(tibble::tibble())
    }

    selected$window_label <- window_row$window_label[[1]]
    selected$window_start_ms <- window_start
    selected$window_end_ms <- window_end

    tibble::as_tibble(selected)
  }

  windowed <- dplyr::bind_rows(
    lapply(
      seq_len(nrow(window_tbl)),
      function(i) assign_window(work, window_tbl[i, , drop = FALSE])
    )
  )

  empty_output <- function() {
    group_empty <- tibble::as_tibble(
      stats::setNames(
        replicate(length(group_cols), character(), simplify = FALSE),
        group_cols
      )
    )

    summary_empty <- tibble::tibble(
      window_label = character(),
      window_start_ms = numeric(),
      window_end_ms = numeric(),
      n_samples = integer(),
      n_valid_pupil = integer(),
      n_missing_pupil = integer(),
      valid_pupil_pct = numeric(),
      missing_pupil_pct = numeric(),
      mean_pupil = numeric(),
      sd_pupil = numeric(),
      median_pupil = numeric(),
      min_pupil = numeric(),
      max_pupil = numeric(),
      q25_pupil = numeric(),
      q75_pupil = numeric(),
      pupil_auc = numeric(),
      pupil_time_span_ms = numeric(),
      pupil_window_status = character(),
      pupil_window_pupil_column = character(),
      pupil_window_time_column = character(),
      pupil_window_min_valid_samples = integer(),
      pupil_window_include_end = logical()
    )

    dplyr::bind_cols(group_empty, summary_empty)
  }

  if (nrow(windowed) == 0) {
    return(empty_output())
  }

  safe_sd <- function(x) {
    x <- x[is.finite(x)]

    if (length(x) < 2) {
      return(NA_real_)
    }

    stats::sd(x, na.rm = TRUE)
  }

  safe_quantile <- function(x, probs) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    stats::quantile(
      x,
      probs = probs,
      na.rm = TRUE,
      names = FALSE
    )
  }

  safe_min <- function(x) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    min(x)
  }

  safe_max <- function(x) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    max(x)
  }

  safe_mean <- function(x) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    mean(x)
  }

  safe_median <- function(x) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    stats::median(x)
  }

  safe_auc <- function(time, pupil) {
    time <- suppressWarnings(as.numeric(time))
    pupil <- suppressWarnings(as.numeric(pupil))

    keep <- is.finite(time) & is.finite(pupil)

    if (sum(keep) < 2) {
      return(NA_real_)
    }

    time <- time[keep]
    pupil <- pupil[keep]

    order_id <- order(time)
    time <- time[order_id]
    pupil <- pupil[order_id]

    sum(diff(time) * (utils::head(pupil, -1) + utils::tail(pupil, -1)) / 2)
  }

  safe_time_span <- function(time, pupil) {
    time <- suppressWarnings(as.numeric(time))
    pupil <- suppressWarnings(as.numeric(pupil))

    keep <- is.finite(time) & is.finite(pupil)

    if (sum(keep) < 2) {
      return(NA_real_)
    }

    max(time[keep]) - min(time[keep])
  }

  summarise_window <- function(df) {
    values <- suppressWarnings(as.numeric(df$pupil_value))
    times <- suppressWarnings(as.numeric(df$time_ms))
    valid <- is.finite(values)

    n_samples_value <- length(values)
    n_valid_value <- sum(valid, na.rm = TRUE)
    n_missing_value <- n_samples_value - n_valid_value

    tibble::tibble(
      n_samples = n_samples_value,
      n_valid_pupil = n_valid_value,
      n_missing_pupil = n_missing_value,
      valid_pupil_pct = if (n_samples_value > 0) {
        (n_valid_value / n_samples_value) * 100
      } else {
        NA_real_
      },
      missing_pupil_pct = if (n_samples_value > 0) {
        (n_missing_value / n_samples_value) * 100
      } else {
        NA_real_
      },
      mean_pupil = safe_mean(values),
      sd_pupil = safe_sd(values),
      median_pupil = safe_median(values),
      min_pupil = safe_min(values),
      max_pupil = safe_max(values),
      q25_pupil = safe_quantile(values, 0.25),
      q75_pupil = safe_quantile(values, 0.75),
      pupil_auc = safe_auc(times, values),
      pupil_time_span_ms = safe_time_span(times, values),
      pupil_window_status = dplyr::case_when(
        n_valid_value >= min_valid_samples ~ "valid",
        n_valid_value == 0 ~ "no_valid_pupil",
        TRUE ~ "insufficient_valid_pupil"
      )
    )
  }

  grouping_vars <- c(
    group_cols,
    "window_label",
    "window_start_ms",
    "window_end_ms"
  )

  output <- windowed |>
    dplyr::group_by(!!!rlang::syms(grouping_vars)) |>
    dplyr::group_modify(~ summarise_window(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      .data$window_start_ms,
      .data$window_end_ms,
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      pupil_window_pupil_column = pupil_source,
      pupil_window_time_column = time_source,
      pupil_window_min_valid_samples = min_valid_samples,
      pupil_window_include_end = include_window_end
    )

  output
}
