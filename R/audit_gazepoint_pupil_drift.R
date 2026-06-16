#' Audit Gazepoint pupil drift
#'
#' Summarise tonic pupil/time-on-task drift in processed Gazepoint pupil data.
#'
#' The function estimates simple linear pupil trends over time within selected
#' grouping variables, usually subjects. It also reports subject-level drift,
#' condition-level drift, and possible condition imbalance in time-on-task.
#'
#' @param data A data frame from a Gazepoint pupil preprocessing pipeline.
#' @param group_cols Character vector of grouping columns for the main drift
#'   audit. The default is `"subject"`.
#' @param pupil_col Name of the pupil column to analyse. If `NULL`, the
#'   function automatically tries `pupil_smoothed`,
#'   `pupil_baseline_corrected`, `pupil_interpolated`, `pupil_clean`, and
#'   `pupil`.
#' @param time_col Name of the within-trial or sample-time column.
#' @param order_col Optional trial/order column used to assess time-on-task
#'   imbalance. If `NULL`, order-based summaries are skipped.
#' @param condition_col Optional condition column used to summarise condition
#'   drift and time-on-task imbalance. If `NULL`, condition summaries are
#'   skipped.
#' @param exclude_col Optional logical exclusion column. If present and
#'   `include_excluded = FALSE`, excluded rows are removed before analysis.
#' @param include_excluded Logical. If `FALSE`, rows marked by `exclude_col`
#'   are excluded when that column exists.
#' @param min_valid_samples Minimum valid pupil samples required to estimate a
#'   drift slope.
#' @param max_abs_slope_per_min Maximum acceptable absolute pupil slope per
#'   minute before a drift warning is raised.
#' @param max_condition_time_mean_diff_ms Maximum acceptable difference in mean
#'   sample time across conditions.
#' @param max_condition_order_mean_diff Maximum acceptable difference in mean
#'   trial/order value across conditions.
#'
#' @return A named list containing `by_group`, `by_subject`, `by_condition`,
#'   `condition_balance`, and `summary` tibbles.
#'
#' @export
#' @importFrom rlang .data
audit_gazepoint_pupil_drift <- function(
    data,
    group_cols = "subject",
    pupil_col = NULL,
    time_col = "time",
    order_col = "trial",
    condition_col = "condition",
    exclude_col = "excluded_trial",
    include_excluded = FALSE,
    min_valid_samples = 3,
    max_abs_slope_per_min = 1,
    max_condition_time_mean_diff_ms = 1000,
    max_condition_order_mean_diff = 1
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
    order_col = order_col,
    condition_col = condition_col,
    exclude_col = exclude_col
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

  numeric_args <- c(
    min_valid_samples = min_valid_samples,
    max_abs_slope_per_min = max_abs_slope_per_min,
    max_condition_time_mean_diff_ms = max_condition_time_mean_diff_ms,
    max_condition_order_mean_diff = max_condition_order_mean_diff
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
      "Threshold arguments must be finite numeric scalars: ",
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

  if (is.null(pupil_col)) {
    pupil_col <- auto_detect_col(
      c(
        "pupil_smoothed",
        "pupil_baseline_corrected",
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

  required_cols <- unique(c(group_cols, pupil_col, time_col))

  if (!is.null(order_col)) {
    required_cols <- unique(c(required_cols, order_col))
  }

  if (!is.null(condition_col)) {
    required_cols <- unique(c(required_cols, condition_col))
  }

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

  sd_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    stats::sd(x)
  }

  slope_or_na <- function(y, x) {
    y <- as_numeric_safe(y)
    x <- as_numeric_safe(x)

    ok <- !is.na(y) & !is.na(x)

    y <- y[ok]
    x <- x[ok]

    if (length(y) < min_valid_samples || length(unique(x)) < 2L) {
      return(NA_real_)
    }

    unname(stats::coef(stats::lm(y ~ x))[["x"]])
  }

  cor_or_na <- function(y, x) {
    y <- as_numeric_safe(y)
    x <- as_numeric_safe(x)

    ok <- !is.na(y) & !is.na(x)

    y <- y[ok]
    x <- x[ok]

    if (length(y) < min_valid_samples || length(unique(x)) < 2L) {
      return(NA_real_)
    }

    suppressWarnings(stats::cor(y, x))
  }

  range_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x) - min(x)
  }

  group_data <- function(x, cols) {
    if (length(cols) > 0L) {
      dplyr::group_by(x, dplyr::across(dplyr::all_of(cols)))
    } else {
      x
    }
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_drift_pupil = as_numeric_safe(.data[[pupil_col]]),
      .gp3_drift_time = as_numeric_safe(.data[[time_col]])
    )

  if (!is.null(order_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_drift_order = as_numeric_safe(.data[[order_col]])
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_drift_order = NA_real_
      )
  }

  if (!is.null(condition_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_drift_condition = as.character(.data[[condition_col]])
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_drift_condition = NA_character_
      )
  }

  if (!is.null(exclude_col) && exclude_col %in% names(working) && !include_excluded) {
    working <- working |>
      dplyr::filter(!as_logical_flag(.data[[exclude_col]]))
  }

  summarise_drift <- function(x, cols) {
    x |>
      group_data(cols) |>
      dplyr::summarise(
        n_rows = dplyr::n(),

        n_valid_pupil = sum(
          !is.na(.data[[".gp3_drift_pupil"]]) &
            !is.na(.data[[".gp3_drift_time"]]),
          na.rm = TRUE
        ),

        valid_pupil_pct = dplyr::if_else(
          .data$n_rows > 0L,
          100 * .data$n_valid_pupil / .data$n_rows,
          NA_real_
        ),

        pupil_mean = mean_or_na(.data[[".gp3_drift_pupil"]]),
        pupil_sd = sd_or_na(.data[[".gp3_drift_pupil"]]),

        time_min = min_or_na(.data[[".gp3_drift_time"]]),
        time_mean = mean_or_na(.data[[".gp3_drift_time"]]),
        time_max = max_or_na(.data[[".gp3_drift_time"]]),
        time_range = range_or_na(.data[[".gp3_drift_time"]]),

        order_min = min_or_na(.data[[".gp3_drift_order"]]),
        order_mean = mean_or_na(.data[[".gp3_drift_order"]]),
        order_max = max_or_na(.data[[".gp3_drift_order"]]),
        order_range = range_or_na(.data[[".gp3_drift_order"]]),

        pupil_time_slope_per_ms = slope_or_na(
          .data[[".gp3_drift_pupil"]],
          .data[[".gp3_drift_time"]]
        ),

        pupil_time_r = cor_or_na(
          .data[[".gp3_drift_pupil"]],
          .data[[".gp3_drift_time"]]
        ),

        .groups = "drop"
      ) |>
      dplyr::mutate(
        pupil_time_slope_per_sec = .data$pupil_time_slope_per_ms * 1000,
        pupil_time_slope_per_min = .data$pupil_time_slope_per_ms * 60000,

        abs_pupil_time_slope_per_min = abs(.data$pupil_time_slope_per_min),

        drift_direction = dplyr::case_when(
          is.na(.data$pupil_time_slope_per_min) ~ "not_estimated",
          .data$pupil_time_slope_per_min > 0 ~ "increasing",
          .data$pupil_time_slope_per_min < 0 ~ "decreasing",
          TRUE ~ "flat"
        ),

        drift_warning =
          !is.na(.data$abs_pupil_time_slope_per_min) &
          .data$abs_pupil_time_slope_per_min > max_abs_slope_per_min,

        drift_status = dplyr::case_when(
          .data$n_valid_pupil < min_valid_samples ~ "insufficient_valid_samples",
          is.na(.data$pupil_time_slope_per_min) ~ "not_estimated",
          .data$drift_warning ~ "possible_drift",
          TRUE ~ "ok"
        )
      )
  }

  by_group <- summarise_drift(working, group_cols)

  subject_cols <- intersect("subject", names(data))

  by_subject <- if (length(subject_cols) > 0L) {
    summarise_drift(working, subject_cols)
  } else {
    tibble::tibble()
  }

  condition_available <-
    !is.null(condition_col) &&
    condition_col %in% names(data)

  has_non_missing_condition <- condition_available &&
    any(!is.na(working[[".gp3_drift_condition"]]))

  by_condition <- if (condition_available && has_non_missing_condition) {
    working |>
      dplyr::filter(!is.na(.data[[".gp3_drift_condition"]])) |>
      summarise_drift(condition_col)
  } else {
    tibble::tibble()
  }

  condition_balance <- if (condition_available && has_non_missing_condition) {
    by_condition |>
      dplyr::summarise(
        n_conditions = dplyr::n(),
        condition_time_mean_range = range_or_na(.data$time_mean),
        condition_order_mean_range = range_or_na(.data$order_mean),
        condition_time_imbalance_warning =
          dplyr::coalesce(
            range_or_na(.data$time_mean) > max_condition_time_mean_diff_ms,
            FALSE
          ),
        condition_order_imbalance_warning =
          dplyr::coalesce(
            range_or_na(.data$order_mean) > max_condition_order_mean_diff,
            FALSE
          ),
        condition_balance_warning =
          .data$condition_time_imbalance_warning |
          .data$condition_order_imbalance_warning,
        condition_balance_reason = dplyr::case_when(
          .data$condition_time_imbalance_warning &
            .data$condition_order_imbalance_warning ~
            "time_mean_diff;order_mean_diff",
          .data$condition_time_imbalance_warning ~ "time_mean_diff",
          .data$condition_order_imbalance_warning ~ "order_mean_diff",
          TRUE ~ "ok"
        )
      )
  } else if (condition_available && !has_non_missing_condition) {
    tibble::tibble(
      n_conditions = 0L,
      condition_time_mean_range = NA_real_,
      condition_order_mean_range = NA_real_,
      condition_time_imbalance_warning = FALSE,
      condition_order_imbalance_warning = FALSE,
      condition_balance_warning = FALSE,
      condition_balance_reason = "no_non_missing_conditions"
    )
  } else {
    tibble::tibble(
      n_conditions = NA_integer_,
      condition_time_mean_range = NA_real_,
      condition_order_mean_range = NA_real_,
      condition_time_imbalance_warning = NA,
      condition_order_imbalance_warning = NA,
      condition_balance_warning = NA,
      condition_balance_reason = "condition_col_not_available"
    )
  }

  summary <- tibble::tibble(
    n_rows = nrow(working),
    pupil_column = pupil_col,
    time_column = time_col,
    order_column = ifelse(is.null(order_col), NA_character_, order_col),
    condition_column = ifelse(
      is.null(condition_col),
      NA_character_,
      condition_col
    ),
    n_by_group = nrow(by_group),
    n_subjects = nrow(by_subject),
    n_conditions = ifelse(nrow(condition_balance) > 0L,
                          condition_balance$n_conditions[[1]],
                          NA_integer_),
    n_group_drift_warnings = sum(by_group$drift_warning, na.rm = TRUE),
    n_subject_drift_warnings = ifelse(
      nrow(by_subject) > 0L,
      sum(by_subject$drift_warning, na.rm = TRUE),
      NA_integer_
    ),
    condition_balance_warning = ifelse(
      nrow(condition_balance) > 0L,
      condition_balance$condition_balance_warning[[1]],
      NA
    )
  )

  list(
    by_group = by_group,
    by_subject = by_subject,
    by_condition = by_condition,
    condition_balance = condition_balance,
    summary = summary
  )
}
