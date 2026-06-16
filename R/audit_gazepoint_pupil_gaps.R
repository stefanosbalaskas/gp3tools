#' Audit Gazepoint pupil interpolation gaps
#'
#' Summarise observed, interpolated, and remaining missing pupil samples,
#' together with gap-level counts and gap duration/sample-size summaries.
#'
#' This function is intended for data returned by
#' [interpolate_gazepoint_pupil()], but it can also be used with any table
#' that contains compatible interpolation-status and gap columns.
#'
#' @param data A data frame containing pupil interpolation status columns.
#' @param group_cols Character vector of grouping columns. Use `character(0)`
#'   for an overall audit.
#' @param status_col Name of the interpolation status column.
#' @param gap_id_col Name of the gap identifier column.
#' @param gap_n_samples_col Name of the column containing gap size in samples.
#' @param gap_duration_col Name of the column containing gap duration in ms.
#' @param interpolated_col Name of the logical column indicating whether a
#'   sample was interpolated.
#' @param pupil_col Name of the pupil column after interpolation.
#'
#' @return A tibble with one row per group, or one row overall when
#'   `group_cols = character(0)`.
#'
#' @export
#' @importFrom rlang .data
audit_gazepoint_pupil_gaps <- function(
    data,
    group_cols = c("subject", "media_id"),
    status_col = "pupil_interpolation_status",
    gap_id_col = "pupil_gap_id",
    gap_n_samples_col = "pupil_gap_n_samples",
    gap_duration_col = "pupil_gap_duration_ms",
    interpolated_col = "pupil_was_interpolated",
    pupil_col = "pupil_interpolated"
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

  column_args <- c(
    status_col = status_col,
    gap_id_col = gap_id_col,
    gap_n_samples_col = gap_n_samples_col,
    gap_duration_col = gap_duration_col,
    interpolated_col = interpolated_col,
    pupil_col = pupil_col
  )

  valid_column_arg <- vapply(
    column_args,
    function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(x)
    },
    logical(1)
  )

  if (any(!valid_column_arg)) {
    stop(
      "Column-name arguments must be non-missing character scalars: ",
      paste(names(column_args)[!valid_column_arg], collapse = ", "),
      call. = FALSE
    )
  }

  required_cols <- unique(c(group_cols, unname(column_args)))
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

  no_time_status <- c(
    "missing_no_time",
    "missing_no_time_gap"
  )

  insufficient_status <- c(
    "missing_insufficient_valid_samples",
    "missing_insufficient_valid"
  )

  unfilled_status <- c(
    "missing_unfilled",
    "unfilled"
  )

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_audit_status = as.character(.data[[status_col]]),
      .gp3_audit_was_interpolated =
        as_logical_flag(.data[[interpolated_col]]) |
        dplyr::coalesce(.data[[".gp3_audit_status"]] == "interpolated", FALSE),
      .gp3_audit_gap_id = .data[[gap_id_col]],
      .gp3_audit_gap_n_samples = as_numeric_safe(.data[[gap_n_samples_col]]),
      .gp3_audit_gap_duration_ms = as_numeric_safe(.data[[gap_duration_col]]),
      .gp3_audit_pupil = .data[[pupil_col]]
    )

  sample_summary <- working |>
    group_data() |>
    dplyr::summarise(
      n_rows = dplyr::n(),

      n_observed_samples = sum(
        .data[[".gp3_audit_status"]] == "observed",
        na.rm = TRUE
      ),

      n_interpolated_samples = sum(
        .data[[".gp3_audit_was_interpolated"]],
        na.rm = TRUE
      ),

      n_missing_edge_gap_samples = sum(
        .data[[".gp3_audit_status"]] == "missing_edge_gap",
        na.rm = TRUE
      ),

      n_missing_long_gap_samples = sum(
        .data[[".gp3_audit_status"]] == "missing_long_gap",
        na.rm = TRUE
      ),

      n_missing_no_time_samples = sum(
        .data[[".gp3_audit_status"]] %in% no_time_status,
        na.rm = TRUE
      ),

      n_missing_insufficient_valid_samples = sum(
        .data[[".gp3_audit_status"]] %in% insufficient_status,
        na.rm = TRUE
      ),

      n_missing_unfilled_samples = sum(
        .data[[".gp3_audit_status"]] %in% unfilled_status,
        na.rm = TRUE
      ),

      n_remaining_missing_samples = sum(
        is.na(.data[[".gp3_audit_pupil"]])
      ),

      n_total_missing_or_gap_samples = sum(
        (
          !is.na(.data[[".gp3_audit_status"]]) &
            .data[[".gp3_audit_status"]] != "observed"
        ) |
          is.na(.data[[".gp3_audit_pupil"]]),
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_observed_samples = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_observed_samples / .data$n_rows,
        NA_real_
      ),

      pct_interpolated_samples = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_interpolated_samples / .data$n_rows,
        NA_real_
      ),

      pct_remaining_missing_samples = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_remaining_missing_samples / .data$n_rows,
        NA_real_
      )
    )

  gap_level <- working |>
    dplyr::filter(!is.na(.data[[".gp3_audit_gap_id"]])) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(unique(c(group_cols, ".gp3_audit_gap_id")))
      )
    ) |>
    dplyr::summarise(
      .gap_was_interpolated = any(
        .data[[".gp3_audit_was_interpolated"]],
        na.rm = TRUE
      ),

      .gap_is_edge = any(
        .data[[".gp3_audit_status"]] == "missing_edge_gap",
        na.rm = TRUE
      ),

      .gap_is_long = any(
        .data[[".gp3_audit_status"]] == "missing_long_gap",
        na.rm = TRUE
      ),

      .gap_n_samples = max_or_na(.data[[".gp3_audit_gap_n_samples"]]),
      .gap_duration_ms = max_or_na(.data[[".gp3_audit_gap_duration_ms"]]),

      .groups = "drop"
    )

  gap_summary <- gap_level |>
    group_data() |>
    dplyr::summarise(
      n_gaps_total = dplyr::n(),

      n_gaps_interpolated = sum(
        .data[[".gap_was_interpolated"]],
        na.rm = TRUE
      ),

      n_gaps_edge = sum(
        .data[[".gap_is_edge"]],
        na.rm = TRUE
      ),

      n_gaps_long = sum(
        .data[[".gap_is_long"]],
        na.rm = TRUE
      ),

      mean_gap_duration_ms = mean_or_na(.data[[".gap_duration_ms"]]),
      max_gap_duration_ms = max_or_na(.data[[".gap_duration_ms"]]),
      mean_gap_n_samples = mean_or_na(.data[[".gap_n_samples"]]),
      max_gap_n_samples = max_or_na(.data[[".gap_n_samples"]]),

      .groups = "drop"
    )

  out <- if (length(group_cols) > 0L) {
    dplyr::left_join(sample_summary, gap_summary, by = group_cols)
  } else {
    dplyr::bind_cols(sample_summary, gap_summary)
  }

  gap_count_cols <- c(
    "n_gaps_total",
    "n_gaps_interpolated",
    "n_gaps_edge",
    "n_gaps_long"
  )

  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(gap_count_cols),
        ~ dplyr::coalesce(as.integer(.x), 0L)
      )
    )

  output_cols <- c(
    "n_rows",
    "n_observed_samples",
    "n_interpolated_samples",
    "n_missing_edge_gap_samples",
    "n_missing_long_gap_samples",
    "n_missing_no_time_samples",
    "n_missing_insufficient_valid_samples",
    "n_missing_unfilled_samples",
    "n_remaining_missing_samples",
    "n_total_missing_or_gap_samples",
    "pct_observed_samples",
    "pct_interpolated_samples",
    "pct_remaining_missing_samples",
    "n_gaps_total",
    "n_gaps_interpolated",
    "n_gaps_edge",
    "n_gaps_long",
    "mean_gap_duration_ms",
    "max_gap_duration_ms",
    "mean_gap_n_samples",
    "max_gap_n_samples"
  )

  out |>
    dplyr::select(dplyr::all_of(c(group_cols, output_cols)))
}
