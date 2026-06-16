#' Summarise Gazepoint AOI entry episodes
#'
#' Convert sample-level AOI states into AOI entry episodes. An entry starts
#' whenever the AOI state changes within a subject, media, trial, or other
#' grouping unit.
#'
#' @param data A Gazepoint master/sample-level data frame.
#' @param aoi_col Name of the AOI-state column. If `NULL`, the function tries
#'   `aoi_current`, `AOI`, and `aoi_state`.
#' @param time_col Name of the time column, in milliseconds.
#' @param group_cols Character vector of columns defining independent
#'   sequences, usually subject/media/trial.
#' @param include_non_aoi Logical. If `TRUE`, non-AOI/background episodes are
#'   retained. If `FALSE`, they are removed after entry order and neighbouring
#'   states have been computed.
#' @param non_aoi_values Character vector of AOI labels treated as background
#'   or non-AOI states.
#' @param missing_aoi_label Label used when the AOI value is missing.
#'
#' @return A tibble with one row per AOI entry episode.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_aoi_entries <- function(
    data,
    aoi_col = NULL,
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    include_non_aoi = TRUE,
    non_aoi_values = c(
      "non_aoi",
      "none",
      "background",
      "outside",
      "outside_aoi",
      "missing",
      "missing_aoi"
    ),
    missing_aoi_label = "missing_aoi"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
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

  if (!valid_optional_column(aoi_col)) {
    stop(
      "`aoi_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.character(time_col) ||
      length(time_col) != 1L ||
      is.na(time_col) ||
      !nzchar(time_col)) {
    stop("`time_col` must be a non-missing character scalar.", call. = FALSE)
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

  if (!is.logical(include_non_aoi) ||
      length(include_non_aoi) != 1L ||
      is.na(include_non_aoi)) {
    stop("`include_non_aoi` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(non_aoi_values) ||
      any(is.na(non_aoi_values)) ||
      any(!nzchar(non_aoi_values))) {
    stop(
      "`non_aoi_values` must be a character vector of non-missing labels.",
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

  auto_detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0L) {
      return(NULL)
    }

    found[[1]]
  }

  if (is.null(aoi_col)) {
    aoi_col <- auto_detect_col(
      c("aoi_current", "AOI", "aoi_state")
    )
  }

  if (is.null(aoi_col)) {
    stop(
      "Could not automatically detect an AOI column. Please provide `aoi_col`.",
      call. = FALSE
    )
  }

  required_cols <- unique(c(group_cols, time_col, aoi_col))
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

  clean_aoi_state <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[is.na(x) | x == ""] <- missing_aoi_label
    x
  }

  median_positive <- function(x) {
    x <- x[!is.na(x) & is.finite(x) & x > 0]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    stats::median(x)
  }

  is_non_aoi_state <- function(x) {
    tolower(trimws(as.character(x))) %in%
      tolower(trimws(non_aoi_values))
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_aoi_time = as_numeric_safe(.data[[time_col]]),
      .gp3_aoi_state = clean_aoi_state(.data[[aoi_col]])
    ) |>
    dplyr::filter(!is.na(.data[[".gp3_aoi_time"]])) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[[".gp3_aoi_time"]]
    )

  if (nrow(working) == 0L) {
    stop(
      "No non-missing time values remain after filtering.",
      call. = FALSE
    )
  }

  working <- working |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      .gp3_next_time = dplyr::lead(.data[[".gp3_aoi_time"]]),
      .gp3_positive_dt = dplyr::if_else(
        !is.na(.data[[".gp3_next_time"]]) &
          .data[[".gp3_next_time"]] > .data[[".gp3_aoi_time"]],
        .data[[".gp3_next_time"]] - .data[[".gp3_aoi_time"]],
        NA_real_
      ),
      .gp3_default_dt = median_positive(.data[[".gp3_positive_dt"]]),
      .gp3_sample_duration_ms = dplyr::coalesce(
        .data[[".gp3_positive_dt"]],
        .data[[".gp3_default_dt"]],
        0
      ),
      .gp3_entry_change = dplyr::if_else(
        dplyr::row_number() == 1L,
        TRUE,
        .data[[".gp3_aoi_state"]] !=
          dplyr::lag(.data[[".gp3_aoi_state"]])
      ),
      .gp3_entry_id = cumsum(.data[[".gp3_entry_change"]])
    ) |>
    dplyr::ungroup()

  entries <- working |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[[".gp3_entry_id"]],
      .data[[".gp3_aoi_state"]]
    ) |>
    dplyr::summarise(
      entry_start_time = min(.data[[".gp3_aoi_time"]], na.rm = TRUE),
      entry_end_time = max(
        .data[[".gp3_aoi_time"]] +
          .data[[".gp3_sample_duration_ms"]],
        na.rm = TRUE
      ),
      entry_duration_ms = sum(
        .data[[".gp3_sample_duration_ms"]],
        na.rm = TRUE
      ),
      n_samples = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      entry_id = .data[[".gp3_entry_id"]],
      aoi_state = .data[[".gp3_aoi_state"]]
    ) |>
    dplyr::select(
      -dplyr::all_of(c(".gp3_entry_id", ".gp3_aoi_state"))
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data$entry_start_time
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      entry_order = dplyr::row_number(),
      previous_aoi_state = dplyr::lag(.data$aoi_state),
      next_aoi_state = dplyr::lead(.data$aoi_state),
      is_non_aoi = is_non_aoi_state(.data$aoi_state)
    ) |>
    dplyr::ungroup()

  if (!include_non_aoi) {
    entries <- entries |>
      dplyr::filter(!.data$is_non_aoi)
  }

  entries |>
    dplyr::select(
      dplyr::all_of(
        c(
          group_cols,
          "entry_id",
          "entry_order",
          "aoi_state",
          "previous_aoi_state",
          "next_aoi_state",
          "entry_start_time",
          "entry_end_time",
          "entry_duration_ms",
          "n_samples",
          "is_non_aoi"
        )
      )
    )
}
