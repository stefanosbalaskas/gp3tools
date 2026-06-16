#' Prepare Gazepoint AOI sequences
#'
#' Create ordered AOI-state sequences from sample-level Gazepoint AOI data or
#' from the output of `summarise_gazepoint_aoi_entries()`. The output is
#' transition-ready and includes the current AOI state, previous state, next
#' state, dwell time before transition, and self-transition flags.
#'
#' @param data A Gazepoint sample-level data frame or an AOI-entry table created
#'   by `summarise_gazepoint_aoi_entries()`.
#' @param aoi_col Name of the AOI-state column. Used only when `data` is
#'   sample-level data. If `NULL`, the function tries `aoi_current`, `AOI`,
#'   and `aoi_state`.
#' @param time_col Name of the time column, in milliseconds. Used only when
#'   `data` is sample-level data.
#' @param group_cols Character vector of columns defining independent AOI
#'   sequences, usually subject/media/trial.
#' @param include_non_aoi Logical. If `TRUE`, non-AOI/background states are
#'   retained. If `FALSE`, non-AOI/background states are removed before sequence
#'   and transition fields are computed.
#' @param non_aoi_values Character vector of AOI labels treated as background
#'   or non-AOI states.
#' @param missing_aoi_label Label used when the AOI value is missing.
#' @param include_terminal Logical. If `TRUE`, the final state of each sequence
#'   is retained with `next_state = NA`. If `FALSE`, terminal states are removed
#'   so that each output row represents an observed transition.
#'
#' @return A tibble with ordered AOI sequence and transition fields.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_aoi_sequences <- function(
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
    missing_aoi_label = "missing_aoi",
    include_terminal = TRUE
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

  if (!is.logical(include_terminal) ||
      length(include_terminal) != 1L ||
      is.na(include_terminal)) {
    stop("`include_terminal` must be TRUE or FALSE.", call. = FALSE)
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

  has_entry_columns <- all(
    c(
      "aoi_state",
      "entry_order",
      "entry_start_time",
      "entry_end_time",
      "entry_duration_ms",
      "n_samples"
    ) %in% names(data)
  )

  if (has_entry_columns) {
    entries <- tibble::as_tibble(data)

    required_cols <- unique(
      c(
        group_cols,
        "aoi_state",
        "entry_order",
        "entry_start_time",
        "entry_end_time",
        "entry_duration_ms",
        "n_samples"
      )
    )

    missing_cols <- setdiff(required_cols, names(entries))

    if (length(missing_cols) > 0L) {
      stop(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    entries <- summarise_gazepoint_aoi_entries(
      data = data,
      aoi_col = aoi_col,
      time_col = time_col,
      group_cols = group_cols,
      include_non_aoi = TRUE,
      non_aoi_values = non_aoi_values,
      missing_aoi_label = missing_aoi_label
    )
  }

  if (nrow(entries) == 0L) {
    stop("No AOI entries are available.", call. = FALSE)
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

  is_non_aoi_state <- function(x) {
    tolower(trimws(as.character(x))) %in%
      tolower(trimws(non_aoi_values))
  }

  entries <- entries |>
    dplyr::mutate(
      aoi_state = clean_aoi_state(.data[["aoi_state"]]),
      entry_start_time = as_numeric_safe(.data[["entry_start_time"]]),
      entry_end_time = as_numeric_safe(.data[["entry_end_time"]]),
      entry_duration_ms = as_numeric_safe(.data[["entry_duration_ms"]]),
      n_samples = suppressWarnings(as.integer(.data[["n_samples"]]))
    ) |>
    dplyr::filter(!is.na(.data[["entry_start_time"]]))

  if (!"entry_id" %in% names(entries)) {
    entries <- entries |>
      dplyr::arrange(
        dplyr::across(dplyr::all_of(group_cols)),
        .data[["entry_start_time"]]
      ) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_cols))
      ) |>
      dplyr::mutate(
        entry_id = dplyr::row_number()
      ) |>
      dplyr::ungroup()
  }

  if (!"is_non_aoi" %in% names(entries)) {
    entries <- entries |>
      dplyr::mutate(
        is_non_aoi = is_non_aoi_state(.data[["aoi_state"]])
      )
  } else {
    entries <- entries |>
      dplyr::mutate(
        is_non_aoi = dplyr::coalesce(
          as.logical(.data[["is_non_aoi"]]),
          is_non_aoi_state(.data[["aoi_state"]])
        )
      )
  }

  if (!include_non_aoi) {
    entries <- entries |>
      dplyr::filter(!.data[["is_non_aoi"]])
  }

  if (nrow(entries) == 0L) {
    stop(
      "No AOI states remain after applying `include_non_aoi`.",
      call. = FALSE
    )
  }

  sequence_data <- entries |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[["entry_start_time"]],
      .data[["entry_order"]]
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      state_order = dplyr::row_number(),
      previous_state = dplyr::lag(.data[["aoi_state"]]),
      next_state = dplyr::lead(.data[["aoi_state"]]),
      transition_from = .data[["aoi_state"]],
      transition_to = .data[["next_state"]],
      dwell_before_transition_ms = .data[["entry_duration_ms"]],
      is_terminal_state = is.na(.data[["next_state"]]),
      is_self_transition =
        !is.na(.data[["transition_to"]]) &
        .data[["transition_from"]] == .data[["transition_to"]],
      transition_order = as.integer(cumsum(!.data[["is_terminal_state"]])),
      transition_order = dplyr::if_else(
        .data[["is_terminal_state"]],
        NA_integer_,
        .data[["transition_order"]]
      )
    ) |>
    dplyr::ungroup()

  if (!include_terminal) {
    sequence_data <- sequence_data |>
      dplyr::filter(!.data[["is_terminal_state"]])
  }

  sequence_data |>
    dplyr::select(
      dplyr::all_of(
        c(
          group_cols,
          "entry_id",
          "state_order",
          "transition_order",
          "aoi_state",
          "previous_state",
          "next_state",
          "transition_from",
          "transition_to",
          "entry_start_time",
          "entry_end_time",
          "entry_duration_ms",
          "dwell_before_transition_ms",
          "n_samples",
          "is_non_aoi",
          "is_self_transition",
          "is_terminal_state"
        )
      )
    )
}
