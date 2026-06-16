#' Summarise Gazepoint AOI transition features
#'
#' Summarise AOI transitions at the trial or group level. The function can work
#' from sample-level Gazepoint AOI data, AOI-entry tables created by
#' `summarise_gazepoint_aoi_entries()`, or AOI-sequence tables created by
#' `prepare_gazepoint_aoi_sequences()`.
#'
#' @param data A Gazepoint sample-level data frame, AOI-entry table, or
#'   AOI-sequence table.
#' @param aoi_col Name of the AOI-state column. Used only when `data` is
#'   sample-level data. If `NULL`, the function tries `aoi_current`, `AOI`,
#'   and `aoi_state`.
#' @param time_col Name of the time column, in milliseconds. Used only when
#'   `data` is sample-level data.
#' @param group_cols Character vector of columns defining independent AOI
#'   sequences, usually subject/media/trial.
#' @param include_non_aoi Logical. If `TRUE`, non-AOI/background states are
#'   retained before transition summaries are computed. This is useful for
#'   background-to-target and target-to-background features.
#' @param target_aoi_values Optional character vector defining target AOI labels.
#' @param distractor_aoi_values Optional character vector defining distractor AOI
#'   labels.
#' @param non_aoi_values Character vector of AOI labels treated as background
#'   or non-AOI states.
#' @param missing_aoi_label Label used when the AOI value is missing.
#'
#' @return A tibble with one row per group and trial-level AOI transition
#'   features.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_aoi_transitions <- function(
    data,
    aoi_col = NULL,
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    include_non_aoi = TRUE,
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

  valid_optional_values <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) >= 1L &&
          all(!is.na(x)) &&
          all(nzchar(x))
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

  if (!valid_optional_values(target_aoi_values)) {
    stop(
      "`target_aoi_values` must be NULL or a character vector of non-missing labels.",
      call. = FALSE
    )
  }

  if (!valid_optional_values(distractor_aoi_values)) {
    stop(
      "`distractor_aoi_values` must be NULL or a character vector of non-missing labels.",
      call. = FALSE
    )
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

  normalise_values <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }

    tolower(trimws(as.character(x)))
  }

  target_values <- normalise_values(target_aoi_values)
  distractor_values <- normalise_values(distractor_aoi_values)
  background_values <- normalise_values(non_aoi_values)

  target_aoi_defined <- length(target_values) > 0L
  distractor_aoi_defined <- length(distractor_values) > 0L

  has_sequence_columns <- all(
    c(
      "aoi_state",
      "transition_from",
      "transition_to",
      "dwell_before_transition_ms",
      "is_terminal_state"
    ) %in% names(data)
  )

  if (has_sequence_columns) {
    sequences <- tibble::as_tibble(data)
  } else {
    sequences <- prepare_gazepoint_aoi_sequences(
      data = data,
      aoi_col = aoi_col,
      time_col = time_col,
      group_cols = group_cols,
      include_non_aoi = include_non_aoi,
      non_aoi_values = non_aoi_values,
      missing_aoi_label = missing_aoi_label,
      include_terminal = TRUE
    )
  }

  required_cols <- unique(
    c(
      group_cols,
      "aoi_state",
      "transition_from",
      "transition_to",
      "entry_duration_ms",
      "dwell_before_transition_ms",
      "is_non_aoi",
      "is_terminal_state"
    )
  )

  missing_cols <- setdiff(required_cols, names(sequences))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(sequences) == 0L) {
    stop("No AOI sequence rows are available.", call. = FALSE)
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

  median_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    stats::median(x)
  }

  max_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x)
  }

  classify_state <- function(x) {
    x_norm <- tolower(trimws(as.character(x)))

    dplyr::case_when(
      is.na(x) ~ NA_character_,
      target_aoi_defined & x_norm %in% target_values ~ "target",
      distractor_aoi_defined & x_norm %in% distractor_values ~ "distractor",
      x_norm %in% background_values ~ "background",
      TRUE ~ "other"
    )
  }

  sequences <- sequences |>
    dplyr::mutate(
      entry_duration_ms = as_numeric_safe(.data[["entry_duration_ms"]]),
      dwell_before_transition_ms =
        as_numeric_safe(.data[["dwell_before_transition_ms"]]),
      is_non_aoi = dplyr::coalesce(
        as.logical(.data[["is_non_aoi"]]),
        FALSE
      ),
      is_terminal_state = dplyr::coalesce(
        as.logical(.data[["is_terminal_state"]]),
        FALSE
      )
    )

  state_summary <- sequences |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::summarise(
      n_states = dplyr::n(),
      n_aoi_states = sum(!.data[["is_non_aoi"]], na.rm = TRUE),
      n_non_aoi_states = sum(.data[["is_non_aoi"]], na.rm = TRUE),
      total_state_dwell_ms = sum(.data[["entry_duration_ms"]], na.rm = TRUE),
      mean_state_dwell_ms = mean_or_na(.data[["entry_duration_ms"]]),
      .groups = "drop"
    )

  transitions <- sequences |>
    dplyr::filter(
      !.data[["is_terminal_state"]],
      !is.na(.data[["transition_to"]])
    ) |>
    dplyr::mutate(
      from_class = classify_state(.data[["transition_from"]]),
      to_class = classify_state(.data[["transition_to"]]),
      is_self_reentry =
        .data[["transition_from"]] == .data[["transition_to"]]
    )

  if (nrow(transitions) > 0L) {
    transition_summary <- transitions |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_cols))
      ) |>
      dplyr::summarise(
        total_transitions = dplyr::n(),
        self_reentries = sum(.data[["is_self_reentry"]], na.rm = TRUE),
        target_to_distractor = sum(
          .data[["from_class"]] == "target" &
            .data[["to_class"]] == "distractor",
          na.rm = TRUE
        ),
        distractor_to_target = sum(
          .data[["from_class"]] == "distractor" &
            .data[["to_class"]] == "target",
          na.rm = TRUE
        ),
        background_to_target = sum(
          .data[["from_class"]] == "background" &
            .data[["to_class"]] == "target",
          na.rm = TRUE
        ),
        target_to_background = sum(
          .data[["from_class"]] == "target" &
            .data[["to_class"]] == "background",
          na.rm = TRUE
        ),
        background_to_distractor = sum(
          .data[["from_class"]] == "background" &
            .data[["to_class"]] == "distractor",
          na.rm = TRUE
        ),
        distractor_to_background = sum(
          .data[["from_class"]] == "distractor" &
            .data[["to_class"]] == "background",
          na.rm = TRUE
        ),
        target_to_target = sum(
          .data[["from_class"]] == "target" &
            .data[["to_class"]] == "target",
          na.rm = TRUE
        ),
        distractor_to_distractor = sum(
          .data[["from_class"]] == "distractor" &
            .data[["to_class"]] == "distractor",
          na.rm = TRUE
        ),
        other_transitions = sum(
          .data[["from_class"]] == "other" |
            .data[["to_class"]] == "other",
          na.rm = TRUE
        ),
        total_pre_transition_dwell_ms = sum(
          .data[["dwell_before_transition_ms"]],
          na.rm = TRUE
        ),
        mean_pre_transition_dwell_ms = mean_or_na(
          .data[["dwell_before_transition_ms"]]
        ),
        median_pre_transition_dwell_ms = median_or_na(
          .data[["dwell_before_transition_ms"]]
        ),
        max_pre_transition_dwell_ms = max_or_na(
          .data[["dwell_before_transition_ms"]]
        ),
        .groups = "drop"
      )
  } else {
    transition_summary <- state_summary |>
      dplyr::select(dplyr::all_of(group_cols)) |>
      dplyr::mutate(
        total_transitions = 0L,
        self_reentries = 0L,
        target_to_distractor = 0L,
        distractor_to_target = 0L,
        background_to_target = 0L,
        target_to_background = 0L,
        background_to_distractor = 0L,
        distractor_to_background = 0L,
        target_to_target = 0L,
        distractor_to_distractor = 0L,
        other_transitions = 0L,
        total_pre_transition_dwell_ms = 0,
        mean_pre_transition_dwell_ms = NA_real_,
        median_pre_transition_dwell_ms = NA_real_,
        max_pre_transition_dwell_ms = NA_real_
      )
  }

  out <- state_summary |>
    dplyr::left_join(
      transition_summary,
      by = group_cols
    )

  count_cols <- c(
    "total_transitions",
    "self_reentries",
    "target_to_distractor",
    "distractor_to_target",
    "background_to_target",
    "target_to_background",
    "background_to_distractor",
    "distractor_to_background",
    "target_to_target",
    "distractor_to_distractor",
    "other_transitions"
  )

  for (col in count_cols) {
    out[[col]] <- dplyr::coalesce(out[[col]], 0L)
  }

  out$total_pre_transition_dwell_ms <- dplyr::coalesce(
    out$total_pre_transition_dwell_ms,
    0
  )

  out |>
    dplyr::mutate(
      target_aoi_defined = target_aoi_defined,
      distractor_aoi_defined = distractor_aoi_defined,
      transition_feature_status = dplyr::case_when(
        .data[["total_transitions"]] == 0L ~ "no_transitions",
        !target_aoi_defined & !distractor_aoi_defined ~
          "no_target_or_distractor_defined",
        !target_aoi_defined ~ "no_target_defined",
        !distractor_aoi_defined ~ "no_distractor_defined",
        TRUE ~ "ok"
      )
    )
}
