#' Summarise Gazepoint AOI trial features
#'
#' Create trial-level AOI features from sample-level Gazepoint AOI data or from
#' AOI-entry tables created by `summarise_gazepoint_aoi_entries()`. The output
#' includes AOI dwell, entry, TTFF, revisit, and transition features.
#'
#' @param data A Gazepoint sample-level data frame, AOI-entry table, or
#'   compatible AOI table.
#' @param aoi_col Name of the AOI-state column. Used only when `data` is
#'   sample-level data. If `NULL`, the function tries `aoi_current`, `AOI`,
#'   and `aoi_state`.
#' @param time_col Name of the time column, in milliseconds. Used only when
#'   `data` is sample-level data.
#' @param group_cols Character vector of columns defining independent trials,
#'   usually subject/media/trial.
#' @param include_non_aoi Logical. If `TRUE`, non-AOI/background states are kept
#'   when computing trial-duration and transition features.
#' @param target_aoi_values Optional character vector defining target AOI labels.
#' @param distractor_aoi_values Optional character vector defining distractor AOI
#'   labels.
#' @param non_aoi_values Character vector of AOI labels treated as background
#'   or non-AOI states.
#' @param missing_aoi_label Label used when the AOI value is missing.
#'
#' @return A tibble with one row per trial/group and AOI trial-level features.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_aoi_trial_features <- function(
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

  has_entry_columns <- all(
    c(
      "aoi_state",
      "entry_start_time",
      "entry_end_time",
      "entry_duration_ms",
      "n_samples"
    ) %in% names(data)
  )

  if (has_entry_columns) {
    entries <- tibble::as_tibble(data)

    required_entry_cols <- unique(
      c(
        group_cols,
        "aoi_state",
        "entry_start_time",
        "entry_end_time",
        "entry_duration_ms",
        "n_samples"
      )
    )

    missing_entry_cols <- setdiff(required_entry_cols, names(entries))

    if (length(missing_entry_cols) > 0L) {
      stop(
        "Missing required columns: ",
        paste(missing_entry_cols, collapse = ", "),
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
    tolower(trimws(as.character(x))) %in% background_values
  }

  classify_state <- function(x, is_non_aoi) {
    x_norm <- tolower(trimws(as.character(x)))

    dplyr::case_when(
      is_non_aoi ~ "background",
      target_aoi_defined & x_norm %in% target_values ~ "target",
      distractor_aoi_defined & x_norm %in% distractor_values ~ "distractor",
      TRUE ~ "other_aoi"
    )
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
      "No AOI entries remain after applying `include_non_aoi`.",
      call. = FALSE
    )
  }

  entries <- entries |>
    dplyr::mutate(
      state_class = classify_state(
        .data[["aoi_state"]],
        .data[["is_non_aoi"]]
      )
    )

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

  median_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    stats::median(x)
  }

  sum_or_zero <- function(x) {
    x <- as_numeric_safe(x)
    sum(x, na.rm = TRUE)
  }

  count_where <- function(condition) {
    sum(condition %in% TRUE, na.rm = TRUE)
  }

  sum_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE

    sum(x[condition], na.rm = TRUE)
  }

  mean_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE
    x <- x[condition]
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  median_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE
    x <- x[condition]
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    stats::median(x)
  }

  max_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE
    x <- x[condition]
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x)
  }

  min_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE
    x <- x[condition]
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    min(x)
  }

  n_unique_where <- function(x, condition) {
    condition <- condition %in% TRUE
    x <- as.character(x[condition])
    x <- x[!is.na(x) & nzchar(x)]

    length(unique(x))
  }

  first_state_where <- function(state, time, condition) {
    condition <- condition %in% TRUE
    state <- as.character(state)
    time <- as_numeric_safe(time)

    keep <- condition & !is.na(time) & !is.na(state) & nzchar(state)

    if (!any(keep)) {
      return(NA_character_)
    }

    state[keep][order(time[keep])][1]
  }

  last_state_where <- function(state, time, condition) {
    condition <- condition %in% TRUE
    state <- as.character(state)
    time <- as_numeric_safe(time)

    keep <- condition & !is.na(time) & !is.na(state) & nzchar(state)

    if (!any(keep)) {
      return(NA_character_)
    }

    state[keep][order(time[keep], decreasing = TRUE)][1]
  }

  safe_ratio <- function(numerator, denominator) {
    numerator <- as_numeric_safe(numerator)
    denominator <- as_numeric_safe(denominator)

    dplyr::if_else(
      is.na(denominator) | denominator <= 0,
      NA_real_,
      numerator / denominator
    )
  }

  feature_summary <- entries |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[["entry_start_time"]]
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::summarise(
      trial_start_time = min_or_na(.data[["entry_start_time"]]),
      trial_end_time = max_or_na(.data[["entry_end_time"]]),
      n_entries = dplyr::n(),
      n_samples_in_entries = sum_or_zero(.data[["n_samples"]]),

      n_aoi_entries = count_where(!.data[["is_non_aoi"]]),
      n_non_aoi_entries = count_where(.data[["is_non_aoi"]]),
      n_unique_aoi_states = n_unique_where(
        .data[["aoi_state"]],
        !.data[["is_non_aoi"]]
      ),

      total_entry_dwell_ms = sum_or_zero(.data[["entry_duration_ms"]]),
      total_aoi_dwell_ms = sum_where(
        .data[["entry_duration_ms"]],
        !.data[["is_non_aoi"]]
      ),
      total_non_aoi_dwell_ms = sum_where(
        .data[["entry_duration_ms"]],
        .data[["is_non_aoi"]]
      ),

      mean_entry_duration_ms = mean_or_na(.data[["entry_duration_ms"]]),
      median_entry_duration_ms = median_or_na(.data[["entry_duration_ms"]]),
      max_entry_duration_ms = max_or_na(.data[["entry_duration_ms"]]),

      mean_aoi_entry_duration_ms = mean_where(
        .data[["entry_duration_ms"]],
        !.data[["is_non_aoi"]]
      ),
      median_aoi_entry_duration_ms = median_where(
        .data[["entry_duration_ms"]],
        !.data[["is_non_aoi"]]
      ),
      max_aoi_entry_duration_ms = max_where(
        .data[["entry_duration_ms"]],
        !.data[["is_non_aoi"]]
      ),

      first_aoi_state = first_state_where(
        .data[["aoi_state"]],
        .data[["entry_start_time"]],
        !.data[["is_non_aoi"]]
      ),
      last_aoi_state = last_state_where(
        .data[["aoi_state"]],
        .data[["entry_start_time"]],
        !.data[["is_non_aoi"]]
      ),
      first_aoi_time_ms = min_where(
        .data[["entry_start_time"]],
        !.data[["is_non_aoi"]]
      ),
      last_aoi_time_ms = max_where(
        .data[["entry_start_time"]],
        !.data[["is_non_aoi"]]
      ),

      target_entries = count_where(.data[["state_class"]] == "target"),
      target_revisits = pmax(
        count_where(.data[["state_class"]] == "target") - 1L,
        0L
      ),
      target_dwell_ms = sum_where(
        .data[["entry_duration_ms"]],
        .data[["state_class"]] == "target"
      ),
      target_ttff_ms = min_where(
        .data[["entry_start_time"]],
        .data[["state_class"]] == "target"
      ),
      mean_target_entry_duration_ms = mean_where(
        .data[["entry_duration_ms"]],
        .data[["state_class"]] == "target"
      ),

      distractor_entries = count_where(.data[["state_class"]] == "distractor"),
      distractor_revisits = pmax(
        count_where(.data[["state_class"]] == "distractor") - 1L,
        0L
      ),
      distractor_dwell_ms = sum_where(
        .data[["entry_duration_ms"]],
        .data[["state_class"]] == "distractor"
      ),
      distractor_ttff_ms = min_where(
        .data[["entry_start_time"]],
        .data[["state_class"]] == "distractor"
      ),
      mean_distractor_entry_duration_ms = mean_where(
        .data[["entry_duration_ms"]],
        .data[["state_class"]] == "distractor"
      ),

      other_aoi_entries = count_where(.data[["state_class"]] == "other_aoi"),
      other_aoi_dwell_ms = sum_where(
        .data[["entry_duration_ms"]],
        .data[["state_class"]] == "other_aoi"
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      trial_duration_ms = .data[["trial_end_time"]] - .data[["trial_start_time"]],
      aoi_dwell_prop = safe_ratio(
        .data[["total_aoi_dwell_ms"]],
        .data[["total_entry_dwell_ms"]]
      ),
      non_aoi_dwell_prop = safe_ratio(
        .data[["total_non_aoi_dwell_ms"]],
        .data[["total_entry_dwell_ms"]]
      ),
      target_dwell_prop_of_aoi = safe_ratio(
        .data[["target_dwell_ms"]],
        .data[["total_aoi_dwell_ms"]]
      ),
      distractor_dwell_prop_of_aoi = safe_ratio(
        .data[["distractor_dwell_ms"]],
        .data[["total_aoi_dwell_ms"]]
      ),
      target_aoi_defined = target_aoi_defined,
      distractor_aoi_defined = distractor_aoi_defined,
      aoi_trial_feature_status = dplyr::case_when(
        .data[["n_aoi_entries"]] == 0L ~ "no_aoi_entries",
        !target_aoi_defined & !distractor_aoi_defined ~
          "no_target_or_distractor_defined",
        target_aoi_defined & .data[["target_entries"]] == 0L ~
          "target_not_observed",
        distractor_aoi_defined & .data[["distractor_entries"]] == 0L ~
          "distractor_not_observed",
        TRUE ~ "ok"
      )
    )

  transition_summary <- summarise_gazepoint_aoi_transitions(
    data = entries,
    group_cols = group_cols,
    include_non_aoi = TRUE,
    target_aoi_values = target_aoi_values,
    distractor_aoi_values = distractor_aoi_values,
    non_aoi_values = non_aoi_values,
    missing_aoi_label = missing_aoi_label
  ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          group_cols,
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
          "other_transitions",
          "mean_pre_transition_dwell_ms",
          "transition_feature_status"
        )
      )
    )

  feature_summary |>
    dplyr::left_join(
      transition_summary,
      by = group_cols
    )
}
