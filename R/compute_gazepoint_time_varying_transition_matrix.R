#' Compute time-varying Gazepoint transition matrices
#'
#' Compute transition-count and transition-probability matrices across time
#' windows. This helper is a convenience wrapper for studies where AOI/state
#' transitions are expected to vary over the course of a stimulus, trial, or
#' analysis window.
#'
#' @param data A data frame containing transition-level rows.
#' @param from_col Transition origin column. If `NULL`, common origin columns are
#'   detected automatically.
#' @param to_col Transition destination column. If `NULL`, common destination
#'   columns are detected automatically.
#' @param time_col Optional numeric time column used to construct windows when
#'   `window_col = NULL`.
#' @param window_col Optional existing time-window column.
#' @param window_size_ms Numeric window size used when `window_col = NULL`.
#' @param by_cols Optional grouping columns, such as subject, condition, trial, or
#'   stimulus.
#' @param count_col Optional count/weight column. If `NULL`, each row contributes
#'   one transition.
#' @param states Optional character vector of allowed states/AOIs. If `NULL`,
#'   states are detected from `from_col` and `to_col`.
#' @param complete_states If `TRUE`, complete all state-pair combinations within
#'   each time window and group.
#' @param drop_self_transitions If `TRUE`, remove transitions where origin and
#'   destination are the same.
#' @param normalise Probability normalisation. Options are `"row"`, `"global"`,
#'   and `"none"`.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_time_varying_transition_matrix`.
#' @export
compute_gazepoint_time_varying_transition_matrix <- function(
    data,
    from_col = NULL,
    to_col = NULL,
    time_col = NULL,
    window_col = NULL,
    window_size_ms = NULL,
    by_cols = NULL,
    count_col = NULL,
    states = NULL,
    complete_states = TRUE,
    drop_self_transitions = FALSE,
    normalise = c("row", "global", "none"),
    name = "gazepoint_time_varying_transition_matrix"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  normalise <- match.arg(normalise)

  if (!is.logical(complete_states) || length(complete_states) != 1L || is.na(complete_states)) {
    stop("`complete_states` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(drop_self_transitions) || length(drop_self_transitions) != 1L || is.na(drop_self_transitions)) {
    stop("`drop_self_transitions` must be TRUE or FALSE.", call. = FALSE)
  }

  .gp3_tvtm_check_label(name, "name")

  names_data <- names(data)

  from_col <- .gp3_tvtm_resolve_or_detect_col(
    col = from_col,
    names_data = names_data,
    arg = "from_col",
    candidates = c(
      "from_aoi",
      "from_state",
      "from",
      "origin",
      "previous_aoi",
      "previous_state",
      "AOI_from"
    ),
    required = TRUE
  )

  to_col <- .gp3_tvtm_resolve_or_detect_col(
    col = to_col,
    names_data = names_data,
    arg = "to_col",
    candidates = c(
      "to_aoi",
      "to_state",
      "to",
      "destination",
      "next_aoi",
      "next_state",
      "AOI_to"
    ),
    required = TRUE
  )

  if (!is.null(window_col)) {
    window_col <- .gp3_tvtm_resolve_col(
      window_col,
      names_data,
      "window_col"
    )
  }

  if (!is.null(time_col)) {
    time_col <- .gp3_tvtm_resolve_col(
      time_col,
      names_data,
      "time_col"
    )
  }

  if (is.null(window_col)) {
    time_col <- .gp3_tvtm_resolve_or_detect_col(
      col = time_col,
      names_data = names_data,
      arg = "time_col",
      candidates = c(
        "time",
        "time_ms",
        "timestamp",
        "TIMESTAMP",
        "TIME",
        "sample_time",
        "transition_time",
        "transition_start_time"
      ),
      required = TRUE
    )

    .gp3_tvtm_check_positive_number(window_size_ms, "window_size_ms")
  }

  if (!is.null(by_cols)) {
    by_cols <- .gp3_tvtm_resolve_cols_allow_empty(
      by_cols,
      names_data,
      "by_cols"
    )
  } else {
    by_cols <- character(0)
  }

  if (!is.null(count_col)) {
    count_col <- .gp3_tvtm_resolve_col(
      count_col,
      names_data,
      "count_col"
    )
  }

  tmp <- tibble::as_tibble(data)

  tmp$.gp3_from <- as.character(tmp[[from_col]])
  tmp$.gp3_to <- as.character(tmp[[to_col]])

  tmp$.gp3_from[is.na(tmp$.gp3_from) | !nzchar(tmp$.gp3_from)] <- NA_character_
  tmp$.gp3_to[is.na(tmp$.gp3_to) | !nzchar(tmp$.gp3_to)] <- NA_character_

  tmp <- tmp[!is.na(tmp$.gp3_from) & !is.na(tmp$.gp3_to), , drop = FALSE]

  if (nrow(tmp) == 0L) {
    stop("No valid non-missing transitions were found.", call. = FALSE)
  }

  if (is.null(states)) {
    states <- sort(unique(c(tmp$.gp3_from, tmp$.gp3_to)))
  } else {
    if (!is.character(states) || length(states) == 0L || anyNA(states) || any(!nzchar(states))) {
      stop("`states` must be a non-empty character vector.", call. = FALSE)
    }

    states <- unique(states)
  }

  tmp <- tmp[tmp$.gp3_from %in% states & tmp$.gp3_to %in% states, , drop = FALSE]

  if (isTRUE(drop_self_transitions)) {
    tmp <- tmp[tmp$.gp3_from != tmp$.gp3_to, , drop = FALSE]
  }

  if (nrow(tmp) == 0L) {
    stop("No transitions remain after applying `states` and `drop_self_transitions`.", call. = FALSE)
  }

  if (!is.null(count_col)) {
    tmp$.gp3_transition_count <- suppressWarnings(as.numeric(tmp[[count_col]]))

    if (
      anyNA(tmp$.gp3_transition_count) ||
      any(!is.finite(tmp$.gp3_transition_count)) ||
      any(tmp$.gp3_transition_count < 0)
    ) {
      stop("`count_col` must contain finite non-negative values.", call. = FALSE)
    }
  } else {
    tmp$.gp3_transition_count <- 1
  }

  if (!is.null(window_col)) {
    tmp$.gp3_time_window <- as.character(tmp[[window_col]])
    tmp$.gp3_time_window[is.na(tmp$.gp3_time_window) | !nzchar(tmp$.gp3_time_window)] <- "missing_window"
    tmp$.gp3_time_window_start <- NA_real_
    tmp$.gp3_time_window_end <- NA_real_
  } else {
    time_values <- suppressWarnings(as.numeric(tmp[[time_col]]))

    if (anyNA(time_values) || any(!is.finite(time_values))) {
      stop("`time_col` must contain finite numeric values when constructing time windows.", call. = FALSE)
    }

    min_time <- min(time_values, na.rm = TRUE)
    window_index <- floor((time_values - min_time) / window_size_ms)

    tmp$.gp3_time_window_start <- min_time + window_index * window_size_ms
    tmp$.gp3_time_window_end <- tmp$.gp3_time_window_start + window_size_ms
    tmp$.gp3_time_window <- paste0(
      tmp$.gp3_time_window_start,
      "-",
      tmp$.gp3_time_window_end
    )
  }

  group_cols <- c(
    by_cols,
    ".gp3_time_window",
    ".gp3_time_window_start",
    ".gp3_time_window_end",
    ".gp3_from",
    ".gp3_to"
  )

  transition_counts <- tmp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      transition_count = sum(.data$.gp3_transition_count, na.rm = TRUE),
      n_transition_rows = dplyr::n(),
      .groups = "drop"
    )

  if (isTRUE(complete_states)) {
    transition_counts <- .gp3_tvtm_complete_state_pairs(
      transition_counts = transition_counts,
      tmp = tmp,
      by_cols = by_cols,
      states = states,
      drop_self_transitions = drop_self_transitions
    )
  }

  probability_group_cols <- c(
    by_cols,
    ".gp3_time_window",
    ".gp3_time_window_start",
    ".gp3_time_window_end"
  )

  if (identical(normalise, "row")) {
    matrix_long <- transition_counts |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(probability_group_cols, ".gp3_from")))
      ) |>
      dplyr::mutate(
        transition_denominator = sum(.data$transition_count, na.rm = TRUE),
        transition_probability = dplyr::if_else(
          .data$transition_denominator > 0,
          .data$transition_count / .data$transition_denominator,
          NA_real_
        )
      ) |>
      dplyr::ungroup()
  } else if (identical(normalise, "global")) {
    matrix_long <- transition_counts |>
      dplyr::group_by(dplyr::across(dplyr::all_of(probability_group_cols))) |>
      dplyr::mutate(
        transition_denominator = sum(.data$transition_count, na.rm = TRUE),
        transition_probability = dplyr::if_else(
          .data$transition_denominator > 0,
          .data$transition_count / .data$transition_denominator,
          NA_real_
        )
      ) |>
      dplyr::ungroup()
  } else {
    matrix_long <- transition_counts |>
      dplyr::mutate(
        transition_denominator = NA_real_,
        transition_probability = NA_real_
      )
  }

  count_wide <- matrix_long |>
    dplyr::select(
      dplyr::all_of(c(probability_group_cols, ".gp3_from", ".gp3_to")),
      "transition_count"
    ) |>
    tidyr::pivot_wider(
      names_from = ".gp3_to",
      values_from = "transition_count",
      values_fill = 0
    )

  probability_wide <- matrix_long |>
    dplyr::select(
      dplyr::all_of(c(probability_group_cols, ".gp3_from", ".gp3_to")),
      "transition_probability"
    ) |>
    tidyr::pivot_wider(
      names_from = ".gp3_to",
      values_from = "transition_probability"
    )

  time_windows <- matrix_long |>
    dplyr::distinct(
      dplyr::across(
        dplyr::all_of(c(
          by_cols,
          ".gp3_time_window",
          ".gp3_time_window_start",
          ".gp3_time_window_end"
        ))
      )
    ) |>
    dplyr::arrange(
      .data$.gp3_time_window_start,
      .data$.gp3_time_window
    )

  overview <- tibble::tibble(
    object_name = name,
    n_input_rows = nrow(data),
    n_rows_used = nrow(tmp),
    n_states = length(states),
    n_time_windows = dplyr::n_distinct(matrix_long$.gp3_time_window),
    n_by_groups = .gp3_tvtm_n_by_groups(tmp, by_cols),
    n_matrix_rows = nrow(matrix_long),
    total_transition_count = sum(matrix_long$transition_count, na.rm = TRUE),
    normalise = normalise,
    complete_states = complete_states,
    drop_self_transitions = drop_self_transitions
  )

  settings <- tibble::tibble(
    setting = c(
      "from_col",
      "to_col",
      "time_col",
      "window_col",
      "window_size_ms",
      "by_cols",
      "count_col",
      "states",
      "complete_states",
      "drop_self_transitions",
      "normalise",
      "name"
    ),
    value = c(
      from_col,
      to_col,
      .gp3_tvtm_collapse_nullable(time_col),
      .gp3_tvtm_collapse_nullable(window_col),
      .gp3_tvtm_collapse_nullable(window_size_ms),
      .gp3_tvtm_collapse_nullable(by_cols),
      .gp3_tvtm_collapse_nullable(count_col),
      .gp3_tvtm_collapse_nullable(states),
      as.character(complete_states),
      as.character(drop_self_transitions),
      normalise,
      name
    )
  )

  out <- list(
    overview = overview,
    time_windows = time_windows,
    matrix_long = matrix_long,
    count_wide = count_wide,
    probability_wide = probability_wide,
    settings = settings
  )

  class(out) <- c("gp3_time_varying_transition_matrix", "list")

  out
}

.gp3_tvtm_complete_state_pairs <- function(
    transition_counts,
    tmp,
    by_cols,
    states,
    drop_self_transitions
) {
  window_cols <- c(
    by_cols,
    ".gp3_time_window",
    ".gp3_time_window_start",
    ".gp3_time_window_end"
  )

  window_grid <- tmp |>
    dplyr::distinct(dplyr::across(dplyr::all_of(window_cols)))

  state_pairs <- expand.grid(
    .gp3_from = states,
    .gp3_to = states,
    stringsAsFactors = FALSE
  )

  if (isTRUE(drop_self_transitions)) {
    state_pairs <- state_pairs[state_pairs$.gp3_from != state_pairs$.gp3_to, , drop = FALSE]
  }

  complete_grid <- merge(
    window_grid,
    state_pairs,
    by = NULL,
    sort = FALSE
  )

  out <- complete_grid |>
    dplyr::left_join(
      transition_counts,
      by = c(window_cols, ".gp3_from", ".gp3_to")
    ) |>
    dplyr::mutate(
      transition_count = dplyr::coalesce(.data$transition_count, 0),
      n_transition_rows = dplyr::coalesce(.data$n_transition_rows, 0L)
    )

  tibble::as_tibble(out)
}

.gp3_tvtm_n_by_groups <- function(tmp, by_cols) {
  if (length(by_cols) == 0L) {
    return(1L)
  }

  tmp |>
    dplyr::distinct(dplyr::across(dplyr::all_of(by_cols))) |>
    nrow()
}

.gp3_tvtm_resolve_cols_allow_empty <- function(cols, names_data, arg) {
  if (!is.character(cols) || anyNA(cols)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }

  if (length(cols) == 0L) {
    return(character(0))
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_tvtm_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_tvtm_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_tvtm_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_tvtm_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_tvtm_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a finite positive number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_tvtm_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
