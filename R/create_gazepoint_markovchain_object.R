#' Create a Gazepoint AOI Markov-chain object
#'
#' Create a dependency-free Markov-chain-style object from AOI/state sequences.
#' The function computes transition counts, transition probabilities, and matrix
#' representations from ordered gaze/AOI states. It does not require the external
#' `markovchain` package; instead, it returns a lightweight `gp3tools` object that
#' can be inspected, exported, or converted later.
#'
#' @param data A data frame containing ordered AOI/state observations.
#' @param state_col AOI/state column. If `NULL`, common AOI/state column names are
#'   detected automatically.
#' @param participant_col Optional participant/subject column.
#' @param trial_col Optional trial/sequence column.
#' @param time_col Optional time/order column.
#' @param sequence_id_cols Optional character vector of columns defining separate
#'   sequences. If `NULL`, participant and trial columns are used when available.
#' @param state_order Optional character vector giving the preferred state order
#'   in the output matrices.
#' @param exclude_states Character vector of states to exclude before transition
#'   calculation.
#' @param missing_state_label Optional label used to retain missing states. If
#'   `NULL`, missing/blank states are removed.
#' @param include_self_transitions Logical. If `FALSE`, transitions from a state
#'   to the same state are removed.
#' @param laplace Numeric smoothing value added to all transition cells when
#'   computing probabilities.
#' @param empty_state_handling How to handle states with no outgoing transitions:
#'   `"self"` creates an absorbing self-transition, `"zero"` leaves a zero row,
#'   and `"NA"` returns `NA` probabilities for that row.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_markovchain_object`.
#' @export
create_gazepoint_markovchain_object <- function(
    data,
    state_col = NULL,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    sequence_id_cols = NULL,
    state_order = NULL,
    exclude_states = c(
      "missing",
      "missing_aoi",
      "missing_coordinate",
      "trackloss",
      "track_loss"
    ),
    missing_state_label = NULL,
    include_self_transitions = TRUE,
    laplace = 0,
    empty_state_handling = c("self", "zero", "NA"),
    name = "gazepoint_markovchain"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  empty_state_handling <- match.arg(empty_state_handling)

  .gp3_markovchain_check_logical_scalar(
    include_self_transitions,
    "include_self_transitions"
  )
  .gp3_markovchain_check_nonnegative_number(laplace, "laplace")
  .gp3_markovchain_check_label(name, "name")

  if (!is.null(missing_state_label)) {
    .gp3_markovchain_check_label(missing_state_label, "missing_state_label")
  }

  if (!is.null(state_order)) {
    .gp3_markovchain_check_character_vector(state_order, "state_order")
    state_order <- unique(as.character(state_order))
  }

  if (!is.null(exclude_states)) {
    .gp3_markovchain_check_character_vector(exclude_states, "exclude_states")
    exclude_states <- unique(as.character(exclude_states))
  } else {
    exclude_states <- character(0)
  }

  names_data <- names(data)

  state_col <- .gp3_markovchain_resolve_or_detect_col(
    col = state_col,
    names_data = names_data,
    arg = "state_col",
    candidates = c(
      "aoi",
      "aoi_current",
      "AOI",
      "aoi_label",
      "state",
      "aoi_state",
      "observed_aoi",
      "derived_aoi",
      "AOI_LABEL"
    ),
    required = TRUE
  )

  participant_col <- .gp3_markovchain_resolve_or_detect_col(
    col = participant_col,
    names_data = names_data,
    arg = "participant_col",
    candidates = c(
      "participant",
      "subject",
      "participant_id",
      "USER_FILE",
      "user",
      "user_id",
      "recording_id"
    ),
    required = FALSE
  )

  trial_col <- .gp3_markovchain_resolve_or_detect_col(
    col = trial_col,
    names_data = names_data,
    arg = "trial_col",
    candidates = c(
      "trial_global",
      "trial",
      "trial_id",
      "TRIAL_INDEX",
      "trial_number",
      "item_trial"
    ),
    required = FALSE
  )

  time_col <- .gp3_markovchain_resolve_or_detect_col(
    col = time_col,
    names_data = names_data,
    arg = "time_col",
    candidates = c(
      "time",
      "time_ms",
      "timestamp",
      "TIMESTAMP",
      "TIME",
      "TIME_TICK",
      "sample_index",
      "CNT"
    ),
    required = FALSE
  )

  if (!is.null(sequence_id_cols)) {
    sequence_id_cols <- .gp3_markovchain_resolve_cols(
      sequence_id_cols,
      names_data,
      "sequence_id_cols"
    )
  } else {
    sequence_id_cols <- unique(c(participant_col, trial_col))
    sequence_id_cols <- sequence_id_cols[!is.na(sequence_id_cols)]
  }

  raw_state <- as.character(data[[state_col]])
  n_missing_state_before <- sum(is.na(raw_state) | !nzchar(trimws(raw_state)))

  if (is.null(missing_state_label)) {
    keep_state <- !is.na(raw_state) & nzchar(trimws(raw_state))
    state <- raw_state[keep_state]
    row_index <- which(keep_state)
  } else {
    state <- raw_state
    state[is.na(state) | !nzchar(trimws(state))] <- missing_state_label
    row_index <- seq_along(state)
  }

  if (length(state) == 0L) {
    stop("No non-missing states are available after state cleaning.", call. = FALSE)
  }

  state <- as.character(state)

  exclude_lower <- tolower(exclude_states)
  excluded_flag <- tolower(state) %in% exclude_lower
  n_excluded_states <- sum(excluded_flag, na.rm = TRUE)

  if (any(excluded_flag)) {
    state <- state[!excluded_flag]
    row_index <- row_index[!excluded_flag]
  }

  if (length(state) == 0L) {
    stop("No states remain after applying `exclude_states`.", call. = FALSE)
  }

  sequence_key <- .gp3_markovchain_create_sequence_key(
    data = data,
    row_index = row_index,
    sequence_id_cols = sequence_id_cols
  )

  order_key <- .gp3_markovchain_create_order_key(
    data = data,
    row_index = row_index,
    sequence_key = sequence_key,
    time_col = time_col
  )

  sequence_data <- tibble::tibble(
    .row_index = row_index,
    .sequence_key = sequence_key,
    .order_key = order_key,
    state = state
  )

  sequence_data <- sequence_data[order(
    sequence_data$.sequence_key,
    sequence_data$.order_key,
    sequence_data$.row_index
  ), , drop = FALSE]

  transitions <- .gp3_markovchain_create_transitions(
    sequence_data = sequence_data,
    include_self_transitions = include_self_transitions
  )

  if (nrow(transitions) == 0L) {
    stop(
      "No transitions could be created. Check sequence length, state filtering, ",
      "and `include_self_transitions`.",
      call. = FALSE
    )
  }

  detected_states <- sort(unique(c(transitions$from_state, transitions$to_state)))

  if (!is.null(state_order)) {
    states <- unique(c(state_order, detected_states))
  } else {
    states <- detected_states
  }

  count_matrix <- .gp3_markovchain_count_matrix(
    transitions = transitions,
    states = states
  )

  probability_matrix <- .gp3_markovchain_probability_matrix(
    count_matrix = count_matrix,
    laplace = laplace,
    empty_state_handling = empty_state_handling
  )

  transition_counts <- .gp3_markovchain_matrix_to_tibble(
    matrix = count_matrix,
    value_name = "count"
  )

  transition_probabilities <- .gp3_markovchain_matrix_to_tibble(
    matrix = probability_matrix,
    value_name = "probability"
  )

  transition_summary <- merge(
    transition_counts,
    transition_probabilities,
    by = c("from_state", "to_state"),
    all = TRUE,
    sort = FALSE
  )

  transition_summary <- tibble::as_tibble(transition_summary)

  overview <- tibble::tibble(
    object_name = name,
    n_input_rows = nrow(data),
    n_rows_used = length(state),
    n_sequences = length(unique(sequence_data$.sequence_key)),
    n_states = length(states),
    n_transitions = nrow(transitions),
    n_self_transitions = sum(transitions$from_state == transitions$to_state),
    n_missing_states_removed = if (is.null(missing_state_label)) {
      n_missing_state_before
    } else {
      0L
    },
    n_missing_states_labelled = if (is.null(missing_state_label)) {
      0L
    } else {
      n_missing_state_before
    },
    n_excluded_states_removed = n_excluded_states,
    include_self_transitions = include_self_transitions,
    laplace = laplace,
    empty_state_handling = empty_state_handling
  )

  settings <- tibble::tibble(
    setting = c(
      "state_col",
      "participant_col",
      "trial_col",
      "time_col",
      "sequence_id_cols",
      "state_order",
      "exclude_states",
      "missing_state_label",
      "include_self_transitions",
      "laplace",
      "empty_state_handling",
      "name"
    ),
    value = c(
      state_col,
      .gp3_markovchain_collapse_nullable(participant_col),
      .gp3_markovchain_collapse_nullable(trial_col),
      .gp3_markovchain_collapse_nullable(time_col),
      .gp3_markovchain_collapse_nullable(sequence_id_cols),
      .gp3_markovchain_collapse_nullable(state_order),
      .gp3_markovchain_collapse_nullable(exclude_states),
      .gp3_markovchain_collapse_nullable(missing_state_label),
      as.character(include_self_transitions),
      as.character(laplace),
      empty_state_handling,
      name
    )
  )

  out <- list(
    overview = overview,
    states = states,
    sequence_data = sequence_data,
    transitions = transitions,
    transition_summary = transition_summary,
    transition_counts = transition_counts,
    transition_probabilities = transition_probabilities,
    transition_count_matrix = count_matrix,
    transition_probability_matrix = probability_matrix,
    settings = settings
  )

  class(out) <- c("gp3_markovchain_object", "list")

  out
}

.gp3_markovchain_create_sequence_key <- function(data, row_index, sequence_id_cols) {
  if (length(sequence_id_cols) == 0L) {
    return(rep("sequence_1", length(row_index)))
  }

  key_data <- data[row_index, sequence_id_cols, drop = FALSE]

  key_data[] <- lapply(key_data, function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr) | !nzchar(x_chr)] <- "missing"
    x_chr
  })

  do.call(paste, c(key_data, sep = "||"))
}

.gp3_markovchain_create_order_key <- function(data, row_index, sequence_key, time_col) {
  if (!is.null(time_col)) {
    time <- suppressWarnings(as.numeric(data[[time_col]][row_index]))

    if (!all(is.na(time))) {
      return(time)
    }
  }

  stats::ave(seq_along(row_index), sequence_key, FUN = seq_along)
}

.gp3_markovchain_create_transitions <- function(
    sequence_data,
    include_self_transitions
) {
  split_data <- split(sequence_data, sequence_data$.sequence_key)

  transition_list <- lapply(split_data, function(x) {
    if (nrow(x) < 2L) {
      return(NULL)
    }

    from <- utils::head(x$state, -1)
    to <- utils::tail(x$state, -1)

    out <- tibble::tibble(
      sequence_key = x$.sequence_key[-nrow(x)],
      from_state = from,
      to_state = to,
      from_row = x$.row_index[-nrow(x)],
      to_row = x$.row_index[-1]
    )

    if (!isTRUE(include_self_transitions)) {
      out <- out[out$from_state != out$to_state, , drop = FALSE]
    }

    out
  })

  transition_list <- transition_list[!vapply(transition_list, is.null, logical(1))]

  if (length(transition_list) == 0L) {
    return(
      tibble::tibble(
        sequence_key = character(),
        from_state = character(),
        to_state = character(),
        from_row = integer(),
        to_row = integer()
      )
    )
  }

  dplyr::bind_rows(transition_list)
}

.gp3_markovchain_count_matrix <- function(transitions, states) {
  tab <- table(
    factor(transitions$from_state, levels = states),
    factor(transitions$to_state, levels = states)
  )

  out <- as.matrix(tab)
  storage.mode(out) <- "numeric"

  rownames(out) <- states
  colnames(out) <- states
  names(dimnames(out)) <- NULL

  out
}

.gp3_markovchain_probability_matrix <- function(
    count_matrix,
    laplace,
    empty_state_handling
) {
  smoothed <- count_matrix + laplace
  row_totals <- rowSums(smoothed)

  out <- smoothed
  out[,] <- NA_real_

  non_empty <- row_totals > 0

  if (any(non_empty)) {
    out[non_empty, ] <- smoothed[non_empty, , drop = FALSE] / row_totals[non_empty]
  }

  empty_rows <- which(!non_empty)

  if (length(empty_rows) > 0L) {
    if (identical(empty_state_handling, "self")) {
      out[empty_rows, ] <- 0

      for (i in empty_rows) {
        out[i, i] <- 1
      }
    }

    if (identical(empty_state_handling, "zero")) {
      out[empty_rows, ] <- 0
    }

    if (identical(empty_state_handling, "NA")) {
      out[empty_rows, ] <- NA_real_
    }
  }

  out
}

.gp3_markovchain_matrix_to_tibble <- function(matrix, value_name) {
  expanded <- expand.grid(
    from_state = rownames(matrix),
    to_state = colnames(matrix),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  expanded[[value_name]] <- as.vector(matrix)

  tibble::as_tibble(expanded)
}

.gp3_markovchain_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_markovchain_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_markovchain_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_markovchain_resolve_col(col, names_data, arg))
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

.gp3_markovchain_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_markovchain_check_character_vector <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_markovchain_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_markovchain_check_nonnegative_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`", arg, "` must be a finite non-negative number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_markovchain_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
