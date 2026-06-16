#' Prepare Gazepoint AOI sequences for semi-Markov modelling
#'
#' Convert ordered AOI/state observations into state-visit and transition-level
#' semi-Markov data. Consecutive repeated states can be collapsed into dwell
#' episodes, producing one row per state visit with dwell duration and next-state
#' information.
#'
#' @param data A data frame containing ordered AOI/state observations.
#' @param state_col AOI/state column. If `NULL`, common AOI/state columns are
#'   detected automatically.
#' @param participant_col Optional participant/subject column.
#' @param trial_col Optional trial/sequence column.
#' @param time_col Optional time/order column.
#' @param duration_col Optional sample-duration column. If supplied, dwell
#'   durations are computed by summing this column within each state visit.
#' @param sequence_id_cols Optional character vector of columns defining separate
#'   sequences. If `NULL`, participant and trial columns are used when available.
#' @param covariate_cols Optional character vector of covariate columns to carry
#'   into the state-visit and transition tables using the first value within each
#'   state visit.
#' @param exclude_states Character vector of states to exclude before creating
#'   state visits.
#' @param missing_state_label Optional label used to retain missing states. If
#'   `NULL`, missing/blank states are removed.
#' @param collapse_repeated_states Logical. If `TRUE`, consecutive repeated
#'   states within a sequence are collapsed into a single dwell episode.
#' @param include_terminal_states Logical. If `TRUE`, the final state visit in
#'   each sequence is retained as a transition to `terminal_next_state_label`.
#' @param terminal_next_state_label Label used for the terminal next state.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_semimarkov_data`.
#' @export
prepare_gazepoint_semimarkov_data <- function(
    data,
    state_col = NULL,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    duration_col = NULL,
    sequence_id_cols = NULL,
    covariate_cols = NULL,
    exclude_states = c(
      "missing",
      "missing_aoi",
      "missing_coordinate",
      "trackloss",
      "track_loss"
    ),
    missing_state_label = NULL,
    collapse_repeated_states = TRUE,
    include_terminal_states = TRUE,
    terminal_next_state_label = "END",
    name = "gazepoint_semimarkov_data"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_semimarkov_check_logical_scalar(
    collapse_repeated_states,
    "collapse_repeated_states"
  )
  .gp3_semimarkov_check_logical_scalar(
    include_terminal_states,
    "include_terminal_states"
  )
  .gp3_semimarkov_check_label(
    terminal_next_state_label,
    "terminal_next_state_label"
  )
  .gp3_semimarkov_check_label(name, "name")

  if (!is.null(missing_state_label)) {
    .gp3_semimarkov_check_label(missing_state_label, "missing_state_label")
  }

  if (!is.null(exclude_states)) {
    .gp3_semimarkov_check_character_vector(exclude_states, "exclude_states")
    exclude_states <- unique(as.character(exclude_states))
  } else {
    exclude_states <- character(0)
  }

  names_data <- names(data)

  state_col <- .gp3_semimarkov_resolve_or_detect_col(
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

  participant_col <- .gp3_semimarkov_resolve_or_detect_col(
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

  trial_col <- .gp3_semimarkov_resolve_or_detect_col(
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

  time_col <- .gp3_semimarkov_resolve_or_detect_col(
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

  duration_col <- .gp3_semimarkov_resolve_or_detect_col(
    col = duration_col,
    names_data = names_data,
    arg = "duration_col",
    candidates = c(
      "sample_duration",
      "sample_duration_ms",
      "duration",
      "duration_ms",
      "dwell_sample_duration"
    ),
    required = FALSE
  )

  if (!is.null(sequence_id_cols)) {
    sequence_id_cols <- .gp3_semimarkov_resolve_cols(
      sequence_id_cols,
      names_data,
      "sequence_id_cols"
    )
  } else {
    sequence_id_cols <- unique(c(participant_col, trial_col))
    sequence_id_cols <- sequence_id_cols[!is.na(sequence_id_cols)]
  }

  if (!is.null(covariate_cols)) {
    covariate_cols <- .gp3_semimarkov_resolve_cols(
      covariate_cols,
      names_data,
      "covariate_cols"
    )
  } else {
    covariate_cols <- character(0)
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

  excluded_flag <- tolower(state) %in% tolower(exclude_states)
  n_excluded_states <- sum(excluded_flag, na.rm = TRUE)

  if (any(excluded_flag)) {
    state <- state[!excluded_flag]
    row_index <- row_index[!excluded_flag]
  }

  if (length(state) == 0L) {
    stop("No states remain after applying `exclude_states`.", call. = FALSE)
  }

  sequence_key <- .gp3_semimarkov_create_sequence_key(
    data = data,
    row_index = row_index,
    sequence_id_cols = sequence_id_cols
  )

  order_key <- .gp3_semimarkov_create_order_key(
    data = data,
    row_index = row_index,
    sequence_key = sequence_key,
    time_col = time_col
  )

  time_value <- .gp3_semimarkov_optional_numeric_by_index(
    data = data,
    row_index = row_index,
    col = time_col
  )

  duration_value <- .gp3_semimarkov_optional_numeric_by_index(
    data = data,
    row_index = row_index,
    col = duration_col
  )

  state_sequence <- tibble::tibble(
    .row_index = row_index,
    .sequence_key = sequence_key,
    .order_key = order_key,
    state = state,
    time = time_value,
    sample_duration = duration_value
  )

  if (length(sequence_id_cols) > 0L) {
    state_sequence <- dplyr::bind_cols(
      state_sequence,
      tibble::as_tibble(data[row_index, sequence_id_cols, drop = FALSE])
    )
  }

  if (length(covariate_cols) > 0L) {
    add_covariates <- setdiff(covariate_cols, names(state_sequence))

    if (length(add_covariates) > 0L) {
      state_sequence <- dplyr::bind_cols(
        state_sequence,
        tibble::as_tibble(data[row_index, add_covariates, drop = FALSE])
      )
    }
  }

  state_sequence <- state_sequence[order(
    state_sequence$.sequence_key,
    state_sequence$.order_key,
    state_sequence$.row_index
  ), , drop = FALSE]

  dwell_data <- .gp3_semimarkov_create_dwell_data(
    state_sequence = state_sequence,
    sequence_id_cols = sequence_id_cols,
    covariate_cols = covariate_cols,
    time_col_available = !is.null(time_col),
    duration_col_available = !is.null(duration_col),
    collapse_repeated_states = collapse_repeated_states,
    include_terminal_states = include_terminal_states,
    terminal_next_state_label = terminal_next_state_label
  )

  if (nrow(dwell_data) == 0L) {
    stop("No state visits could be created.", call. = FALSE)
  }

  transition_data <- dwell_data
  transition_data <- transition_data[!is.na(transition_data$next_state), , drop = FALSE]

  if (nrow(transition_data) == 0L) {
    stop("No semi-Markov transitions could be created.", call. = FALSE)
  }

  transition_data <- transition_data |>
    dplyr::transmute(
      .sequence_key = .data$.sequence_key,
      visit_index = .data$visit_index,
      from_state = .data$state,
      to_state = .data$next_state,
      dwell_duration = .data$dwell_duration,
      n_samples = .data$n_samples,
      start_time = .data$start_time,
      end_time = .data$end_time,
      is_terminal = .data$is_terminal,
      dplyr::across(dplyr::any_of(c(sequence_id_cols, covariate_cols)))
    )

  state_summary <- dwell_data |>
    dplyr::group_by(.data$state) |>
    dplyr::summarise(
      n_visits = dplyr::n(),
      n_sequences = dplyr::n_distinct(.data$.sequence_key),
      mean_dwell_duration = mean(.data$dwell_duration, na.rm = TRUE),
      median_dwell_duration = stats::median(.data$dwell_duration, na.rm = TRUE),
      max_dwell_duration = max(.data$dwell_duration, na.rm = TRUE),
      total_dwell_duration = sum(.data$dwell_duration, na.rm = TRUE),
      .groups = "drop"
    )

  transition_summary <- transition_data |>
    dplyr::group_by(.data$from_state, .data$to_state) |>
    dplyr::summarise(
      n_transitions = dplyr::n(),
      n_sequences = dplyr::n_distinct(.data$.sequence_key),
      mean_dwell_duration = mean(.data$dwell_duration, na.rm = TRUE),
      median_dwell_duration = stats::median(.data$dwell_duration, na.rm = TRUE),
      max_dwell_duration = max(.data$dwell_duration, na.rm = TRUE),
      total_dwell_duration = sum(.data$dwell_duration, na.rm = TRUE),
      .groups = "drop"
    )

  overview <- tibble::tibble(
    object_name = name,
    n_input_rows = nrow(data),
    n_rows_used = nrow(state_sequence),
    n_sequences = length(unique(state_sequence$.sequence_key)),
    n_states = length(unique(dwell_data$state)),
    n_state_visits = nrow(dwell_data),
    n_transitions = nrow(transition_data),
    n_terminal_transitions = sum(transition_data$is_terminal, na.rm = TRUE),
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
    collapse_repeated_states = collapse_repeated_states,
    include_terminal_states = include_terminal_states,
    duration_source = .gp3_semimarkov_duration_source(
      time_col_available = !is.null(time_col),
      duration_col_available = !is.null(duration_col)
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "state_col",
      "participant_col",
      "trial_col",
      "time_col",
      "duration_col",
      "sequence_id_cols",
      "covariate_cols",
      "exclude_states",
      "missing_state_label",
      "collapse_repeated_states",
      "include_terminal_states",
      "terminal_next_state_label",
      "name"
    ),
    value = c(
      state_col,
      .gp3_semimarkov_collapse_nullable(participant_col),
      .gp3_semimarkov_collapse_nullable(trial_col),
      .gp3_semimarkov_collapse_nullable(time_col),
      .gp3_semimarkov_collapse_nullable(duration_col),
      .gp3_semimarkov_collapse_nullable(sequence_id_cols),
      .gp3_semimarkov_collapse_nullable(covariate_cols),
      .gp3_semimarkov_collapse_nullable(exclude_states),
      .gp3_semimarkov_collapse_nullable(missing_state_label),
      as.character(collapse_repeated_states),
      as.character(include_terminal_states),
      terminal_next_state_label,
      name
    )
  )

  out <- list(
    overview = overview,
    state_sequence = state_sequence,
    dwell_data = dwell_data,
    transition_data = transition_data,
    state_summary = state_summary,
    transition_summary = transition_summary,
    settings = settings
  )

  class(out) <- c("gp3_semimarkov_data", "list")

  out
}

.gp3_semimarkov_create_dwell_data <- function(
    state_sequence,
    sequence_id_cols,
    covariate_cols,
    time_col_available,
    duration_col_available,
    collapse_repeated_states,
    include_terminal_states,
    terminal_next_state_label
) {
  split_data <- split(state_sequence, state_sequence$.sequence_key)

  dwell_list <- lapply(split_data, function(x) {
    x <- x[order(x$.order_key, x$.row_index), , drop = FALSE]

    if (nrow(x) == 0L) {
      return(NULL)
    }

    if (isTRUE(collapse_repeated_states)) {
      run_start <- c(TRUE, x$state[-1] != x$state[-nrow(x)])
      run_id <- cumsum(run_start)
    } else {
      run_id <- seq_len(nrow(x))
    }

    x$.run_id <- run_id

    sequence_step <- .gp3_semimarkov_estimate_sequence_step(
      time = x$time,
      time_col_available = time_col_available
    )

    run_list <- split(x, x$.run_id)

    out <- lapply(seq_along(run_list), function(i) {
      run <- run_list[[i]]

      dwell_duration <- .gp3_semimarkov_compute_dwell_duration(
        run = run,
        sequence_step = sequence_step,
        time_col_available = time_col_available,
        duration_col_available = duration_col_available
      )

      base <- tibble::tibble(
        .sequence_key = run$.sequence_key[[1]],
        visit_index = i,
        state = run$state[[1]],
        start_row = run$.row_index[[1]],
        end_row = run$.row_index[[nrow(run)]],
        start_order = run$.order_key[[1]],
        end_order = run$.order_key[[nrow(run)]],
        start_time = run$time[[1]],
        end_time = run$time[[nrow(run)]],
        n_samples = nrow(run),
        dwell_duration = dwell_duration
      )

      carry_cols <- unique(c(sequence_id_cols, covariate_cols))
      carry_cols <- carry_cols[carry_cols %in% names(run)]

      if (length(carry_cols) > 0L) {
        carry_values <- run[1, carry_cols, drop = FALSE]
        base <- dplyr::bind_cols(base, tibble::as_tibble(carry_values))
      }

      base
    })

    dplyr::bind_rows(out)
  })

  dwell_list <- dwell_list[!vapply(dwell_list, is.null, logical(1))]

  if (length(dwell_list) == 0L) {
    return(tibble::tibble())
  }

  dwell_data <- dplyr::bind_rows(dwell_list)

  dwell_data <- dwell_data |>
    dplyr::group_by(.data$.sequence_key) |>
    dplyr::arrange(.data$visit_index, .by_group = TRUE) |>
    dplyr::mutate(
      next_state = dplyr::lead(.data$state),
      is_terminal = is.na(.data$next_state)
    ) |>
    dplyr::ungroup()

  if (isTRUE(include_terminal_states)) {
    dwell_data$next_state[dwell_data$is_terminal] <- terminal_next_state_label
  } else {
    dwell_data <- dwell_data[!dwell_data$is_terminal, , drop = FALSE]
  }

  dwell_data
}

.gp3_semimarkov_compute_dwell_duration <- function(
    run,
    sequence_step,
    time_col_available,
    duration_col_available
) {
  if (isTRUE(duration_col_available)) {
    duration_values <- run$sample_duration

    if (!all(is.na(duration_values))) {
      duration <- sum(duration_values, na.rm = TRUE)

      if (is.finite(duration)) {
        return(duration)
      }
    }
  }

  if (isTRUE(time_col_available) && !all(is.na(run$time))) {
    start_time <- run$time[[1]]
    end_time <- run$time[[nrow(run)]]

    if (is.finite(start_time) && is.finite(end_time)) {
      step <- sequence_step

      if (!is.finite(step) || is.na(step) || step <= 0) {
        step <- 1
      }

      return((end_time - start_time) + step)
    }
  }

  nrow(run)
}

.gp3_semimarkov_estimate_sequence_step <- function(time, time_col_available) {
  if (!isTRUE(time_col_available)) {
    return(NA_real_)
  }

  time <- as.numeric(time)
  time <- time[is.finite(time)]

  if (length(time) < 2L) {
    return(NA_real_)
  }

  diffs <- diff(sort(unique(time)))
  diffs <- diffs[is.finite(diffs) & diffs > 0]

  if (length(diffs) == 0L) {
    return(NA_real_)
  }

  stats::median(diffs, na.rm = TRUE)
}

.gp3_semimarkov_duration_source <- function(
    time_col_available,
    duration_col_available
) {
  if (isTRUE(duration_col_available)) {
    return("duration_col")
  }

  if (isTRUE(time_col_available)) {
    return("time_col_plus_estimated_step")
  }

  "state_visit_sample_count"
}

.gp3_semimarkov_create_sequence_key <- function(data, row_index, sequence_id_cols) {
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

.gp3_semimarkov_create_order_key <- function(
    data,
    row_index,
    sequence_key,
    time_col
) {
  if (!is.null(time_col)) {
    time <- suppressWarnings(as.numeric(data[[time_col]][row_index]))

    if (!all(is.na(time))) {
      return(time)
    }
  }

  stats::ave(seq_along(row_index), sequence_key, FUN = seq_along)
}

.gp3_semimarkov_optional_numeric_by_index <- function(data, row_index, col) {
  if (is.null(col)) {
    return(rep(NA_real_, length(row_index)))
  }

  suppressWarnings(as.numeric(data[[col]][row_index]))
}

.gp3_semimarkov_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_semimarkov_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_semimarkov_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_semimarkov_resolve_col(col, names_data, arg))
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

.gp3_semimarkov_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_semimarkov_check_character_vector <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_semimarkov_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_semimarkov_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
