#' Prepare Gazepoint AOI/state sequences for HMM-style workflows
#'
#' Convert ordered Gazepoint AOI/state observations into a dependency-free
#' hidden-Markov-model-ready structure. The helper creates ordered sequence
#' data, transition tables, initial-state probabilities, transition-probability
#' matrices, and observation/emission summaries. It does not fit an HMM and does
#' not import external HMM packages.
#'
#' @param data A data frame containing ordered AOI/state observations.
#' @param state_col AOI/state column. If `NULL`, common AOI/state columns are
#'   detected automatically.
#' @param participant_col Optional participant/subject column.
#' @param trial_col Optional trial/sequence column.
#' @param time_col Optional time/order column.
#' @param observation_cols Optional observation columns to carry into the HMM
#'   data. If `NULL`, common gaze, pupil, fixation, and validity columns are
#'   detected automatically.
#' @param sequence_id_cols Optional character vector of columns defining separate
#'   sequences. If `NULL`, participant and trial columns are used when available.
#' @param covariate_cols Optional covariate columns to carry into the HMM data.
#' @param state_order Optional preferred hidden-state order.
#' @param exclude_states Character vector of states to exclude before sequence
#'   construction.
#' @param missing_state_label Optional label used to retain missing states. If
#'   `NULL`, missing/blank states are removed.
#' @param scale_numeric_observations Logical. If `TRUE`, z-scored versions of
#'   numeric observation columns are added with suffix `_z`.
#' @param include_terminal_state Logical. If `TRUE`, each sequence contributes a
#'   final transition to `terminal_state_label`.
#' @param terminal_state_label Terminal-state label.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_hmm_data`.
#' @export
prepare_gazepoint_hmm_data <- function(
    data,
    state_col = NULL,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    observation_cols = NULL,
    sequence_id_cols = NULL,
    covariate_cols = NULL,
    state_order = NULL,
    exclude_states = c(
      "missing",
      "missing_aoi",
      "missing_coordinate",
      "trackloss",
      "track_loss"
    ),
    missing_state_label = NULL,
    scale_numeric_observations = FALSE,
    include_terminal_state = FALSE,
    terminal_state_label = "END",
    name = "gazepoint_hmm_data"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_hmm_check_logical_scalar(
    scale_numeric_observations,
    "scale_numeric_observations"
  )
  .gp3_hmm_check_logical_scalar(
    include_terminal_state,
    "include_terminal_state"
  )
  .gp3_hmm_check_label(terminal_state_label, "terminal_state_label")
  .gp3_hmm_check_label(name, "name")

  if (!is.null(missing_state_label)) {
    .gp3_hmm_check_label(missing_state_label, "missing_state_label")
  }

  if (!is.null(state_order)) {
    .gp3_hmm_check_character_vector(state_order, "state_order")
    state_order <- unique(as.character(state_order))
  }

  if (!is.null(exclude_states)) {
    .gp3_hmm_check_character_vector(exclude_states, "exclude_states")
    exclude_states <- unique(as.character(exclude_states))
  } else {
    exclude_states <- character(0)
  }

  names_data <- names(data)

  state_col <- .gp3_hmm_resolve_or_detect_col(
    col = state_col,
    names_data = names_data,
    arg = "state_col",
    candidates = c(
      "aoi",
      "aoi_current",
      "AOI",
      "aoi_label",
      "state",
      "hidden_state",
      "aoi_state",
      "observed_aoi",
      "derived_aoi",
      "AOI_LABEL"
    ),
    required = TRUE
  )

  participant_col <- .gp3_hmm_resolve_or_detect_col(
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

  trial_col <- .gp3_hmm_resolve_or_detect_col(
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

  time_col <- .gp3_hmm_resolve_or_detect_col(
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
    sequence_id_cols <- .gp3_hmm_resolve_cols(
      sequence_id_cols,
      names_data,
      "sequence_id_cols"
    )
  } else {
    sequence_id_cols <- unique(c(participant_col, trial_col))
    sequence_id_cols <- sequence_id_cols[!is.na(sequence_id_cols)]
  }

  if (!is.null(covariate_cols)) {
    covariate_cols <- .gp3_hmm_resolve_cols(
      covariate_cols,
      names_data,
      "covariate_cols"
    )
  } else {
    covariate_cols <- character(0)
  }

  if (!is.null(observation_cols)) {
    observation_cols <- .gp3_hmm_resolve_cols(
      observation_cols,
      names_data,
      "observation_cols"
    )
  } else {
    observation_cols <- intersect(
      c(
        "x",
        "y",
        "gaze_x",
        "gaze_y",
        "left_x",
        "left_y",
        "right_x",
        "right_y",
        "pupil",
        "mean_pupil",
        "left_pupil",
        "right_pupil",
        "pupil_bc_processed",
        "pupil_smoothed",
        "dwell_duration",
        "fixation_duration",
        "saccade_duration",
        "saccade_amplitude",
        "valid_gaze",
        "valid_sample",
        "trackloss",
        "missing_gaze",
        "missing_pupil"
      ),
      names_data
    )
  }

  observation_cols <- unique(as.character(observation_cols))
  observation_cols <- setdiff(
    observation_cols,
    c(state_col, sequence_id_cols, covariate_cols, time_col)
  )

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

  sequence_key <- .gp3_hmm_create_sequence_key(
    data = data,
    row_index = row_index,
    sequence_id_cols = sequence_id_cols
  )

  order_key <- .gp3_hmm_create_order_key(
    data = data,
    row_index = row_index,
    sequence_key = sequence_key,
    time_col = time_col
  )

  time_value <- .gp3_hmm_optional_numeric_by_index(
    data = data,
    row_index = row_index,
    col = time_col
  )

  sequence_data <- tibble::tibble(
    .row_index = row_index,
    .sequence_key = sequence_key,
    .order_key = order_key,
    state = state,
    time = time_value
  )

  add_sequence_cols <- setdiff(sequence_id_cols, names(sequence_data))

  if (length(add_sequence_cols) > 0L) {
    sequence_data <- dplyr::bind_cols(
      sequence_data,
      tibble::as_tibble(data[row_index, add_sequence_cols, drop = FALSE])
    )
  }

  add_covariate_cols <- setdiff(covariate_cols, names(sequence_data))

  if (length(add_covariate_cols) > 0L) {
    sequence_data <- dplyr::bind_cols(
      sequence_data,
      tibble::as_tibble(data[row_index, add_covariate_cols, drop = FALSE])
    )
  }

  add_observation_cols <- setdiff(observation_cols, names(sequence_data))

  if (length(add_observation_cols) > 0L) {
    sequence_data <- dplyr::bind_cols(
      sequence_data,
      tibble::as_tibble(data[row_index, add_observation_cols, drop = FALSE])
    )
  }

  sequence_data <- sequence_data[order(
    sequence_data$.sequence_key,
    sequence_data$.order_key,
    sequence_data$.row_index
  ), , drop = FALSE]

  sequence_data <- sequence_data |>
    dplyr::group_by(.data$.sequence_key) |>
    dplyr::mutate(
      observation_index = dplyr::row_number()
    ) |>
    dplyr::ungroup()

  scaled_observation_cols <- character(0)

  if (isTRUE(scale_numeric_observations) && length(observation_cols) > 0L) {
    numeric_observation_cols <- observation_cols[
      vapply(sequence_data[observation_cols], is.numeric, logical(1))
    ]

    if (length(numeric_observation_cols) > 0L) {
      for (col in numeric_observation_cols) {
        scaled_col <- paste0(col, "_z")
        sequence_data[[scaled_col]] <- .gp3_hmm_z_score(sequence_data[[col]])
        scaled_observation_cols <- c(scaled_observation_cols, scaled_col)
      }
    }
  }

  observation_data <- sequence_data |>
    dplyr::select(
      dplyr::all_of(c(".sequence_key", "observation_index", "state", "time")),
      dplyr::any_of(c(sequence_id_cols, covariate_cols, observation_cols, scaled_observation_cols))
    )

  initial_state_data <- sequence_data |>
    dplyr::group_by(.data$.sequence_key) |>
    dplyr::slice_min(
      order_by = .data$observation_index,
      n = 1,
      with_ties = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      .sequence_key = .data$.sequence_key,
      initial_state = .data$state,
      initial_time = .data$time,
      dplyr::across(dplyr::any_of(sequence_id_cols))
    )

  transition_data <- .gp3_hmm_create_transition_data(
    sequence_data = sequence_data,
    include_terminal_state = include_terminal_state,
    terminal_state_label = terminal_state_label
  )

  if (nrow(transition_data) == 0L) {
    stop("No HMM transitions could be created.", call. = FALSE)
  }

  carry_cols <- unique(c(sequence_id_cols, covariate_cols))
  carry_cols <- carry_cols[carry_cols %in% names(sequence_data)]

  if (length(carry_cols) > 0L) {
    carry_data <- sequence_data |>
      dplyr::select(dplyr::all_of(".row_index"), dplyr::any_of(carry_cols))

    transition_data <- transition_data |>
      dplyr::left_join(
        carry_data,
        by = c("from_row" = ".row_index")
      )
  }

  detected_states <- sort(unique(sequence_data$state))

  if (!is.null(state_order)) {
    states <- unique(c(state_order, detected_states))
  } else {
    states <- detected_states
  }

  to_states <- unique(c(states, sort(setdiff(unique(transition_data$to_state), states))))

  transition_count_matrix <- .gp3_hmm_count_matrix(
    transitions = transition_data,
    from_states = states,
    to_states = to_states
  )

  transition_probability_matrix <- .gp3_hmm_probability_matrix(
    transition_count_matrix
  )

  transition_counts <- .gp3_hmm_matrix_to_tibble(
    matrix = transition_count_matrix,
    value_name = "count"
  )

  transition_probabilities <- .gp3_hmm_matrix_to_tibble(
    matrix = transition_probability_matrix,
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

  initial_state_probabilities <- .gp3_hmm_initial_probabilities(
    initial_state_data = initial_state_data,
    states = states
  )

  sequence_summary <- sequence_data |>
    dplyr::group_by(.data$.sequence_key) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      first_state = dplyr::first(.data$state),
      last_state = dplyr::last(.data$state),
      start_time = .gp3_hmm_safe_min(.data$time),
      end_time = .gp3_hmm_safe_max(.data$time),
      sequence_duration = .data$end_time - .data$start_time,
      .groups = "drop"
    )

  state_summary <- sequence_data |>
    dplyr::group_by(.data$state) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      n_sequences = dplyr::n_distinct(.data$.sequence_key),
      n_initial = sum(.data$observation_index == 1, na.rm = TRUE),
      mean_time = mean(.data$time, na.rm = TRUE),
      min_time = .gp3_hmm_safe_min(.data$time),
      max_time = .gp3_hmm_safe_max(.data$time),
      .groups = "drop"
    )

  observation_summary <- .gp3_hmm_observation_summary(
    sequence_data = sequence_data,
    observation_cols = observation_cols
  )

  emission_data <- .gp3_hmm_emission_data(
    sequence_data = sequence_data,
    observation_cols = observation_cols
  )

  overview <- tibble::tibble(
    object_name = name,
    n_input_rows = nrow(data),
    n_rows_used = nrow(sequence_data),
    n_sequences = length(unique(sequence_data$.sequence_key)),
    n_states = length(states),
    n_observation_cols = length(observation_cols),
    n_scaled_observation_cols = length(scaled_observation_cols),
    n_observations = nrow(observation_data),
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
    include_terminal_state = include_terminal_state,
    scale_numeric_observations = scale_numeric_observations
  )

  settings <- tibble::tibble(
    setting = c(
      "state_col",
      "participant_col",
      "trial_col",
      "time_col",
      "observation_cols",
      "scaled_observation_cols",
      "sequence_id_cols",
      "covariate_cols",
      "state_order",
      "exclude_states",
      "missing_state_label",
      "scale_numeric_observations",
      "include_terminal_state",
      "terminal_state_label",
      "name"
    ),
    value = c(
      state_col,
      .gp3_hmm_collapse_nullable(participant_col),
      .gp3_hmm_collapse_nullable(trial_col),
      .gp3_hmm_collapse_nullable(time_col),
      .gp3_hmm_collapse_nullable(observation_cols),
      .gp3_hmm_collapse_nullable(scaled_observation_cols),
      .gp3_hmm_collapse_nullable(sequence_id_cols),
      .gp3_hmm_collapse_nullable(covariate_cols),
      .gp3_hmm_collapse_nullable(state_order),
      .gp3_hmm_collapse_nullable(exclude_states),
      .gp3_hmm_collapse_nullable(missing_state_label),
      as.character(scale_numeric_observations),
      as.character(include_terminal_state),
      terminal_state_label,
      name
    )
  )

  out <- list(
    overview = overview,
    states = states,
    observation_cols = observation_cols,
    scaled_observation_cols = scaled_observation_cols,
    sequence_data = sequence_data,
    observation_data = observation_data,
    initial_state_data = initial_state_data,
    initial_state_probabilities = initial_state_probabilities,
    transition_data = transition_data,
    transition_summary = transition_summary,
    transition_counts = transition_counts,
    transition_probabilities = transition_probabilities,
    transition_count_matrix = transition_count_matrix,
    transition_probability_matrix = transition_probability_matrix,
    sequence_summary = sequence_summary,
    state_summary = state_summary,
    observation_summary = observation_summary,
    emission_data = emission_data,
    settings = settings
  )

  class(out) <- c("gp3_hmm_data", "list")

  out
}

.gp3_hmm_create_transition_data <- function(
    sequence_data,
    include_terminal_state,
    terminal_state_label
) {
  split_data <- split(sequence_data, sequence_data$.sequence_key)

  transition_list <- lapply(split_data, function(x) {
    x <- x[order(x$observation_index), , drop = FALSE]

    regular <- tibble::tibble(
      .sequence_key = character(),
      from_state = character(),
      to_state = character(),
      from_observation_index = integer(),
      to_observation_index = integer(),
      from_row = integer(),
      to_row = integer(),
      from_time = numeric(),
      to_time = numeric(),
      is_terminal = logical()
    )

    if (nrow(x) >= 2L) {
      regular <- tibble::tibble(
        .sequence_key = x$.sequence_key[-nrow(x)],
        from_state = x$state[-nrow(x)],
        to_state = x$state[-1],
        from_observation_index = x$observation_index[-nrow(x)],
        to_observation_index = x$observation_index[-1],
        from_row = x$.row_index[-nrow(x)],
        to_row = x$.row_index[-1],
        from_time = x$time[-nrow(x)],
        to_time = x$time[-1],
        is_terminal = FALSE
      )
    }

    if (isTRUE(include_terminal_state)) {
      terminal <- tibble::tibble(
        .sequence_key = x$.sequence_key[[nrow(x)]],
        from_state = x$state[[nrow(x)]],
        to_state = terminal_state_label,
        from_observation_index = x$observation_index[[nrow(x)]],
        to_observation_index = NA_integer_,
        from_row = x$.row_index[[nrow(x)]],
        to_row = NA_integer_,
        from_time = x$time[[nrow(x)]],
        to_time = NA_real_,
        is_terminal = TRUE
      )

      regular <- dplyr::bind_rows(regular, terminal)
    }

    regular
  })

  transition_list <- transition_list[!vapply(transition_list, is.null, logical(1))]

  if (length(transition_list) == 0L) {
    return(
      tibble::tibble(
        .sequence_key = character(),
        from_state = character(),
        to_state = character(),
        from_observation_index = integer(),
        to_observation_index = integer(),
        from_row = integer(),
        to_row = integer(),
        from_time = numeric(),
        to_time = numeric(),
        is_terminal = logical()
      )
    )
  }

  dplyr::bind_rows(transition_list)
}

.gp3_hmm_initial_probabilities <- function(initial_state_data, states) {
  counts <- table(factor(initial_state_data$initial_state, levels = states))
  counts <- as.numeric(counts)

  total <- sum(counts)

  probabilities <- if (total > 0) {
    counts / total
  } else {
    rep(NA_real_, length(states))
  }

  tibble::tibble(
    state = states,
    initial_count = counts,
    initial_probability = probabilities
  )
}

.gp3_hmm_count_matrix <- function(transitions, from_states, to_states) {
  tab <- table(
    factor(transitions$from_state, levels = from_states),
    factor(transitions$to_state, levels = to_states)
  )

  out <- as.matrix(tab)
  storage.mode(out) <- "numeric"

  rownames(out) <- from_states
  colnames(out) <- to_states
  names(dimnames(out)) <- NULL

  out
}

.gp3_hmm_probability_matrix <- function(count_matrix) {
  row_totals <- rowSums(count_matrix)

  out <- count_matrix
  out[,] <- 0

  non_empty <- row_totals > 0

  if (any(non_empty)) {
    out[non_empty, ] <- count_matrix[non_empty, , drop = FALSE] / row_totals[non_empty]
  }

  out
}

.gp3_hmm_matrix_to_tibble <- function(matrix, value_name) {
  expanded <- expand.grid(
    from_state = rownames(matrix),
    to_state = colnames(matrix),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  expanded[[value_name]] <- as.vector(matrix)

  tibble::as_tibble(expanded)
}

.gp3_hmm_observation_summary <- function(sequence_data, observation_cols) {
  if (length(observation_cols) == 0L) {
    return(
      tibble::tibble(
        state = character(),
        observation = character(),
        observation_type = character(),
        n = integer(),
        n_missing = integer(),
        missing_prop = numeric(),
        mean = numeric(),
        sd = numeric(),
        median = numeric(),
        min = numeric(),
        max = numeric(),
        n_unique = integer(),
        most_common_value = character()
      )
    )
  }

  summaries <- lapply(observation_cols, function(col) {
    x <- sequence_data[[col]]
    states <- sequence_data$state

    if (is.numeric(x)) {
      split_x <- split(x, states)

      dplyr::bind_rows(lapply(names(split_x), function(st) {
        values <- split_x[[st]]
        non_missing <- values[!is.na(values)]

        tibble::tibble(
          state = st,
          observation = col,
          observation_type = "numeric",
          n = length(values),
          n_missing = sum(is.na(values)),
          missing_prop = mean(is.na(values)),
          mean = if (length(non_missing) == 0L) NA_real_ else mean(non_missing),
          sd = if (length(non_missing) <= 1L) NA_real_ else stats::sd(non_missing),
          median = if (length(non_missing) == 0L) NA_real_ else stats::median(non_missing),
          min = if (length(non_missing) == 0L) NA_real_ else min(non_missing),
          max = if (length(non_missing) == 0L) NA_real_ else max(non_missing),
          n_unique = length(unique(non_missing)),
          most_common_value = NA_character_
        )
      }))
    } else {
      x_chr <- as.character(x)
      split_x <- split(x_chr, states)

      dplyr::bind_rows(lapply(names(split_x), function(st) {
        values <- split_x[[st]]
        non_missing <- values[!is.na(values) & nzchar(values)]

        most_common <- if (length(non_missing) == 0L) {
          NA_character_
        } else {
          names(sort(table(non_missing), decreasing = TRUE))[[1]]
        }

        tibble::tibble(
          state = st,
          observation = col,
          observation_type = "categorical",
          n = length(values),
          n_missing = sum(is.na(values) | !nzchar(values)),
          missing_prop = mean(is.na(values) | !nzchar(values)),
          mean = NA_real_,
          sd = NA_real_,
          median = NA_real_,
          min = NA_real_,
          max = NA_real_,
          n_unique = length(unique(non_missing)),
          most_common_value = most_common
        )
      }))
    }
  })

  dplyr::bind_rows(summaries)
}

.gp3_hmm_emission_data <- function(sequence_data, observation_cols) {
  if (length(observation_cols) == 0L) {
    return(
      tibble::tibble(
        .sequence_key = character(),
        observation_index = integer(),
        state = character(),
        observation = character(),
        value = character(),
        numeric_value = numeric()
      )
    )
  }

  emission_list <- lapply(observation_cols, function(col) {
    value <- sequence_data[[col]]

    tibble::tibble(
      .sequence_key = sequence_data$.sequence_key,
      observation_index = sequence_data$observation_index,
      state = sequence_data$state,
      observation = col,
      value = as.character(value),
      numeric_value = suppressWarnings(as.numeric(value))
    )
  })

  dplyr::bind_rows(emission_list)
}

.gp3_hmm_z_score <- function(x) {
  x <- as.numeric(x)
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(mu) || !is.finite(sigma) || sigma == 0) {
    out <- rep(NA_real_, length(x))
    out[!is.na(x)] <- 0
    return(out)
  }

  (x - mu) / sigma
}

.gp3_hmm_create_sequence_key <- function(data, row_index, sequence_id_cols) {
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

.gp3_hmm_create_order_key <- function(data, row_index, sequence_key, time_col) {
  if (!is.null(time_col)) {
    time <- suppressWarnings(as.numeric(data[[time_col]][row_index]))

    if (!all(is.na(time))) {
      return(time)
    }
  }

  stats::ave(seq_along(row_index), sequence_key, FUN = seq_along)
}

.gp3_hmm_optional_numeric_by_index <- function(data, row_index, col) {
  if (is.null(col)) {
    return(rep(NA_real_, length(row_index)))
  }

  suppressWarnings(as.numeric(data[[col]][row_index]))
}

.gp3_hmm_safe_min <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

.gp3_hmm_safe_max <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

.gp3_hmm_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_hmm_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_hmm_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_hmm_resolve_col(col, names_data, arg))
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

.gp3_hmm_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hmm_check_character_vector <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hmm_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_hmm_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
