.gp3_find_column <- function(data, candidates, required = FALSE, label = "column") {
  nms <- names(data)
  idx <- match(tolower(candidates), tolower(nms), nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) > 0L) {
    return(nms[idx[1L]])
  }
  if (isTRUE(required)) {
    stop("Could not identify ", label, ". Please supply the column name explicitly.", call. = FALSE)
  }
  NULL
}

.gp3_find_matrix_in_list <- function(x, pattern) {
  if (is.matrix(x) || is.table(x)) {
    return(as.matrix(x))
  }
  if (!is.list(x)) {
    return(NULL)
  }
  nms <- names(x)
  if (!is.null(nms)) {
    hit <- which(grepl(pattern, nms, ignore.case = TRUE))
    for (i in hit) {
      if (is.matrix(x[[i]]) || is.table(x[[i]])) {
        return(as.matrix(x[[i]]))
      }
    }
  }
  for (item in x) {
    if (is.matrix(item) || is.table(item)) {
      return(as.matrix(item))
    }
  }
  NULL
}

.gp3_find_dataframe_in_list <- function(x, pattern = NULL) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  nms <- names(x)
  if (!is.null(pattern) && !is.null(nms)) {
    hit <- which(grepl(pattern, nms, ignore.case = TRUE))
    for (i in hit) {
      if (is.data.frame(x[[i]])) {
        return(x[[i]])
      }
    }
  }
  for (item in x) {
    if (is.data.frame(item)) {
      return(item)
    }
  }
  NULL
}

#' Summarise a Gazepoint Markov-chain object
#'
#' Convert a Gazepoint Markov-chain object, transition matrix, or transition
#' data frame into a tidy transition summary. The function is deliberately
#' permissive so that it can summarise objects created by
#' \code{create_gazepoint_markovchain_object()} as well as simple matrices
#' used in examples or tests.
#'
#' @param markov_object A Markov-chain object, matrix, table, list, or data
#'   frame containing transition information.
#' @param include_zero Should zero-valued transitions be retained?
#' @param from_col Optional source-state column when \code{markov_object} is a
#'   data frame.
#' @param to_col Optional destination-state column when \code{markov_object}
#'   is a data frame.
#' @param count_col Optional transition-count column when available.
#' @param probability_col Optional transition-probability column when
#'   available.
#'
#' @return A data frame with source state, destination state, transition count
#'   or weight, row total, transition probability, and status columns.
#' @export
summarise_gazepoint_markovchain <- function(markov_object,
                                           include_zero = FALSE,
                                           from_col = NULL,
                                           to_col = NULL,
                                           count_col = NULL,
                                           probability_col = NULL) {
  if (is.data.frame(markov_object)) {
    data <- markov_object
    from_col <- if (is.null(from_col)) {
      .gp3_find_column(data, c("from_state", "from", "source", "origin"), TRUE, "from_col")
    } else {
      .gp3_ext_check_scalar_string(from_col, "from_col")
    }
    to_col <- if (is.null(to_col)) {
      .gp3_find_column(data, c("to_state", "to", "target", "destination"), TRUE, "to_col")
    } else {
      .gp3_ext_check_scalar_string(to_col, "to_col")
    }
    count_col <- if (is.null(count_col)) {
      .gp3_find_column(data, c("transition_count", "count", "n", "frequency", "weight"))
    } else {
      .gp3_ext_check_scalar_string(count_col, "count_col")
    }
    probability_col <- if (is.null(probability_col)) {
      .gp3_find_column(data, c("transition_probability", "probability", "prob", "p"))
    } else {
      .gp3_ext_check_scalar_string(probability_col, "probability_col")
    }
    .gp3_ext_check_columns(data, c(from_col, to_col, count_col, probability_col))

    if (is.null(count_col) && is.null(probability_col)) {
      data$.gp3_count <- 1
      count_col <- ".gp3_count"
    }
    if (!is.null(count_col)) {
      value <- data[[count_col]]
      value_kind <- "count"
    } else {
      value <- data[[probability_col]]
      value_kind <- "probability"
    }
    out <- data.frame(
      from_state = as.character(data[[from_col]]),
      to_state = as.character(data[[to_col]]),
      transition_value = as.numeric(value),
      value_kind = value_kind,
      stringsAsFactors = FALSE
    )
    out <- out[!is.na(out$from_state) & !is.na(out$to_state), , drop = FALSE]
    if (!isTRUE(include_zero)) {
      out <- out[is.na(out$transition_value) | out$transition_value != 0, , drop = FALSE]
    }
    totals <- stats::aggregate(transition_value ~ from_state, data = out, sum, na.rm = TRUE)
    names(totals)[2L] <- "row_total"
    out <- merge(out, totals, by = "from_state", all.x = TRUE, sort = FALSE)
    out$transition_probability <- ifelse(out$row_total > 0, out$transition_value / out$row_total, NA_real_)
    if (!is.null(probability_col) && is.null(count_col)) {
      out$transition_probability <- out$transition_value
    }
    out$summary_status <- "ok"
    rownames(out) <- NULL
    return(out[, c("from_state", "to_state", "transition_value", "value_kind",
                   "row_total", "transition_probability", "summary_status")])
  }

  count_matrix <- .gp3_find_matrix_in_list(markov_object, "count|frequency|weight")
  probability_matrix <- .gp3_find_matrix_in_list(markov_object, "prob|transition")
  matrix_used <- if (!is.null(count_matrix)) count_matrix else probability_matrix
  if (is.null(matrix_used)) {
    stop("Could not identify a transition matrix in markov_object.", call. = FALSE)
  }
  matrix_used <- as.matrix(matrix_used)
  storage.mode(matrix_used) <- "numeric"
  if (is.null(rownames(matrix_used))) {
    rownames(matrix_used) <- paste0("state_", seq_len(nrow(matrix_used)))
  }
  if (is.null(colnames(matrix_used))) {
    colnames(matrix_used) <- paste0("state_", seq_len(ncol(matrix_used)))
  }
  row_total <- rowSums(matrix_used, na.rm = TRUE)
  rows <- list()
  k <- 1L
  for (i in seq_len(nrow(matrix_used))) {
    for (j in seq_len(ncol(matrix_used))) {
      value <- matrix_used[i, j]
      if (!isTRUE(include_zero) && !is.na(value) && value == 0) {
        next
      }
      rows[[k]] <- data.frame(
        from_state = rownames(matrix_used)[i],
        to_state = colnames(matrix_used)[j],
        transition_value = value,
        value_kind = if (!is.null(count_matrix)) "count" else "probability_or_weight",
        row_total = row_total[i],
        transition_probability = if (row_total[i] > 0) value / row_total[i] else NA_real_,
        summary_status = "ok",
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  out <- .gp3_ext_bind_rows(rows)
  rownames(out) <- NULL
  out
}

#' Summarise Gazepoint semi-Markov data
#'
#' Summarise state visits and state-to-state transitions from semi-Markov
#' preparation output or a compatible data frame. The function returns a list
#' with a state-duration summary and a transition summary.
#'
#' @param semimarkov_data Output from \code{prepare_gazepoint_semimarkov_data()}
#'   or a compatible data frame/list.
#' @param state_col Optional state/AOI column.
#' @param duration_col Optional state-duration column.
#' @param sequence_col Optional sequence, subject, or trial column.
#' @param time_col Optional time/order column.
#' @param from_col Optional transition source-state column.
#' @param to_col Optional transition destination-state column.
#'
#' @return A list with \code{state_summary}, \code{transition_summary},
#'   \code{columns}, and \code{summary_status}.
#' @export
summarise_gazepoint_semimarkov <- function(semimarkov_data,
                                           state_col = NULL,
                                           duration_col = NULL,
                                           sequence_col = NULL,
                                           time_col = NULL,
                                           from_col = NULL,
                                           to_col = NULL) {
  data <- .gp3_find_dataframe_in_list(semimarkov_data, "semi|state|duration|transition")
  if (is.null(data)) {
    stop("Could not identify a data frame in semimarkov_data.", call. = FALSE)
  }
  state_col <- if (is.null(state_col)) {
    .gp3_find_column(data, c("state", "aoi", "AOI", "state_label", "aoi_label"), TRUE, "state_col")
  } else {
    .gp3_ext_check_scalar_string(state_col, "state_col")
  }
  duration_col <- if (is.null(duration_col)) {
    .gp3_find_column(data, c("duration", "duration_ms", "dwell_time", "state_duration",
                             "visit_duration", "fixation_duration"))
  } else {
    .gp3_ext_check_scalar_string(duration_col, "duration_col")
  }
  sequence_col <- if (is.null(sequence_col)) {
    .gp3_find_column(data, c("sequence", "sequence_id", "trial", "trial_id",
                             "subject", "subject_id", "participant", "id"))
  } else {
    .gp3_ext_check_scalar_string(sequence_col, "sequence_col")
  }
  time_col <- if (is.null(time_col)) {
    .gp3_find_column(data, c("time", "time_ms", "timestamp", "start_time",
                             "fixation_start", "order", "index"))
  } else {
    .gp3_ext_check_scalar_string(time_col, "time_col")
  }
  from_col <- if (is.null(from_col)) {
    .gp3_find_column(data, c("from_state", "from", "source", "previous_state"))
  } else {
    .gp3_ext_check_scalar_string(from_col, "from_col")
  }
  to_col <- if (is.null(to_col)) {
    .gp3_find_column(data, c("to_state", "to", "target", "next_state"))
  } else {
    .gp3_ext_check_scalar_string(to_col, "to_col")
  }
  .gp3_ext_check_columns(data, c(state_col, duration_col, sequence_col, time_col, from_col, to_col))

  d <- data[!is.na(data[[state_col]]), , drop = FALSE]
  d[[state_col]] <- as.character(d[[state_col]])
  state_groups <- split(d, d[[state_col]], drop = TRUE)
  state_rows <- lapply(state_groups, function(block) {
    dur <- if (!is.null(duration_col)) as.numeric(block[[duration_col]]) else rep(NA_real_, nrow(block))
    data.frame(
      state = block[[state_col]][1L],
      n_visits = nrow(block),
      n_sequences = if (!is.null(sequence_col)) length(unique(block[[sequence_col]])) else NA_integer_,
      total_duration = if (all(is.na(dur))) NA_real_ else sum(dur, na.rm = TRUE),
      mean_duration = if (all(is.na(dur))) NA_real_ else mean(dur, na.rm = TRUE),
      median_duration = if (all(is.na(dur))) NA_real_ else stats::median(dur, na.rm = TRUE),
      min_duration = if (all(is.na(dur))) NA_real_ else min(dur, na.rm = TRUE),
      max_duration = if (all(is.na(dur))) NA_real_ else max(dur, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  state_summary <- .gp3_ext_bind_rows(state_rows)
  rownames(state_summary) <- NULL

  if (!is.null(from_col) && !is.null(to_col)) {
    transitions <- data.frame(
      from_state = as.character(data[[from_col]]),
      to_state = as.character(data[[to_col]]),
      stringsAsFactors = FALSE
    )
    transitions <- transitions[!is.na(transitions$from_state) & !is.na(transitions$to_state), , drop = FALSE]
  } else {
    ordered <- d
    if (!is.null(time_col)) {
      if (!is.null(sequence_col)) {
        ordered <- ordered[order(ordered[[sequence_col]], ordered[[time_col]]), , drop = FALSE]
      } else {
        ordered <- ordered[order(ordered[[time_col]]), , drop = FALSE]
      }
    }
    groups <- if (!is.null(sequence_col)) split(ordered, ordered[[sequence_col]], drop = TRUE) else list(all = ordered)
    transition_rows <- lapply(groups, function(block) {
      if (nrow(block) < 2L) {
        return(NULL)
      }
      data.frame(
        from_state = as.character(utils::head(block[[state_col]], -1L)),
        to_state = as.character(utils::tail(block[[state_col]], -1L)),
        stringsAsFactors = FALSE
      )
    })
    transitions <- .gp3_ext_bind_rows(transition_rows)
  }

  if (nrow(transitions) == 0L) {
    transition_summary <- data.frame(
      from_state = character(0),
      to_state = character(0),
      transition_count = integer(0),
      row_total = integer(0),
      transition_probability = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    transitions$.gp3_n <- 1L
    transition_summary <- stats::aggregate(
      .gp3_n ~ from_state + to_state,
      data = transitions,
      sum
    )
    names(transition_summary)[names(transition_summary) == ".gp3_n"] <- "transition_count"
    row_totals <- stats::aggregate(
      transition_count ~ from_state,
      data = transition_summary,
      sum
    )
    names(row_totals)[2L] <- "row_total"
    transition_summary <- merge(transition_summary, row_totals, by = "from_state", all.x = TRUE, sort = FALSE)
    transition_summary$transition_probability <- transition_summary$transition_count / transition_summary$row_total
    transition_summary <- transition_summary[, c("from_state", "to_state", "transition_count",
                                                 "row_total", "transition_probability")]
  }
  rownames(transition_summary) <- NULL

  out <- list(
    state_summary = state_summary,
    transition_summary = transition_summary,
    columns = list(
      state_col = state_col,
      duration_col = duration_col,
      sequence_col = sequence_col,
      time_col = time_col,
      from_col = from_col,
      to_col = to_col
    ),
    summary_status = "ok"
  )
  class(out) <- c("gp3_semimarkov_summary", "list")
  out
}
