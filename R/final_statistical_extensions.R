#' Detect simple I-VT fixations from gaze samples
#'
#' Apply a lightweight velocity-threshold fixation detector to ordered gaze
#' samples. This helper is intended for exploratory checks and teaching; it
#' does not replace dedicated event-detection packages or Gazepoint's native
#' fixation export.
#'
#' @param data A sample-level gaze data frame.
#' @param x_col Horizontal gaze coordinate column.
#' @param y_col Vertical gaze coordinate column.
#' @param time_col Time column, in milliseconds by default.
#' @param group_cols Optional columns defining independent recordings/trials.
#' @param velocity_threshold Maximum velocity for a sample-to-sample interval
#'   to be treated as fixation-like.
#' @param min_duration_ms Minimum fixation duration.
#' @param distance_scale Multiplicative scale applied to coordinate distances.
#' @param time_scale Multiplicative scale applied to time differences.
#'
#' @return A fixation-level data frame.
#' @export
detect_gazepoint_fixations_ivt <- function(data,
                                          x_col,
                                          y_col,
                                          time_col,
                                          group_cols = NULL,
                                          velocity_threshold = 0.01,
                                          min_duration_ms = 60,
                                          distance_scale = 1,
                                          time_scale = 1) {
  .gp3_ext_check_data(data)
  x_col <- .gp3_ext_check_scalar_string(x_col, "x_col")
  y_col <- .gp3_ext_check_scalar_string(y_col, "y_col")
  time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  if (!is.null(group_cols)) group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
  .gp3_ext_check_columns(data, c(x_col, y_col, time_col, group_cols))
  if (!is.numeric(data[[x_col]]) || !is.numeric(data[[y_col]]) || !is.numeric(data[[time_col]])) {
    stop("x_col, y_col, and time_col must identify numeric columns.", call. = FALSE)
  }
  if (!is.numeric(velocity_threshold) || velocity_threshold <= 0) {
    stop("velocity_threshold must be positive.", call. = FALSE)
  }
  if (!is.numeric(min_duration_ms) || min_duration_ms < 0) {
    stop("min_duration_ms must be non-negative.", call. = FALSE)
  }

  groups <- .gp3_ext_split_groups(data, group_cols)
  rows <- lapply(groups, function(group_data) {
    group_data <- .gp3_ext_order_data(group_data, time_col)
    keep <- !is.na(group_data[[x_col]]) & !is.na(group_data[[y_col]]) & !is.na(group_data[[time_col]])
    group_data <- group_data[keep, , drop = FALSE]
    n <- nrow(group_data)
    if (n < 2L) return(NULL)
    x <- as.numeric(group_data[[x_col]])
    y <- as.numeric(group_data[[y_col]])
    time <- as.numeric(group_data[[time_col]])
    dt <- diff(time) * time_scale
    dist <- sqrt(diff(x)^2 + diff(y)^2) * distance_scale
    velocity <- ifelse(dt > 0, dist / dt, Inf)
    fixation_like <- c(FALSE, velocity <= velocity_threshold)
    fixation_like[is.na(fixation_like)] <- FALSE
    if (!any(fixation_like)) return(NULL)
    run_id <- cumsum(c(TRUE, fixation_like[-1L] != fixation_like[-length(fixation_like)]))
    candidate <- split(seq_len(n), run_id)
    candidate <- candidate[vapply(candidate, function(idx) all(fixation_like[idx]), logical(1))]
    if (length(candidate) == 0L) return(NULL)
    fix_rows <- lapply(seq_along(candidate), function(i) {
      idx <- candidate[[i]]
      start_time <- min(time[idx])
      end_time <- max(time[idx])
      duration <- end_time - start_time
      if (duration < min_duration_ms) return(NULL)
      out <- data.frame(
        fixation_index = i,
        start_time = start_time,
        end_time = end_time,
        duration_ms = duration,
        n_samples = length(idx),
        x = mean(x[idx]),
        y = mean(y[idx]),
        mean_velocity = mean(c(velocity[pmax(idx - 1L, 1L)]), na.rm = TRUE),
        detection_method = "I-VT_lightweight",
        detection_status = "ok",
        stringsAsFactors = FALSE
      )
      group_values <- .gp3_ext_group_values(group_data, group_cols)
      cbind(group_values, out)
    })
    .gp3_ext_bind_rows(fix_rows)
  })
  out <- .gp3_ext_bind_rows(rows)
  rownames(out) <- NULL
  out
}

#' Prepare AOI sequences for TraMineR-style workflows
#'
#' Convert long AOI observations into one row per sequence and one column per
#' ordered state position. If \pkg{TraMineR} is installed and
#' \code{as_traminer = TRUE}, the function also returns a TraMineR sequence
#' object.
#'
#' @param data Long-format AOI data.
#' @param aoi_col AOI/state column.
#' @param sequence_cols Columns defining each sequence.
#' @param time_col Optional ordering column.
#' @param include_missing Should missing AOIs be kept as a state?
#' @param missing_label Label used for retained missing AOIs.
#' @param collapse_repeats Should consecutive repeated states be collapsed?
#' @param state_prefix Prefix for wide state columns.
#' @param as_traminer Should TraMineR::seqdef() be called if available?
#'
#' @return A list containing wide data, state columns, alphabet, and optionally
#'   a TraMineR sequence object.
#' @export
prepare_gazepoint_traminer_data <- function(data,
                                           aoi_col,
                                           sequence_cols,
                                           time_col = NULL,
                                           include_missing = FALSE,
                                           missing_label = "missing",
                                           collapse_repeats = FALSE,
                                           state_prefix = "state_",
                                           as_traminer = FALSE) {
  .gp3_ext_check_data(data)
  aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
  sequence_cols <- .gp3_ext_check_character_vector(sequence_cols, "sequence_cols")
  if (!is.null(time_col)) time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  .gp3_ext_check_columns(data, c(aoi_col, sequence_cols, time_col))

  groups <- .gp3_ext_split_groups(data, sequence_cols)
  seqs <- lapply(groups, function(block) {
    block <- .gp3_ext_order_data(block, time_col)
    x <- .gp3_ext_prepare_aoi(block[[aoi_col]], include_missing, missing_label)
    .gp3_ext_collapse_repeats(x, collapse_repeats)
  })
  max_len <- max(vapply(seqs, length, integer(1)), 0L)
  state_cols <- paste0(state_prefix, seq_len(max_len))
  rows <- lapply(seq_along(groups), function(i) {
    values <- .gp3_ext_group_values(groups[[i]], sequence_cols)
    states <- rep(NA_character_, max_len)
    if (length(seqs[[i]]) > 0L) states[seq_along(seqs[[i]])] <- seqs[[i]]
    states <- as.data.frame(as.list(states), stringsAsFactors = FALSE)
    names(states) <- state_cols
    cbind(values, states)
  })
  wide <- .gp3_ext_bind_rows(rows)
  alphabet <- sort(unique(unlist(seqs, use.names = FALSE)))
  traminer_sequence <- NULL
  if (isTRUE(as_traminer)) {
    if (!requireNamespace("TraMineR", quietly = TRUE)) {
      stop("Package 'TraMineR' is required when as_traminer = TRUE.", call. = FALSE)
    }
    traminer_sequence <- TraMineR::seqdef(wide[, state_cols, drop = FALSE])
  }
  out <- list(
    wide_data = wide,
    state_cols = state_cols,
    alphabet = alphabet,
    traminer_sequence = traminer_sequence,
    sequence_status = "ok"
  )
  class(out) <- c("gp3_traminer_data", "list")
  out
}

#' Compute simple categorical sequence recurrence metrics
#'
#' Compute lightweight recurrence metrics from AOI/state sequences. This is a
#' compact categorical recurrence helper, not a full replacement for dedicated
#' CRQA/RQA packages.
#'
#' @param data Optional long-format data frame.
#' @param sequence Optional AOI/state vector used when \code{data} is absent.
#' @param aoi_col AOI/state column when \code{data} is supplied.
#' @param group_cols Optional grouping columns.
#' @param time_col Optional ordering column.
#' @param min_line Minimum diagonal-line length for determinism.
#' @param include_missing Should missing states be retained?
#' @param missing_label Label used for retained missing states.
#'
#' @return A data frame of recurrence metrics.
#' @export
compute_gazepoint_sequence_recurrence <- function(data = NULL,
                                                  sequence = NULL,
                                                  aoi_col = NULL,
                                                  group_cols = NULL,
                                                  time_col = NULL,
                                                  min_line = 2,
                                                  include_missing = FALSE,
                                                  missing_label = "missing") {
  one_seq <- function(x) {
    x <- .gp3_ext_prepare_aoi(x, include_missing, missing_label)
    n <- length(x)
    if (n < 2L) {
      return(data.frame(sequence_length = n, recurrence_points = 0, recurrence_rate = NA_real_,
                        determinism = NA_real_, mean_diagonal_length = NA_real_, recurrence_status = "too_short"))
    }
    mat <- outer(x, x, FUN = "==")
    diag(mat) <- FALSE
    upper <- mat[upper.tri(mat)]
    recurrence_points <- sum(upper, na.rm = TRUE)
    possible <- length(upper)
    recurrence_rate <- recurrence_points / possible
    line_lengths <- integer(0)
    for (lag in seq_len(n - 1L)) {
      diag_vals <- diag(mat[-seq_len(lag), seq_len(n - lag), drop = FALSE])
      if (length(diag_vals) == 0L) next
      r <- rle(diag_vals)
      line_lengths <- c(line_lengths, r$lengths[r$values & r$lengths >= min_line])
    }
    deterministic_points <- sum(line_lengths)
    determinism <- if (recurrence_points > 0) deterministic_points / recurrence_points else NA_real_
    data.frame(
      sequence_length = n,
      recurrence_points = recurrence_points,
      recurrence_rate = recurrence_rate,
      determinism = determinism,
      mean_diagonal_length = if (length(line_lengths) == 0L) NA_real_ else mean(line_lengths),
      recurrence_status = "ok",
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(data)) {
    .gp3_ext_check_data(data)
    aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
    if (!is.null(group_cols)) group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
    if (!is.null(time_col)) time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
    .gp3_ext_check_columns(data, c(aoi_col, group_cols, time_col))
    groups <- .gp3_ext_split_groups(data, group_cols)
    rows <- lapply(groups, function(block) {
      block <- .gp3_ext_order_data(block, time_col)
      cbind(.gp3_ext_group_values(block, group_cols), one_seq(block[[aoi_col]]))
    })
    out <- .gp3_ext_bind_rows(rows)
    rownames(out) <- NULL
    return(out)
  }
  if (is.null(sequence)) stop("Supply either data with aoi_col or sequence.", call. = FALSE)
  one_seq(sequence)
}

#' Compute lightweight AOI transition-network metrics
#'
#' Compute graph-style summaries from AOI transitions without requiring
#' network packages. Metrics include state count, edge count, density,
#' self-loops, and in/out-degree summaries.
#'
#' @param data Optional data frame of AOI observations or transition rows.
#' @param aoi_col AOI column for raw sequence data.
#' @param from_col Source-state column for transition data.
#' @param to_col Destination-state column for transition data.
#' @param group_cols Optional grouping columns for raw sequence data.
#' @param time_col Optional ordering column.
#' @param include_self_loops Should self-transitions be included?
#'
#' @return A list with graph-level and state-level summaries.
#' @export
compute_gazepoint_transition_network_metrics <- function(data,
                                                         aoi_col = NULL,
                                                         from_col = NULL,
                                                         to_col = NULL,
                                                         group_cols = NULL,
                                                         time_col = NULL,
                                                         include_self_loops = TRUE) {
  .gp3_ext_check_data(data)
  if (!is.null(aoi_col)) aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
  if (!is.null(from_col)) from_col <- .gp3_ext_check_scalar_string(from_col, "from_col")
  if (!is.null(to_col)) to_col <- .gp3_ext_check_scalar_string(to_col, "to_col")
  if (!is.null(group_cols)) group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
  if (!is.null(time_col)) time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  if (!is.null(from_col) && !is.null(to_col)) {
    .gp3_ext_check_columns(data, c(from_col, to_col))
    transitions <- data.frame(from_state = as.character(data[[from_col]]),
                              to_state = as.character(data[[to_col]]), stringsAsFactors = FALSE)
  } else {
    aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
    .gp3_ext_check_columns(data, c(aoi_col, group_cols, time_col))
    groups <- .gp3_ext_split_groups(data, group_cols)
    transitions <- .gp3_ext_bind_rows(lapply(groups, function(block) {
      block <- .gp3_ext_order_data(block, time_col)
      x <- .gp3_ext_prepare_aoi(block[[aoi_col]], FALSE, "missing")
      if (length(x) < 2L) return(NULL)
      data.frame(from_state = utils::head(x, -1L), to_state = utils::tail(x, -1L), stringsAsFactors = FALSE)
    }))
  }
  transitions <- transitions[!is.na(transitions$from_state) & !is.na(transitions$to_state), , drop = FALSE]
  if (!isTRUE(include_self_loops)) transitions <- transitions[transitions$from_state != transitions$to_state, , drop = FALSE]
  if (nrow(transitions) == 0L) {
    return(list(graph_summary = data.frame(n_states = 0, n_edges = 0, density = NA_real_,
                                           self_loops = 0, total_transitions = 0),
                state_summary = data.frame(stringsAsFactors = FALSE), network_status = "empty"))
  }
  transitions$count <- 1L
  edge_summary <- stats::aggregate(count ~ from_state + to_state, transitions, sum)
  states <- sort(unique(c(edge_summary$from_state, edge_summary$to_state)))
  n_states <- length(states)
  n_edges <- nrow(edge_summary)
  possible_edges <- if (isTRUE(include_self_loops)) n_states^2 else n_states * (n_states - 1L)
  out_degree <- table(factor(edge_summary$from_state, levels = states))
  in_degree <- table(factor(edge_summary$to_state, levels = states))
  weighted_out <- stats::aggregate(count ~ from_state, edge_summary, sum)
  weighted_in <- stats::aggregate(count ~ to_state, edge_summary, sum)
  state_summary <- data.frame(state = states,
                              out_degree = as.numeric(out_degree),
                              in_degree = as.numeric(in_degree),
                              stringsAsFactors = FALSE)
  state_summary$weighted_out_degree <- weighted_out$count[match(state_summary$state, weighted_out$from_state)]
  state_summary$weighted_in_degree <- weighted_in$count[match(state_summary$state, weighted_in$to_state)]
  state_summary$weighted_out_degree[is.na(state_summary$weighted_out_degree)] <- 0
  state_summary$weighted_in_degree[is.na(state_summary$weighted_in_degree)] <- 0
  graph_summary <- data.frame(
    n_states = n_states,
    n_edges = n_edges,
    density = n_edges / possible_edges,
    self_loops = sum(edge_summary$from_state == edge_summary$to_state),
    total_transitions = sum(edge_summary$count),
    mean_out_degree = mean(state_summary$out_degree),
    max_out_degree = max(state_summary$out_degree),
    mean_in_degree = mean(state_summary$in_degree),
    max_in_degree = max(state_summary$in_degree),
    stringsAsFactors = FALSE
  )
  out <- list(graph_summary = graph_summary, state_summary = state_summary,
              edge_summary = edge_summary, network_status = "ok")
  class(out) <- c("gp3_transition_network_metrics", "list")
  out
}

#' Launch or describe a lightweight QC dashboard
#'
#' Provide a minimal optional Shiny dashboard launcher for inspecting a data
#' frame. With \code{launch = FALSE}, the function returns a dashboard
#' specification without requiring Shiny.
#'
#' @param data Optional data frame to inspect.
#' @param title Dashboard title.
#' @param launch Should a Shiny app be launched?
#'
#' @return A dashboard specification, or a Shiny app object when launched.
#' @export
launch_gazepoint_qc_dashboard <- function(data = NULL,
                                          title = "gp3tools QC dashboard",
                                          launch = FALSE) {
  if (!is.null(data)) .gp3_ext_check_data(data)
  spec <- list(
    title = title,
    n_rows = if (is.null(data)) NA_integer_ else nrow(data),
    n_cols = if (is.null(data)) NA_integer_ else ncol(data),
    columns = if (is.null(data)) character(0) else names(data),
    launch = isTRUE(launch),
    dashboard_status = if (isTRUE(launch)) "pending_launch" else "specification_only"
  )
  class(spec) <- c("gp3_qc_dashboard_spec", "list")
  if (!isTRUE(launch)) return(spec)
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to launch the dashboard. Use launch = FALSE for a specification only.", call. = FALSE)
  }
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::verbatimTextOutput("summary"),
    shiny::tableOutput("head")
  )
  server <- function(input, output, session) {
    output$summary <- shiny::renderPrint({
      if (is.null(data)) "No data supplied" else utils::str(data)
    })
    output$head <- shiny::renderTable({
      if (is.null(data)) data.frame() else utils::head(data)
    })
  }
  shiny::shinyApp(ui, server)
}
