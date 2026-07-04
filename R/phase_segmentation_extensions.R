#' Segment Gazepoint-style data into task phases
#'
#' Assigns each row of a Gazepoint-style data set to a task phase using a
#' user-supplied phase-window table. The helper is deterministic and descriptive:
#' it labels rows according to declared time windows and does not infer phases.
#'
#' @param data A data frame.
#' @param time_col Character name of the time column.
#' @param phase_windows A data frame containing phase labels and start/end times.
#' @param phase_col Output column name for assigned phases.
#' @param window_phase_col,window_start_col,window_end_col Column names in
#'   `phase_windows`.
#' @param outside_label Label assigned to rows outside all phase windows. If
#'   `NULL`, outside rows receive `NA_character_`.
#' @param include_lower,include_upper Logical values controlling whether phase
#'   window boundaries are closed on the lower and upper sides.
#' @param keep_window_metadata If `TRUE`, adds assigned phase-window start and
#'   end columns.
#'
#' @return A copy of `data` with phase labels and assignment diagnostics.
#' @export
#'
#' @examples
#' x <- data.frame(time_ms = c(0, 250, 750, 1250, 1750))
#' windows <- data.frame(
#'   phase = c("baseline", "stimulus", "response"),
#'   start = c(0, 500, 1000),
#'   end = c(500, 1000, 2000)
#' )
#' segment_gazepoint_task_phases(x, "time_ms", windows)
segment_gazepoint_task_phases <- function(data,
                                          time_col,
                                          phase_windows,
                                          phase_col = "task_phase",
                                          window_phase_col = "phase",
                                          window_start_col = "start",
                                          window_end_col = "end",
                                          outside_label = "outside",
                                          include_lower = TRUE,
                                          include_upper = FALSE,
                                          keep_window_metadata = FALSE) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, time_col, "data")
  .gp3_require_data_frame(phase_windows, "phase_windows")
  .gp3_require_columns(
    phase_windows,
    c(window_phase_col, window_start_col, window_end_col),
    "phase_windows"
  )

  if (!is.character(phase_col) || length(phase_col) != 1L || !nzchar(phase_col)) {
    stop("`phase_col` must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.logical(include_lower) || length(include_lower) != 1L || is.na(include_lower)) {
    stop("`include_lower` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(include_upper) || length(include_upper) != 1L || is.na(include_upper)) {
    stop("`include_upper` must be TRUE or FALSE.", call. = FALSE)
  }

  windows <- .gp3_prepare_phase_windows(
    phase_windows = phase_windows,
    phase_col = window_phase_col,
    start_col = window_start_col,
    end_col = window_end_col
  )

  time_value <- suppressWarnings(as.numeric(data[[time_col]]))

  assigned_phase <- rep(NA_character_, length(time_value))
  assigned_start <- rep(NA_real_, length(time_value))
  assigned_end <- rep(NA_real_, length(time_value))

  for (i in seq_len(nrow(windows))) {
    lower_ok <- if (isTRUE(include_lower)) {
      time_value >= windows$start[i]
    } else {
      time_value > windows$start[i]
    }

    upper_ok <- if (isTRUE(include_upper)) {
      time_value <= windows$end[i]
    } else {
      time_value < windows$end[i]
    }

    in_window <- is.finite(time_value) & lower_ok & upper_ok & is.na(assigned_phase)

    assigned_phase[in_window] <- windows$phase[i]
    assigned_start[in_window] <- windows$start[i]
    assigned_end[in_window] <- windows$end[i]
  }

  phase_assigned <- !is.na(assigned_phase)

  if (!is.null(outside_label)) {
    assigned_phase[!phase_assigned] <- outside_label
  }

  out <- data
  out[[phase_col]] <- assigned_phase
  out[[".gp3_phase_assigned"]] <- phase_assigned

  if (isTRUE(keep_window_metadata)) {
    out[[".gp3_phase_window_start"]] <- assigned_start
    out[[".gp3_phase_window_end"]] <- assigned_end
  }

  attr(out, "gp3_phase_windows") <- windows
  attr(out, "gp3_phase_segmentation") <- list(
    time_col = time_col,
    phase_col = phase_col,
    outside_label = outside_label,
    include_lower = include_lower,
    include_upper = include_upper
  )

  out
}


#' Summarize task-phase coverage
#'
#' Summarizes row counts, optional time coverage, and optional value completeness
#' by task phase and optional grouping variables. The helper is intended for
#' documenting task-stage data availability, not for inferential modelling.
#'
#' @param data A data frame containing a phase column.
#' @param phase_col Character name of the phase column.
#' @param group_cols Optional grouping columns, such as subject, trial, or
#'   condition.
#' @param time_col Optional time column used to summarize phase timing.
#' @param value_cols Optional value columns used to summarize complete-value
#'   coverage within phases.
#'
#' @return A data frame with one row per group-phase combination.
#' @export
#'
#' @examples
#' x <- data.frame(time_ms = c(0, 250, 750, 1250), value = c(1, NA, 3, 4))
#' windows <- data.frame(phase = c("baseline", "stimulus"), start = c(0, 500), end = c(500, 1500))
#' segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)
#' summarize_gazepoint_phase_coverage(segmented, phase_col = "task_phase", time_col = "time_ms")
summarize_gazepoint_phase_coverage <- function(data,
                                               phase_col = "task_phase",
                                               group_cols = NULL,
                                               time_col = NULL,
                                               value_cols = NULL) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, c(phase_col, group_cols, time_col, value_cols), "data")

  phase_value <- as.character(data[[phase_col]])

  if (is.null(group_cols) || length(group_cols) == 0L) {
    group_id <- rep("all", nrow(data))
  } else {
    group_id <- as.character(interaction(data[group_cols], drop = TRUE, lex.order = TRUE))
  }

  phase_group <- paste(group_id, phase_value, sep = "|||")
  split_rows <- split(seq_len(nrow(data)), phase_group)

  out <- lapply(names(split_rows), function(id) {
    rows <- split_rows[[id]]
    parts <- strsplit(id, "\\|\\|\\|", fixed = FALSE)[[1]]

    group_label <- parts[1]
    phase_label <- paste(parts[-1], collapse = "|||")

    time_summary <- .gp3_phase_time_summary(data, rows, time_col)
    value_summary <- .gp3_phase_value_summary(data, rows, value_cols)

    data.frame(
      group_id = group_label,
      phase = phase_label,
      n_rows = length(rows),
      time_summary,
      value_summary,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, out)
  row.names(result) <- NULL
  result
}


#' @rdname summarize_gazepoint_phase_coverage
#' @export
summarise_gazepoint_phase_coverage <- summarize_gazepoint_phase_coverage


#' Report task-phase coverage
#'
#' Produces compact, cautious text describing task-phase data coverage. The
#' report is intended for methods/results documentation and does not define
#' exclusion rules.
#'
#' @param data A raw data frame or a summary produced by
#'   `summarize_gazepoint_phase_coverage()`.
#' @param phase_col,group_cols,time_col,value_cols Arguments used when `data` is
#'   raw data.
#' @param digits Number of decimal places used in report percentages.
#'
#' @return A list with `summary`, `overall`, and `report_text`.
#' @export
#'
#' @examples
#' x <- data.frame(time_ms = c(0, 250, 750, 1250), value = c(1, NA, 3, 4))
#' windows <- data.frame(phase = c("baseline", "stimulus"), start = c(0, 500), end = c(500, 1500))
#' segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)
#' report_gazepoint_phase_coverage(segmented, phase_col = "task_phase", time_col = "time_ms")
report_gazepoint_phase_coverage <- function(data,
                                            phase_col = "task_phase",
                                            group_cols = NULL,
                                            time_col = NULL,
                                            value_cols = NULL,
                                            digits = 1) {
  .gp3_require_data_frame(data, "data")

  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative numeric value.", call. = FALSE)
  }

  if (.gp3_is_phase_coverage_summary(data)) {
    summary <- data
  } else {
    summary <- summarize_gazepoint_phase_coverage(
      data,
      phase_col = phase_col,
      group_cols = group_cols,
      time_col = time_col,
      value_cols = value_cols
    )
  }

  .gp3_require_columns(summary, c("group_id", "phase", "n_rows"), "phase coverage summary")

  phase_totals <- stats::aggregate(
    n_rows ~ phase,
    data = summary,
    FUN = sum
  )

  phase_totals <- phase_totals[order(phase_totals$n_rows, decreasing = TRUE), , drop = FALSE]

  total_rows <- sum(phase_totals$n_rows)
  n_phases <- length(unique(summary$phase))
  n_groups <- length(unique(summary$group_id))

  least_phase <- phase_totals[order(phase_totals$n_rows), , drop = FALSE][1, ]

  if ("complete_value_rate" %in% names(summary)) {
    weighted_complete <- stats::weighted.mean(
      summary$complete_value_rate,
      w = summary$n_rows,
      na.rm = TRUE
    )
  } else {
    weighted_complete <- NA_real_
  }

  complete_text <- if (is.finite(weighted_complete)) {
    paste0(
      " The weighted complete-value rate across summarized phases was ",
      round(100 * weighted_complete, digits),
      "%."
    )
  } else {
    ""
  }

  report_text <- paste0(
    "Task-phase coverage was summarized across ",
    n_phases,
    " phase(s) and ",
    n_groups,
    " group(s), representing ",
    total_rows,
    " row(s). The least represented phase was '",
    least_phase$phase,
    "' with ",
    least_phase$n_rows,
    " row(s).",
    complete_text,
    " These values are descriptive data-coverage diagnostics and do not by themselves define exclusion decisions."
  )

  overall <- data.frame(
    n_phases = n_phases,
    n_groups = n_groups,
    total_rows = total_rows,
    least_represented_phase = least_phase$phase,
    least_represented_phase_rows = least_phase$n_rows,
    weighted_complete_value_rate = weighted_complete,
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    overall = overall,
    phase_totals = phase_totals,
    report_text = report_text
  )
}


.gp3_prepare_phase_windows <- function(phase_windows,
                                       phase_col,
                                       start_col,
                                       end_col) {
  windows <- data.frame(
    phase = as.character(phase_windows[[phase_col]]),
    start = suppressWarnings(as.numeric(phase_windows[[start_col]])),
    end = suppressWarnings(as.numeric(phase_windows[[end_col]])),
    stringsAsFactors = FALSE
  )

  if (nrow(windows) < 1L) {
    stop("`phase_windows` must contain at least one row.", call. = FALSE)
  }

  if (anyNA(windows$phase) || any(!nzchar(windows$phase))) {
    stop("Phase labels must be non-missing, non-empty values.", call. = FALSE)
  }

  if (any(!is.finite(windows$start)) || any(!is.finite(windows$end))) {
    stop("Phase-window start and end values must be finite numeric values.", call. = FALSE)
  }

  if (any(windows$end <= windows$start)) {
    stop("Each phase-window end value must be greater than its start value.", call. = FALSE)
  }

  windows[order(windows$start, windows$end), , drop = FALSE]
}


.gp3_phase_time_summary <- function(data, rows, time_col) {
  if (is.null(time_col)) {
    return(data.frame(
      n_finite_time = NA_integer_,
      min_time = NA_real_,
      max_time = NA_real_,
      time_span = NA_real_
    ))
  }

  time_value <- suppressWarnings(as.numeric(data[[time_col]][rows]))
  finite_time <- is.finite(time_value)

  if (!any(finite_time)) {
    return(data.frame(
      n_finite_time = 0L,
      min_time = NA_real_,
      max_time = NA_real_,
      time_span = NA_real_
    ))
  }

  min_time <- min(time_value[finite_time])
  max_time <- max(time_value[finite_time])

  data.frame(
    n_finite_time = sum(finite_time),
    min_time = min_time,
    max_time = max_time,
    time_span = max_time - min_time
  )
}


.gp3_phase_value_summary <- function(data, rows, value_cols) {
  if (is.null(value_cols) || length(value_cols) == 0L) {
    return(data.frame(
      n_complete_value_rows = NA_integer_,
      complete_value_rate = NA_real_,
      n_any_value_missing = NA_integer_,
      any_value_missing_rate = NA_real_
    ))
  }

  value_data <- data[rows, value_cols, drop = FALSE]
  complete_rows <- stats::complete.cases(value_data)

  data.frame(
    n_complete_value_rows = sum(complete_rows),
    complete_value_rate = mean(complete_rows),
    n_any_value_missing = sum(!complete_rows),
    any_value_missing_rate = mean(!complete_rows)
  )
}


.gp3_is_phase_coverage_summary <- function(data) {
  is.data.frame(data) &&
    all(c("group_id", "phase", "n_rows") %in% names(data))
}
#' Plot a Gazepoint task-phase timeline
#'
#' Creates a descriptive timeline plot from segmented Gazepoint-style data or
#' from a phase-coverage summary. The plot is intended for visual inspection of
#' task-phase coverage and timing, not for inferential analysis.
#'
#' @param data A segmented data frame or a summary produced by
#'   `summarize_gazepoint_phase_coverage()`.
#' @param phase_col Character name of the phase column when `data` is raw data.
#' @param group_cols Optional grouping columns when `data` is raw data.
#' @param time_col Optional time column when `data` is raw data.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' x <- data.frame(time_ms = c(0, 250, 750, 1250))
#' windows <- data.frame(
#'   phase = c("baseline", "stimulus"),
#'   start = c(0, 500),
#'   end = c(500, 1500)
#' )
#' segmented <- segment_gazepoint_task_phases(x, "time_ms", windows)
#' plot_gazepoint_phase_timeline(segmented, time_col = "time_ms")
plot_gazepoint_phase_timeline <- function(data,
                                          phase_col = "task_phase",
                                          group_cols = NULL,
                                          time_col = NULL,
                                          title = NULL) {
  .gp3_require_data_frame(data, "data")

  if (.gp3_is_phase_coverage_summary(data)) {
    summary <- data
  } else {
    summary <- summarize_gazepoint_phase_coverage(
      data,
      phase_col = phase_col,
      group_cols = group_cols,
      time_col = time_col
    )
  }

  .gp3_require_columns(summary, c("group_id", "phase", "n_rows"), "phase coverage summary")

  plot_data <- summary
  plot_data$.gp3_group <- factor(plot_data$group_id, levels = unique(plot_data$group_id))
  plot_data$.gp3_phase <- factor(plot_data$phase, levels = unique(plot_data$phase))

  has_time <- all(c("min_time", "max_time") %in% names(plot_data)) &&
    any(is.finite(plot_data$min_time)) &&
    any(is.finite(plot_data$max_time))

  if (isTRUE(has_time)) {
    plot_data$.gp3_min_time <- as.numeric(plot_data$min_time)
    plot_data$.gp3_max_time <- as.numeric(plot_data$max_time)

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .gp3_min_time,
          xend = .gp3_max_time,
          y = .gp3_group,
          yend = .gp3_group,
          colour = .gp3_phase
        )
      ) +
        ggplot2::geom_segment(linewidth = 6, na.rm = TRUE) +
        ggplot2::labs(
          title = title,
          x = "Time",
          y = "Group",
          colour = "Phase"
        ) +
        ggplot2::theme_minimal()
    )
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .gp3_phase,
      y = n_rows,
      fill = .gp3_phase
    )
  ) +
    ggplot2::geom_col(na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(.gp3_group), scales = "free_y") +
    ggplot2::labs(
      title = title,
      x = "Phase",
      y = "Rows",
      fill = "Phase"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
