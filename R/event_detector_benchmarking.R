#' Create a manual event-review template
#'
#' Create a sequence-level CSV-ready template for manually annotating fixation
#' intervals. The returned table contains one or more placeholder rows per
#' participant/trial sequence together with the observed sequence boundaries.
#' Reviewers should fill `start_time` and `end_time`, set `review_status` to
#' `"accepted"`, and add rows when a sequence contains multiple events.
#'
#' @param data Sample-level gaze data.
#' @param id_col Participant identifier column.
#' @param trial_col Optional trial identifier column.
#' @param group_cols Optional additional sequence columns.
#' @param time_col Timestamp column.
#' @param rows_per_sequence Number of placeholder rows created per sequence.
#' @param event_type Event label inserted in the template.
#' @param reviewer Optional reviewer identifier.
#'
#' @return A data frame suitable for export to CSV and manual review.
#'
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   USER_ID = rep(c("P01", "P02"), each = 10),
#'   trial = rep("T01", 20),
#'   TIME = rep(seq(0, 0.09, by = 0.01), 2),
#'   FPOGX = 0.5,
#'   FPOGY = 0.5
#' )
#'
#' template <- create_gazepoint_event_review_template(
#'   gaze,
#'   trial_col = "trial"
#' )
#'
#' template
create_gazepoint_event_review_template <- function(
    data,
    id_col = "USER_ID",
    trial_col = NULL,
    group_cols = NULL,
    time_col = "TIME",
    rows_per_sequence = 1L,
    event_type = "fixation",
    reviewer = NA_character_) {

  .gp3_hp_assert_data_frame(data, "data")

  sequence_cols <- unique(c(
    id_col,
    trial_col,
    group_cols
  ))

  sequence_cols <- sequence_cols[
    !is.na(sequence_cols) &
      nzchar(sequence_cols)
  ]

  .gp3_hp_assert_columns(
    data,
    unique(c(sequence_cols, time_col)),
    "data"
  )

  if (!nrow(data)) {
    stop(
      "`data` must contain at least one sample.",
      call. = FALSE
    )
  }

  if (!is.numeric(rows_per_sequence) ||
      length(rows_per_sequence) != 1L ||
      is.na(rows_per_sequence) ||
      !is.finite(rows_per_sequence) ||
      rows_per_sequence < 1 ||
      rows_per_sequence != as.integer(rows_per_sequence)) {
    stop(
      "`rows_per_sequence` must be one positive integer.",
      call. = FALSE
    )
  }

  rows_per_sequence <- as.integer(rows_per_sequence)

  if (!is.character(event_type) ||
      length(event_type) != 1L ||
      is.na(event_type) ||
      !nzchar(event_type)) {
    stop(
      "`event_type` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (length(reviewer) != 1L) {
    stop(
      "`reviewer` must have length one.",
      call. = FALSE
    )
  }

  time_values <- suppressWarnings(
    as.numeric(data[[time_col]])
  )

  if (all(!is.finite(time_values))) {
    stop(
      paste0(
        "`", time_col,
        "` must contain at least one finite timestamp."
      ),
      call. = FALSE
    )
  }

  sequence_key <- .gp3_detector_sequence_key(
    data,
    sequence_cols
  )

  sequence_groups <- split(
    seq_len(nrow(data)),
    sequence_key,
    drop = TRUE
  )

  rows <- vector("list", length(sequence_groups))
  row_counter <- 0L

  for (idx in sequence_groups) {
    finite_time <- time_values[idx]
    finite_time <- finite_time[is.finite(finite_time)]

    if (!length(finite_time)) {
      next
    }

    row_counter <- row_counter + 1L

    sequence_values <- data[
      idx[[1L]],
      sequence_cols,
      drop = FALSE
    ]

    sequence_values <- sequence_values[
      rep(1L, rows_per_sequence),
      ,
      drop = FALSE
    ]

    rows[[row_counter]] <- cbind(
      sequence_values,
      data.frame(
        review_event_id = seq_len(rows_per_sequence),
        sequence_start = min(finite_time),
        sequence_end = max(finite_time),
        start_time = NA_real_,
        end_time = NA_real_,
        event_type = rep(event_type, rows_per_sequence),
        review_status = rep("pending", rows_per_sequence),
        reviewer = rep(as.character(reviewer), rows_per_sequence),
        notes = rep(NA_character_, rows_per_sequence),
        stringsAsFactors = FALSE
      )
    )
  }

  rows <- rows[seq_len(row_counter)]

  if (!length(rows)) {
    stop(
      "No sequence contained a finite timestamp.",
      call. = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Benchmark Gazepoint event detectors against reviewed events
#'
#' Compare standardized fixation intervals from one or more detectors with a
#' manually reviewed or synthetic reference-event table. Matching is one-to-one
#' within each participant/trial sequence and is based on interval
#' intersection-over-union. Results quantify methodological agreement with the
#' supplied reference annotations; they do not establish a universally correct
#' detector.
#'
#' @param x An object returned by [compare_gazepoint_event_detectors()] or a
#'   standardized detector-event data frame.
#' @param reviewed_events A data frame containing reviewed reference intervals.
#' @param sequence_cols Sequence identifier columns. When `x` is a comparison
#'   object, its stored sequence columns are used by default.
#' @param reviewed_start_col,reviewed_end_col Start and end columns in
#'   `reviewed_events`.
#' @param reviewed_id_col Optional reviewed-event identifier column.
#' @param review_status_col Optional review-status column. When present, only
#'   rows whose status is included in `accepted_status` are used.
#' @param accepted_status Character values treated as accepted reviews.
#' @param min_overlap Minimum interval intersection-over-union required for a
#'   true-positive match.
#' @param time_unit Unit used by event start/end values. This controls conversion
#'   of timing errors to milliseconds.
#'
#' @return An object of class `"gp3_event_detector_benchmark"` containing
#'   detector-level metrics, sequence-level metrics, one-to-one matches,
#'   unmatched-event diagnostics, accepted reviewed events, detector events,
#'   detector-run information, and settings.
#'
#' @export
#'
#' @examples
#' reviewed <- data.frame(
#'   USER_ID = "P01",
#'   trial = "T01",
#'   review_event_id = 1:2,
#'   start_time = c(0, 2),
#'   end_time = c(1, 3)
#' )
#'
#' detected <- data.frame(
#'   USER_ID = "P01",
#'   trial = "T01",
#'   detector = "velocity_10",
#'   family = "velocity",
#'   threshold = 10,
#'   event_id = 1:2,
#'   start_time = c(0.05, 2.05),
#'   end_time = c(1.05, 3.05),
#'   duration_ms = 1000
#' )
#'
#' benchmark <- benchmark_gazepoint_event_detectors(
#'   detected,
#'   reviewed,
#'   sequence_cols = c("USER_ID", "trial")
#' )
#'
#' benchmark$detector_metrics
benchmark_gazepoint_event_detectors <- function(
    x,
    reviewed_events,
    sequence_cols = NULL,
    reviewed_start_col = "start_time",
    reviewed_end_col = "end_time",
    reviewed_id_col = "review_event_id",
    review_status_col = "review_status",
    accepted_status = c(
      "accepted",
      "include",
      "reviewed",
      "confirmed"
    ),
    min_overlap = 0.5,
    time_unit = c("seconds", "milliseconds")) {

  time_unit <- match.arg(time_unit)

  if (!is.numeric(min_overlap) ||
      length(min_overlap) != 1L ||
      is.na(min_overlap) ||
      !is.finite(min_overlap) ||
      min_overlap < 0 ||
      min_overlap > 1) {
    stop(
      "`min_overlap` must be between 0 and 1.",
      call. = FALSE
    )
  }

  .gp3_hp_assert_data_frame(
    reviewed_events,
    "reviewed_events"
  )

  input <- .gp3_benchmark_extract_input(
    x,
    sequence_cols = sequence_cols
  )

  detected_events <- input$events
  sequence_cols <- input$sequence_cols
  detector_runs <- input$runs

  if (!length(sequence_cols)) {
    stop(
      "At least one sequence identifier column is required.",
      call. = FALSE
    )
  }

  time_scale <- if (identical(time_unit, "seconds")) {
    1000
  } else {
    1
  }

  detected_events <- .gp3_benchmark_prepare_detected(
    detected_events,
    sequence_cols = sequence_cols,
    time_scale = time_scale
  )

  reviewed_events <- .gp3_benchmark_prepare_reviewed(
    reviewed_events,
    sequence_cols = sequence_cols,
    reviewed_start_col = reviewed_start_col,
    reviewed_end_col = reviewed_end_col,
    reviewed_id_col = reviewed_id_col,
    review_status_col = review_status_col,
    accepted_status = accepted_status,
    time_scale = time_scale
  )

  detector_map <- .gp3_benchmark_detector_map(
    detected_events,
    detector_runs
  )

  detectors <- detector_map$detector

  if (!length(detectors)) {
    stop(
      "No successful detector was available for benchmarking.",
      call. = FALSE
    )
  }

  detected_key <- .gp3_detector_sequence_key(
    detected_events,
    sequence_cols
  )

  reviewed_key <- .gp3_detector_sequence_key(
    reviewed_events,
    sequence_cols
  )

  sequence_keys <- unique(c(
    detected_key,
    reviewed_key
  ))

  sequence_rows <- list()
  match_rows <- list()
  error_rows <- list()
  sequence_counter <- 0L
  match_counter <- 0L
  error_counter <- 0L

  for (detector_name in detectors) {
    detector_info <- detector_map[
      detector_map$detector == detector_name,
      ,
      drop = FALSE
    ]

    detector_block <- detected_events[
      detected_events$detector == detector_name,
      ,
      drop = FALSE
    ]

    detector_block_key <- .gp3_detector_sequence_key(
      detector_block,
      sequence_cols
    )

    for (sequence_key in sequence_keys) {
      detected_block <- detector_block[
        detector_block_key == sequence_key,
        ,
        drop = FALSE
      ]

      reviewed_block <- reviewed_events[
        reviewed_key == sequence_key,
        ,
        drop = FALSE
      ]

      if (!nrow(detected_block) && !nrow(reviewed_block)) {
        next
      }

      sequence_values <- .gp3_benchmark_sequence_values(
        detected_block,
        reviewed_block,
        sequence_cols
      )

      pairs <- .gp3_benchmark_greedy_matches(
        detected_block,
        reviewed_block,
        min_overlap = min_overlap
      )

      matched_detected <- pairs$detected_index
      matched_reviewed <- pairs$reviewed_index

      true_positive <- nrow(pairs)
      false_positive <- nrow(detected_block) - true_positive
      false_negative <- nrow(reviewed_block) - true_positive

      precision <- .gp3_benchmark_ratio(
        true_positive,
        true_positive + false_positive
      )

      recall <- .gp3_benchmark_ratio(
        true_positive,
        true_positive + false_negative
      )

      f1 <- .gp3_benchmark_ratio(
        2 * true_positive,
        2 * true_positive + false_positive + false_negative
      )

      pair_metrics <- .gp3_benchmark_pair_metrics(
        pairs,
        detected_block,
        reviewed_block,
        time_scale = time_scale
      )

      sequence_counter <- sequence_counter + 1L

      sequence_rows[[sequence_counter]] <- cbind(
        sequence_values,
        data.frame(
          detector = detector_name,
          family = detector_info$family[[1L]],
          threshold = detector_info$threshold[[1L]],
          n_reviewed = nrow(reviewed_block),
          n_detected = nrow(detected_block),
          true_positive = true_positive,
          false_positive = false_positive,
          false_negative = false_negative,
          precision = precision,
          recall = recall,
          f1 = f1,
          mean_iou = pair_metrics$mean_iou,
          median_iou = pair_metrics$median_iou,
          mean_onset_error_ms = pair_metrics$mean_onset_error_ms,
          mean_abs_onset_error_ms = pair_metrics$mean_abs_onset_error_ms,
          mean_offset_error_ms = pair_metrics$mean_offset_error_ms,
          mean_abs_offset_error_ms = pair_metrics$mean_abs_offset_error_ms,
          mean_duration_error_ms = pair_metrics$mean_duration_error_ms,
          mean_abs_duration_error_ms = pair_metrics$mean_abs_duration_error_ms,
          detection_count_bias = nrow(detected_block) - nrow(reviewed_block),
          min_overlap = min_overlap,
          stringsAsFactors = FALSE
        )
      )

      if (nrow(pairs)) {
        for (pair_i in seq_len(nrow(pairs))) {
          detected_i <- pairs$detected_index[[pair_i]]
          reviewed_i <- pairs$reviewed_index[[pair_i]]

          detected_row <- detected_block[
            detected_i,
            ,
            drop = FALSE
          ]

          reviewed_row <- reviewed_block[
            reviewed_i,
            ,
            drop = FALSE
          ]

          onset_error_ms <- (
            detected_row$start_time -
              reviewed_row$review_start_time
          ) * time_scale

          offset_error_ms <- (
            detected_row$end_time -
              reviewed_row$review_end_time
          ) * time_scale

          duration_error_ms <- (
            detected_row$duration_ms -
              reviewed_row$review_duration_ms
          )

          match_counter <- match_counter + 1L

          match_rows[[match_counter]] <- cbind(
            sequence_values,
            data.frame(
              detector = detector_name,
              family = detector_info$family[[1L]],
              threshold = detector_info$threshold[[1L]],
              detected_event_id = detected_row$event_id,
              review_event_id = reviewed_row$review_event_id,
              detected_start_time = detected_row$start_time,
              detected_end_time = detected_row$end_time,
              reviewed_start_time = reviewed_row$review_start_time,
              reviewed_end_time = reviewed_row$review_end_time,
              iou = pairs$iou[[pair_i]],
              onset_error_ms = onset_error_ms,
              abs_onset_error_ms = abs(onset_error_ms),
              offset_error_ms = offset_error_ms,
              abs_offset_error_ms = abs(offset_error_ms),
              duration_error_ms = duration_error_ms,
              abs_duration_error_ms = abs(duration_error_ms),
              stringsAsFactors = FALSE
            )
          )
        }
      }

      unmatched_detected <- setdiff(
        seq_len(nrow(detected_block)),
        matched_detected
      )

      if (length(unmatched_detected)) {
        for (detected_i in unmatched_detected) {
          detected_row <- detected_block[
            detected_i,
            ,
            drop = FALSE
          ]

          error_counter <- error_counter + 1L

          error_rows[[error_counter]] <- cbind(
            sequence_values,
            data.frame(
              detector = detector_name,
              family = detector_info$family[[1L]],
              threshold = detector_info$threshold[[1L]],
              error_type = "false_positive",
              detected_event_id = detected_row$event_id,
              review_event_id = NA_integer_,
              start_time = detected_row$start_time,
              end_time = detected_row$end_time,
              duration_ms = detected_row$duration_ms,
              stringsAsFactors = FALSE
            )
          )
        }
      }

      unmatched_reviewed <- setdiff(
        seq_len(nrow(reviewed_block)),
        matched_reviewed
      )

      if (length(unmatched_reviewed)) {
        for (reviewed_i in unmatched_reviewed) {
          reviewed_row <- reviewed_block[
            reviewed_i,
            ,
            drop = FALSE
          ]

          error_counter <- error_counter + 1L

          error_rows[[error_counter]] <- cbind(
            sequence_values,
            data.frame(
              detector = detector_name,
              family = detector_info$family[[1L]],
              threshold = detector_info$threshold[[1L]],
              error_type = "false_negative",
              detected_event_id = NA_integer_,
              review_event_id = reviewed_row$review_event_id,
              start_time = reviewed_row$review_start_time,
              end_time = reviewed_row$review_end_time,
              duration_ms = reviewed_row$review_duration_ms,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }

  sequence_metrics <- .gp3_benchmark_bind_rows(
    sequence_rows,
    .gp3_benchmark_empty_sequence_metrics(sequence_cols)
  )

  matches <- .gp3_benchmark_bind_rows(
    match_rows,
    .gp3_benchmark_empty_matches(sequence_cols)
  )

  errors <- .gp3_benchmark_bind_rows(
    error_rows,
    .gp3_benchmark_empty_errors(sequence_cols)
  )

  detector_metrics <- .gp3_benchmark_detector_metrics(
    sequence_metrics,
    matches,
    detector_map
  )

  out <- list(
    detector_metrics = detector_metrics,
    sequence_metrics = sequence_metrics,
    matches = matches,
    errors = errors,
    reviewed_events = reviewed_events,
    detected_events = detected_events,
    runs = detector_runs,
    settings = list(
      sequence_cols = sequence_cols,
      reviewed_start_col = reviewed_start_col,
      reviewed_end_col = reviewed_end_col,
      reviewed_id_col = reviewed_id_col,
      review_status_col = review_status_col,
      accepted_status = accepted_status,
      min_overlap = min_overlap,
      time_unit = time_unit
    )
  )

  class(out) <- c(
    "gp3_event_detector_benchmark",
    "list"
  )

  out
}

#' Summarise an event-detector benchmark
#'
#' Extract detector-level, sequence-level, matched-event, or unmatched-event
#' tables from an event-detector benchmark.
#'
#' @param x An object returned by [benchmark_gazepoint_event_detectors()].
#' @param level Summary level to return.
#' @param sort Should detector summaries be ordered by decreasing F1 score?
#'
#' @return A data frame.
#'
#' @export
summarise_gazepoint_event_detector_benchmark <- function(
    x,
    level = c("detector", "sequence", "matches", "errors"),
    sort = TRUE) {

  if (!inherits(x, "gp3_event_detector_benchmark")) {
    stop(
      paste0(
        "`x` must be returned by ",
        "`benchmark_gazepoint_event_detectors()`."
      ),
      call. = FALSE
    )
  }

  level <- match.arg(level)

  if (!is.logical(sort) ||
      length(sort) != 1L ||
      is.na(sort)) {
    stop(
      "`sort` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  out <- switch(
    level,
    detector = x$detector_metrics,
    sequence = x$sequence_metrics,
    matches = x$matches,
    errors = x$errors
  )

  if (identical(level, "detector") &&
      isTRUE(sort) &&
      nrow(out)) {
    order_value <- out$f1
    order_value[!is.finite(order_value)] <- -Inf

    out <- out[
      order(-order_value, out$detector),
      ,
      drop = FALSE
    ]

    rownames(out) <- NULL
  }

  out
}

#' Plot event-detector benchmark diagnostics
#'
#' @param x An object returned by [benchmark_gazepoint_event_detectors()].
#' @param plot Plot type: `"f1"`, `"precision_recall"`, `"overlap"`,
#'   `"timing_error"`, or `"counts"`.
#' @param main Optional plot title.
#' @param ylab Optional vertical-axis label.
#' @param las Axis-label orientation passed to base graphics.
#'
#' @return Invisibly returns the detector-level data used by the plot.
#'
#' @export
plot_gazepoint_event_detector_benchmark <- function(
    x,
    plot = c(
      "f1",
      "precision_recall",
      "overlap",
      "timing_error",
      "counts"
    ),
    main = NULL,
    ylab = NULL,
    las = 2) {

  if (!inherits(x, "gp3_event_detector_benchmark")) {
    stop(
      paste0(
        "`x` must be returned by ",
        "`benchmark_gazepoint_event_detectors()`."
      ),
      call. = FALSE
    )
  }

  plot <- match.arg(plot)
  plot_data <- x$detector_metrics

  if (!nrow(plot_data)) {
    stop(
      "No detector-level benchmark data are available to plot.",
      call. = FALSE
    )
  }

  detector_names <- plot_data$detector

  if (identical(plot, "f1")) {
    values <- plot_data$f1
    values[!is.finite(values)] <- 0

    if (is.null(main)) {
      main <- "Event-level F1 by detector"
    }

    if (is.null(ylab)) {
      ylab <- "F1 score"
    }

    graphics::barplot(
      height = values,
      names.arg = detector_names,
      ylim = c(0, 1),
      las = las,
      main = main,
      ylab = ylab
    )
  }

  if (identical(plot, "precision_recall")) {
    values <- rbind(
      precision = plot_data$precision,
      recall = plot_data$recall
    )

    values[!is.finite(values)] <- 0

    if (is.null(main)) {
      main <- "Precision and recall by detector"
    }

    if (is.null(ylab)) {
      ylab <- "Proportion"
    }

    graphics::barplot(
      height = values,
      names.arg = detector_names,
      beside = TRUE,
      ylim = c(0, 1),
      las = las,
      main = main,
      ylab = ylab
    )

    graphics::legend(
      "topright",
      legend = rownames(values),
      fill = seq_len(nrow(values)),
      bty = "n"
    )
  }

  if (identical(plot, "overlap")) {
    values <- plot_data$mean_iou
    values[!is.finite(values)] <- 0

    if (is.null(main)) {
      main <- "Mean matched-event overlap"
    }

    if (is.null(ylab)) {
      ylab <- "Mean intersection-over-union"
    }

    graphics::barplot(
      height = values,
      names.arg = detector_names,
      ylim = c(0, 1),
      las = las,
      main = main,
      ylab = ylab
    )
  }

  if (identical(plot, "timing_error")) {
    values <- rbind(
      onset = plot_data$mean_abs_onset_error_ms,
      offset = plot_data$mean_abs_offset_error_ms,
      duration = plot_data$mean_abs_duration_error_ms
    )

    values[!is.finite(values)] <- 0

    if (is.null(main)) {
      main <- "Absolute timing error by detector"
    }

    if (is.null(ylab)) {
      ylab <- "Mean absolute error (ms)"
    }

    graphics::barplot(
      height = values,
      names.arg = detector_names,
      beside = TRUE,
      las = las,
      main = main,
      ylab = ylab
    )

    graphics::legend(
      "topright",
      legend = rownames(values),
      fill = seq_len(nrow(values)),
      bty = "n"
    )
  }

  if (identical(plot, "counts")) {
    values <- rbind(
      reviewed = plot_data$n_reviewed,
      detected = plot_data$n_detected
    )

    if (is.null(main)) {
      main <- "Reviewed and detected event counts"
    }

    if (is.null(ylab)) {
      ylab <- "Number of events"
    }

    graphics::barplot(
      height = values,
      names.arg = detector_names,
      beside = TRUE,
      las = las,
      main = main,
      ylab = ylab
    )

    graphics::legend(
      "topright",
      legend = rownames(values),
      fill = seq_len(nrow(values)),
      bty = "n"
    )
  }

  invisible(plot_data)
}

.gp3_benchmark_extract_input <- function(x, sequence_cols = NULL) {
  if (inherits(x, "gp3_event_detector_comparison")) {
    events <- x$events

    if (is.null(sequence_cols)) {
      sequence_cols <- x$settings$sequence_cols
    }

    runs <- x$runs
  } else if (is.data.frame(x)) {
    events <- x
    runs <- NULL

    if (is.null(sequence_cols)) {
      standard_columns <- c(
        "detector",
        "family",
        "threshold",
        "event_id",
        "start_time",
        "end_time",
        "duration_ms",
        "mean_x",
        "mean_y",
        "n_samples",
        "source_status"
      )

      sequence_cols <- setdiff(
        names(events),
        standard_columns
      )
    }
  } else {
    stop(
      paste0(
        "`x` must be a detector-comparison object or a ",
        "standardized event data frame."
      ),
      call. = FALSE
    )
  }

  .gp3_hp_assert_data_frame(events, "detector events")

  list(
    events = events,
    sequence_cols = unique(as.character(sequence_cols)),
    runs = runs
  )
}

.gp3_benchmark_prepare_detected <- function(
    events,
    sequence_cols,
    time_scale) {

  required <- c(
    sequence_cols,
    "detector",
    "start_time",
    "end_time"
  )

  .gp3_hp_assert_columns(
    events,
    required,
    "detector events"
  )

  out <- events

  out$detector <- as.character(out$detector)

  if (anyNA(out$detector) ||
      any(!nzchar(out$detector))) {
    stop(
      "Detector names must be non-missing and non-empty.",
      call. = FALSE
    )
  }

  out$start_time <- suppressWarnings(
    as.numeric(out$start_time)
  )

  out$end_time <- suppressWarnings(
    as.numeric(out$end_time)
  )

  invalid_interval <- !is.finite(out$start_time) |
    !is.finite(out$end_time) |
    out$end_time <= out$start_time

  if (any(invalid_interval)) {
    stop(
      paste0(
        "Detector events must contain finite intervals with ",
        "`end_time > start_time`."
      ),
      call. = FALSE
    )
  }

  if (!"family" %in% names(out)) {
    out$family <- NA_character_
  }

  out$family <- as.character(out$family)

  if (!"threshold" %in% names(out)) {
    out$threshold <- NA_real_
  }

  out$threshold <- suppressWarnings(
    as.numeric(out$threshold)
  )

  if (!"event_id" %in% names(out)) {
    out$event_id <- NA_integer_
  }

  event_key <- paste(
    out$detector,
    .gp3_detector_sequence_key(out, sequence_cols),
    sep = "\r"
  )

  event_groups <- split(
    seq_len(nrow(out)),
    event_key,
    drop = TRUE
  )

  for (idx in event_groups) {
    current <- suppressWarnings(
      as.integer(out$event_id[idx])
    )

    if (anyNA(current) ||
        anyDuplicated(current)) {
      out$event_id[idx] <- seq_along(idx)
    } else {
      out$event_id[idx] <- current
    }
  }

  out$event_id <- as.integer(out$event_id)

  if (!"duration_ms" %in% names(out)) {
    out$duration_ms <- (
      out$end_time - out$start_time
    ) * time_scale
  }

  out$duration_ms <- suppressWarnings(
    as.numeric(out$duration_ms)
  )

  invalid_duration <- !is.finite(out$duration_ms) |
    out$duration_ms <= 0

  out$duration_ms[invalid_duration] <- (
    out$end_time[invalid_duration] -
      out$start_time[invalid_duration]
  ) * time_scale

  rownames(out) <- NULL
  out
}

.gp3_benchmark_prepare_reviewed <- function(
    reviewed_events,
    sequence_cols,
    reviewed_start_col,
    reviewed_end_col,
    reviewed_id_col,
    review_status_col,
    accepted_status,
    time_scale) {

  .gp3_hp_assert_columns(
    reviewed_events,
    unique(c(
      sequence_cols,
      reviewed_start_col,
      reviewed_end_col
    )),
    "reviewed_events"
  )

  if (!is.character(accepted_status) ||
      !length(accepted_status) ||
      anyNA(accepted_status) ||
      any(!nzchar(accepted_status))) {
    stop(
      "`accepted_status` must contain non-empty character values.",
      call. = FALSE
    )
  }

  out <- reviewed_events

  if (!is.null(review_status_col) &&
      length(review_status_col) == 1L &&
      !is.na(review_status_col) &&
      nzchar(review_status_col) &&
      review_status_col %in% names(out)) {

    status <- tolower(trimws(
      as.character(out[[review_status_col]])
    ))

    accepted <- status %in% tolower(accepted_status)
    out <- out[accepted, , drop = FALSE]
  }

  if (!nrow(out)) {
    stop(
      "No accepted reviewed events were available.",
      call. = FALSE
    )
  }

  out$review_start_time <- suppressWarnings(
    as.numeric(out[[reviewed_start_col]])
  )

  out$review_end_time <- suppressWarnings(
    as.numeric(out[[reviewed_end_col]])
  )

  invalid_interval <- !is.finite(out$review_start_time) |
    !is.finite(out$review_end_time) |
    out$review_end_time <= out$review_start_time

  if (any(invalid_interval)) {
    stop(
      paste0(
        "Accepted reviewed events must contain finite intervals ",
        "with end values greater than start values."
      ),
      call. = FALSE
    )
  }

  if (!is.null(reviewed_id_col) &&
      length(reviewed_id_col) == 1L &&
      !is.na(reviewed_id_col) &&
      nzchar(reviewed_id_col) &&
      reviewed_id_col %in% names(out)) {

    out$review_event_id <- suppressWarnings(
      as.integer(out[[reviewed_id_col]])
    )
  } else {
    out$review_event_id <- NA_integer_
  }

  review_key <- .gp3_detector_sequence_key(
    out,
    sequence_cols
  )

  review_groups <- split(
    seq_len(nrow(out)),
    review_key,
    drop = TRUE
  )

  for (idx in review_groups) {
    current <- out$review_event_id[idx]

    if (anyNA(current) ||
        anyDuplicated(current)) {
      out$review_event_id[idx] <- seq_along(idx)
    }
  }

  out$review_event_id <- as.integer(
    out$review_event_id
  )

  out$review_duration_ms <- (
    out$review_end_time -
      out$review_start_time
  ) * time_scale

  keep <- unique(c(
    sequence_cols,
    "review_event_id",
    "review_start_time",
    "review_end_time",
    "review_duration_ms",
    intersect(
      c(
        "event_type",
        review_status_col,
        "reviewer",
        "notes"
      ),
      names(out)
    )
  ))

  out <- out[, keep, drop = FALSE]
  rownames(out) <- NULL
  out
}

.gp3_benchmark_detector_map <- function(events, runs = NULL) {
  if (nrow(events)) {
    event_map <- events[
      !duplicated(events$detector),
      c("detector", "family", "threshold"),
      drop = FALSE
    ]
  } else {
    event_map <- data.frame(
      detector = character(),
      family = character(),
      threshold = numeric(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(runs) &&
      is.data.frame(runs) &&
      all(c("detector", "family", "status") %in% names(runs))) {

    successful_runs <- runs[
      runs$status == "ok",
      c("detector", "family"),
      drop = FALSE
    ]

    successful_runs <- successful_runs[
      !duplicated(successful_runs$detector),
      ,
      drop = FALSE
    ]

    missing_detector <- setdiff(
      successful_runs$detector,
      event_map$detector
    )

    if (length(missing_detector)) {
      missing_rows <- successful_runs[
        successful_runs$detector %in% missing_detector,
        ,
        drop = FALSE
      ]

      missing_rows$threshold <- NA_real_
      event_map <- rbind(event_map, missing_rows)
    }
  }

  rownames(event_map) <- NULL
  event_map
}

.gp3_benchmark_sequence_values <- function(
    detected,
    reviewed,
    sequence_cols) {

  if (nrow(detected)) {
    return(detected[1L, sequence_cols, drop = FALSE])
  }

  reviewed[1L, sequence_cols, drop = FALSE]
}

.gp3_benchmark_greedy_matches <- function(
    detected,
    reviewed,
    min_overlap) {

  empty <- data.frame(
    detected_index = integer(),
    reviewed_index = integer(),
    iou = numeric(),
    stringsAsFactors = FALSE
  )

  if (!nrow(detected) || !nrow(reviewed)) {
    return(empty)
  }

  intersection <- outer(
    detected$end_time,
    reviewed$review_end_time,
    pmin
  ) - outer(
    detected$start_time,
    reviewed$review_start_time,
    pmax
  )

  intersection <- pmax(intersection, 0)

  union <- outer(
    detected$end_time,
    reviewed$review_end_time,
    pmax
  ) - outer(
    detected$start_time,
    reviewed$review_start_time,
    pmin
  )

  iou <- intersection / union
  iou[!is.finite(iou)] <- 0

  candidate <- which(
    iou >= min_overlap,
    arr.ind = TRUE
  )

  if (!nrow(candidate)) {
    return(empty)
  }

  candidate <- data.frame(
    detected_index = candidate[, "row"],
    reviewed_index = candidate[, "col"],
    iou = iou[candidate],
    stringsAsFactors = FALSE
  )

  candidate <- candidate[
    order(
      -candidate$iou,
      candidate$detected_index,
      candidate$reviewed_index
    ),
    ,
    drop = FALSE
  ]

  used_detected <- rep(FALSE, nrow(detected))
  used_reviewed <- rep(FALSE, nrow(reviewed))
  keep <- logical(nrow(candidate))

  for (i in seq_len(nrow(candidate))) {
    detected_i <- candidate$detected_index[[i]]
    reviewed_i <- candidate$reviewed_index[[i]]

    if (!used_detected[[detected_i]] &&
        !used_reviewed[[reviewed_i]]) {
      keep[[i]] <- TRUE
      used_detected[[detected_i]] <- TRUE
      used_reviewed[[reviewed_i]] <- TRUE
    }
  }

  out <- candidate[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.gp3_benchmark_pair_metrics <- function(
    pairs,
    detected,
    reviewed,
    time_scale) {

  empty_value <- list(
    mean_iou = NA_real_,
    median_iou = NA_real_,
    mean_onset_error_ms = NA_real_,
    mean_abs_onset_error_ms = NA_real_,
    mean_offset_error_ms = NA_real_,
    mean_abs_offset_error_ms = NA_real_,
    mean_duration_error_ms = NA_real_,
    mean_abs_duration_error_ms = NA_real_
  )

  if (!nrow(pairs)) {
    return(empty_value)
  }

  detected_match <- detected[
    pairs$detected_index,
    ,
    drop = FALSE
  ]

  reviewed_match <- reviewed[
    pairs$reviewed_index,
    ,
    drop = FALSE
  ]

  onset_error_ms <- (
    detected_match$start_time -
      reviewed_match$review_start_time
  ) * time_scale

  offset_error_ms <- (
    detected_match$end_time -
      reviewed_match$review_end_time
  ) * time_scale

  duration_error_ms <- (
    detected_match$duration_ms -
      reviewed_match$review_duration_ms
  )

  list(
    mean_iou = mean(pairs$iou),
    median_iou = stats::median(pairs$iou),
    mean_onset_error_ms = mean(onset_error_ms),
    mean_abs_onset_error_ms = mean(abs(onset_error_ms)),
    mean_offset_error_ms = mean(offset_error_ms),
    mean_abs_offset_error_ms = mean(abs(offset_error_ms)),
    mean_duration_error_ms = mean(duration_error_ms),
    mean_abs_duration_error_ms = mean(abs(duration_error_ms))
  )
}

.gp3_benchmark_ratio <- function(numerator, denominator) {
  if (!is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }

  numerator / denominator
}

.gp3_benchmark_detector_metrics <- function(
    sequence_metrics,
    matches,
    detector_map) {

  rows <- vector("list", nrow(detector_map))

  for (i in seq_len(nrow(detector_map))) {
    detector_name <- detector_map$detector[[i]]

    metric_block <- sequence_metrics[
      sequence_metrics$detector == detector_name,
      ,
      drop = FALSE
    ]

    match_block <- matches[
      matches$detector == detector_name,
      ,
      drop = FALSE
    ]

    n_reviewed <- sum(metric_block$n_reviewed)
    n_detected <- sum(metric_block$n_detected)
    true_positive <- sum(metric_block$true_positive)
    false_positive <- sum(metric_block$false_positive)
    false_negative <- sum(metric_block$false_negative)

    rows[[i]] <- data.frame(
      detector = detector_name,
      family = detector_map$family[[i]],
      threshold = detector_map$threshold[[i]],
      n_sequences = nrow(metric_block),
      n_reviewed = n_reviewed,
      n_detected = n_detected,
      true_positive = true_positive,
      false_positive = false_positive,
      false_negative = false_negative,
      precision = .gp3_benchmark_ratio(
        true_positive,
        true_positive + false_positive
      ),
      recall = .gp3_benchmark_ratio(
        true_positive,
        true_positive + false_negative
      ),
      f1 = .gp3_benchmark_ratio(
        2 * true_positive,
        2 * true_positive + false_positive + false_negative
      ),
      mean_iou = .gp3_benchmark_mean(match_block$iou),
      median_iou = .gp3_benchmark_median(match_block$iou),
      mean_onset_error_ms = .gp3_benchmark_mean(
        match_block$onset_error_ms
      ),
      mean_abs_onset_error_ms = .gp3_benchmark_mean(
        match_block$abs_onset_error_ms
      ),
      mean_offset_error_ms = .gp3_benchmark_mean(
        match_block$offset_error_ms
      ),
      mean_abs_offset_error_ms = .gp3_benchmark_mean(
        match_block$abs_offset_error_ms
      ),
      mean_duration_error_ms = .gp3_benchmark_mean(
        match_block$duration_error_ms
      ),
      mean_abs_duration_error_ms = .gp3_benchmark_mean(
        match_block$abs_duration_error_ms
      ),
      detection_count_bias = n_detected - n_reviewed,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.gp3_benchmark_mean <- function(x) {
  x <- x[is.finite(x)]

  if (!length(x)) {
    return(NA_real_)
  }

  mean(x)
}

.gp3_benchmark_median <- function(x) {
  x <- x[is.finite(x)]

  if (!length(x)) {
    return(NA_real_)
  }

  stats::median(x)
}

.gp3_benchmark_bind_rows <- function(rows, empty) {
  if (!length(rows)) {
    return(empty)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.gp3_benchmark_empty_sequence_metrics <- function(sequence_cols) {
  sequence_template <- as.data.frame(
    stats::setNames(
      replicate(
        length(sequence_cols),
        character(),
        simplify = FALSE
      ),
      sequence_cols
    ),
    stringsAsFactors = FALSE
  )

  cbind(
    sequence_template,
    data.frame(
      detector = character(),
      family = character(),
      threshold = numeric(),
      n_reviewed = integer(),
      n_detected = integer(),
      true_positive = integer(),
      false_positive = integer(),
      false_negative = integer(),
      precision = numeric(),
      recall = numeric(),
      f1 = numeric(),
      mean_iou = numeric(),
      median_iou = numeric(),
      mean_onset_error_ms = numeric(),
      mean_abs_onset_error_ms = numeric(),
      mean_offset_error_ms = numeric(),
      mean_abs_offset_error_ms = numeric(),
      mean_duration_error_ms = numeric(),
      mean_abs_duration_error_ms = numeric(),
      detection_count_bias = integer(),
      min_overlap = numeric(),
      stringsAsFactors = FALSE
    )
  )
}

.gp3_benchmark_empty_matches <- function(sequence_cols) {
  sequence_template <- .gp3_benchmark_empty_sequence_metrics(
    sequence_cols
  )

  sequence_template <- sequence_template[
    0,
    sequence_cols,
    drop = FALSE
  ]

  cbind(
    sequence_template,
    data.frame(
      detector = character(),
      family = character(),
      threshold = numeric(),
      detected_event_id = integer(),
      review_event_id = integer(),
      detected_start_time = numeric(),
      detected_end_time = numeric(),
      reviewed_start_time = numeric(),
      reviewed_end_time = numeric(),
      iou = numeric(),
      onset_error_ms = numeric(),
      abs_onset_error_ms = numeric(),
      offset_error_ms = numeric(),
      abs_offset_error_ms = numeric(),
      duration_error_ms = numeric(),
      abs_duration_error_ms = numeric(),
      stringsAsFactors = FALSE
    )
  )
}

.gp3_benchmark_empty_errors <- function(sequence_cols) {
  sequence_template <- .gp3_benchmark_empty_sequence_metrics(
    sequence_cols
  )

  sequence_template <- sequence_template[
    0,
    sequence_cols,
    drop = FALSE
  ]

  cbind(
    sequence_template,
    data.frame(
      detector = character(),
      family = character(),
      threshold = numeric(),
      error_type = character(),
      detected_event_id = integer(),
      review_event_id = integer(),
      start_time = numeric(),
      end_time = numeric(),
      duration_ms = numeric(),
      stringsAsFactors = FALSE
    )
  )
}
