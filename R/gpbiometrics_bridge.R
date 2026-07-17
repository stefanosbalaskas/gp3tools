#' Prepare gp3tools gaze output for a gpbiometrics workflow
#'
#' Standardises participant, trial, time, gaze, AOI, pupil, and validity fields
#' without changing the underlying measurements.
#'
#' @param gaze_data Sample-level gp3tools gaze/master data.
#' @param participant_col,trial_col,time_col Explicit source columns.
#' @param time_unit Time unit for the source time column.
#' @param x_col,y_col,aoi_col,pupil_col,validity_col Optional source columns.
#' @param keep_cols Additional source columns to retain.
#'
#' @return A data frame of class `"gazepoint_gpbiometrics_bridge"`.
#' @export
prepare_gazepoint_gpbiometrics_bridge <- function(
    gaze_data,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    time_unit = c("auto", "seconds", "milliseconds"),
    x_col = NULL,
    y_col = NULL,
    aoi_col = NULL,
    pupil_col = NULL,
    validity_col = NULL,
    keep_cols = NULL) {
  time_unit <- match.arg(time_unit)
  if (!is.data.frame(gaze_data) || nrow(gaze_data) == 0L) {
    stop(
      "`gaze_data` must be a non-empty data frame.",
      call. = FALSE
    )
  }

  participant_col <- .gp3_bridge_resolve_col(
    gaze_data,
    participant_col,
    c(
      "participant_id", "participant", "subject", "subject_id",
      "USER_ID", "USER", "user_id"
    ),
    "participant",
    TRUE
  )
  trial_col <- .gp3_bridge_resolve_col(
    gaze_data,
    trial_col,
    c(
      "trial_id", "trial", "TRIAL_ID", "MEDIA_ID",
      "media_id", "stimulus_id"
    ),
    "trial",
    TRUE
  )
  time_col <- .gp3_bridge_resolve_col(
    gaze_data,
    time_col,
    c(
      "time_s", "time_ms", "TIME", "MSTIMER", "timestamp",
      "timestamp_s", "timestamp_ms"
    ),
    "time",
    TRUE
  )
  x_col <- .gp3_bridge_resolve_col(
    gaze_data,
    x_col,
    c("gaze_x", "BPOGX", "FPOGX", "LPOGX", "RPOGX", "x"),
    "horizontal gaze",
    FALSE
  )
  y_col <- .gp3_bridge_resolve_col(
    gaze_data,
    y_col,
    c("gaze_y", "BPOGY", "FPOGY", "LPOGY", "RPOGY", "y"),
    "vertical gaze",
    FALSE
  )
  aoi_col <- .gp3_bridge_resolve_col(
    gaze_data,
    aoi_col,
    c("aoi", "AOI", "aoi_label", "region", "roi"),
    "AOI",
    FALSE
  )
  pupil_col <- .gp3_bridge_resolve_col(
    gaze_data,
    pupil_col,
    c(
      "pupil", "pupil_mean", "pupil_combined", "LPD", "RPD",
      "pupil_left", "pupil_right"
    ),
    "pupil",
    FALSE
  )
  validity_col <- .gp3_bridge_resolve_col(
    gaze_data,
    validity_col,
    c(
      "gaze_valid", "valid", "BPOGV", "FPOGV", "LPOGV",
      "RPOGV"
    ),
    "validity",
    FALSE
  )

  keep_cols <- .gp3_bridge_existing_cols(
    gaze_data,
    keep_cols,
    "keep_cols"
  )
  resolved_unit <- .gp3_bridge_time_unit(
    gaze_data[[time_col]],
    time_col,
    time_unit
  )

  participant_id <- as.character(gaze_data[[participant_col]])
  trial_id <- as.character(gaze_data[[trial_col]])
  time_numeric <- suppressWarnings(as.numeric(gaze_data[[time_col]]))
  time_s <- if (identical(resolved_unit, "milliseconds")) {
    time_numeric / 1000
  } else {
    time_numeric
  }

  out <- data.frame(
    participant_id = participant_id,
    trial_id = trial_id,
    time_s = time_s,
    gaze_x = .gp3_bridge_numeric_or_na(gaze_data, x_col),
    gaze_y = .gp3_bridge_numeric_or_na(gaze_data, y_col),
    aoi = .gp3_bridge_character_or_na(gaze_data, aoi_col),
    pupil = .gp3_bridge_numeric_or_na(gaze_data, pupil_col),
    gaze_valid = .gp3_bridge_validity_or_na(gaze_data, validity_col),
    stringsAsFactors = FALSE
  )

  if (length(keep_cols) > 0L) {
    duplicated_names <- intersect(names(out), keep_cols)
    keep_names <- keep_cols
    keep_names[keep_names %in% duplicated_names] <- paste0(
      keep_names[keep_names %in% duplicated_names],
      "_source"
    )
    retained <- gaze_data[keep_cols]
    names(retained) <- keep_names
    out <- cbind(out, retained)
  }

  audit <- list(
    source_rows = nrow(gaze_data),
    output_rows = nrow(out),
    finite_time_rows = sum(is.finite(out$time_s)),
    valid_gaze_rows = sum(out$gaze_valid %in% TRUE, na.rm = TRUE),
    source_columns = list(
      participant = participant_col,
      trial = trial_col,
      time = time_col,
      x = x_col,
      y = y_col,
      aoi = aoi_col,
      pupil = pupil_col,
      validity = validity_col,
      retained = keep_cols
    ),
    time_unit = resolved_unit
  )

  structure(
    out,
    class = c("gazepoint_gpbiometrics_bridge", "data.frame"),
    audit = audit
  )
}

#' Run a tested gp3tools-to-gpbiometrics integration workflow
#'
#' Aligns gp3tools gaze output with an already imported biometric data frame,
#' computes AOI/event-contingent signal summaries, and returns a combined audit
#' and cautious report text. A user-supplied adapter can replace the native
#' nearest-timestamp join without making gpbiometrics a mandatory dependency.
#'
#' @param gaze_data Raw gp3tools gaze/master data or a prepared bridge.
#' @param biometrics_data Biometric samples, including participant, trial, time,
#'   and one or more numeric signal columns.
#' @param gaze_args Named list forwarded to
#'   [prepare_gazepoint_gpbiometrics_bridge()].
#' @param biometric_participant_col,biometric_trial_col,biometric_time_col
#'   Explicit biometric source columns.
#' @param biometric_time_unit Unit for the biometric time column.
#' @param signal_cols Numeric biometric signals to summarise.
#' @param event_col Optional biometric event column.
#' @param tolerance_s Maximum absolute time difference for a match. When `NULL`,
#'   it is estimated conservatively from biometric sampling intervals.
#' @param adapter Optional function with arguments `gaze`, `biometrics`, and
#'   `tolerance_s`; it must return a synchronized data frame.
#' @param include_unmatched Retain unmatched gaze rows.
#'
#' @return A `"gazepoint_cross_package_workflow"` object.
#' @export
run_gazepoint_gpbiometrics_workflow <- function(
    gaze_data,
    biometrics_data,
    gaze_args = list(),
    biometric_participant_col = NULL,
    biometric_trial_col = NULL,
    biometric_time_col = NULL,
    biometric_time_unit = c("auto", "seconds", "milliseconds"),
    signal_cols = NULL,
    event_col = NULL,
    tolerance_s = NULL,
    adapter = NULL,
    include_unmatched = TRUE) {
  biometric_time_unit <- match.arg(biometric_time_unit)
  .gp3_bridge_logical(include_unmatched, "include_unmatched")

  gaze <- if (inherits(gaze_data, "gazepoint_gpbiometrics_bridge")) {
    gaze_data
  } else {
    do.call(
      prepare_gazepoint_gpbiometrics_bridge,
      c(list(gaze_data = gaze_data), gaze_args)
    )
  }

  biometrics <- .gp3_bridge_prepare_biometrics(
    biometrics_data = biometrics_data,
    participant_col = biometric_participant_col,
    trial_col = biometric_trial_col,
    time_col = biometric_time_col,
    time_unit = biometric_time_unit,
    signal_cols = signal_cols,
    event_col = event_col
  )
  signal_cols <- attr(biometrics, "signal_cols")
  event_col_resolved <- attr(biometrics, "event_col")

  if (is.null(tolerance_s)) {
    tolerance_s <- .gp3_bridge_default_tolerance(biometrics)
  }
  .gp3_bridge_nonnegative_scalar(tolerance_s, "tolerance_s")

  if (is.null(adapter)) {
    synchronized <- .gp3_bridge_nearest_join(
      gaze,
      biometrics,
      tolerance_s = tolerance_s,
      include_unmatched = include_unmatched
    )
    engine <- "native_nearest_time"
  } else {
    if (!is.function(adapter)) {
      stop("`adapter` must be NULL or a function.", call. = FALSE)
    }
    synchronized <- adapter(
      gaze = gaze,
      biometrics = biometrics,
      tolerance_s = tolerance_s
    )
    if (!is.data.frame(synchronized)) {
      stop(
        "The adapter must return a data frame.",
        call. = FALSE
      )
    }
    synchronized <- .gp3_bridge_normalize_adapter_output(
      synchronized,
      gaze,
      tolerance_s,
      include_unmatched
    )
    engine <- "external_adapter"
  }

  signal_summary <- .gp3_bridge_signal_summary(
    synchronized,
    signal_cols = signal_cols,
    event_col = event_col_resolved
  )

  matched <- synchronized$.matched %in% TRUE
  abs_diff <- abs(synchronized$.sync_diff_s[matched])
  audit <- data.frame(
    engine = engine,
    gaze_rows = nrow(gaze),
    biometric_rows = nrow(biometrics),
    synchronized_rows = nrow(synchronized),
    matched_rows = sum(matched),
    unmatched_rows = sum(!matched),
    matched_rate = if (nrow(synchronized) > 0L) mean(matched) else NA_real_,
    tolerance_ms = 1000 * tolerance_s,
    median_absolute_difference_ms = if (length(abs_diff) > 0L) {
      1000 * stats::median(abs_diff)
    } else {
      NA_real_
    },
    maximum_absolute_difference_ms = if (length(abs_diff) > 0L) {
      1000 * max(abs_diff)
    } else {
      NA_real_
    },
    signal_count = length(signal_cols),
    summary_rows = nrow(signal_summary),
    gp3tools_version = as.character(utils::packageVersion("gp3tools")),
    gpbiometrics_version = .gp3_bridge_optional_package_version(
      "gpbiometrics"
    ),
    stringsAsFactors = FALSE
  )

  report_text <- .gp3_bridge_report_text(audit, signal_cols)

  structure(
    list(
      gaze_bridge = gaze,
      biometric_bridge = biometrics,
      synchronized = synchronized,
      signal_summary = signal_summary,
      audit = audit,
      report_text = report_text,
      settings = list(
        tolerance_s = tolerance_s,
        signal_cols = signal_cols,
        event_col = event_col_resolved,
        include_unmatched = include_unmatched,
        engine = engine
      )
    ),
    class = c("gazepoint_cross_package_workflow", "list")
  )
}

#' Create a combined gp3tools-gpbiometrics workflow report
#'
#' @param x A `"gazepoint_cross_package_workflow"` object.
#' @param output_file Optional Markdown output path.
#'
#' @return Character vector containing a compact Markdown report.
#' @export
create_gazepoint_cross_package_report <- function(
    x,
    output_file = NULL) {
  if (!inherits(x, "gazepoint_cross_package_workflow")) {
    stop(
      "`x` must be a gazepoint_cross_package_workflow object.",
      call. = FALSE
    )
  }

  audit <- x$audit[1L, , drop = FALSE]
  lines <- c(
    "# gp3tools-gpbiometrics workflow audit",
    "",
    x$report_text,
    "",
    "## Alignment summary",
    "",
    paste0("- Engine: `", audit$engine, "`"),
    paste0("- Gaze rows: ", audit$gaze_rows),
    paste0("- Biometric rows: ", audit$biometric_rows),
    paste0("- Matched rows: ", audit$matched_rows),
    paste0("- Unmatched rows: ", audit$unmatched_rows),
    paste0(
      "- Match rate: ",
      formatC(100 * audit$matched_rate, digits = 2, format = "f"),
      "%"
    ),
    paste0(
      "- Median absolute timing difference: ",
      formatC(
        audit$median_absolute_difference_ms,
        digits = 3,
        format = "f"
      ),
      " ms"
    ),
    paste0(
      "- Maximum absolute timing difference: ",
      formatC(
        audit$maximum_absolute_difference_ms,
        digits = 3,
        format = "f"
      ),
      " ms"
    ),
    "",
    "## Interpretation guardrail",
    "",
    paste(
      "The synchronized signal summaries describe measured gaze allocation",
      "and physiological signal values within the specified timing and AOI",
      "structure. They do not, by themselves, establish emotion, stress,",
      "preference, cognition, comprehension, or diagnosis."
    )
  )

  if (!is.null(output_file)) {
    output_file <- .gp3_bridge_nonempty_string(
      output_file,
      "output_file"
    )
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, output_file, useBytes = TRUE)
  }

  lines
}

#' @export
print.gazepoint_gpbiometrics_bridge <- function(x, ...) {
  audit <- attr(x, "audit")
  cat("gp3tools gaze bridge for gpbiometrics\n")
  cat("  Rows: ", nrow(x), "\n", sep = "")
  cat("  Time unit: ", audit$time_unit, "\n", sep = "")
  invisible(x)
}

#' @export
print.gazepoint_cross_package_workflow <- function(x, ...) {
  cat("gp3tools-gpbiometrics workflow\n")
  cat("  Engine: ", x$audit$engine, "\n", sep = "")
  cat("  Matched rows: ", x$audit$matched_rows, "\n", sep = "")
  cat(
    "  Match rate: ",
    formatC(100 * x$audit$matched_rate, digits = 2, format = "f"),
    "%\n",
    sep = ""
  )
  invisible(x)
}

.gp3_bridge_prepare_biometrics <- function(
    biometrics_data,
    participant_col,
    trial_col,
    time_col,
    time_unit,
    signal_cols,
    event_col) {
  if (!is.data.frame(biometrics_data) || nrow(biometrics_data) == 0L) {
    stop(
      "`biometrics_data` must be a non-empty data frame.",
      call. = FALSE
    )
  }

  participant_col <- .gp3_bridge_resolve_col(
    biometrics_data,
    participant_col,
    c(
      "participant_id", "participant", "subject", "subject_id",
      "USER_ID", "USER", "user_id"
    ),
    "biometric participant",
    TRUE
  )
  trial_col <- .gp3_bridge_resolve_col(
    biometrics_data,
    trial_col,
    c(
      "trial_id", "trial", "TRIAL_ID", "MEDIA_ID",
      "media_id", "stimulus_id"
    ),
    "biometric trial",
    TRUE
  )
  time_col <- .gp3_bridge_resolve_col(
    biometrics_data,
    time_col,
    c(
      "time_s", "time_ms", "TIME", "MSTIMER", "timestamp",
      "timestamp_s", "timestamp_ms"
    ),
    "biometric time",
    TRUE
  )
  event_col <- .gp3_bridge_resolve_col(
    biometrics_data,
    event_col,
    c("event", "EVENT", "event_label", "marker", "ttl_label"),
    "event",
    FALSE
  )
  resolved_unit <- .gp3_bridge_time_unit(
    biometrics_data[[time_col]],
    time_col,
    time_unit
  )

  excluded <- unique(c(
    participant_col,
    trial_col,
    time_col,
    event_col
  ))
  excluded <- excluded[!is.na(excluded) & nzchar(excluded)]

  if (is.null(signal_cols)) {
    numeric_candidates <- names(biometrics_data)[
      vapply(biometrics_data, is.numeric, logical(1))
    ]
    signal_cols <- setdiff(numeric_candidates, excluded)
  } else {
    signal_cols <- .gp3_bridge_existing_cols(
      biometrics_data,
      signal_cols,
      "signal_cols"
    )
    nonnumeric <- signal_cols[
      !vapply(biometrics_data[signal_cols], is.numeric, logical(1))
    ]
    if (length(nonnumeric) > 0L) {
      stop(
        "Biometric signal columns must be numeric: ",
        paste(nonnumeric, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if (length(signal_cols) == 0L) {
    stop(
      "No numeric biometric signal columns were identified.",
      call. = FALSE
    )
  }

  time_numeric <- suppressWarnings(
    as.numeric(biometrics_data[[time_col]])
  )
  time_s <- if (identical(resolved_unit, "milliseconds")) {
    time_numeric / 1000
  } else {
    time_numeric
  }

  out <- data.frame(
    participant_id = as.character(
      biometrics_data[[participant_col]]
    ),
    trial_id = as.character(biometrics_data[[trial_col]]),
    time_s = time_s,
    stringsAsFactors = FALSE
  )
  out <- cbind(out, biometrics_data[signal_cols])
  if (!is.null(event_col)) {
    out$event <- as.character(biometrics_data[[event_col]])
    event_col_out <- "event"
  } else {
    event_col_out <- NULL
  }

  attr(out, "signal_cols") <- signal_cols
  attr(out, "event_col") <- event_col_out
  attr(out, "source_columns") <- list(
    participant = participant_col,
    trial = trial_col,
    time = time_col,
    signals = signal_cols,
    event = event_col
  )
  attr(out, "time_unit") <- resolved_unit
  out
}

.gp3_bridge_nearest_join <- function(
    gaze,
    biometrics,
    tolerance_s,
    include_unmatched) {
  gaze_key <- paste(
    gaze$participant_id,
    gaze$trial_id,
    sep = "\r"
  )
  biometrics_key <- paste(
    biometrics$participant_id,
    biometrics$trial_id,
    sep = "\r"
  )
  groups <- split(seq_len(nrow(biometrics)), biometrics_key)
  signal_cols <- attr(biometrics, "signal_cols")
  event_col <- attr(biometrics, "event_col")

  matched_index <- rep(NA_integer_, nrow(gaze))
  difference <- rep(NA_real_, nrow(gaze))

  for (key in unique(gaze_key)) {
    gaze_index <- which(gaze_key == key)
    biometric_index <- groups[[key]]
    if (is.null(biometric_index) || length(biometric_index) == 0L) {
      next
    }

    biometric_time <- biometrics$time_s[biometric_index]
    finite_biometric <- is.finite(biometric_time)
    biometric_index <- biometric_index[finite_biometric]
    biometric_time <- biometric_time[finite_biometric]
    if (length(biometric_index) == 0L) {
      next
    }

    ordering <- order(biometric_time)
    biometric_time <- biometric_time[ordering]
    biometric_index <- biometric_index[ordering]

    gaze_time <- gaze$time_s[gaze_index]
    positions <- findInterval(gaze_time, biometric_time)
    left <- pmax(1L, positions)
    right <- pmin(length(biometric_time), positions + 1L)
    left_diff <- abs(gaze_time - biometric_time[left])
    right_diff <- abs(gaze_time - biometric_time[right])
    choose_right <- right_diff < left_diff
    nearest_position <- ifelse(choose_right, right, left)
    selected_index <- biometric_index[nearest_position]
    selected_difference <- biometrics$time_s[selected_index] - gaze_time
    within <- is.finite(selected_difference) &
      abs(selected_difference) <= tolerance_s

    matched_index[gaze_index[within]] <- selected_index[within]
    difference[gaze_index[within]] <- selected_difference[within]
  }

  out <- gaze
  out$.biometric_time_s <- NA_real_
  for (signal in signal_cols) {
    output_name <- if (signal %in% names(out)) {
      paste0(signal, "_biometric")
    } else {
      signal
    }
    values <- rep(NA_real_, nrow(out))
    matched <- !is.na(matched_index)
    values[matched] <- biometrics[[signal]][matched_index[matched]]
    out[[output_name]] <- values
    if (!identical(output_name, signal)) {
      signal_cols[signal_cols == signal] <- output_name
    }
  }

  if (!is.null(event_col)) {
    event_values <- rep(NA_character_, nrow(out))
    matched <- !is.na(matched_index)
    event_values[matched] <- biometrics[[event_col]][
      matched_index[matched]
    ]
    if ("event" %in% names(out)) {
      out$event_biometric <- event_values
      event_col <- "event_biometric"
    } else {
      out$event <- event_values
      event_col <- "event"
    }
  }

  matched <- !is.na(matched_index)
  out$.biometric_time_s[matched] <- biometrics$time_s[
    matched_index[matched]
  ]
  out$.sync_diff_s <- difference
  out$.matched <- matched

  if (!isTRUE(include_unmatched)) {
    out <- out[out$.matched, , drop = FALSE]
  }
  attr(out, "signal_cols") <- signal_cols
  attr(out, "event_col") <- event_col
  out
}

.gp3_bridge_normalize_adapter_output <- function(
    synchronized,
    gaze,
    tolerance_s,
    include_unmatched) {
  required <- c("participant_id", "trial_id", "time_s")
  missing <- setdiff(required, names(synchronized))
  if (length(missing) > 0L) {
    stop(
      "Adapter output is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!".sync_diff_s" %in% names(synchronized)) {
    synchronized$.sync_diff_s <- NA_real_
  }
  if (!".matched" %in% names(synchronized)) {
    synchronized$.matched <- is.finite(synchronized$.sync_diff_s) &
      abs(synchronized$.sync_diff_s) <= tolerance_s
    if (all(is.na(synchronized$.sync_diff_s))) {
      synchronized$.matched <- TRUE
    }
  }
  if (!isTRUE(include_unmatched)) {
    synchronized <- synchronized[
      synchronized$.matched %in% TRUE,
      ,
      drop = FALSE
    ]
  }
  synchronized
}

.gp3_bridge_signal_summary <- function(
    synchronized,
    signal_cols,
    event_col) {
  actual_signals <- signal_cols
  actual_signals[!actual_signals %in% names(synchronized)] <- paste0(
    actual_signals[!actual_signals %in% names(synchronized)],
    "_biometric"
  )
  actual_signals <- actual_signals[
    actual_signals %in% names(synchronized)
  ]
  if (length(actual_signals) == 0L) {
    return(data.frame())
  }

  group_cols <- c("participant_id", "trial_id")
  if ("aoi" %in% names(synchronized)) {
    group_cols <- c(group_cols, "aoi")
  }
  if (!is.null(event_col) && event_col %in% names(synchronized)) {
    group_cols <- c(group_cols, event_col)
  }
  group_cols <- unique(group_cols)

  group_key <- do.call(
    paste,
    c(
      lapply(synchronized[group_cols], function(x) {
        value <- as.character(x)
        value[is.na(value)] <- "<NA>"
        value
      }),
      sep = "\r"
    )
  )

  group_indices <- split(seq_len(nrow(synchronized)), group_key)
  rows <- list()
  row_index <- 0L

  for (indices in group_indices) {
    grouping <- synchronized[indices[1L], group_cols, drop = FALSE]
    for (signal in actual_signals) {
      values <- suppressWarnings(
        as.numeric(synchronized[[signal]][indices])
      )
      finite <- is.finite(values)
      row_index <- row_index + 1L
      rows[[row_index]] <- cbind(
        grouping,
        data.frame(
          signal = signal,
          n_rows = length(indices),
          n_nonmissing = sum(finite),
          mean = if (any(finite)) mean(values[finite]) else NA_real_,
          sd = if (sum(finite) > 1L) {
            stats::sd(values[finite])
          } else {
            NA_real_
          },
          minimum = if (any(finite)) min(values[finite]) else NA_real_,
          maximum = if (any(finite)) max(values[finite]) else NA_real_,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.gp3_bridge_default_tolerance <- function(biometrics) {
  key <- paste(
    biometrics$participant_id,
    biometrics$trial_id,
    sep = "\r"
  )
  intervals <- unlist(
    lapply(split(biometrics$time_s, key), function(time) {
      time <- sort(unique(time[is.finite(time)]))
      diff(time)
    }),
    use.names = FALSE
  )
  intervals <- intervals[is.finite(intervals) & intervals > 0]
  if (length(intervals) == 0L) {
    return(0.05)
  }
  max(0.001, 0.75 * stats::median(intervals))
}

.gp3_bridge_report_text <- function(audit, signals) {
  paste0(
    "The cross-package workflow aligned ",
    audit$matched_rows,
    " of ",
    audit$synchronized_rows,
    " retained gaze rows (",
    formatC(100 * audit$matched_rate, digits = 2, format = "f"),
    "%) using ",
    audit$engine,
    " with a ",
    formatC(audit$tolerance_ms, digits = 3, format = "f"),
    " ms tolerance. ",
    length(signals),
    " biometric signal",
    if (length(signals) == 1L) " was" else "s were",
    " summarized within the available participant, trial, AOI, and event ",
    "structure. These summaries describe recorded signal values and timing; ",
    "they do not directly establish psychological or clinical states."
  )
}

.gp3_bridge_optional_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion(package))
}

.gp3_bridge_resolve_col <- function(
    data,
    supplied,
    candidates,
    description,
    required) {
  if (!is.null(supplied)) {
    supplied <- .gp3_bridge_nonempty_string(
      supplied,
      paste0(description, "_col")
    )
    if (!supplied %in% names(data)) {
      stop(
        "Selected ", description, " column was not found: ",
        supplied,
        ".",
        call. = FALSE
      )
    }
    return(supplied)
  }

  lower <- tolower(names(data))
  for (candidate in candidates) {
    hit <- which(lower == tolower(candidate))
    if (length(hit) > 0L) {
      return(names(data)[hit[1L]])
    }
  }

  if (isTRUE(required)) {
    stop(
      "Could not infer the ", description,
      " column; supply it explicitly.",
      call. = FALSE
    )
  }
  NULL
}

.gp3_bridge_time_unit <- function(x, column, requested) {
  if (!identical(requested, "auto")) {
    return(requested)
  }
  lower <- tolower(column)
  if (grepl("ms|mstimer|millisecond", lower)) {
    return("milliseconds")
  }
  if (grepl("time_s|timestamp_s|second", lower)) {
    return("seconds")
  }
  numeric <- suppressWarnings(as.numeric(x))
  difference <- diff(sort(unique(numeric[is.finite(numeric)])))
  difference <- difference[is.finite(difference) & difference > 0]
  if (length(difference) == 0L) {
    stop(
      "Could not infer the time unit; supply it explicitly.",
      call. = FALSE
    )
  }
  if (stats::median(difference) >= 5) {
    "milliseconds"
  } else {
    "seconds"
  }
}

.gp3_bridge_numeric_or_na <- function(data, column) {
  if (is.null(column)) {
    return(rep(NA_real_, nrow(data)))
  }
  suppressWarnings(as.numeric(data[[column]]))
}

.gp3_bridge_character_or_na <- function(data, column) {
  if (is.null(column)) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[column]])
}

.gp3_bridge_validity_or_na <- function(data, column) {
  if (is.null(column)) {
    return(rep(NA, nrow(data)))
  }
  value <- data[[column]]
  if (is.logical(value)) {
    return(value)
  }
  if (is.numeric(value)) {
    return(is.finite(value) & value > 0)
  }
  text <- tolower(trimws(as.character(value)))
  text %in% c("1", "true", "valid", "yes", "y")
}

.gp3_bridge_existing_cols <- function(data, columns, argument) {
  if (is.null(columns)) {
    return(character())
  }
  columns <- unique(as.character(columns))
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      "Columns in `", argument, "` were not found: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  columns
}

.gp3_bridge_logical <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

.gp3_bridge_nonnegative_scalar <- function(x, argument) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    stop(
      "`", argument, "` must be one non-negative finite number.",
      call. = FALSE
    )
  }
  invisible(x)
}

.gp3_bridge_nonempty_string <- function(x, argument) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(
      "`", argument, "` must be one non-empty character value.",
      call. = FALSE
    )
  }
  x
}
