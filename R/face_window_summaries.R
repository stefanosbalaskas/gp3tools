#' Summarise external facial-behaviour data within analysis windows
#'
#' Summarises numeric external facial-behaviour variables within time windows.
#' The helper can use a separate window table or data rows that already contain
#' window labels. It is intended for external face-analysis data imported,
#' standardised, and optionally synchronised with Gazepoint data. It does not
#' infer facial expressions or emotional states.
#'
#' @param data A face-analysis data frame, usually returned by
#'   `standardize_gazepoint_face_columns()` or `sync_gazepoint_face_data()`.
#' @param windows Optional data frame defining windows. When supplied, it must
#'   contain start and end columns.
#' @param time_col Time column in `data`, in seconds. Auto-detected when
#'   possible.
#' @param window_start_col Window-start column in seconds.
#' @param window_end_col Window-end column in seconds.
#' @param group_cols Optional grouping columns shared by `data` and `windows`.
#' @param window_id_col Optional window identifier column.
#' @param window_label_col Optional human-readable window label column.
#' @param measure_cols Numeric facial-behaviour columns to summarise. When
#'   `NULL`, likely facial-behaviour columns are detected automatically.
#' @param validity_col Optional validity column. Auto-detected when possible.
#' @param confidence_col Optional confidence column. Auto-detected when possible.
#' @param require_valid Should measure summaries use only rows where the validity
#'   column is `TRUE` when such a column is available?
#' @param include_empty_windows Should windows with no matching rows be kept in
#'   the output?
#'
#' @return A tibble with one row per group/window and summary columns for each
#'   measure. The returned object has class `gp3_face_window_summary`.
#' @export
#'
#' @examples
#' face <- data.frame(
#'   participant_id = "P001",
#'   face_time_sec = c(0.00, 0.05, 0.10),
#'   face_confidence = c(0.95, 0.94, 0.93),
#'   face_valid = c(TRUE, TRUE, TRUE),
#'   AU12_r = c(0.1, 0.2, 0.3)
#' )
#'
#' windows <- data.frame(
#'   participant_id = "P001",
#'   window = c("baseline", "response"),
#'   window_start_sec = c(0.00, 0.05),
#'   window_end_sec = c(0.05, 0.15)
#' )
#'
#' summarize_gazepoint_face_windows(
#'   face,
#'   windows = windows,
#'   group_cols = "participant_id",
#'   window_label_col = "window"
#' )
summarize_gazepoint_face_windows <- function(data,
                                             windows = NULL,
                                             time_col = NULL,
                                             window_start_col = "window_start_sec",
                                             window_end_col = "window_end_sec",
                                             group_cols = NULL,
                                             window_id_col = NULL,
                                             window_label_col = NULL,
                                             measure_cols = NULL,
                                             validity_col = NULL,
                                             confidence_col = NULL,
                                             require_valid = TRUE,
                                             include_empty_windows = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)

  if (!is.null(windows) && !is.data.frame(windows)) {
    stop("`windows` must be a data frame or `NULL`.", call. = FALSE)
  }

  if (!is.null(windows)) {
    windows <- as.data.frame(windows, stringsAsFactors = FALSE)
  }

  time_col <- .gp3_face_windows_optional_col(
    data,
    supplied = time_col,
    candidates = c(
      "face_time_sec",
      "time_sec",
      "timestamp_sec",
      "timestamp",
      "trial_time_sec",
      "relative_time_sec",
      "time"
    ),
    arg_name = "`time_col`"
  )

  if (!is.null(windows) && is.null(time_col)) {
    stop(
      "`time_col` could not be detected automatically. Please supply it.",
      call. = FALSE
    )
  }

  group_cols <- .gp3_face_windows_validate_group_cols(
    group_cols = group_cols,
    data = data,
    windows = windows
  )

  validity_col <- .gp3_face_windows_optional_col(
    data,
    supplied = validity_col,
    candidates = c("face_valid", "valid", "success_valid"),
    arg_name = "`validity_col`"
  )

  confidence_col <- .gp3_face_windows_optional_col(
    data,
    supplied = confidence_col,
    candidates = c("face_confidence", "confidence", "detection_confidence"),
    arg_name = "`confidence_col`"
  )

  measure_cols <- .gp3_face_windows_measure_cols(
    data = data,
    supplied = measure_cols,
    exclude = unique(c(
      group_cols,
      time_col,
      window_start_col,
      window_end_col,
      window_id_col,
      window_label_col,
      validity_col,
      confidence_col
    ))
  )

  if (length(measure_cols) < 1L) {
    stop(
      "No numeric facial-behaviour measure columns were found. Supply `measure_cols`.",
      call. = FALSE
    )
  }

  if (!is.null(windows)) {
    out <- .gp3_face_windows_from_window_table(
      data = data,
      windows = windows,
      time_col = time_col,
      window_start_col = window_start_col,
      window_end_col = window_end_col,
      group_cols = group_cols,
      window_id_col = window_id_col,
      window_label_col = window_label_col,
      measure_cols = measure_cols,
      validity_col = validity_col,
      confidence_col = confidence_col,
      require_valid = require_valid,
      include_empty_windows = include_empty_windows
    )
  } else {
    out <- .gp3_face_windows_from_labelled_data(
      data = data,
      time_col = time_col,
      window_start_col = window_start_col,
      window_end_col = window_end_col,
      group_cols = group_cols,
      window_id_col = window_id_col,
      window_label_col = window_label_col,
      measure_cols = measure_cols,
      validity_col = validity_col,
      confidence_col = confidence_col,
      require_valid = require_valid
    )
  }

  out <- tibble::as_tibble(out)
  class(out) <- c("gp3_face_window_summary", class(out))
  attr(out, "gp3_face_window_settings") <- list(
    time_col = time_col,
    window_start_col = window_start_col,
    window_end_col = window_end_col,
    group_cols = group_cols,
    window_id_col = window_id_col,
    window_label_col = window_label_col,
    measure_cols = measure_cols,
    validity_col = validity_col,
    confidence_col = confidence_col,
    require_valid = require_valid,
    include_empty_windows = include_empty_windows,
    used_window_table = !is.null(windows)
  )

  out
}


#' Summarise facial-behaviour reactivity between two windows
#'
#' Computes baseline-to-response differences from
#' `summarize_gazepoint_face_windows()` output. The function returns one row per
#' group and measure. Reactivity is reported as response minus baseline. The
#' helper summarises technical facial-behaviour measures only and does not infer
#' emotional states.
#'
#' @param data A window-summary table returned by
#'   `summarize_gazepoint_face_windows()`.
#' @param baseline_window Value in `window_col` identifying the baseline window.
#' @param response_window Value in `window_col` identifying the response window.
#' @param group_cols Optional grouping columns.
#' @param window_col Column identifying window labels or IDs. Auto-detected when
#'   possible.
#' @param measure_cols Measure names or summary columns. If `NULL`, columns
#'   ending in the selected statistic suffix are detected automatically.
#' @param statistic Statistic used for reactivity. One of `"mean"` or
#'   `"median"`.
#'
#' @return A tibble with one row per group and measure. The returned object has
#'   class `gp3_face_reactivity_summary`.
#' @export
#'
#' @examples
#' face <- data.frame(
#'   participant_id = "P001",
#'   face_time_sec = c(0.00, 0.05, 0.10),
#'   face_valid = c(TRUE, TRUE, TRUE),
#'   AU12_r = c(0.1, 0.2, 0.3)
#' )
#'
#' windows <- data.frame(
#'   participant_id = "P001",
#'   window = c("baseline", "response"),
#'   window_start_sec = c(0.00, 0.05),
#'   window_end_sec = c(0.05, 0.15)
#' )
#'
#' summary <- summarize_gazepoint_face_windows(
#'   face,
#'   windows = windows,
#'   group_cols = "participant_id",
#'   window_label_col = "window"
#' )
#'
#' summarize_gazepoint_face_reactivity(
#'   summary,
#'   baseline_window = "baseline",
#'   response_window = "response",
#'   group_cols = "participant_id"
#' )
summarize_gazepoint_face_reactivity <- function(data,
                                                baseline_window,
                                                response_window,
                                                group_cols = NULL,
                                                window_col = NULL,
                                                measure_cols = NULL,
                                                statistic = c("mean", "median")) {
  statistic <- match.arg(statistic)

  if (!is.data.frame(data)) {
    stop("`data` must be a window-summary data frame.", call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)

  window_col <- .gp3_face_windows_optional_col(
    data,
    supplied = window_col,
    candidates = c(
      "face_window_label",
      "window_label",
      "window",
      "phase",
      "task_phase",
      "face_window_id",
      "window_id"
    ),
    arg_name = "`window_col`"
  )

  if (is.null(window_col)) {
    stop(
      "`window_col` could not be detected automatically. Please supply it.",
      call. = FALSE
    )
  }

  if (!is.null(group_cols)) {
    missing_groups <- setdiff(group_cols, names(data))
    if (length(missing_groups) > 0L) {
      stop(
        "Grouping column(s) not found: ",
        paste(missing_groups, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (is.null(group_cols)) {
    group_cols <- character(0)
  }

  suffix <- paste0("_", statistic)

  value_cols <- .gp3_face_reactivity_value_cols(
    data = data,
    supplied = measure_cols,
    suffix = suffix
  )

  if (length(value_cols) < 1L) {
    stop(
      "No reactivity measure columns were found. Supply `measure_cols`.",
      call. = FALSE
    )
  }

  base <- data[as.character(data[[window_col]]) %in% baseline_window, ,
               drop = FALSE]
  response <- data[as.character(data[[window_col]]) %in% response_window, ,
                   drop = FALSE]

  if (nrow(base) < 1L) {
    stop("No baseline-window rows were found.", call. = FALSE)
  }

  if (nrow(response) < 1L) {
    stop("No response-window rows were found.", call. = FALSE)
  }

  group_keys <- .gp3_face_reactivity_group_keys(base, response, group_cols)

  rows <- list()
  counter <- 1L

  for (key in group_keys) {
    base_idx <- .gp3_face_reactivity_key_index(base, group_cols, key)
    response_idx <- .gp3_face_reactivity_key_index(response, group_cols, key)

    group_values <- .gp3_face_reactivity_group_values(
      base = base,
      response = response,
      group_cols = group_cols,
      base_idx = base_idx,
      response_idx = response_idx
    )

    for (value_col in value_cols) {
      baseline_value <- .gp3_face_reactivity_mean(base[[value_col]][base_idx])
      response_value <- .gp3_face_reactivity_mean(
        response[[value_col]][response_idx]
      )

      measure <- sub(paste0(suffix, "$"), "", value_col)

      rows[[counter]] <- cbind(
        group_values,
        data.frame(
          measure = measure,
          statistic = statistic,
          baseline_window = paste(baseline_window, collapse = " | "),
          response_window = paste(response_window, collapse = " | "),
          baseline_value = baseline_value,
          response_value = response_value,
          reactivity = response_value - baseline_value,
          absolute_reactivity = abs(response_value - baseline_value),
          percent_reactivity = .gp3_face_reactivity_percent_change(
            baseline_value,
            response_value
          ),
          n_baseline_windows = length(base_idx),
          n_response_windows = length(response_idx),
          stringsAsFactors = FALSE
        ),
        stringsAsFactors = FALSE
      )

      counter <- counter + 1L
    }
  }

  out <- .gp3_face_windows_bind_rows(rows)
  out <- tibble::as_tibble(out)

  class(out) <- c("gp3_face_reactivity_summary", class(out))
  attr(out, "gp3_face_reactivity_settings") <- list(
    baseline_window = baseline_window,
    response_window = response_window,
    group_cols = group_cols,
    window_col = window_col,
    measure_cols = value_cols,
    statistic = statistic
  )

  out
}


.gp3_face_windows_from_window_table <- function(data,
                                                windows,
                                                time_col,
                                                window_start_col,
                                                window_end_col,
                                                group_cols,
                                                window_id_col,
                                                window_label_col,
                                                measure_cols,
                                                validity_col,
                                                confidence_col,
                                                require_valid,
                                                include_empty_windows) {
  missing_window_cols <- setdiff(
    c(window_start_col, window_end_col),
    names(windows)
  )

  if (length(missing_window_cols) > 0L) {
    stop(
      "Window column(s) not found: ",
      paste(missing_window_cols, collapse = ", "),
      call. = FALSE
    )
  }

  window_id_col <- .gp3_face_windows_optional_col(
    windows,
    supplied = window_id_col,
    candidates = c("face_window_id", "window_id", "id"),
    arg_name = "`window_id_col`"
  )

  window_label_col <- .gp3_face_windows_optional_col(
    windows,
    supplied = window_label_col,
    candidates = c("face_window_label", "window_label", "window", "phase"),
    arg_name = "`window_label_col`"
  )

  time <- suppressWarnings(as.numeric(data[[time_col]]))
  rows <- list()
  counter <- 1L

  for (i in seq_len(nrow(windows))) {
    start <- suppressWarnings(as.numeric(windows[[window_start_col]][[i]]))
    end <- suppressWarnings(as.numeric(windows[[window_end_col]][[i]]))

    idx <- is.finite(time) & time >= start & time <= end

    if (length(group_cols) > 0L) {
      for (g in group_cols) {
        idx <- idx & .gp3_face_windows_equal(data[[g]], windows[[g]][[i]])
      }
    }

    idx <- which(idx)

    if (length(idx) < 1L && !include_empty_windows) {
      next
    }

    window_values <- .gp3_face_windows_window_values(
      windows = windows,
      row = i,
      window_id_col = window_id_col,
      window_label_col = window_label_col,
      window_start_col = window_start_col,
      window_end_col = window_end_col,
      generated_id = i
    )

    group_values <- .gp3_face_windows_group_values_from_window(
      windows = windows,
      row = i,
      group_cols = group_cols
    )

    rows[[counter]] <- .gp3_face_windows_summarise_subset(
      data = data,
      idx = idx,
      group_values = group_values,
      window_values = window_values,
      measure_cols = measure_cols,
      validity_col = validity_col,
      confidence_col = confidence_col,
      require_valid = require_valid
    )

    counter <- counter + 1L
  }

  .gp3_face_windows_bind_rows(rows)
}


.gp3_face_windows_from_labelled_data <- function(data,
                                                 time_col,
                                                 window_start_col,
                                                 window_end_col,
                                                 group_cols,
                                                 window_id_col,
                                                 window_label_col,
                                                 measure_cols,
                                                 validity_col,
                                                 confidence_col,
                                                 require_valid) {
  window_id_col <- .gp3_face_windows_optional_col(
    data,
    supplied = window_id_col,
    candidates = c("face_window_id", "window_id", "id"),
    arg_name = "`window_id_col`"
  )

  window_label_col <- .gp3_face_windows_optional_col(
    data,
    supplied = window_label_col,
    candidates = c("face_window_label", "window_label", "window", "phase"),
    arg_name = "`window_label_col`"
  )

  split_cols <- unique(c(group_cols, window_id_col, window_label_col))
  split_cols <- split_cols[!is.na(split_cols)]

  if (length(split_cols) < 1L) {
    groups <- list(overall = seq_len(nrow(data)))
  } else {
    key_data <- data[, split_cols, drop = FALSE]
    key_data[] <- lapply(key_data, function(x) {
      x <- as.character(x)
      x[is.na(x) | x == ""] <- "missing"
      x
    })

    groups <- split(seq_len(nrow(data)), do.call(paste, c(key_data, sep = " | ")))
  }

  rows <- list()
  counter <- 1L

  for (idx in groups) {
    first <- idx[[1L]]

    group_values <- .gp3_face_windows_group_values_from_data(
      data = data,
      row = first,
      group_cols = group_cols
    )

    window_values <- .gp3_face_windows_window_values_from_data(
      data = data,
      idx = idx,
      time_col = time_col,
      window_start_col = window_start_col,
      window_end_col = window_end_col,
      window_id_col = window_id_col,
      window_label_col = window_label_col,
      generated_id = counter
    )

    rows[[counter]] <- .gp3_face_windows_summarise_subset(
      data = data,
      idx = idx,
      group_values = group_values,
      window_values = window_values,
      measure_cols = measure_cols,
      validity_col = validity_col,
      confidence_col = confidence_col,
      require_valid = require_valid
    )

    counter <- counter + 1L
  }

  .gp3_face_windows_bind_rows(rows)
}


.gp3_face_windows_summarise_subset <- function(data,
                                               idx,
                                               group_values,
                                               window_values,
                                               measure_cols,
                                               validity_col,
                                               confidence_col,
                                               require_valid) {
  n_rows <- length(idx)

  valid <- if (!is.null(validity_col)) {
    data[[validity_col]][idx] %in% TRUE
  } else {
    rep(NA, n_rows)
  }

  n_valid <- if (!is.null(validity_col)) {
    sum(valid, na.rm = TRUE)
  } else {
    NA_integer_
  }

  n_invalid <- if (!is.null(validity_col)) {
    sum(!valid, na.rm = TRUE)
  } else {
    NA_integer_
  }

  valid_percent <- if (!is.null(validity_col)) {
    .gp3_face_windows_percent(n_valid, n_rows)
  } else {
    NA_real_
  }

  use_idx <- idx

  if (!is.null(validity_col) && require_valid) {
    use_idx <- idx[valid %in% TRUE]
  }

  confidence <- if (!is.null(confidence_col)) {
    suppressWarnings(as.numeric(data[[confidence_col]][use_idx]))
  } else {
    numeric(0)
  }

  metrics <- data.frame(
    n_rows = n_rows,
    n_used = length(use_idx),
    n_valid = n_valid,
    n_invalid = n_invalid,
    valid_percent = valid_percent,
    face_confidence_mean = .gp3_face_windows_mean(confidence),
    face_confidence_median = .gp3_face_windows_median(confidence),
    stringsAsFactors = FALSE
  )

  for (m in measure_cols) {
    x <- suppressWarnings(as.numeric(data[[m]][use_idx]))
    safe <- .gp3_face_windows_safe_name(m)

    metrics[[paste0(safe, "_n")]] <- sum(!is.na(x))
    metrics[[paste0(safe, "_mean")]] <- .gp3_face_windows_mean(x)
    metrics[[paste0(safe, "_median")]] <- .gp3_face_windows_median(x)
    metrics[[paste0(safe, "_sd")]] <- .gp3_face_windows_sd(x)
    metrics[[paste0(safe, "_min")]] <- .gp3_face_windows_min(x)
    metrics[[paste0(safe, "_max")]] <- .gp3_face_windows_max(x)
  }

  cbind(group_values, window_values, metrics, stringsAsFactors = FALSE)
}


.gp3_face_windows_window_values <- function(windows,
                                            row,
                                            window_id_col,
                                            window_label_col,
                                            window_start_col,
                                            window_end_col,
                                            generated_id) {
  id <- if (!is.null(window_id_col)) {
    windows[[window_id_col]][[row]]
  } else {
    generated_id
  }

  label <- if (!is.null(window_label_col)) {
    windows[[window_label_col]][[row]]
  } else {
    as.character(id)
  }

  data.frame(
    face_window_id = id,
    face_window_label = label,
    window_start_sec = suppressWarnings(as.numeric(
      windows[[window_start_col]][[row]]
    )),
    window_end_sec = suppressWarnings(as.numeric(
      windows[[window_end_col]][[row]]
    )),
    stringsAsFactors = FALSE
  )
}


.gp3_face_windows_window_values_from_data <- function(data,
                                                      idx,
                                                      time_col,
                                                      window_start_col,
                                                      window_end_col,
                                                      window_id_col,
                                                      window_label_col,
                                                      generated_id) {
  first <- idx[[1L]]

  id <- if (!is.null(window_id_col)) {
    data[[window_id_col]][[first]]
  } else {
    generated_id
  }

  label <- if (!is.null(window_label_col)) {
    data[[window_label_col]][[first]]
  } else {
    as.character(id)
  }

  start <- if (window_start_col %in% names(data)) {
    suppressWarnings(as.numeric(data[[window_start_col]][[first]]))
  } else if (!is.null(time_col)) {
    min(suppressWarnings(as.numeric(data[[time_col]][idx])), na.rm = TRUE)
  } else {
    NA_real_
  }

  end <- if (window_end_col %in% names(data)) {
    suppressWarnings(as.numeric(data[[window_end_col]][[first]]))
  } else if (!is.null(time_col)) {
    max(suppressWarnings(as.numeric(data[[time_col]][idx])), na.rm = TRUE)
  } else {
    NA_real_
  }

  if (!is.finite(start)) {
    start <- NA_real_
  }

  if (!is.finite(end)) {
    end <- NA_real_
  }

  data.frame(
    face_window_id = id,
    face_window_label = label,
    window_start_sec = start,
    window_end_sec = end,
    stringsAsFactors = FALSE
  )
}


.gp3_face_windows_group_values_from_window <- function(windows,
                                                       row,
                                                       group_cols) {
  if (length(group_cols) < 1L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  out <- windows[row, group_cols, drop = FALSE]
  out[] <- lapply(out, as.character)
  as.data.frame(out, stringsAsFactors = FALSE)
}


.gp3_face_windows_group_values_from_data <- function(data,
                                                     row,
                                                     group_cols) {
  if (length(group_cols) < 1L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  out <- data[row, group_cols, drop = FALSE]
  out[] <- lapply(out, as.character)
  as.data.frame(out, stringsAsFactors = FALSE)
}


.gp3_face_windows_validate_group_cols <- function(group_cols, data, windows) {
  if (is.null(group_cols)) {
    return(character(0))
  }

  missing_data <- setdiff(group_cols, names(data))

  if (length(missing_data) > 0L) {
    stop(
      "Grouping column(s) not found in `data`: ",
      paste(missing_data, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(windows)) {
    missing_windows <- setdiff(group_cols, names(windows))

    if (length(missing_windows) > 0L) {
      stop(
        "Grouping column(s) not found in `windows`: ",
        paste(missing_windows, collapse = ", "),
        call. = FALSE
      )
    }
  }

  group_cols
}


.gp3_face_windows_optional_col <- function(data,
                                           supplied,
                                           candidates,
                                           arg_name) {
  if (!is.null(supplied)) {
    if (!supplied %in% names(data)) {
      stop(arg_name, " was not found in the data.", call. = FALSE)
    }

    return(supplied)
  }

  idx <- match(tolower(candidates), tolower(names(data)))
  idx <- idx[!is.na(idx)]

  if (length(idx) > 0L) {
    return(names(data)[idx[[1L]]])
  }

  NULL
}


.gp3_face_windows_measure_cols <- function(data, supplied, exclude) {
  if (!is.null(supplied)) {
    missing <- setdiff(supplied, names(data))

    if (length(missing) > 0L) {
      stop(
        "Measure column(s) not found: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }

    non_numeric <- supplied[!vapply(data[supplied], is.numeric, logical(1))]

    if (length(non_numeric) > 0L) {
      stop(
        "Measure column(s) must be numeric: ",
        paste(non_numeric, collapse = ", "),
        call. = FALSE
      )
    }

    return(supplied)
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, exclude)

  metadata_patterns <- paste(
    c(
      "sync_",
      "quality_",
      "confidence",
      "valid$",
      "success$",
      "time",
      "frame",
      "row",
      "file",
      "path",
      "source",
      "session",
      "participant",
      "^id$",
      "_id$"
    ),
    collapse = "|"
  )

  candidate_cols <- numeric_cols[
    !grepl(metadata_patterns, numeric_cols, ignore.case = TRUE)
  ]

  face_prefixed <- candidate_cols[
    startsWith(candidate_cols, "face_") |
      startsWith(candidate_cols, "AU") |
      grepl("valence|arousal|emotion|pose", candidate_cols, ignore.case = TRUE)
  ]

  if (length(face_prefixed) > 0L) {
    return(face_prefixed)
  }

  candidate_cols
}


.gp3_face_reactivity_value_cols <- function(data, supplied, suffix) {
  if (!is.null(supplied)) {
    value_cols <- supplied

    without_suffix <- !endsWith(value_cols, suffix)
    value_cols[without_suffix] <- paste0(value_cols[without_suffix], suffix)

    missing <- setdiff(value_cols, names(data))

    if (length(missing) > 0L) {
      stop(
        "Reactivity measure column(s) not found: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }

    return(value_cols)
  }

  value_cols <- grep(paste0(suffix, "$"), names(data), value = TRUE)

  value_cols <- value_cols[
    !grepl(
      "confidence|valid|sampling|time|n_rows|n_used",
      value_cols,
      ignore.case = TRUE
    )
  ]

  value_cols
}


.gp3_face_reactivity_group_keys <- function(base, response, group_cols) {
  if (length(group_cols) < 1L) {
    return("overall")
  }

  unique(c(
    .gp3_face_reactivity_key(base, group_cols),
    .gp3_face_reactivity_key(response, group_cols)
  ))
}


.gp3_face_reactivity_key <- function(data, group_cols) {
  if (length(group_cols) < 1L) {
    return(rep("overall", nrow(data)))
  }

  key_data <- data[, group_cols, drop = FALSE]
  key_data[] <- lapply(key_data, function(x) {
    x <- as.character(x)
    x[is.na(x) | x == ""] <- "missing"
    x
  })

  do.call(paste, c(key_data, sep = " | "))
}


.gp3_face_reactivity_key_index <- function(data, group_cols, key) {
  if (length(group_cols) < 1L) {
    return(seq_len(nrow(data)))
  }

  which(.gp3_face_reactivity_key(data, group_cols) == key)
}


.gp3_face_reactivity_group_values <- function(base,
                                              response,
                                              group_cols,
                                              base_idx,
                                              response_idx) {
  if (length(group_cols) < 1L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  if (length(base_idx) > 0L) {
    out <- base[base_idx[[1L]], group_cols, drop = FALSE]
  } else {
    out <- response[response_idx[[1L]], group_cols, drop = FALSE]
  }

  out[] <- lapply(out, as.character)
  as.data.frame(out, stringsAsFactors = FALSE)
}


.gp3_face_windows_equal <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)

  out <- x == y
  out[is.na(out)] <- is.na(x[is.na(out)]) & is.na(y)
  out
}


.gp3_face_windows_safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  if (identical(x, "")) {
    x <- "measure"
  }

  x
}


.gp3_face_windows_percent <- function(x, n) {
  if (is.na(n) || n <= 0) {
    return(NA_real_)
  }

  100 * x / n
}


.gp3_face_windows_mean <- function(x) {
  if (length(x) < 1L || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


.gp3_face_windows_median <- function(x) {
  if (length(x) < 1L || all(is.na(x))) {
    return(NA_real_)
  }

  stats::median(x, na.rm = TRUE)
}


.gp3_face_windows_sd <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) < 2L) {
    return(NA_real_)
  }

  stats::sd(x)
}


.gp3_face_windows_min <- function(x) {
  if (length(x) < 1L || all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}


.gp3_face_windows_max <- function(x) {
  if (length(x) < 1L || all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}


.gp3_face_reactivity_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))

  if (length(x) < 1L || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


.gp3_face_reactivity_percent_change <- function(baseline_value,
                                                response_value) {
  if (
    is.na(baseline_value) ||
    is.na(response_value) ||
    baseline_value == 0
  ) {
    return(NA_real_)
  }

  100 * (response_value - baseline_value) / abs(baseline_value)
}


.gp3_face_windows_bind_rows <- function(x) {
  if (length(x) < 1L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  all_names <- unique(unlist(lapply(x, names), use.names = FALSE))

  x <- lapply(x, function(dat) {
    missing <- setdiff(all_names, names(dat))
    for (m in missing) {
      dat[[m]] <- NA
    }
    dat[, all_names, drop = FALSE]
  })

  do.call(rbind, x)
}
