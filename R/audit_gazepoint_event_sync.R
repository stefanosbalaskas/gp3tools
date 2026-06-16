#' Audit Gazepoint event and timing synchronisation
#'
#' Create a publication-level audit of event timing, trial timing, and event
#' availability in a Gazepoint master table or sample-level export.
#'
#' @param data A data frame containing sample-level Gazepoint data.
#' @param time_col Name of the time column.
#' @param event_col Optional event-label column. If `NULL`, the function tries
#'   to detect a common event column.
#' @param group_cols Columns defining a trial or recording unit.
#' @param condition_col Optional condition column.
#' @param expected_event_labels Optional character vector of expected event
#'   labels.
#' @param onset_event_label Optional event label identifying trial/stimulus
#'   onset.
#' @param response_event_label Optional event label identifying response events.
#' @param min_samples_per_unit Minimum number of samples expected per unit.
#' @param max_time_gap_ms Optional maximum allowed within-unit time gap in
#'   milliseconds.
#'
#' @return A list with class `gp3_event_sync_audit` containing overview,
#'   unit_summary, event_summary, expected_event_summary, flagged_units, and
#'   settings tables.
#' @export
audit_gazepoint_event_sync <- function(
    data,
    time_col = "time",
    event_col = NULL,
    group_cols = c("subject", "media_id", "trial_global"),
    condition_col = NULL,
    expected_event_labels = NULL,
    onset_event_label = NULL,
    response_event_label = NULL,
    min_samples_per_unit = 1L,
    max_time_gap_ms = NULL
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  time_col <- .gp3_event_sync_resolve_col(time_col, names(data), "time_col")
  condition_col <- .gp3_event_sync_resolve_optional_col(
    condition_col,
    names(data),
    "condition_col"
  )

  data <- .gp3_event_sync_standardise_aliases(data)

  group_cols <- .gp3_event_sync_standardise_group_cols(group_cols)
  group_cols <- group_cols[group_cols %in% names(data)]

  if (length(group_cols) == 0L) {
    stop("At least one usable `group_cols` column must be present in `data`.", call. = FALSE)
  }

  event_col <- .gp3_event_sync_resolve_event_col(event_col, names(data))

  .gp3_event_sync_check_positive_integer(
    min_samples_per_unit,
    "min_samples_per_unit"
  )

  if (!is.null(max_time_gap_ms)) {
    .gp3_event_sync_check_positive_numeric(
      max_time_gap_ms,
      "max_time_gap_ms"
    )
  }

  if (!is.null(expected_event_labels)) {
    .gp3_event_sync_check_character_vector(
      expected_event_labels,
      "expected_event_labels"
    )
  }

  if (!is.null(onset_event_label)) {
    .gp3_event_sync_check_character_scalar(
      onset_event_label,
      "onset_event_label"
    )
  }

  if (!is.null(response_event_label)) {
    .gp3_event_sync_check_character_scalar(
      response_event_label,
      "response_event_label"
    )
  }

  unit_summary <- .gp3_event_sync_create_unit_summary(
    data = data,
    time_col = time_col,
    event_col = event_col,
    group_cols = group_cols,
    condition_col = condition_col,
    expected_event_labels = expected_event_labels,
    onset_event_label = onset_event_label,
    response_event_label = response_event_label,
    min_samples_per_unit = min_samples_per_unit,
    max_time_gap_ms = max_time_gap_ms
  )

  event_summary <- .gp3_event_sync_create_event_summary(
    data = data,
    event_col = event_col,
    condition_col = condition_col
  )

  expected_event_summary <- .gp3_event_sync_create_expected_event_summary(
    unit_summary = unit_summary,
    expected_event_labels = expected_event_labels,
    event_col = event_col
  )

  flagged_units <- unit_summary[
    unit_summary$event_sync_status != "ok",
    ,
    drop = FALSE
  ]

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_units = nrow(unit_summary),
    n_flagged_units = nrow(flagged_units),
    event_col = ifelse(is.null(event_col), NA_character_, event_col),
    has_event_col = !is.null(event_col),
    has_expected_events = !is.null(expected_event_labels),
    audit_status = dplyr::case_when(
      is.null(event_col) ~ "event_column_not_available",
      nrow(flagged_units) == 0L ~ "ok",
      TRUE ~ "review"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "time_col",
      "event_col",
      "group_cols",
      "condition_col",
      "expected_event_labels",
      "onset_event_label",
      "response_event_label",
      "min_samples_per_unit",
      "max_time_gap_ms"
    ),
    value = c(
      time_col,
      ifelse(is.null(event_col), NA_character_, event_col),
      paste(group_cols, collapse = ", "),
      ifelse(is.null(condition_col), NA_character_, condition_col),
      .gp3_event_sync_collapse_nullable(expected_event_labels),
      .gp3_event_sync_collapse_nullable(onset_event_label),
      .gp3_event_sync_collapse_nullable(response_event_label),
      as.character(min_samples_per_unit),
      .gp3_event_sync_collapse_nullable(max_time_gap_ms)
    )
  )

  out <- list(
    overview = overview,
    unit_summary = unit_summary,
    event_summary = event_summary,
    expected_event_summary = expected_event_summary,
    flagged_units = flagged_units,
    settings = settings
  )

  class(out) <- c("gp3_event_sync_audit", "list")

  out
}

.gp3_event_sync_create_unit_summary <- function(
    data,
    time_col,
    event_col,
    group_cols,
    condition_col,
    expected_event_labels,
    onset_event_label,
    response_event_label,
    min_samples_per_unit,
    max_time_gap_ms
) {
  split_key <- interaction(data[group_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(data)), split_key)

  rows <- vector("list", length(split_idx))

  for (i in seq_along(split_idx)) {
    idx <- split_idx[[i]]
    d <- data[idx, , drop = FALSE]
    time_values <- suppressWarnings(as.numeric(d[[time_col]]))
    finite_time <- time_values[is.finite(time_values)]

    event_values <- character()
    if (!is.null(event_col)) {
      event_values <- as.character(d[[event_col]])
      event_values <- event_values[!is.na(event_values) & nzchar(event_values)]
    }

    event_unique <- sort(unique(event_values))

    expected_missing <- character()
    if (!is.null(expected_event_labels) && !is.null(event_col)) {
      expected_missing <- setdiff(expected_event_labels, event_unique)
    }

    onset_count <- NA_integer_
    response_count <- NA_integer_

    if (!is.null(event_col) && !is.null(onset_event_label)) {
      onset_count <- sum(event_values == onset_event_label, na.rm = TRUE)
    }

    if (!is.null(event_col) && !is.null(response_event_label)) {
      response_count <- sum(event_values == response_event_label, na.rm = TRUE)
    }

    sorted_time <- sort(finite_time)
    time_gaps <- diff(sorted_time)

    max_gap <- if (length(time_gaps) > 0L) {
      max(time_gaps, na.rm = TRUE)
    } else {
      NA_real_
    }

    has_large_gap <- FALSE
    if (!is.null(max_time_gap_ms) && !is.na(max_gap)) {
      has_large_gap <- max_gap > max_time_gap_ms
    }

    n_duplicate_time <- sum(duplicated(time_values[is.finite(time_values)]))

    status <- .gp3_event_sync_unit_status(
      n_samples = nrow(d),
      n_finite_time = length(finite_time),
      has_event_col = !is.null(event_col),
      n_events = length(event_values),
      n_missing_expected = length(expected_missing),
      onset_count = onset_count,
      response_count = response_count,
      n_duplicate_time = n_duplicate_time,
      has_large_gap = has_large_gap,
      min_samples_per_unit = min_samples_per_unit
    )

    group_row <- d[1, group_cols, drop = FALSE]

    if (!is.null(condition_col)) {
      group_row[[condition_col]] <- d[[condition_col]][[1]]
    }

    rows[[i]] <- cbind(
      tibble::as_tibble(group_row),
      tibble::tibble(
        n_samples = nrow(d),
        n_finite_time = length(finite_time),
        time_start = ifelse(length(finite_time) > 0L, min(finite_time), NA_real_),
        time_end = ifelse(length(finite_time) > 0L, max(finite_time), NA_real_),
        time_span = ifelse(length(finite_time) > 0L, max(finite_time) - min(finite_time), NA_real_),
        max_time_gap = max_gap,
        n_duplicate_time = n_duplicate_time,
        n_event_samples = length(event_values),
        n_unique_events = length(event_unique),
        event_labels = paste(event_unique, collapse = "; "),
        n_missing_expected_events = length(expected_missing),
        missing_expected_events = paste(expected_missing, collapse = "; "),
        onset_event_count = onset_count,
        response_event_count = response_count,
        event_sync_status = status
      )
    )
  }

  dplyr::bind_rows(rows)
}

.gp3_event_sync_unit_status <- function(
    n_samples,
    n_finite_time,
    has_event_col,
    n_events,
    n_missing_expected,
    onset_count,
    response_count,
    n_duplicate_time,
    has_large_gap,
    min_samples_per_unit
) {
  if (n_samples < min_samples_per_unit) {
    return("too_few_samples")
  }

  if (n_finite_time == 0L) {
    return("missing_time")
  }

  if (n_duplicate_time > 0L) {
    return("duplicate_time_values")
  }

  if (isTRUE(has_large_gap)) {
    return("large_time_gap")
  }

  if (!has_event_col) {
    return("event_column_not_available")
  }

  if (n_events == 0L) {
    return("no_events_observed")
  }

  if (n_missing_expected > 0L) {
    return("missing_expected_events")
  }

  if (!is.na(onset_count) && onset_count == 0L) {
    return("missing_onset_event")
  }

  if (!is.na(response_count) && response_count == 0L) {
    return("missing_response_event")
  }

  "ok"
}

.gp3_event_sync_create_event_summary <- function(
    data,
    event_col,
    condition_col
) {
  if (is.null(event_col)) {
    return(tibble::tibble(
      event_col = NA_character_,
      event_label = character(),
      n_event_samples = integer(),
      event_summary_status = "event_column_not_available"
    ))
  }

  event_raw <- as.character(data[[event_col]])
  keep <- !is.na(event_raw) & nzchar(event_raw)

  if (!any(keep)) {
    return(tibble::tibble(
      event_col = event_col,
      event_label = character(),
      n_event_samples = integer(),
      event_summary_status = "no_events_observed"
    ))
  }

  if (!is.null(condition_col)) {
    condition_values <- as.character(data[[condition_col]][keep])
    event_values <- event_raw[keep]

    tab <- as.data.frame(
      table(
        condition = condition_values,
        event_label = event_values
      ),
      stringsAsFactors = FALSE
    )

    tab <- tab[tab$Freq > 0L, , drop = FALSE]

    return(tibble::tibble(
      event_col = event_col,
      condition = as.character(tab$condition),
      event_label = as.character(tab$event_label),
      n_event_samples = as.integer(tab$Freq),
      event_summary_status = "ok"
    ))
  }

  tab <- as.data.frame(
    table(event_label = event_raw[keep]),
    stringsAsFactors = FALSE
  )

  tab <- tab[tab$Freq > 0L, , drop = FALSE]

  tibble::tibble(
    event_col = event_col,
    event_label = as.character(tab$event_label),
    n_event_samples = as.integer(tab$Freq),
    event_summary_status = "ok"
  )
}
.gp3_event_sync_create_expected_event_summary <- function(
    unit_summary,
    expected_event_labels,
    event_col
) {
  if (is.null(event_col)) {
    return(tibble::tibble(
      expected_event_label = character(),
      n_units_missing = integer(),
      expected_event_status = "event_column_not_available"
    ))
  }

  if (is.null(expected_event_labels)) {
    return(tibble::tibble(
      expected_event_label = character(),
      n_units_missing = integer(),
      expected_event_status = "not_requested"
    ))
  }

  rows <- lapply(expected_event_labels, function(label) {
    missing <- grepl(
      paste0("(^|; )", label, "($|; )"),
      unit_summary$missing_expected_events,
      fixed = FALSE
    )

    tibble::tibble(
      expected_event_label = label,
      n_units_missing = sum(missing, na.rm = TRUE),
      expected_event_status = ifelse(
        sum(missing, na.rm = TRUE) == 0L,
        "ok",
        "missing_in_some_units"
      )
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_event_sync_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_event_sync_standardise_group_cols <- function(group_cols) {
  if (is.null(group_cols)) {
    return(character())
  }

  group_cols <- as.character(group_cols)
  group_cols[group_cols == "MEDIA_ID"] <- "media_id"
  group_cols[group_cols == "USER_FILE"] <- "subject"
  group_cols
}

.gp3_event_sync_resolve_event_col <- function(event_col, names_data) {
  if (!is.null(event_col)) {
    return(.gp3_event_sync_resolve_col(event_col, names_data, "event_col"))
  }

  candidates <- c(
    "event",
    "event_label",
    "event_name",
    "EVENT",
    "EVENT_LABEL",
    "Event",
    "EventLabel",
    "marker",
    "MARKER",
    "message",
    "MESSAGE"
  )

  found <- candidates[candidates %in% names_data]

  if (length(found) == 0L) {
    return(NULL)
  }

  found[[1]]
}

.gp3_event_sync_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (col == "USER_FILE" && "subject" %in% names_data) {
    return("subject")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_event_sync_resolve_optional_col <- function(col, names_data, arg) {
  if (is.null(col)) {
    return(NULL)
  }

  .gp3_event_sync_resolve_col(col, names_data, arg)
}

.gp3_event_sync_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_event_sync_check_positive_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_event_sync_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_event_sync_check_character_scalar <- function(x, arg) {
  if (!is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_event_sync_collapse_nullable <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
