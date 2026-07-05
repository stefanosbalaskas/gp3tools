#' Collect Gazepoint QC summaries
#'
#' Collects compact overview/status information from gp3tools audit, workflow,
#' checklist, readiness, diagnostic, or reporting objects. The helper is intended
#' to make existing QC outputs easier to review together; it does not rerun
#' checks or define exclusion rules.
#'
#' @param objects A list of gp3tools objects, audit objects, overview data
#'   frames, or a single such object.
#' @param object_names Optional character names for unnamed objects.
#' @param name Character label stored in the returned object.
#' @param include_overview_rows Logical. If `TRUE`, returns a combined long-form
#'   table of interpretable overview rows.
#'
#' @return A list with `overview`, `object_summary`, `overview_rows`, and
#'   `settings`.
#' @export
#'
#' @examples
#' audit <- list(
#'   overview = data.frame(
#'     audit_status = "ok",
#'     message = "Example audit passed."
#'   )
#' )
#' collect_gazepoint_qc_summaries(list(example_audit = audit))
collect_gazepoint_qc_summaries <- function(objects,
                                           object_names = NULL,
                                           name = "gazepoint_qc_summary_bundle",
                                           include_overview_rows = TRUE) {
  .gp3_qc_check_label(name, "name")
  .gp3_qc_check_logical(include_overview_rows, "include_overview_rows")

  objects <- .gp3_qc_normalise_objects(objects)

  if (!is.null(object_names)) {
    if (!is.character(object_names) || length(object_names) != length(objects)) {
      stop("`object_names` must be a character vector with one name per object.", call. = FALSE)
    }
    names(objects) <- object_names
  }

  if (is.null(names(objects))) {
    names(objects) <- rep("", length(objects))
  }

  empty_names <- !nzchar(names(objects))
  names(objects)[empty_names] <- paste0("object_", which(empty_names))

  object_summaries <- vector("list", length(objects))
  overview_rows <- vector("list", length(objects))

  for (i in seq_along(objects)) {
    collected <- .gp3_qc_collect_one(objects[[i]], names(objects)[[i]], i)

    object_summaries[[i]] <- collected$object_summary

    if (isTRUE(include_overview_rows)) {
      overview_rows[[i]] <- collected$overview_rows
    }
  }

  object_summary <- do.call(rbind, object_summaries)
  row.names(object_summary) <- NULL

  if (isTRUE(include_overview_rows)) {
    overview_rows <- .gp3_qc_bind_overview_rows(overview_rows)
  } else {
    overview_rows <- data.frame()
  }

  status_counts <- .gp3_qc_status_counts(object_summary$qc_status)
  overall_status <- .gp3_qc_overall_status(object_summary$qc_status)

  overview <- data.frame(
    object_name = name,
    n_objects = nrow(object_summary),
    n_overview_rows = sum(object_summary$n_overview_rows, na.rm = TRUE),
    n_pass = .gp3_qc_count_status(object_summary$qc_status, "pass"),
    n_warn = .gp3_qc_count_status(object_summary$qc_status, "warn"),
    n_fail = .gp3_qc_count_status(object_summary$qc_status, "fail"),
    n_info = .gp3_qc_count_status(object_summary$qc_status, "info"),
    n_unknown = .gp3_qc_count_status(object_summary$qc_status, "unknown"),
    qc_bundle_status = overall_status,
    stringsAsFactors = FALSE
  )

  out <- list(
    overview = overview,
    object_summary = object_summary,
    status_counts = status_counts,
    overview_rows = overview_rows,
    settings = data.frame(
      setting = c("name", "include_overview_rows"),
      value = c(name, as.character(include_overview_rows)),
      stringsAsFactors = FALSE
    )
  )

  class(out) <- c("gp3_qc_summary_bundle", "list")
  out
}


#' Summarize Gazepoint QC status
#'
#' Summarizes pass/warn/fail/info/unknown status counts from a QC bundle or an
#' object-summary table produced by `collect_gazepoint_qc_summaries()`.
#'
#' @param qc_bundle A `gp3_qc_summary_bundle`, object-summary data frame, or list
#'   of objects that can be passed to `collect_gazepoint_qc_summaries()`.
#'
#' @return A list with `overview`, `status_counts`, and `object_summary`.
#' @export
#'
#' @examples
#' x <- list(overview = data.frame(audit_status = "ok"))
#' summarize_gazepoint_qc_status(list(x))
summarize_gazepoint_qc_status <- function(qc_bundle) {
  object_summary <- .gp3_qc_get_object_summary(qc_bundle)

  .gp3_require_columns(
    object_summary,
    c("object_name", "qc_status"),
    "QC object summary"
  )

  status_counts <- .gp3_qc_status_counts(object_summary$qc_status)
  overall_status <- .gp3_qc_overall_status(object_summary$qc_status)

  overview <- data.frame(
    n_objects = nrow(object_summary),
    n_pass = .gp3_qc_count_status(object_summary$qc_status, "pass"),
    n_warn = .gp3_qc_count_status(object_summary$qc_status, "warn"),
    n_fail = .gp3_qc_count_status(object_summary$qc_status, "fail"),
    n_info = .gp3_qc_count_status(object_summary$qc_status, "info"),
    n_unknown = .gp3_qc_count_status(object_summary$qc_status, "unknown"),
    qc_overview_status = overall_status,
    stringsAsFactors = FALSE
  )

  out <- list(
    overview = overview,
    status_counts = status_counts,
    object_summary = object_summary
  )

  class(out) <- c("gp3_qc_status_summary", "list")
  out
}


#' @rdname summarize_gazepoint_qc_status
#' @export
summarise_gazepoint_qc_status <- summarize_gazepoint_qc_status


#' Report Gazepoint QC overview
#'
#' Produces compact, cautious text from a QC summary bundle. The report describes
#' available QC outputs and status patterns, but it does not replace the
#' underlying audit, readiness-gate, checklist, or exclusion-recommendation
#' functions.
#'
#' @param qc_bundle A `gp3_qc_summary_bundle`, object-summary data frame, or list
#'   of objects that can be passed to `collect_gazepoint_qc_summaries()`.
#' @param max_objects Maximum number of non-pass objects to name in the report.
#'
#' @return A list with `summary`, `object_summary`, and `report_text`.
#' @export
#'
#' @examples
#' x <- list(overview = data.frame(audit_status = "review", message = "Check coverage."))
#' report_gazepoint_qc_overview(list(example = x))
report_gazepoint_qc_overview <- function(qc_bundle,
                                         max_objects = 5) {
  .gp3_qc_check_positive_integer(max_objects, "max_objects")

  summary <- summarize_gazepoint_qc_status(qc_bundle)
  object_summary <- summary$object_summary
  overview <- summary$overview

  review_objects <- object_summary[
    object_summary$qc_status %in% c("fail", "warn", "unknown"),
    ,
    drop = FALSE
  ]

  review_objects <- review_objects[
    order(
      match(review_objects$qc_status, c("fail", "warn", "unknown")),
      review_objects$object_name
    ),
    ,
    drop = FALSE
  ]

  named_review <- utils::head(review_objects$object_name, max_objects)
  review_text <- paste(named_review, collapse = ", ")

  if (!nzchar(review_text)) {
    review_text <- "none"
  }

  report_text <- paste0(
    "QC overview collected ",
    overview$n_objects[[1]],
    " object(s): ",
    overview$n_pass[[1]],
    " pass, ",
    overview$n_warn[[1]],
    " warn, ",
    overview$n_fail[[1]],
    " fail, ",
    overview$n_info[[1]],
    " info, and ",
    overview$n_unknown[[1]],
    " unknown. Overall QC overview status was '",
    overview$qc_overview_status[[1]],
    "'. Object(s) needing review or interpretation: ",
    review_text,
    ". This overview is a reporting aid only; it does not replace the underlying audit outputs, readiness gates, or exclusion decisions."
  )

  out <- list(
    summary = summary,
    object_summary = object_summary,
    report_text = report_text
  )

  class(out) <- c("gp3_qc_overview_report", "list")
  out
}


.gp3_qc_normalise_objects <- function(objects) {
  if (missing(objects) || is.null(objects)) {
    stop("`objects` must contain at least one object.", call. = FALSE)
  }

  if (is.data.frame(objects) || .gp3_qc_has_overview(objects)) {
    return(list(objects))
  }

  if (!is.list(objects)) {
    return(list(objects))
  }

  if (length(objects) < 1L) {
    stop("`objects` must contain at least one object.", call. = FALSE)
  }

  objects
}


.gp3_qc_has_overview <- function(object) {
  is.list(object) &&
    "overview" %in% names(object) &&
    is.data.frame(object[["overview"]])
}


.gp3_qc_collect_one <- function(object, object_name, index) {
  overview <- .gp3_qc_extract_overview(object)
  object_class <- paste(class(object), collapse = "|")

  if (is.null(overview)) {
    object_summary <- data.frame(
      object_name = object_name,
      object_index = index,
      object_class = object_class,
      overview_available = FALSE,
      n_overview_rows = 0L,
      status_columns = NA_character_,
      message_columns = NA_character_,
      qc_status = "unknown",
      qc_message = "Object had no interpretable overview data frame.",
      stringsAsFactors = FALSE
    )

    return(list(
      object_summary = object_summary,
      overview_rows = data.frame()
    ))
  }

  status_cols <- .gp3_qc_status_columns(overview)
  message_cols <- .gp3_qc_message_columns(overview)

  qc_status <- .gp3_qc_status_from_overview(overview, status_cols)
  qc_message <- .gp3_qc_message_from_overview(overview, message_cols, qc_status)

  object_summary <- data.frame(
    object_name = object_name,
    object_index = index,
    object_class = object_class,
    overview_available = TRUE,
    n_overview_rows = nrow(overview),
    status_columns = .gp3_qc_collapse(status_cols),
    message_columns = .gp3_qc_collapse(message_cols),
    qc_status = qc_status,
    qc_message = qc_message,
    stringsAsFactors = FALSE
  )

  overview_rows <- .gp3_qc_prepare_overview_rows(overview, object_name, index)

  list(
    object_summary = object_summary,
    overview_rows = overview_rows
  )
}


.gp3_qc_extract_overview <- function(object) {
  if (is.data.frame(object)) {
    return(object)
  }

  if (.gp3_qc_has_overview(object)) {
    return(object[["overview"]])
  }

  NULL
}


.gp3_qc_status_columns <- function(overview) {
  candidates <- grep(
    "status|decision|ready|valid|passed|complete|review|flag|warn|fail|error",
    names(overview),
    ignore.case = TRUE,
    value = TRUE
  )

  message_like <- grep(
    "message|reason|recommendation|caution|note|evidence",
    candidates,
    ignore.case = TRUE,
    value = TRUE
  )

  setdiff(candidates, message_like)
}


.gp3_qc_message_columns <- function(overview) {
  grep(
    "message|reason|recommendation|caution|note|evidence",
    names(overview),
    ignore.case = TRUE,
    value = TRUE
  )
}


.gp3_qc_status_from_overview <- function(overview, status_cols) {
  if (length(status_cols) == 0L || nrow(overview) == 0L) {
    return("unknown")
  }

  worst <- "pass"

  for (col in status_cols) {
    values <- overview[[col]]
    col_lower <- tolower(col)

    if (is.logical(values)) {
      if (any(values %in% TRUE, na.rm = TRUE) &&
          grepl("review|flag|warn|fail|error|exclude|problem", col_lower)) {
        worst <- .gp3_qc_worse_status(worst, "warn")
      }

      if (any(values %in% FALSE, na.rm = TRUE) &&
          grepl("ready|valid|passed|complete", col_lower)) {
        worst <- .gp3_qc_worse_status(worst, "fail")
      }

      next
    }

    char_values <- tolower(trimws(as.character(values)))
    char_values <- char_values[!is.na(char_values) & nzchar(char_values)]

    if (length(char_values) == 0L) {
      next
    }

    if (any(grepl("fail|failed|error|invalid|not_ready|not ready|blocked", char_values))) {
      worst <- .gp3_qc_worse_status(worst, "fail")
    } else if (any(grepl("warn|warning|review|caution|partial|incomplete|singular|conditional", char_values))) {
      worst <- .gp3_qc_worse_status(worst, "warn")
    } else if (any(grepl("info|unknown|not_run|not run|missing", char_values))) {
      worst <- .gp3_qc_worse_status(worst, "info")
    } else if (any(grepl("pass|passed|ok|ready|valid|complete|completed|clean|true|yes", char_values))) {
      worst <- .gp3_qc_worse_status(worst, "pass")
    } else {
      worst <- .gp3_qc_worse_status(worst, "info")
    }
  }

  worst
}


.gp3_qc_worse_status <- function(current, candidate) {
  order <- c(pass = 0L, info = 1L, unknown = 1L, warn = 2L, fail = 3L)

  if (unname(order[[candidate]]) > unname(order[[current]])) {
    return(candidate)
  }

  current
}


.gp3_qc_message_from_overview <- function(overview, message_cols, qc_status) {
  if (length(message_cols) == 0L || nrow(overview) == 0L) {
    return(paste0("QC status interpreted as '", qc_status, "'."))
  }

  values <- unlist(overview[message_cols], use.names = FALSE)
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0L) {
    return(paste0("QC status interpreted as '", qc_status, "'."))
  }

  paste(utils::head(unique(values), 3L), collapse = " | ")
}


.gp3_qc_prepare_overview_rows <- function(overview, object_name, index) {
  out <- overview
  out$.gp3_qc_object_name <- object_name
  out$.gp3_qc_object_index <- index
  out$.gp3_qc_row <- seq_len(nrow(out))

  first_cols <- c(".gp3_qc_object_name", ".gp3_qc_object_index", ".gp3_qc_row")
  out[c(first_cols, setdiff(names(out), first_cols))]
}


.gp3_qc_bind_overview_rows <- function(rows) {
  rows <- rows[vapply(rows, is.data.frame, logical(1))]
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]

  if (length(rows) == 0L) {
    return(data.frame())
  }

  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))

  rows <- lapply(rows, function(x) {
    missing_cols <- setdiff(all_names, names(x))
    for (col in missing_cols) {
      x[[col]] <- NA
    }
    x[all_names]
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}


.gp3_qc_get_object_summary <- function(qc_bundle) {
  if (inherits(qc_bundle, "gp3_qc_summary_bundle") &&
      is.data.frame(qc_bundle$object_summary)) {
    return(qc_bundle$object_summary)
  }

  if (is.data.frame(qc_bundle) &&
      all(c("object_name", "qc_status") %in% names(qc_bundle))) {
    return(qc_bundle)
  }

  collect_gazepoint_qc_summaries(qc_bundle)$object_summary
}


.gp3_qc_status_counts <- function(status) {
  status <- as.character(status)
  levels <- c("pass", "warn", "fail", "info", "unknown")

  data.frame(
    qc_status = levels,
    n_objects = vapply(levels, function(x) sum(status == x, na.rm = TRUE), integer(1)),
    stringsAsFactors = FALSE
  )
}


.gp3_qc_count_status <- function(status, value) {
  sum(as.character(status) == value, na.rm = TRUE)
}


.gp3_qc_overall_status <- function(status) {
  status <- as.character(status)

  if (any(status == "fail", na.rm = TRUE)) {
    return("fail")
  }

  if (any(status == "warn", na.rm = TRUE)) {
    return("warn")
  }

  if (any(status %in% c("info", "unknown"), na.rm = TRUE)) {
    return("info")
  }

  "pass"
}


.gp3_qc_collapse <- function(x) {
  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(unique(as.character(x)), collapse = ", ")
}


.gp3_qc_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a single non-empty character string.", call. = FALSE)
  }

  invisible(TRUE)
}


.gp3_qc_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}


.gp3_qc_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}
