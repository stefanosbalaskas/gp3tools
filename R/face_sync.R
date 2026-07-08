#' Synchronise external facial-behaviour data with Gazepoint data
#'
#' Aligns external facial-behaviour data to Gazepoint rows using either nearest
#' time matching or exact frame matching. The helper is designed for facial data
#' previously imported with `read_gazepoint_face_export()` and standardised with
#' `standardize_gazepoint_face_columns()`. It does not infer facial expressions
#' from Gazepoint exports and does not interpret facial behaviour as emotion.
#'
#' @param gazepoint_data Gazepoint data frame, typically a master table, trial
#'   table, or sample-level table.
#' @param face_data External facial-behaviour data frame, preferably returned by
#'   `standardize_gazepoint_face_columns()`.
#' @param method Synchronisation method. `"nearest_time"` matches each Gazepoint
#'   row to the nearest facial-data row within group. `"frame_exact"` matches by
#'   exact frame index within group.
#' @param by Optional named character vector mapping Gazepoint grouping columns
#'   to facial-data grouping columns. For example,
#'   `c(subject_id = "participant_id")`. Use `NULL` for no grouping.
#' @param gaze_time_col Gazepoint time column. Required for
#'   `method = "nearest_time"` unless auto-detected.
#' @param face_time_col Facial-data time column. Defaults to `face_time_sec`
#'   when available.
#' @param gaze_frame_col Gazepoint frame column. Required for
#'   `method = "frame_exact"` unless auto-detected.
#' @param face_frame_col Facial-data frame column. Defaults to `face_frame`
#'   when available.
#' @param tolerance_sec Maximum allowed absolute time difference in seconds for
#'   nearest-time matching.
#' @param prefix Prefix added to non-standard facial-data columns before joining.
#' @param keep_unmatched Should Gazepoint rows without a valid face-data match be
#'   retained?
#' @param standardize_face Should `face_data` be passed through
#'   `standardize_gazepoint_face_columns()` before synchronisation when needed?
#'
#' @return A tibble with Gazepoint columns plus matched facial-behaviour columns
#'   and synchronisation metadata. The returned object has class
#'   `gp3_face_sync`.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   subject_id = "P001",
#'   time_sec = c(0.00, 0.03, 0.07),
#'   AOI = c("A", "A", "B")
#' )
#'
#' face <- data.frame(
#'   participant_id = "P001",
#'   frame = 1:3,
#'   timestamp = c(0.00, 0.033, 0.066),
#'   confidence = c(0.95, 0.94, 0.93),
#'   success = c(1, 1, 1),
#'   AU12_r = c(0.1, 0.2, 0.3)
#' )
#'
#' sync_gazepoint_face_data(
#'   gaze,
#'   face,
#'   by = c(subject_id = "participant_id"),
#'   gaze_time_col = "time_sec"
#' )
sync_gazepoint_face_data <- function(gazepoint_data,
                                     face_data,
                                     method = c("nearest_time", "frame_exact"),
                                     by = NULL,
                                     gaze_time_col = NULL,
                                     face_time_col = NULL,
                                     gaze_frame_col = NULL,
                                     face_frame_col = NULL,
                                     tolerance_sec = 0.050,
                                     prefix = "face_",
                                     keep_unmatched = TRUE,
                                     standardize_face = TRUE) {
  method <- match.arg(method)

  if (!is.data.frame(gazepoint_data)) {
    stop("`gazepoint_data` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(face_data)) {
    stop("`face_data` must be a data frame.", call. = FALSE)
  }

  gaze <- as.data.frame(gazepoint_data, stringsAsFactors = FALSE)

  face <- .gp3_face_sync_prepare_face(
    face_data = face_data,
    standardize_face = standardize_face
  )

  by <- .gp3_face_sync_validate_by(
    by = by,
    gaze = gaze,
    face = face
  )

  gaze$.gp3_face_sync_gaze_row <- seq_len(nrow(gaze))
  face$.gp3_face_sync_face_row <- seq_len(nrow(face))

  if (identical(method, "nearest_time")) {
    gaze_time_col <- .gp3_face_sync_choose_col(
      gaze,
      gaze_time_col,
      c(
        "time_sec",
        "time_seconds",
        "time",
        "TIME",
        "timestamp",
        "timestamp_sec",
        "trial_time_sec",
        "relative_time_sec"
      ),
      "`gaze_time_col`"
    )

    face_time_col <- .gp3_face_sync_choose_col(
      face,
      face_time_col,
      c(
        "face_time_sec",
        "timestamp",
        "time_sec",
        "time_seconds",
        "time",
        "seconds"
      ),
      "`face_time_col`"
    )

    out <- .gp3_face_sync_nearest_time(
      gaze = gaze,
      face = face,
      by = by,
      gaze_time_col = gaze_time_col,
      face_time_col = face_time_col,
      tolerance_sec = tolerance_sec,
      prefix = prefix,
      keep_unmatched = keep_unmatched
    )
  } else {
    gaze_frame_col <- .gp3_face_sync_choose_col(
      gaze,
      gaze_frame_col,
      c("VID_FRAME", "video_frame", "frame", "frame_id", "face_frame"),
      "`gaze_frame_col`"
    )

    face_frame_col <- .gp3_face_sync_choose_col(
      face,
      face_frame_col,
      c("face_frame", "frame", "Frame", "FRAME", "video_frame", "frame_id"),
      "`face_frame_col`"
    )

    out <- .gp3_face_sync_frame_exact(
      gaze = gaze,
      face = face,
      by = by,
      gaze_frame_col = gaze_frame_col,
      face_frame_col = face_frame_col,
      prefix = prefix,
      keep_unmatched = keep_unmatched
    )
  }

  out <- tibble::as_tibble(out)
  class(out) <- c("gp3_face_sync", class(out))
  attr(out, "gp3_face_sync_settings") <- list(
    method = method,
    by = by,
    gaze_time_col = if (exists("gaze_time_col")) gaze_time_col else NULL,
    face_time_col = if (exists("face_time_col")) face_time_col else NULL,
    gaze_frame_col = if (exists("gaze_frame_col")) gaze_frame_col else NULL,
    face_frame_col = if (exists("face_frame_col")) face_frame_col else NULL,
    tolerance_sec = tolerance_sec,
    prefix = prefix,
    keep_unmatched = keep_unmatched,
    standardize_face = standardize_face
  )

  out
}


.gp3_face_sync_prepare_face <- function(face_data,
                                        standardize_face = TRUE) {
  face <- as.data.frame(face_data, stringsAsFactors = FALSE)

  required <- c("face_frame", "face_time_sec", "face_confidence", "face_valid")
  missing_required <- setdiff(required, names(face))

  if (standardize_face || length(missing_required) > 0L) {
    face <- standardize_gazepoint_face_columns(face)
  }

  as.data.frame(face, stringsAsFactors = FALSE)
}


.gp3_face_sync_validate_by <- function(by, gaze, face) {
  if (is.null(by)) {
    return(NULL)
  }

  if (!is.character(by) || length(by) < 1L) {
    stop("`by` must be a named character vector or `NULL`.", call. = FALSE)
  }

  if (is.null(names(by)) || any(names(by) == "")) {
    stop(
      "`by` must be named as Gazepoint columns with facial-data columns as values.",
      call. = FALSE
    )
  }

  missing_gaze <- setdiff(names(by), names(gaze))
  missing_face <- setdiff(unname(by), names(face))

  if (length(missing_gaze) > 0L) {
    stop(
      "Gazepoint grouping column(s) not found: ",
      paste(missing_gaze, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_face) > 0L) {
    stop(
      "Facial-data grouping column(s) not found: ",
      paste(missing_face, collapse = ", "),
      call. = FALSE
    )
  }

  by
}


.gp3_face_sync_choose_col <- function(data, supplied, candidates, arg_name) {
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

  stop(
    arg_name,
    " could not be detected automatically. Please supply it explicitly.",
    call. = FALSE
  )
}


.gp3_face_sync_group_key <- function(data, cols) {
  if (is.null(cols) || length(cols) < 1L) {
    return(rep("overall", nrow(data)))
  }

  x <- data[, cols, drop = FALSE]
  x[] <- lapply(x, function(v) {
    v <- as.character(v)
    v[is.na(v) | v == ""] <- "missing"
    v
  })

  do.call(paste, c(x, sep = " | "))
}


.gp3_face_sync_prefix_face <- function(face,
                                       prefix = "face_",
                                       protected = character(0)) {
  face_names <- names(face)

  for (i in seq_along(face_names)) {
    nm <- face_names[[i]]

    if (nm %in% protected) {
      next
    }

    if (startsWith(nm, prefix)) {
      next
    }

    face_names[[i]] <- paste0(prefix, nm)
  }

  names(face) <- make.unique(face_names, sep = "_")
  face
}


.gp3_face_sync_nearest_time <- function(gaze,
                                        face,
                                        by = NULL,
                                        gaze_time_col,
                                        face_time_col,
                                        tolerance_sec = 0.050,
                                        prefix = "face_",
                                        keep_unmatched = TRUE) {
  gaze_time <- suppressWarnings(as.numeric(gaze[[gaze_time_col]]))
  face_time <- suppressWarnings(as.numeric(face[[face_time_col]]))

  gaze_key <- .gp3_face_sync_group_key(gaze, names(by))
  face_key <- .gp3_face_sync_group_key(face, unname(by))

  matched_face_row <- rep(NA_integer_, nrow(gaze))
  sync_diff_sec <- rep(NA_real_, nrow(gaze))
  sync_status <- rep("unmatched", nrow(gaze))

  face_by_key <- split(seq_len(nrow(face)), face_key)

  for (key in unique(gaze_key)) {
    gaze_idx <- which(gaze_key == key)
    face_idx <- face_by_key[[key]]

    if (is.null(face_idx) || length(face_idx) < 1L) {
      next
    }

    face_time_key <- face_time[face_idx]
    valid_face <- is.finite(face_time_key)

    if (!any(valid_face)) {
      next
    }

    face_idx <- face_idx[valid_face]
    face_time_key <- face_time[face_idx]

    for (g in gaze_idx) {
      if (!is.finite(gaze_time[[g]])) {
        sync_status[[g]] <- "missing_gaze_time"
        next
      }

      diff_abs <- abs(face_time_key - gaze_time[[g]])
      best <- which.min(diff_abs)

      if (length(best) < 1L || !is.finite(diff_abs[[best]])) {
        next
      }

      matched_face_row[[g]] <- face_idx[[best]]
      sync_diff_sec[[g]] <- face_time_key[[best]] - gaze_time[[g]]

      if (abs(sync_diff_sec[[g]]) <= tolerance_sec) {
        sync_status[[g]] <- "matched"
      } else {
        sync_status[[g]] <- "outside_tolerance"
      }
    }
  }

  matched <- !is.na(matched_face_row)

  face_join <- .gp3_face_sync_prefix_face(
    face,
    prefix = prefix,
    protected = ".gp3_face_sync_face_row"
  )

  joined <- cbind(
    gaze,
    face_join[matched_face_row, , drop = FALSE],
    stringsAsFactors = FALSE
  )

  joined$face_sync_method <- "nearest_time"
  joined$face_sync_status <- sync_status
  joined$face_sync_diff_sec <- sync_diff_sec
  joined$face_sync_abs_diff_sec <- abs(sync_diff_sec)
  joined$face_sync_within_tolerance <- sync_status == "matched"
  joined$face_sync_tolerance_sec <- tolerance_sec

  if (!keep_unmatched) {
    joined <- joined[matched & sync_status == "matched", , drop = FALSE]
  }

  .gp3_face_sync_reorder(joined)
}


.gp3_face_sync_frame_exact <- function(gaze,
                                       face,
                                       by = NULL,
                                       gaze_frame_col,
                                       face_frame_col,
                                       prefix = "face_",
                                       keep_unmatched = TRUE) {
  gaze_frame <- suppressWarnings(as.character(gaze[[gaze_frame_col]]))
  face_frame <- suppressWarnings(as.character(face[[face_frame_col]]))

  gaze_key <- .gp3_face_sync_group_key(gaze, names(by))
  face_key <- .gp3_face_sync_group_key(face, unname(by))

  gaze_match_key <- paste(gaze_key, gaze_frame, sep = " || ")
  face_match_key <- paste(face_key, face_frame, sep = " || ")

  face_first <- !duplicated(face_match_key)
  face_lookup_key <- face_match_key[face_first]
  face_lookup_row <- seq_len(nrow(face))[face_first]

  matched_face_row <- face_lookup_row[match(gaze_match_key, face_lookup_key)]

  sync_status <- ifelse(
    is.na(gaze_frame) | gaze_frame == "NA",
    "missing_gaze_frame",
    ifelse(is.na(matched_face_row), "unmatched", "matched")
  )

  face_join <- .gp3_face_sync_prefix_face(
    face,
    prefix = prefix,
    protected = ".gp3_face_sync_face_row"
  )

  joined <- cbind(
    gaze,
    face_join[matched_face_row, , drop = FALSE],
    stringsAsFactors = FALSE
  )

  joined$face_sync_method <- "frame_exact"
  joined$face_sync_status <- sync_status
  joined$face_sync_diff_sec <- NA_real_
  joined$face_sync_abs_diff_sec <- NA_real_
  joined$face_sync_within_tolerance <- sync_status == "matched"
  joined$face_sync_tolerance_sec <- NA_real_

  if (!keep_unmatched) {
    joined <- joined[sync_status == "matched", , drop = FALSE]
  }

  .gp3_face_sync_reorder(joined)
}


.gp3_face_sync_reorder <- function(x) {
  sync_cols <- c(
    "face_sync_method",
    "face_sync_status",
    "face_sync_diff_sec",
    "face_sync_abs_diff_sec",
    "face_sync_within_tolerance",
    "face_sync_tolerance_sec"
  )

  sync_cols <- intersect(sync_cols, names(x))
  other_cols <- setdiff(names(x), sync_cols)

  x[, c(other_cols, sync_cols), drop = FALSE]
}
#' Audit synchronisation between Gazepoint and external facial-behaviour data
#'
#' Summarises the output of `sync_gazepoint_face_data()` by reporting matched,
#' unmatched, outside-tolerance, and missing-timing/frame rows. For nearest-time
#' synchronisation, it also reports absolute time-difference summaries. The
#' helper audits alignment quality only; it does not infer facial expressions or
#' emotional states.
#'
#' @param data A data frame returned by `sync_gazepoint_face_data()`.
#' @param group_cols Optional character vector of grouping columns. Columns not
#'   present in `data` are ignored. Use `NULL` for an overall-only audit.
#' @param min_matched_percent Minimum percentage of rows that must have
#'   `face_sync_status == "matched"` for a group to pass.
#' @param warning_matched_percent Percentage below which a group is marked as
#'   `"warn"` when still above `min_matched_percent`.
#' @param max_abs_diff_sec Optional maximum allowed absolute synchronisation
#'   difference in seconds. Only evaluated when `face_sync_abs_diff_sec` is
#'   available.
#'
#' @return A list with `overview`, `group_summary`, `issue_summary`, `data`, and
#'   `settings`. The returned object has class `gp3_face_sync_audit`.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   subject_id = "P001",
#'   time_sec = c(0.00, 0.03, 0.07)
#' )
#'
#' face <- data.frame(
#'   participant_id = "P001",
#'   frame = 1:3,
#'   timestamp = c(0.00, 0.033, 0.066),
#'   confidence = c(0.95, 0.94, 0.93),
#'   success = c(1, 1, 1)
#' )
#'
#' synced <- sync_gazepoint_face_data(
#'   gaze,
#'   face,
#'   by = c(subject_id = "participant_id"),
#'   gaze_time_col = "time_sec"
#' )
#'
#' audit_gazepoint_face_sync(synced)
audit_gazepoint_face_sync <- function(data,
                                      group_cols = NULL,
                                      min_matched_percent = 70,
                                      warning_matched_percent = 85,
                                      max_abs_diff_sec = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame returned by sync_gazepoint_face_data().",
         call. = FALSE)
  }

  required <- c(
    "face_sync_method",
    "face_sync_status",
    "face_sync_within_tolerance"
  )

  missing_required <- setdiff(required, names(data))

  if (length(missing_required) > 0L) {
    stop(
      "`data` does not look like synchronised face data. Missing column(s): ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)

  if (!is.null(group_cols)) {
    group_cols <- intersect(group_cols, names(data))
  }

  if (length(group_cols) < 1L) {
    group_cols <- NULL
  }

  groups <- .gp3_face_sync_audit_group_indices(data, group_cols)

  group_summary <- lapply(groups, function(idx) {
    .gp3_face_sync_audit_summarise_subset(
      data = data,
      idx = idx,
      group_cols = group_cols,
      min_matched_percent = min_matched_percent,
      warning_matched_percent = warning_matched_percent,
      max_abs_diff_sec = max_abs_diff_sec
    )
  })

  group_summary <- .gp3_face_sync_audit_bind_rows(group_summary)
  group_summary <- tibble::as_tibble(group_summary)

  overview <- .gp3_face_sync_audit_summarise_subset(
    data = data,
    idx = seq_len(nrow(data)),
    group_cols = NULL,
    min_matched_percent = min_matched_percent,
    warning_matched_percent = warning_matched_percent,
    max_abs_diff_sec = max_abs_diff_sec
  )

  overview$face_sync_group <- NULL
  overview <- cbind(
    data.frame(
      n_groups = nrow(group_summary),
      stringsAsFactors = FALSE
    ),
    overview,
    stringsAsFactors = FALSE
  )

  overview <- tibble::as_tibble(overview)

  issue_summary <- .gp3_face_sync_audit_issue_summary(
    group_summary = group_summary,
    min_matched_percent = min_matched_percent,
    warning_matched_percent = warning_matched_percent,
    max_abs_diff_sec = max_abs_diff_sec
  )

  out <- list(
    overview = overview,
    group_summary = group_summary,
    issue_summary = issue_summary,
    data = tibble::as_tibble(data),
    settings = list(
      group_cols = group_cols,
      min_matched_percent = min_matched_percent,
      warning_matched_percent = warning_matched_percent,
      max_abs_diff_sec = max_abs_diff_sec
    )
  )

  class(out) <- c("gp3_face_sync_audit", class(out))

  out
}


.gp3_face_sync_audit_group_indices <- function(data, group_cols = NULL) {
  if (is.null(group_cols) || length(group_cols) < 1L) {
    return(list(overall = seq_len(nrow(data))))
  }

  group_data <- data[, group_cols, drop = FALSE]
  group_data[] <- lapply(group_data, function(x) {
    x <- as.character(x)
    x[is.na(x) | x == ""] <- "missing"
    x
  })

  group_id <- do.call(paste, c(group_data, sep = " | "))
  split(seq_len(nrow(data)), group_id)
}


.gp3_face_sync_audit_summarise_subset <- function(data,
                                                  idx,
                                                  group_cols = NULL,
                                                  min_matched_percent = 70,
                                                  warning_matched_percent = 85,
                                                  max_abs_diff_sec = NULL) {
  n_rows <- length(idx)

  group_values <- .gp3_face_sync_audit_group_values(data, idx, group_cols)

  status <- as.character(data$face_sync_status[idx])
  within_tolerance <- data$face_sync_within_tolerance[idx]

  abs_diff <- if ("face_sync_abs_diff_sec" %in% names(data)) {
    suppressWarnings(as.numeric(data$face_sync_abs_diff_sec[idx]))
  } else {
    rep(NA_real_, n_rows)
  }

  n_matched <- sum(status == "matched", na.rm = TRUE)
  n_unmatched <- sum(status == "unmatched", na.rm = TRUE)
  n_outside_tolerance <- sum(status == "outside_tolerance", na.rm = TRUE)
  n_missing_gaze_time <- sum(status == "missing_gaze_time", na.rm = TRUE)
  n_missing_gaze_frame <- sum(status == "missing_gaze_frame", na.rm = TRUE)
  n_unknown_status <- sum(is.na(status) | status == "", na.rm = TRUE)

  matched_percent <- .gp3_face_sync_audit_percent(n_matched, n_rows)
  unmatched_percent <- .gp3_face_sync_audit_percent(n_unmatched, n_rows)
  outside_tolerance_percent <- .gp3_face_sync_audit_percent(
    n_outside_tolerance,
    n_rows
  )

  n_within_tolerance <- sum(within_tolerance %in% TRUE, na.rm = TRUE)
  within_tolerance_percent <- .gp3_face_sync_audit_percent(
    n_within_tolerance,
    n_rows
  )

  finite_abs_diff <- abs_diff[is.finite(abs_diff)]

  mean_abs_diff_sec <- if (length(finite_abs_diff) > 0L) {
    mean(finite_abs_diff)
  } else {
    NA_real_
  }

  median_abs_diff_sec <- if (length(finite_abs_diff) > 0L) {
    stats::median(finite_abs_diff)
  } else {
    NA_real_
  }

  max_abs_diff_sec_observed <- if (length(finite_abs_diff) > 0L) {
    max(finite_abs_diff)
  } else {
    NA_real_
  }

  p95_abs_diff_sec <- if (length(finite_abs_diff) > 0L) {
    as.numeric(stats::quantile(finite_abs_diff, probs = 0.95, names = FALSE))
  } else {
    NA_real_
  }

  n_abs_diff_above_limit <- if (!is.null(max_abs_diff_sec)) {
    sum(abs_diff > max_abs_diff_sec, na.rm = TRUE)
  } else {
    NA_integer_
  }

  face_sync_audit_status <- .gp3_face_sync_audit_status(
    n_rows = n_rows,
    matched_percent = matched_percent,
    max_abs_diff_sec_observed = max_abs_diff_sec_observed,
    min_matched_percent = min_matched_percent,
    warning_matched_percent = warning_matched_percent,
    max_abs_diff_sec = max_abs_diff_sec
  )

  message <- .gp3_face_sync_audit_message(
    face_sync_audit_status = face_sync_audit_status,
    matched_percent = matched_percent,
    min_matched_percent = min_matched_percent,
    warning_matched_percent = warning_matched_percent
  )

  metrics <- data.frame(
    n_rows = n_rows,
    n_matched = n_matched,
    matched_percent = matched_percent,
    n_unmatched = n_unmatched,
    unmatched_percent = unmatched_percent,
    n_outside_tolerance = n_outside_tolerance,
    outside_tolerance_percent = outside_tolerance_percent,
    n_missing_gaze_time = n_missing_gaze_time,
    n_missing_gaze_frame = n_missing_gaze_frame,
    n_unknown_status = n_unknown_status,
    n_within_tolerance = n_within_tolerance,
    within_tolerance_percent = within_tolerance_percent,
    mean_abs_diff_sec = mean_abs_diff_sec,
    median_abs_diff_sec = median_abs_diff_sec,
    p95_abs_diff_sec = p95_abs_diff_sec,
    max_abs_diff_sec = max_abs_diff_sec_observed,
    n_abs_diff_above_limit = n_abs_diff_above_limit,
    face_sync_audit_status = face_sync_audit_status,
    message = message,
    stringsAsFactors = FALSE
  )

  cbind(group_values, metrics, stringsAsFactors = FALSE)
}


.gp3_face_sync_audit_group_values <- function(data, idx, group_cols = NULL) {
  if (is.null(group_cols) || length(group_cols) < 1L) {
    return(
      data.frame(
        face_sync_group = "overall",
        stringsAsFactors = FALSE
      )
    )
  }

  vals <- data[idx[[1L]], group_cols, drop = FALSE]
  vals[] <- lapply(vals, as.character)
  vals[is.na(vals)] <- "missing"

  group_label <- paste(
    paste0(names(vals), "=", unlist(vals, use.names = FALSE)),
    collapse = " | "
  )

  cbind(
    data.frame(
      face_sync_group = group_label,
      stringsAsFactors = FALSE
    ),
    vals,
    stringsAsFactors = FALSE
  )
}


.gp3_face_sync_audit_status <- function(n_rows,
                                        matched_percent,
                                        max_abs_diff_sec_observed,
                                        min_matched_percent = 70,
                                        warning_matched_percent = 85,
                                        max_abs_diff_sec = NULL) {
  if (n_rows < 1L) {
    return("fail")
  }

  if (is.na(matched_percent)) {
    return("unknown")
  }

  if (matched_percent < min_matched_percent) {
    return("fail")
  }

  if (matched_percent < warning_matched_percent) {
    return("warn")
  }

  if (
    !is.null(max_abs_diff_sec) &&
    !is.na(max_abs_diff_sec_observed) &&
    max_abs_diff_sec_observed > max_abs_diff_sec
  ) {
    return("warn")
  }

  "pass"
}


.gp3_face_sync_audit_message <- function(face_sync_audit_status,
                                         matched_percent,
                                         min_matched_percent = 70,
                                         warning_matched_percent = 85) {
  if (identical(face_sync_audit_status, "unknown")) {
    return("Face-data synchronisation quality could not be evaluated.")
  }

  if (identical(face_sync_audit_status, "fail")) {
    return(
      paste0(
        "Face-data synchronisation is below the minimum threshold (",
        round(matched_percent, 1),
        "% matched; minimum ",
        min_matched_percent,
        "%)."
      )
    )
  }

  if (identical(face_sync_audit_status, "warn")) {
    return(
      paste0(
        "Face-data synchronisation should be reviewed before analysis (",
        round(matched_percent, 1),
        "% matched; warning threshold ",
        warning_matched_percent,
        "%)."
      )
    )
  }

  "Face-data synchronisation passed the configured checks."
}


.gp3_face_sync_audit_issue_summary <- function(group_summary,
                                               min_matched_percent = 70,
                                               warning_matched_percent = 85,
                                               max_abs_diff_sec = NULL) {
  n_groups <- nrow(group_summary)

  out <- data.frame(
    issue = c(
      "matched_percent_below_minimum",
      "matched_percent_below_warning",
      "unmatched_rows",
      "outside_tolerance_rows",
      "missing_gaze_time_rows",
      "missing_gaze_frame_rows",
      "large_time_differences"
    ),
    n_groups_affected = c(
      sum(group_summary$matched_percent < min_matched_percent, na.rm = TRUE),
      sum(group_summary$matched_percent < warning_matched_percent, na.rm = TRUE),
      sum(group_summary$n_unmatched > 0, na.rm = TRUE),
      sum(group_summary$n_outside_tolerance > 0, na.rm = TRUE),
      sum(group_summary$n_missing_gaze_time > 0, na.rm = TRUE),
      sum(group_summary$n_missing_gaze_frame > 0, na.rm = TRUE),
      if (!is.null(max_abs_diff_sec)) {
        sum(group_summary$max_abs_diff_sec > max_abs_diff_sec, na.rm = TRUE)
      } else {
        NA_integer_
      }
    ),
    n_groups = n_groups,
    threshold = c(
      min_matched_percent,
      warning_matched_percent,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      if (is.null(max_abs_diff_sec)) NA_real_ else max_abs_diff_sec
    ),
    stringsAsFactors = FALSE
  )

  out$status <- ifelse(
    is.na(out$n_groups_affected),
    "not_checked",
    ifelse(out$n_groups_affected > 0L, "review", "ok")
  )

  tibble::as_tibble(out)
}


.gp3_face_sync_audit_percent <- function(x, n) {
  if (is.na(n) || n <= 0) {
    return(NA_real_)
  }

  100 * x / n
}


.gp3_face_sync_audit_bind_rows <- function(x) {
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
