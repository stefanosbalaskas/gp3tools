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
