#' Prepare Gazepoint master data for eyetools-style workflows
#'
#' Convert a `gp3tools` master table into a dependency-free, eyetools-friendly
#' sample-level table. The returned data frame keeps one row per sample and
#' creates standard participant, trial, time, gaze-coordinate, binocular
#' coordinate, pupil, AOI, fixation, event, validity, trackloss, and status
#' columns.
#'
#' @param data A Gazepoint master table or sample-level gaze data frame.
#' @param participant_col Participant/subject identifier column.
#' @param trial_col Trial identifier column. If `NULL`, a trial identifier is
#'   created from `media_col` when available.
#' @param time_col Sample time column.
#' @param x_col Optional primary gaze x-coordinate column.
#' @param y_col Optional primary gaze y-coordinate column.
#' @param left_x_col Optional left-eye x-coordinate column.
#' @param left_y_col Optional left-eye y-coordinate column.
#' @param right_x_col Optional right-eye x-coordinate column.
#' @param right_y_col Optional right-eye y-coordinate column.
#' @param pupil_col Optional primary pupil column.
#' @param left_pupil_col Optional left-eye pupil column.
#' @param right_pupil_col Optional right-eye pupil column.
#' @param media_col Optional media/stimulus identifier column.
#' @param condition_col Optional condition/grouping column.
#' @param aoi_col Optional AOI label/state column.
#' @param fixation_col Optional fixation identifier column.
#' @param event_col Optional event/marker column.
#' @param validity_cols Optional validity columns used to define trackloss.
#' @param trackloss_col Optional existing trackloss column. If supplied, it is
#'   used directly after coercion to logical.
#' @param screen_x_range Numeric length-2 vector defining the screen x range.
#' @param screen_y_range Numeric length-2 vector defining the screen y range.
#' @param missing_aoi_label Label used for missing AOI values.
#' @param keep_original_cols Logical. If `TRUE`, original columns are retained
#'   after the standard adapter columns.
#'
#' @return A tibble with class `gp3_eyetools_data`.
#' @export
prepare_gazepoint_eyetools_data <- function(
    data,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    x_col = NULL,
    y_col = NULL,
    left_x_col = NULL,
    left_y_col = NULL,
    right_x_col = NULL,
    right_y_col = NULL,
    pupil_col = NULL,
    left_pupil_col = NULL,
    right_pupil_col = NULL,
    media_col = NULL,
    condition_col = NULL,
    aoi_col = NULL,
    fixation_col = NULL,
    event_col = NULL,
    validity_cols = NULL,
    trackloss_col = NULL,
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1),
    missing_aoi_label = "missing_aoi",
    keep_original_cols = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_eyetools_check_range(screen_x_range, "screen_x_range")
  .gp3_eyetools_check_range(screen_y_range, "screen_y_range")
  .gp3_eyetools_check_label(missing_aoi_label, "missing_aoi_label")
  .gp3_eyetools_check_logical_scalar(keep_original_cols, "keep_original_cols")

  participant_col <- .gp3_eyetools_resolve_or_detect_col(
    col = participant_col,
    names_data = names(data),
    arg = "participant_col",
    candidates = c(
      "subject", "participant", "participant_id", "USER_FILE",
      "user", "user_id", "recording_id"
    ),
    required = TRUE
  )

  time_col <- .gp3_eyetools_resolve_or_detect_col(
    col = time_col,
    names_data = names(data),
    arg = "time_col",
    candidates = c(
      "time", "time_ms", "timestamp", "TIMESTAMP",
      "TIME", "TIME_TICK", "CNT"
    ),
    required = TRUE
  )

  media_col <- .gp3_eyetools_resolve_or_detect_col(
    col = media_col,
    names_data = names(data),
    arg = "media_col",
    candidates = c("media_id", "MEDIA_ID", "stimulus", "stimulus_id", "image"),
    required = FALSE
  )

  trial_col <- .gp3_eyetools_resolve_or_detect_col(
    col = trial_col,
    names_data = names(data),
    arg = "trial_col",
    candidates = c(
      "trial_global", "trial", "trial_id", "TRIAL_INDEX",
      "trial_number", "item_trial"
    ),
    required = FALSE
  )

  x_col <- .gp3_eyetools_resolve_or_detect_col(
    col = x_col,
    names_data = names(data),
    arg = "x_col",
    candidates = c("x", "X", "gaze_x", "gaze_x_norm", "FPOGX", "BPOGX"),
    required = FALSE
  )

  y_col <- .gp3_eyetools_resolve_or_detect_col(
    col = y_col,
    names_data = names(data),
    arg = "y_col",
    candidates = c("y", "Y", "gaze_y", "gaze_y_norm", "FPOGY", "BPOGY"),
    required = FALSE
  )

  left_x_col <- .gp3_eyetools_resolve_or_detect_col(
    col = left_x_col,
    names_data = names(data),
    arg = "left_x_col",
    candidates = c("left_x", "gaze_left_x", "LPOGX", "LPCX"),
    required = FALSE
  )

  left_y_col <- .gp3_eyetools_resolve_or_detect_col(
    col = left_y_col,
    names_data = names(data),
    arg = "left_y_col",
    candidates = c("left_y", "gaze_left_y", "LPOGY", "LPCY"),
    required = FALSE
  )

  right_x_col <- .gp3_eyetools_resolve_or_detect_col(
    col = right_x_col,
    names_data = names(data),
    arg = "right_x_col",
    candidates = c("right_x", "gaze_right_x", "RPOGX", "RPCX"),
    required = FALSE
  )

  right_y_col <- .gp3_eyetools_resolve_or_detect_col(
    col = right_y_col,
    names_data = names(data),
    arg = "right_y_col",
    candidates = c("right_y", "gaze_right_y", "RPOGY", "RPCY"),
    required = FALSE
  )

  pupil_col <- .gp3_eyetools_resolve_or_detect_col(
    col = pupil_col,
    names_data = names(data),
    arg = "pupil_col",
    candidates = c(
      "pupil_smoothed",
      "pupil_baseline_corrected",
      "pupil_interpolated",
      "pupil_clean",
      "pupil_for_preprocessing",
      "pupil",
      "PUPIL",
      "BPOPD",
      "LPOPD",
      "RPOPD",
      "LPD",
      "RPD"
    ),
    required = FALSE
  )

  left_pupil_col <- .gp3_eyetools_resolve_or_detect_col(
    col = left_pupil_col,
    names_data = names(data),
    arg = "left_pupil_col",
    candidates = c("left_pupil", "pupil_left", "LPOPD", "LPD"),
    required = FALSE
  )

  right_pupil_col <- .gp3_eyetools_resolve_or_detect_col(
    col = right_pupil_col,
    names_data = names(data),
    arg = "right_pupil_col",
    candidates = c("right_pupil", "pupil_right", "RPOPD", "RPD"),
    required = FALSE
  )

  condition_col <- .gp3_eyetools_resolve_or_detect_col(
    col = condition_col,
    names_data = names(data),
    arg = "condition_col",
    candidates = c("condition", "Condition", "group", "GROUP"),
    required = FALSE
  )

  aoi_col <- .gp3_eyetools_resolve_or_detect_col(
    col = aoi_col,
    names_data = names(data),
    arg = "aoi_col",
    candidates = c(
      "aoi_current", "aoi", "AOI", "aoi_label",
      "observed_aoi", "AOI_LABEL"
    ),
    required = FALSE
  )

  fixation_col <- .gp3_eyetools_resolve_or_detect_col(
    col = fixation_col,
    names_data = names(data),
    arg = "fixation_col",
    candidates = c(
      "fixation_id", "fixation", "FPOGID", "FPOG_ID",
      "CURRENT_FIX_INDEX"
    ),
    required = FALSE
  )

  event_col <- .gp3_eyetools_resolve_or_detect_col(
    col = event_col,
    names_data = names(data),
    arg = "event_col",
    candidates = c("event_label", "event", "EVENT", "marker", "marker_label"),
    required = FALSE
  )

  trackloss_col <- .gp3_eyetools_resolve_or_detect_col(
    col = trackloss_col,
    names_data = names(data),
    arg = "trackloss_col",
    candidates = c("trackloss", "track_loss", "is_trackloss"),
    required = FALSE
  )

  if (!is.null(validity_cols)) {
    validity_cols <- .gp3_eyetools_resolve_cols(
      validity_cols,
      names(data),
      "validity_cols"
    )
  } else {
    validity_cols <- intersect(
      c(
        "FPOGV", "LPOGV", "RPOGV", "BPOGV",
        "valid_gaze", "gaze_valid", "is_valid_gaze"
      ),
      names(data)
    )
  }

  participant <- as.character(data[[participant_col]])
  time <- suppressWarnings(as.numeric(data[[time_col]]))

  media <- .gp3_eyetools_optional_character(data, media_col)

  trial <- .gp3_eyetools_create_trial_id(
    data = data,
    trial_col = trial_col,
    media_col = media_col
  )

  x <- .gp3_eyetools_optional_numeric(data, x_col)
  y <- .gp3_eyetools_optional_numeric(data, y_col)

  left_x <- .gp3_eyetools_optional_numeric(data, left_x_col)
  left_y <- .gp3_eyetools_optional_numeric(data, left_y_col)
  right_x <- .gp3_eyetools_optional_numeric(data, right_x_col)
  right_y <- .gp3_eyetools_optional_numeric(data, right_y_col)

  if (all(is.na(x)) && !all(is.na(left_x)) && !all(is.na(right_x))) {
    x <- rowMeans(cbind(left_x, right_x), na.rm = TRUE)
    x[is.nan(x)] <- NA_real_
  }

  if (all(is.na(y)) && !all(is.na(left_y)) && !all(is.na(right_y))) {
    y <- rowMeans(cbind(left_y, right_y), na.rm = TRUE)
    y[is.nan(y)] <- NA_real_
  }

  pupil <- .gp3_eyetools_optional_numeric(data, pupil_col)
  left_pupil <- .gp3_eyetools_optional_numeric(data, left_pupil_col)
  right_pupil <- .gp3_eyetools_optional_numeric(data, right_pupil_col)

  if (all(is.na(pupil)) && !all(is.na(left_pupil)) && !all(is.na(right_pupil))) {
    pupil <- rowMeans(cbind(left_pupil, right_pupil), na.rm = TRUE)
    pupil[is.nan(pupil)] <- NA_real_
  }

  condition <- .gp3_eyetools_optional_character(data, condition_col)
  aoi_raw <- .gp3_eyetools_optional_character(data, aoi_col)
  fixation_id <- .gp3_eyetools_optional_character(data, fixation_col)
  event <- .gp3_eyetools_optional_character(data, event_col)

  aoi <- .gp3_eyetools_standardise_aoi(
    aoi_raw,
    missing_aoi_label = missing_aoi_label
  )

  missing_gaze <- is.na(x) |
    !is.finite(x) |
    is.na(y) |
    !is.finite(y)

  offscreen_gaze <- !missing_gaze &
    (
      x < min(screen_x_range) |
        x > max(screen_x_range) |
        y < min(screen_y_range) |
        y > max(screen_y_range)
    )

  validity_bad <- .gp3_eyetools_create_validity_bad(
    data = data,
    validity_cols = validity_cols
  )

  trackloss <- .gp3_eyetools_create_trackloss(
    data = data,
    trackloss_col = trackloss_col,
    validity_bad = validity_bad,
    missing_gaze = missing_gaze
  )

  valid_gaze <- !trackloss & !missing_gaze & !offscreen_gaze

  pupil_missing <- is.na(pupil) | !is.finite(pupil)

  out <- tibble::tibble(
    participant = participant,
    trial = trial,
    time = time,
    x = x,
    y = y,
    left_x = left_x,
    left_y = left_y,
    right_x = right_x,
    right_y = right_y,
    pupil = pupil,
    left_pupil = left_pupil,
    right_pupil = right_pupil,
    media_id = media,
    condition = condition,
    aoi = aoi,
    aoi_raw = aoi_raw,
    fixation_id = fixation_id,
    event = event,
    missing_gaze = missing_gaze,
    offscreen_gaze = offscreen_gaze,
    validity_bad = validity_bad,
    trackloss = trackloss,
    valid_gaze = valid_gaze,
    pupil_missing = pupil_missing,
    eyetools_data_status = .gp3_eyetools_row_status(
      participant = participant,
      trial = trial,
      time = time,
      missing_gaze = missing_gaze,
      offscreen_gaze = offscreen_gaze,
      trackloss = trackloss
    )
  )

  if (isTRUE(keep_original_cols)) {
    original_names <- names(data)
    keep_names <- setdiff(original_names, names(out))

    if (length(keep_names) > 0L) {
      out <- dplyr::bind_cols(
        out,
        tibble::as_tibble(data[keep_names])
      )
    }
  }

  attr(out, "gp3_adapter") <- "eyetools"
  attr(out, "gp3_coordinate_cols") <- c("x", "y")
  attr(out, "gp3_binocular_coordinate_cols") <- c(
    "left_x", "left_y", "right_x", "right_y"
  )
  attr(out, "gp3_pupil_cols") <- c("pupil", "left_pupil", "right_pupil")
  attr(out, "gp3_settings") <- tibble::tibble(
    setting = c(
      "participant_col",
      "trial_col",
      "time_col",
      "x_col",
      "y_col",
      "left_x_col",
      "left_y_col",
      "right_x_col",
      "right_y_col",
      "pupil_col",
      "left_pupil_col",
      "right_pupil_col",
      "media_col",
      "condition_col",
      "aoi_col",
      "fixation_col",
      "event_col",
      "validity_cols",
      "trackloss_col",
      "screen_x_range",
      "screen_y_range",
      "missing_aoi_label",
      "keep_original_cols"
    ),
    value = c(
      .gp3_eyetools_collapse_nullable(x_col = NULL, participant_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, trial_col),
      time_col,
      .gp3_eyetools_collapse_nullable(x_col = NULL, x_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, y_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, left_x_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, left_y_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, right_x_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, right_y_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, pupil_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, left_pupil_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, right_pupil_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, media_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, condition_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, aoi_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, fixation_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, event_col),
      .gp3_eyetools_collapse_nullable(x_col = NULL, validity_cols),
      .gp3_eyetools_collapse_nullable(x_col = NULL, trackloss_col),
      paste(screen_x_range, collapse = ", "),
      paste(screen_y_range, collapse = ", "),
      missing_aoi_label,
      as.character(keep_original_cols)
    )
  )

  class(out) <- c("gp3_eyetools_data", class(out))

  out
}

.gp3_eyetools_create_trial_id <- function(data, trial_col, media_col) {
  if (!is.null(trial_col)) {
    return(as.character(data[[trial_col]]))
  }

  if (!is.null(media_col)) {
    return(as.character(data[[media_col]]))
  }

  rep("trial_1", nrow(data))
}

.gp3_eyetools_optional_character <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_character_, nrow(data)))
  }

  as.character(data[[col]])
}

.gp3_eyetools_optional_numeric <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_real_, nrow(data)))
  }

  suppressWarnings(as.numeric(data[[col]]))
}

.gp3_eyetools_standardise_aoi <- function(aoi, missing_aoi_label) {
  out <- as.character(aoi)
  out[is.na(out) | !nzchar(trimws(out))] <- missing_aoi_label
  out
}

.gp3_eyetools_create_validity_bad <- function(data, validity_cols) {
  if (length(validity_cols) == 0L) {
    return(rep(FALSE, nrow(data)))
  }

  validity_bad <- rep(FALSE, nrow(data))

  for (col in validity_cols) {
    validity_bad <- validity_bad | !.gp3_eyetools_as_validity(data[[col]])
  }

  validity_bad
}

.gp3_eyetools_create_trackloss <- function(
    data,
    trackloss_col,
    validity_bad,
    missing_gaze
) {
  if (!is.null(trackloss_col)) {
    return(.gp3_eyetools_as_logical(data[[trackloss_col]]))
  }

  validity_bad | missing_gaze
}

.gp3_eyetools_as_logical <- function(x) {
  if (is.logical(x)) {
    out <- x
    out[is.na(out)] <- FALSE
    return(out)
  }

  if (is.numeric(x)) {
    out <- x != 0
    out[is.na(out)] <- FALSE
    return(out)
  }

  x_chr <- tolower(trimws(as.character(x)))

  out <- x_chr %in% c(
    "true", "t", "yes", "y", "1",
    "trackloss", "track_loss", "lost", "invalid"
  )

  out[is.na(out)] <- FALSE
  out
}

.gp3_eyetools_as_validity <- function(x) {
  if (is.logical(x)) {
    out <- x
    out[is.na(out)] <- FALSE
    return(out)
  }

  if (is.numeric(x)) {
    out <- x > 0
    out[is.na(out)] <- FALSE
    return(out)
  }

  x_chr <- tolower(trimws(as.character(x)))

  out <- x_chr %in% c(
    "true", "t", "yes", "y", "1",
    "valid", "ok"
  )

  out[is.na(out)] <- FALSE
  out
}

.gp3_eyetools_row_status <- function(
    participant,
    trial,
    time,
    missing_gaze,
    offscreen_gaze,
    trackloss
) {
  status <- rep("ready", length(participant))

  status[is.na(participant) | !nzchar(participant)] <- "missing_participant"
  status[is.na(trial) | !nzchar(trial)] <- "missing_trial"
  status[is.na(time) | !is.finite(time)] <- "missing_time"
  status[missing_gaze] <- "missing_gaze"
  status[offscreen_gaze] <- "offscreen_gaze"
  status[trackloss] <- "trackloss"

  status
}

.gp3_eyetools_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_eyetools_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_eyetools_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_eyetools_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    if (found[[1]] == "MEDIA_ID" && "media_id" %in% names_data) {
      return("media_id")
    }

    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_eyetools_check_range <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 2L ||
      anyNA(x) ||
      any(!is.finite(x)) ||
      x[[1]] == x[[2]]) {
    stop("`", arg, "` must be a finite numeric vector of length 2.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_collapse_nullable <- function(x_col = NULL, x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
