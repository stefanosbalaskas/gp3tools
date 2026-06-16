#' Prepare Gazepoint master data for pupillometryR-style workflows
#'
#' Convert a `gp3tools` master table into a dependency-free,
#' pupillometryR-friendly sample-level pupil table. The returned data frame keeps
#' one row per sample and creates standard participant, trial, time, pupil,
#' condition, event, baseline, validity, trackloss, and status columns.
#'
#' @param data A Gazepoint master table or sample-level gaze/pupil data frame.
#' @param participant_col Participant/subject identifier column.
#' @param trial_col Trial identifier column. If `NULL`, a trial identifier is
#'   created from `media_col` when available.
#' @param time_col Sample time column.
#' @param pupil_col Pupil column to export.
#' @param media_col Optional media/stimulus identifier column.
#' @param condition_col Optional condition/grouping column.
#' @param event_col Optional event/marker column.
#' @param baseline_col Optional baseline-period indicator column.
#' @param validity_cols Optional validity columns used to define trackloss.
#' @param pupil_status_col Optional pupil-status column used to mark invalid
#'   pupil samples.
#' @param trackloss_col Optional existing trackloss column. If supplied, it is
#'   used directly after coercion to logical.
#' @param invalid_pupil_status Character values in `pupil_status_col` treated as
#'   invalid pupil samples.
#' @param keep_original_cols Logical. If `TRUE`, original columns are retained
#'   after the standard adapter columns.
#'
#' @return A tibble with class `gp3_pupillometryr_data`.
#' @export
prepare_gazepoint_pupillometryr_data <- function(
    data,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    pupil_col = NULL,
    media_col = NULL,
    condition_col = NULL,
    event_col = NULL,
    baseline_col = NULL,
    validity_cols = NULL,
    pupil_status_col = NULL,
    trackloss_col = NULL,
    invalid_pupil_status = c(
      "missing", "artifact", "blink", "trackloss", "track_loss",
      "invalid", "excluded", "bad", "outlier"
    ),
    keep_original_cols = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_pupillometryr_check_character_vector(
    invalid_pupil_status,
    "invalid_pupil_status"
  )
  .gp3_pupillometryr_check_logical_scalar(
    keep_original_cols,
    "keep_original_cols"
  )

  participant_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = participant_col,
    names_data = names(data),
    arg = "participant_col",
    candidates = c(
      "subject", "participant", "participant_id", "USER_FILE",
      "user", "user_id", "recording_id"
    ),
    required = TRUE
  )

  time_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = time_col,
    names_data = names(data),
    arg = "time_col",
    candidates = c(
      "time", "time_ms", "timestamp", "TIMESTAMP",
      "TIME", "TIME_TICK", "CNT"
    ),
    required = TRUE
  )

  pupil_col <- .gp3_pupillometryr_resolve_or_detect_col(
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
    required = TRUE
  )

  media_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = media_col,
    names_data = names(data),
    arg = "media_col",
    candidates = c("media_id", "MEDIA_ID", "stimulus", "stimulus_id", "image"),
    required = FALSE
  )

  trial_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = trial_col,
    names_data = names(data),
    arg = "trial_col",
    candidates = c(
      "trial_global", "trial", "trial_id", "TRIAL_INDEX",
      "trial_number", "item_trial"
    ),
    required = FALSE
  )

  condition_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = condition_col,
    names_data = names(data),
    arg = "condition_col",
    candidates = c("condition", "Condition", "group", "GROUP"),
    required = FALSE
  )

  event_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = event_col,
    names_data = names(data),
    arg = "event_col",
    candidates = c("event_label", "event", "EVENT", "marker", "marker_label"),
    required = FALSE
  )

  baseline_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = baseline_col,
    names_data = names(data),
    arg = "baseline_col",
    candidates = c(
      "baseline",
      "is_baseline",
      "baseline_period",
      "baseline_sample",
      "pre_stimulus",
      "prestimulus"
    ),
    required = FALSE
  )

  pupil_status_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = pupil_status_col,
    names_data = names(data),
    arg = "pupil_status_col",
    candidates = c(
      "pupil_status",
      "pupil_preprocessing_status",
      "pupil_artifact_status",
      "pupil_quality_status"
    ),
    required = FALSE
  )

  trackloss_col <- .gp3_pupillometryr_resolve_or_detect_col(
    col = trackloss_col,
    names_data = names(data),
    arg = "trackloss_col",
    candidates = c("trackloss", "track_loss", "is_trackloss"),
    required = FALSE
  )

  if (!is.null(validity_cols)) {
    validity_cols <- .gp3_pupillometryr_resolve_cols(
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
  pupil <- suppressWarnings(as.numeric(data[[pupil_col]]))

  media <- .gp3_pupillometryr_optional_character(data, media_col)

  trial <- .gp3_pupillometryr_create_trial_id(
    data = data,
    trial_col = trial_col,
    media_col = media_col
  )

  condition <- .gp3_pupillometryr_optional_character(data, condition_col)
  event <- .gp3_pupillometryr_optional_character(data, event_col)

  baseline <- .gp3_pupillometryr_optional_logical(data, baseline_col)

  pupil_status_raw <- .gp3_pupillometryr_optional_character(
    data,
    pupil_status_col
  )

  pupil_missing <- is.na(pupil) | !is.finite(pupil)

  pupil_invalid_status <- .gp3_pupillometryr_detect_invalid_pupil_status(
    pupil_status_raw,
    invalid_pupil_status = invalid_pupil_status
  )

  trackloss <- .gp3_pupillometryr_create_trackloss(
    data = data,
    trackloss_col = trackloss_col,
    validity_cols = validity_cols
  )

  pupil_valid <- !pupil_missing & !pupil_invalid_status & !trackloss

  out <- tibble::tibble(
    participant = participant,
    trial = trial,
    time = time,
    pupil = pupil,
    media_id = media,
    condition = condition,
    event = event,
    baseline = baseline,
    pupil_status_raw = pupil_status_raw,
    pupil_missing = pupil_missing,
    pupil_invalid_status = pupil_invalid_status,
    trackloss = trackloss,
    pupil_valid = pupil_valid,
    pupillometryr_data_status = .gp3_pupillometryr_row_status(
      participant = participant,
      trial = trial,
      time = time,
      pupil_missing = pupil_missing,
      pupil_invalid_status = pupil_invalid_status,
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

  attr(out, "gp3_adapter") <- "pupillometryr"
  attr(out, "gp3_pupil_col") <- pupil_col
  attr(out, "gp3_settings") <- tibble::tibble(
    setting = c(
      "participant_col",
      "trial_col",
      "time_col",
      "pupil_col",
      "media_col",
      "condition_col",
      "event_col",
      "baseline_col",
      "validity_cols",
      "pupil_status_col",
      "trackloss_col",
      "invalid_pupil_status",
      "keep_original_cols"
    ),
    value = c(
      participant_col,
      .gp3_pupillometryr_collapse_nullable(trial_col),
      time_col,
      pupil_col,
      .gp3_pupillometryr_collapse_nullable(media_col),
      .gp3_pupillometryr_collapse_nullable(condition_col),
      .gp3_pupillometryr_collapse_nullable(event_col),
      .gp3_pupillometryr_collapse_nullable(baseline_col),
      .gp3_pupillometryr_collapse_nullable(validity_cols),
      .gp3_pupillometryr_collapse_nullable(pupil_status_col),
      .gp3_pupillometryr_collapse_nullable(trackloss_col),
      paste(invalid_pupil_status, collapse = ", "),
      as.character(keep_original_cols)
    )
  )

  class(out) <- c("gp3_pupillometryr_data", class(out))

  out
}

.gp3_pupillometryr_create_trial_id <- function(data, trial_col, media_col) {
  if (!is.null(trial_col)) {
    return(as.character(data[[trial_col]]))
  }

  if (!is.null(media_col)) {
    return(as.character(data[[media_col]]))
  }

  rep("trial_1", nrow(data))
}

.gp3_pupillometryr_optional_character <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_character_, nrow(data)))
  }

  as.character(data[[col]])
}

.gp3_pupillometryr_optional_logical <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA, nrow(data)))
  }

  .gp3_pupillometryr_as_logical(data[[col]])
}

.gp3_pupillometryr_detect_invalid_pupil_status <- function(
    pupil_status_raw,
    invalid_pupil_status
) {
  if (all(is.na(pupil_status_raw))) {
    return(rep(FALSE, length(pupil_status_raw)))
  }

  status <- tolower(trimws(as.character(pupil_status_raw)))
  invalid <- tolower(trimws(as.character(invalid_pupil_status)))

  out <- status %in% invalid
  out[is.na(out)] <- FALSE
  out
}

.gp3_pupillometryr_create_trackloss <- function(
    data,
    trackloss_col,
    validity_cols
) {
  if (!is.null(trackloss_col)) {
    return(.gp3_pupillometryr_as_logical(data[[trackloss_col]]))
  }

  trackloss <- rep(FALSE, nrow(data))

  if (length(validity_cols) > 0L) {
    validity_bad <- rep(FALSE, nrow(data))

    for (col in validity_cols) {
      validity_bad <- validity_bad | !.gp3_pupillometryr_as_validity(data[[col]])
    }

    trackloss <- trackloss | validity_bad
  }

  trackloss
}

.gp3_pupillometryr_as_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    out <- x != 0
    out[is.na(out)] <- FALSE
    return(out)
  }

  x_chr <- tolower(trimws(as.character(x)))

  out <- x_chr %in% c(
    "true", "t", "yes", "y", "1",
    "baseline", "pre_stimulus", "prestimulus",
    "trackloss", "track_loss", "lost", "invalid"
  )

  out[is.na(out)] <- FALSE
  out
}

.gp3_pupillometryr_as_validity <- function(x) {
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

.gp3_pupillometryr_row_status <- function(
    participant,
    trial,
    time,
    pupil_missing,
    pupil_invalid_status,
    trackloss
) {
  status <- rep("ready", length(participant))

  status[is.na(participant) | !nzchar(participant)] <- "missing_participant"
  status[is.na(trial) | !nzchar(trial)] <- "missing_trial"
  status[is.na(time) | !is.finite(time)] <- "missing_time"
  status[pupil_missing] <- "missing_pupil"
  status[pupil_invalid_status] <- "invalid_pupil_status"
  status[trackloss] <- "trackloss"

  status
}

.gp3_pupillometryr_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_pupillometryr_resolve_col <- function(col, names_data, arg) {
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

.gp3_pupillometryr_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_pupillometryr_resolve_col(col, names_data, arg))
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

.gp3_pupillometryr_check_character_vector <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pupillometryr_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pupillometryr_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
