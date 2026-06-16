#' Prepare Gazepoint master data for eyetrackingR-style workflows
#'
#' Convert a `gp3tools` master table into a dependency-free, eyetrackingR-friendly
#' sample-level table. The returned data frame keeps one row per gaze sample and
#' creates standard participant, trial, time, gaze-coordinate, AOI, trackloss, and
#' AOI-indicator columns.
#'
#' @param data A Gazepoint master table or sample-level gaze data frame.
#' @param participant_col Participant/subject identifier column.
#' @param trial_col Trial identifier column. If `NULL`, a trial identifier is
#'   created from `media_col` when available.
#' @param time_col Sample time column.
#' @param aoi_col AOI label/state column.
#' @param x_col Optional gaze x-coordinate column.
#' @param y_col Optional gaze y-coordinate column.
#' @param media_col Optional media/stimulus identifier column.
#' @param condition_col Optional condition/grouping column.
#' @param validity_cols Optional validity columns used to define trackloss.
#' @param aoi_values Optional AOI values for which logical indicator columns
#'   should be created. If `NULL`, values are detected from `aoi_col`.
#' @param aoi_prefix Prefix for generated AOI indicator columns.
#' @param missing_aoi_label Label used for missing AOI values.
#' @param non_aoi_values Character values treated as non-AOI/background states.
#' @param trackloss_col Optional existing trackloss column. If supplied, it is
#'   used directly after coercion to logical.
#' @param keep_original_cols Logical. If `TRUE`, original columns are retained
#'   after the standard adapter columns.
#'
#' @return A tibble with class `gp3_eyetrackingr_data`.
#' @export
prepare_gazepoint_eyetrackingr_data <- function(
    data,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    aoi_col = NULL,
    x_col = NULL,
    y_col = NULL,
    media_col = NULL,
    condition_col = NULL,
    validity_cols = NULL,
    aoi_values = NULL,
    aoi_prefix = "aoi_",
    missing_aoi_label = "missing_aoi",
    non_aoi_values = c(
      "outside", "none", "no_aoi", "non_aoi", "background",
      "off_aoi", "missing", "NA"
    ),
    trackloss_col = NULL,
    keep_original_cols = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_eyetrackingr_check_label(aoi_prefix, "aoi_prefix")
  .gp3_eyetrackingr_check_label(missing_aoi_label, "missing_aoi_label")
  .gp3_eyetrackingr_check_character_vector(non_aoi_values, "non_aoi_values")
  .gp3_eyetrackingr_check_logical_scalar(keep_original_cols, "keep_original_cols")

  participant_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = participant_col,
    names_data = names(data),
    arg = "participant_col",
    candidates = c(
      "subject", "participant", "participant_id", "USER_FILE",
      "user", "user_id", "recording_id"
    ),
    required = TRUE
  )

  time_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = time_col,
    names_data = names(data),
    arg = "time_col",
    candidates = c(
      "time", "time_ms", "timestamp", "TIMESTAMP",
      "TIME", "TIME_TICK", "CNT"
    ),
    required = TRUE
  )

  media_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = media_col,
    names_data = names(data),
    arg = "media_col",
    candidates = c("media_id", "MEDIA_ID", "stimulus", "stimulus_id", "image"),
    required = FALSE
  )

  trial_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = trial_col,
    names_data = names(data),
    arg = "trial_col",
    candidates = c(
      "trial_global", "trial", "trial_id", "TRIAL_INDEX",
      "trial_number", "item_trial"
    ),
    required = FALSE
  )

  aoi_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = aoi_col,
    names_data = names(data),
    arg = "aoi_col",
    candidates = c(
      "aoi_current", "aoi", "AOI", "aoi_label",
      "observed_aoi", "AOI_LABEL"
    ),
    required = FALSE
  )

  x_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = x_col,
    names_data = names(data),
    arg = "x_col",
    candidates = c("x", "X", "gaze_x", "gaze_x_norm", "FPOGX", "BPOGX"),
    required = FALSE
  )

  y_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = y_col,
    names_data = names(data),
    arg = "y_col",
    candidates = c("y", "Y", "gaze_y", "gaze_y_norm", "FPOGY", "BPOGY"),
    required = FALSE
  )

  condition_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = condition_col,
    names_data = names(data),
    arg = "condition_col",
    candidates = c("condition", "Condition", "group", "GROUP"),
    required = FALSE
  )

  trackloss_col <- .gp3_eyetrackingr_resolve_or_detect_col(
    col = trackloss_col,
    names_data = names(data),
    arg = "trackloss_col",
    candidates = c("trackloss", "track_loss", "is_trackloss"),
    required = FALSE
  )

  if (!is.null(validity_cols)) {
    validity_cols <- .gp3_eyetrackingr_resolve_cols(
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

  media <- .gp3_eyetrackingr_optional_character(data, media_col)

  trial <- .gp3_eyetrackingr_create_trial_id(
    data = data,
    trial_col = trial_col,
    media_col = media_col
  )

  condition <- .gp3_eyetrackingr_optional_character(data, condition_col)

  gaze_x <- .gp3_eyetrackingr_optional_numeric(data, x_col)
  gaze_y <- .gp3_eyetrackingr_optional_numeric(data, y_col)

  aoi_raw <- .gp3_eyetrackingr_optional_character(data, aoi_col)

  aoi <- .gp3_eyetrackingr_standardise_aoi(
    aoi_raw,
    missing_aoi_label = missing_aoi_label
  )

  trackloss <- .gp3_eyetrackingr_create_trackloss(
    data = data,
    trackloss_col = trackloss_col,
    validity_cols = validity_cols,
    gaze_x = gaze_x,
    gaze_y = gaze_y
  )

  if (is.null(aoi_values)) {
    aoi_values <- .gp3_eyetrackingr_detect_aoi_values(
      aoi = aoi,
      missing_aoi_label = missing_aoi_label,
      non_aoi_values = non_aoi_values
    )
  } else {
    .gp3_eyetrackingr_check_character_vector(aoi_values, "aoi_values")
    aoi_values <- unique(as.character(aoi_values))
  }

  out <- tibble::tibble(
    participant = participant,
    trial = trial,
    time = time,
    gaze_x = gaze_x,
    gaze_y = gaze_y,
    media_id = media,
    condition = condition,
    aoi = aoi,
    aoi_raw = aoi_raw,
    trackloss = trackloss,
    eyetrackingr_data_status = .gp3_eyetrackingr_row_status(
      participant = participant,
      trial = trial,
      time = time,
      trackloss = trackloss
    )
  )

  aoi_indicator_cols <- character(0)

  for (value in aoi_values) {
    indicator_name <- .gp3_eyetrackingr_make_aoi_col_name(
      value = value,
      prefix = aoi_prefix,
      existing = c(names(out), aoi_indicator_cols)
    )

    out[[indicator_name]] <- !is.na(aoi) & aoi == value & !trackloss
    aoi_indicator_cols <- c(aoi_indicator_cols, indicator_name)
  }

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

  attr(out, "gp3_adapter") <- "eyetrackingr"
  attr(out, "gp3_aoi_indicator_cols") <- aoi_indicator_cols
  attr(out, "gp3_settings") <- tibble::tibble(
    setting = c(
      "participant_col",
      "trial_col",
      "time_col",
      "aoi_col",
      "x_col",
      "y_col",
      "media_col",
      "condition_col",
      "validity_cols",
      "trackloss_col",
      "aoi_values",
      "aoi_prefix",
      "missing_aoi_label",
      "non_aoi_values",
      "keep_original_cols"
    ),
    value = c(
      participant_col,
      .gp3_eyetrackingr_collapse_nullable(trial_col),
      time_col,
      .gp3_eyetrackingr_collapse_nullable(aoi_col),
      .gp3_eyetrackingr_collapse_nullable(x_col),
      .gp3_eyetrackingr_collapse_nullable(y_col),
      .gp3_eyetrackingr_collapse_nullable(media_col),
      .gp3_eyetrackingr_collapse_nullable(condition_col),
      .gp3_eyetrackingr_collapse_nullable(validity_cols),
      .gp3_eyetrackingr_collapse_nullable(trackloss_col),
      .gp3_eyetrackingr_collapse_nullable(aoi_values),
      aoi_prefix,
      missing_aoi_label,
      paste(non_aoi_values, collapse = ", "),
      as.character(keep_original_cols)
    )
  )

  class(out) <- c("gp3_eyetrackingr_data", class(out))

  out
}

.gp3_eyetrackingr_create_trial_id <- function(data, trial_col, media_col) {
  if (!is.null(trial_col)) {
    return(as.character(data[[trial_col]]))
  }

  if (!is.null(media_col)) {
    return(as.character(data[[media_col]]))
  }

  rep("trial_1", nrow(data))
}

.gp3_eyetrackingr_optional_character <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_character_, nrow(data)))
  }

  as.character(data[[col]])
}

.gp3_eyetrackingr_optional_numeric <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_real_, nrow(data)))
  }

  suppressWarnings(as.numeric(data[[col]]))
}

.gp3_eyetrackingr_standardise_aoi <- function(aoi, missing_aoi_label) {
  out <- as.character(aoi)
  out[is.na(out) | !nzchar(trimws(out))] <- missing_aoi_label
  out
}

.gp3_eyetrackingr_create_trackloss <- function(
    data,
    trackloss_col,
    validity_cols,
    gaze_x,
    gaze_y
) {
  if (!is.null(trackloss_col)) {
    return(.gp3_eyetrackingr_as_logical(data[[trackloss_col]]))
  }

  trackloss <- rep(FALSE, nrow(data))

  has_x <- !all(is.na(gaze_x))
  has_y <- !all(is.na(gaze_y))

  if (has_x) {
    trackloss <- trackloss | is.na(gaze_x) | !is.finite(gaze_x)
  }

  if (has_y) {
    trackloss <- trackloss | is.na(gaze_y) | !is.finite(gaze_y)
  }

  if (length(validity_cols) > 0L) {
    validity_bad <- rep(FALSE, nrow(data))

    for (col in validity_cols) {
      validity_bad <- validity_bad | !.gp3_eyetrackingr_as_validity(data[[col]])
    }

    trackloss <- trackloss | validity_bad
  }

  trackloss
}

.gp3_eyetrackingr_as_logical <- function(x) {
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

.gp3_eyetrackingr_as_validity <- function(x) {
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

.gp3_eyetrackingr_row_status <- function(participant, trial, time, trackloss) {
  status <- rep("ready", length(participant))

  status[is.na(participant) | !nzchar(participant)] <- "missing_participant"
  status[is.na(trial) | !nzchar(trial)] <- "missing_trial"
  status[is.na(time) | !is.finite(time)] <- "missing_time"

  status[isTRUE(length(trackloss) == length(status)) & trackloss] <- "trackloss"

  status
}

.gp3_eyetrackingr_detect_aoi_values <- function(
    aoi,
    missing_aoi_label,
    non_aoi_values
) {
  values <- unique(as.character(aoi))
  values <- values[!is.na(values)]
  values <- values[nzchar(values)]

  drop_values <- unique(c(missing_aoi_label, non_aoi_values))
  values <- values[!tolower(values) %in% tolower(drop_values)]

  sort(values)
}

.gp3_eyetrackingr_make_aoi_col_name <- function(value, prefix, existing) {
  clean <- tolower(trimws(as.character(value)))
  clean <- gsub("[^a-z0-9]+", "_", clean)
  clean <- gsub("^_+|_+$", "", clean)

  if (!nzchar(clean)) {
    clean <- "unnamed"
  }

  candidate <- paste0(prefix, clean)

  if (!candidate %in% existing) {
    return(candidate)
  }

  i <- 2L

  repeat {
    candidate_i <- paste0(candidate, "_", i)

    if (!candidate_i %in% existing) {
      return(candidate_i)
    }

    i <- i + 1L
  }
}

.gp3_eyetrackingr_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_eyetrackingr_resolve_col <- function(col, names_data, arg) {
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

.gp3_eyetrackingr_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_eyetrackingr_resolve_col(col, names_data, arg))
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

.gp3_eyetrackingr_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetrackingr_check_character_vector <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetrackingr_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetrackingr_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
