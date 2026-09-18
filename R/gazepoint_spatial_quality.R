# Gazepoint adapters for vendor-neutral spatial-quality metrics -----------------

.gp3_quality_require_eyeprocess <- function() {
  if (!requireNamespace("eyeprocess", quietly = TRUE)) {
    stop(
      "Gazepoint spatial-quality wrappers require the suggested eyeprocess package ",
      "(development API >= 0.11.1.9000).",
      call. = FALSE
    )
  }
  needed <- c(
    "summarise_spatial_quality",
    "create_gaze_quality_report",
    "plot_gaze_quality_dashboard",
    "report_gaze_quality"
  )
  missing <- setdiff(needed, getNamespaceExports("eyeprocess"))
  if (length(missing)) {
    stop(
      "The installed eyeprocess is too old for Gazepoint spatial-quality wrappers. ",
      "Missing API: ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gp3_quality_resolve_col <- function(data, supplied, candidates, label, required = FALSE) {
  if (!is.null(supplied)) {
    if (length(supplied) != 1L || is.na(supplied) || !nzchar(supplied)) {
      stop(label, " column must be one non-empty column name.", call. = FALSE)
    }
    if (!supplied %in% names(data)) {
      stop("Column ", supplied, " requested for ", label, " is not present.", call. = FALSE)
    }
    return(supplied)
  }
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) return(hit[[1L]])
  if (required) {
    stop(
      "Could not identify a ", label, " column. Supply it explicitly. Candidates checked: ",
      paste(candidates, collapse = ", "), ".",
      call. = FALSE
    )
  }
  NULL
}

.gp3_quality_coordinate_unit <- function(data, x_col, coordinate_unit) {
  coordinate_unit <- match.arg(
    coordinate_unit,
    c("auto", "normalized", "pixels", "degrees")
  )
  if (coordinate_unit != "auto") return(coordinate_unit)

  if ("coordinate_unit" %in% names(data)) {
    values <- unique(tolower(as.character(stats::na.omit(data$coordinate_unit))))
    if (length(values) == 1L && values %in% c("normalized", "pixels", "degrees")) {
      return(values)
    }
    if (length(values) > 1L) {
      stop(
        "coordinate_unit contains mixed units. Resolve units before quality analysis.",
        call. = FALSE
      )
    }
  }

  if (x_col %in% c("BPOGX", "FPOGX", "LPOGX", "RPOGX")) {
    return("normalized")
  }

  stop(
    "Coordinate unit cannot be inferred safely from column ", x_col,
    ". Set coordinate_unit explicitly; numeric ranges are never used to guess units.",
    call. = FALSE
  )
}

.gp3_quality_time_unit <- function(time_col, time_unit) {
  time_unit <- match.arg(time_unit, c("auto", "ms", "s", "us", "ns"))
  if (time_unit != "auto") return(time_unit)
  nm <- toupper(time_col)
  if (nm %in% c("MSTIMER", "TIMESTAMP_MS", "TIME_MS")) return("ms")
  if (nm %in% c("TIME", "TIMESTAMP", "TIMESTAMP_S", "TIME_S")) return("s")
  stop(
    "Time unit cannot be inferred safely from column ", time_col,
    ". Set time_unit explicitly.",
    call. = FALSE
  )
}

.gp3_quality_constant <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (!length(hit)) return(NULL)
  values <- suppressWarnings(as.numeric(data[[hit[[1L]]]]))
  values <- unique(values[is.finite(values)])
  if (length(values) == 1L && values > 0) values[[1L]] else NULL
}

.gp3_quality_geometry <- function(data, geometry) {
  if (!is.null(geometry)) return(geometry)
  out <- list(
    screen_width_px = .gp3_quality_constant(
      data, c("screen_width_px", "SCREEN_WIDTH_PX", "SCREEN_WIDTH")
    ),
    screen_height_px = .gp3_quality_constant(
      data, c("screen_height_px", "SCREEN_HEIGHT_PX", "SCREEN_HEIGHT")
    ),
    screen_width_cm = .gp3_quality_constant(
      data, c("screen_width_cm", "SCREEN_WIDTH_CM")
    ),
    screen_height_cm = .gp3_quality_constant(
      data, c("screen_height_cm", "SCREEN_HEIGHT_CM")
    ),
    viewing_distance_cm = .gp3_quality_constant(
      data, c("viewing_distance_cm", "VIEWING_DISTANCE_CM", "DISTANCE_CM")
    )
  )
  if (all(vapply(out, function(x) !is.null(x), logical(1)))) out else NULL
}

.gp3_quality_prepare <- function(
    data,
    x_col = NULL,
    y_col = NULL,
    time_col = NULL,
    target_x_col = NULL,
    target_y_col = NULL,
    valid_col = NULL,
    missing_reason_col = NULL,
    participant_col = NULL,
    session_col = NULL,
    trial_col = NULL,
    file_col = NULL,
    coordinate_unit = c("auto", "normalized", "pixels", "degrees"),
    time_unit = c("auto", "ms", "s", "us", "ns")) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("data must be a non-empty Gazepoint sample-level data frame.", call. = FALSE)
  }

  x_col <- .gp3_quality_resolve_col(
    data, x_col, c("gaze_x", "BPOGX", "FPOGX", "LPOGX", "RPOGX"),
    "horizontal gaze", TRUE
  )
  y_col <- .gp3_quality_resolve_col(
    data, y_col, c("gaze_y", "BPOGY", "FPOGY", "LPOGY", "RPOGY"),
    "vertical gaze", TRUE
  )
  time_col <- .gp3_quality_resolve_col(
    data, time_col,
    c("timestamp_ms", "time_ms", "MSTIMER", "TIME", "timestamp", "time_s", "timestamp_s"),
    "timestamp", TRUE
  )
  target_x_col <- .gp3_quality_resolve_col(
    data, target_x_col,
    c("target_x", "TARGET_X", "validation_target_x", "VALIDATION_TARGET_X"),
    "horizontal validation target", FALSE
  )
  target_y_col <- .gp3_quality_resolve_col(
    data, target_y_col,
    c("target_y", "TARGET_Y", "validation_target_y", "VALIDATION_TARGET_Y"),
    "vertical validation target", FALSE
  )
  if (xor(is.null(target_x_col), is.null(target_y_col))) {
    stop(
      "Validation-target coordinates must be supplied as both x and y columns.",
      call. = FALSE
    )
  }

  valid_col <- .gp3_quality_resolve_col(
    data, valid_col,
    c("gaze_valid", "valid", "BPOGV", "FPOGV", "LPOGV", "RPOGV"),
    "gaze validity", FALSE
  )
  missing_reason_col <- .gp3_quality_resolve_col(
    data, missing_reason_col,
    c("missing_reason", "gaze_missing_reason", "invalid_reason"),
    "missingness reason", FALSE
  )
  participant_col <- .gp3_quality_resolve_col(
    data, participant_col,
    c("participant_id", "participant", "subject_id", "subject", "USER_ID", "USER"),
    "participant", FALSE
  )
  session_col <- .gp3_quality_resolve_col(
    data, session_col, c("session_id", "session", "SESSION_ID"),
    "session", FALSE
  )
  trial_col <- .gp3_quality_resolve_col(
    data, trial_col,
    c("trial_id", "trial", "TRIAL_ID", "MEDIA_ID", "media_id"),
    "trial", FALSE
  )
  file_col <- .gp3_quality_resolve_col(
    data, file_col,
    c("source_file", "file_id", "file_name", "filename"),
    "file", FALSE
  )

  resolved_coordinate_unit <- .gp3_quality_coordinate_unit(
    data, x_col, coordinate_unit
  )
  resolved_time_unit <- .gp3_quality_time_unit(time_col, time_unit)

  out <- data
  out$.gp3_quality_x <- suppressWarnings(as.numeric(data[[x_col]]))
  out$.gp3_quality_y <- suppressWarnings(as.numeric(data[[y_col]]))
  out$.gp3_quality_time <- suppressWarnings(as.numeric(data[[time_col]]))
  if (!is.null(target_x_col)) {
    out$.gp3_quality_target_x <- suppressWarnings(as.numeric(data[[target_x_col]]))
    out$.gp3_quality_target_y <- suppressWarnings(as.numeric(data[[target_y_col]]))
  }
  if (!is.null(valid_col)) out$.gp3_quality_valid <- data[[valid_col]]
  if (!is.null(missing_reason_col)) {
    out$.gp3_quality_missing_reason <- as.character(data[[missing_reason_col]])
  }
  if (!is.null(participant_col)) {
    out$.gp3_quality_participant <- as.character(data[[participant_col]])
  }
  if (!is.null(session_col)) {
    out$.gp3_quality_session <- as.character(data[[session_col]])
  }
  if (!is.null(trial_col)) {
    out$.gp3_quality_trial <- as.character(data[[trial_col]])
  }
  if (!is.null(file_col)) {
    out$.gp3_quality_file <- as.character(data[[file_col]])
  }

  list(
    data = out,
    columns = list(
      x = ".gp3_quality_x",
      y = ".gp3_quality_y",
      time = ".gp3_quality_time",
      target_x = if (!is.null(target_x_col)) ".gp3_quality_target_x" else NULL,
      target_y = if (!is.null(target_y_col)) ".gp3_quality_target_y" else NULL,
      valid = if (!is.null(valid_col)) ".gp3_quality_valid" else NULL,
      missing_reason = if (!is.null(missing_reason_col)) ".gp3_quality_missing_reason" else NULL,
      participant = if (!is.null(participant_col)) ".gp3_quality_participant" else NULL,
      session = if (!is.null(session_col)) ".gp3_quality_session" else NULL,
      trial = if (!is.null(trial_col)) ".gp3_quality_trial" else NULL,
      file = if (!is.null(file_col)) ".gp3_quality_file" else NULL
    ),
    source_columns = list(
      x = x_col, y = y_col, time = time_col,
      target_x = target_x_col, target_y = target_y_col,
      valid = valid_col, missing_reason = missing_reason_col,
      participant = participant_col, session = session_col,
      trial = trial_col, file = file_col
    ),
    coordinate_unit = resolved_coordinate_unit,
    time_unit = resolved_time_unit
  )
}

.gp3_quality_by <- function(prepared, level, by = NULL) {
  if (!is.null(by)) {
    if (!all(by %in% names(prepared$data))) {
      stop("Every by column must exist in the supplied data.", call. = FALSE)
    }
    return(as.character(by))
  }
  level <- match.arg(level, c("dataset", "participant", "session", "trial", "file"))
  cols <- prepared$columns
  required <- switch(
    level,
    dataset = NULL,
    participant = cols$participant,
    session = cols$session,
    trial = cols$trial,
    file = cols$file
  )
  if (level != "dataset" && is.null(required)) {
    stop(
      "The requested aggregation level is unavailable in these data. ",
      "Supply the identifier column explicitly or use by.",
      call. = FALSE
    )
  }
  out <- switch(
    level,
    dataset = character(),
    participant = cols$participant,
    session = c(cols$participant, cols$session),
    trial = c(cols$participant, cols$session, cols$trial),
    file = cols$file
  )
  unique(out[!vapply(out, is.null, logical(1))])
}

#' Summarise Gazepoint spatial accuracy and precision
#'
#' Thin Gazepoint adapter for the vendor-neutral eyeprocess spatial-quality
#' implementation. No accuracy, precision, or BCEA formula is reimplemented in
#' gp3tools.
#'
#' @param data Sample-level Gazepoint data.
#' @param x_col,y_col,time_col,target_x_col,target_y_col Optional explicit source
#'   column names. Vendor-native columns are recognized conservatively.
#' @param coordinate_unit Input coordinate unit. Auto is allowed only when the
#'   unit can be established from vendor-native column names or an explicit
#'   coordinate_unit column; numeric ranges are never used to guess units.
#' @param output_unit Optional explicit output unit.
#' @param geometry Optional screen/viewing geometry forwarded to eyeprocess.
#' @param time_unit Timestamp unit.
#' @param level Aggregation level when by is not supplied.
#' @param by Optional explicit grouping columns in the source data.
#' @param probability BCEA probability level.
#' @param max_gap_ms Optional explicit maximum adjacent-sample interval.
#' @return A data frame returned by eyeprocess with Gazepoint adapter provenance.
#' @export
summarise_gazepoint_spatial_quality <- function(
    data,
    x_col = NULL,
    y_col = NULL,
    time_col = NULL,
    target_x_col = NULL,
    target_y_col = NULL,
    coordinate_unit = c("auto", "normalized", "pixels", "degrees"),
    output_unit = NULL,
    geometry = NULL,
    time_unit = c("auto", "ms", "s", "us", "ns"),
    level = c("dataset", "participant", "session", "trial", "file"),
    by = NULL,
    probability = 0.68,
    max_gap_ms = NULL) {
  .gp3_quality_require_eyeprocess()
  level <- match.arg(level)
  prepared <- .gp3_quality_prepare(
    data = data,
    x_col = x_col,
    y_col = y_col,
    time_col = time_col,
    target_x_col = target_x_col,
    target_y_col = target_y_col,
    coordinate_unit = coordinate_unit,
    time_unit = time_unit
  )
  if (is.null(prepared$columns$target_x)) {
    stop(
      "Spatial accuracy requires known validation-target coordinates. ",
      "Supply target_x_col and target_y_col.",
      call. = FALSE
    )
  }
  groups <- .gp3_quality_by(prepared, level, by)
  resolved_geometry <- .gp3_quality_geometry(data, geometry)
  out <- eyeprocess::summarise_spatial_quality(
    prepared$data,
    x = prepared$columns$x,
    y = prepared$columns$y,
    target_x = prepared$columns$target_x,
    target_y = prepared$columns$target_y,
    time = prepared$columns$time,
    by = groups,
    unit = prepared$coordinate_unit,
    output_unit = output_unit,
    geometry = resolved_geometry,
    time_unit = prepared$time_unit,
    max_gap_ms = max_gap_ms,
    probability = probability
  )
  attr(out, "gazepoint_quality_adapter") <- list(
    source_columns = prepared$source_columns,
    coordinate_unit = prepared$coordinate_unit,
    output_unit = if (is.null(output_unit)) prepared$coordinate_unit else output_unit,
    time_unit = prepared$time_unit,
    level = level,
    by = groups,
    formula_engine = "eyeprocess"
  )
  out
}

#' Create a Gazepoint quality report
#'
#' Resolves Gazepoint columns and metadata, then delegates accuracy, precision,
#' BCEA, effective sampling, jitter, validity, and data-loss calculations to
#' eyeprocess.
#'
#' @inheritParams summarise_gazepoint_spatial_quality
#' @param valid_col Optional Gazepoint validity column.
#' @param missing_reason_col Optional reason-specific missingness column.
#' @param participant_col,session_col,trial_col,file_col Optional identifier columns.
#' @param nominal_sampling_hz Optional nominal hardware frequency.
#' @param bcea_probability Explicit BCEA probability level.
#' @param thresholds Optional study-specific review thresholds. They never cause
#'   automatic exclusion.
#' @param preprocessing_spec,event_detector,aoi_specification,quality_rules,model_specification
#'   Optional provenance fields forwarded to eyeprocess.
#' @return A gaze_quality_report returned by eyeprocess with Gazepoint adapter
#'   provenance attached.
#' @export
create_gazepoint_quality_report <- function(
    data,
    x_col = NULL,
    y_col = NULL,
    time_col = NULL,
    target_x_col = NULL,
    target_y_col = NULL,
    valid_col = NULL,
    missing_reason_col = NULL,
    participant_col = NULL,
    session_col = NULL,
    trial_col = NULL,
    file_col = NULL,
    coordinate_unit = c("auto", "normalized", "pixels", "degrees"),
    output_unit = NULL,
    geometry = NULL,
    time_unit = c("auto", "ms", "s", "us", "ns"),
    level = c("dataset", "participant", "session", "trial", "file"),
    by = NULL,
    nominal_sampling_hz = 60,
    bcea_probability = 0.68,
    max_gap_ms = NULL,
    thresholds = NULL,
    preprocessing_spec = NULL,
    event_detector = NULL,
    aoi_specification = NULL,
    quality_rules = NULL,
    model_specification = NULL) {
  .gp3_quality_require_eyeprocess()
  level <- match.arg(level)
  prepared <- .gp3_quality_prepare(
    data = data,
    x_col = x_col,
    y_col = y_col,
    time_col = time_col,
    target_x_col = target_x_col,
    target_y_col = target_y_col,
    valid_col = valid_col,
    missing_reason_col = missing_reason_col,
    participant_col = participant_col,
    session_col = session_col,
    trial_col = trial_col,
    file_col = file_col,
    coordinate_unit = coordinate_unit,
    time_unit = time_unit
  )
  groups <- .gp3_quality_by(prepared, level, by)
  resolved_geometry <- .gp3_quality_geometry(data, geometry)

  out <- eyeprocess::create_gaze_quality_report(
    prepared$data,
    x = prepared$columns$x,
    y = prepared$columns$y,
    time = prepared$columns$time,
    target_x = prepared$columns$target_x,
    target_y = prepared$columns$target_y,
    valid = prepared$columns$valid,
    missing_reason = prepared$columns$missing_reason,
    by = groups,
    unit = prepared$coordinate_unit,
    output_unit = output_unit,
    geometry = resolved_geometry,
    time_unit = prepared$time_unit,
    nominal_sampling_hz = nominal_sampling_hz,
    bcea_probability = bcea_probability,
    max_gap_ms = max_gap_ms,
    thresholds = thresholds,
    preprocessing_spec = preprocessing_spec,
    event_detector = event_detector,
    aoi_specification = aoi_specification,
    quality_rules = quality_rules,
    model_specification = model_specification,
    software_version = paste0(
      "gp3tools ", as.character(utils::packageVersion("gp3tools")),
      "; eyeprocess ", as.character(utils::packageVersion("eyeprocess"))
    )
  )
  attr(out, "gazepoint_quality_adapter") <- list(
    source_columns = prepared$source_columns,
    coordinate_unit = prepared$coordinate_unit,
    output_unit = if (is.null(output_unit)) prepared$coordinate_unit else output_unit,
    time_unit = prepared$time_unit,
    nominal_sampling_hz = nominal_sampling_hz,
    level = level,
    by = groups,
    formula_engine = "eyeprocess",
    automatic_exclusion = FALSE
  )
  out
}

#' Plot a Gazepoint quality dashboard
#'
#' @param x A gaze_quality_report, or raw Gazepoint data.
#' @param ... When x is raw data, arguments forwarded to
#'   create_gazepoint_quality_report().
#' @return The plotting object returned by eyeprocess.
#' @export
plot_gazepoint_quality_dashboard <- function(x, ...) {
  .gp3_quality_require_eyeprocess()
  report <- if (inherits(x, "gaze_quality_report")) {
    x
  } else {
    create_gazepoint_quality_report(x, ...)
  }
  eyeprocess::plot_gaze_quality_dashboard(report)
}

#' Create compact Gazepoint data-quality reporting text
#'
#' @param x A gaze_quality_report, or raw Gazepoint data.
#' @param digits Number of digits used by the core reporting helper.
#' @param ... When x is raw data, arguments forwarded to
#'   create_gazepoint_quality_report().
#' @return A compact character summary from eyeprocess.
#' @export
report_gazepoint_quality <- function(x, digits = 3L, ...) {
  .gp3_quality_require_eyeprocess()
  report <- if (inherits(x, "gaze_quality_report")) {
    x
  } else {
    create_gazepoint_quality_report(x, ...)
  }
  eyeprocess::report_gaze_quality(report, digits = digits)
}
