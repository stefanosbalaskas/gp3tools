#' Read external facial-analysis exports
#'
#' Reads one or more external facial-analysis CSV files into a single tidy table.
#' The helper is designed for outputs produced outside Gazepoint, such as
#' OpenFace-, py-feat-, MediaPipe-, FaceReader-, or generic frame-level facial
#' behaviour exports. It does not infer facial expressions from Gazepoint CSV
#' files.
#'
#' @param path Path to one CSV file, several CSV files, or a directory containing
#'   CSV files.
#' @param source Facial-analysis source. Use `"auto"` to infer a likely source
#'   from column names, or one of `"openface"`, `"pyfeat"`, `"mediapipe"`,
#'   `"facereader"`, or `"generic"`.
#' @param participant_id Optional participant identifier. Either length one or
#'   the same length as the number of files read.
#' @param session_id Optional session identifier. Either length one or the same
#'   length as the number of files read.
#' @param recursive If `path` is a directory, should CSV files be searched
#'   recursively?
#' @param trim_names Should leading and trailing whitespace be removed from
#'   column names?
#' @param encoding File encoding passed to `utils::read.csv()`.
#' @param na Character values treated as missing.
#' @param ... Additional arguments passed to `utils::read.csv()`.
#'
#' @return A tibble with metadata columns identifying the source file and
#'   detected source. The returned object has class `gp3_face_export`.
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(
#'     frame = 1:2,
#'     timestamp = c(0, 0.033),
#'     confidence = c(0.98, 0.97),
#'     success = c(1, 1),
#'     AU12_r = c(0.1, 0.2)
#'   ),
#'   tmp,
#'   row.names = FALSE
#' )
#'
#' read_gazepoint_face_export(tmp)
read_gazepoint_face_export <- function(path,
                                       source = c(
                                         "auto",
                                         "openface",
                                         "pyfeat",
                                         "mediapipe",
                                         "facereader",
                                         "generic"
                                       ),
                                       participant_id = NULL,
                                       session_id = NULL,
                                       recursive = TRUE,
                                       trim_names = TRUE,
                                       encoding = "UTF-8",
                                       na = c("", "NA", "NaN"),
                                       ...) {
  source <- match.arg(source)

  files <- .gp3_face_list_files(path, recursive = recursive)

  participant_id <- .gp3_face_recycle_metadata(
    participant_id,
    length(files),
    "participant_id"
  )
  session_id <- .gp3_face_recycle_metadata(
    session_id,
    length(files),
    "session_id"
  )

  out <- vector("list", length(files))

  for (i in seq_along(files)) {
    dat <- utils::read.csv(
      files[[i]],
      check.names = FALSE,
      stringsAsFactors = FALSE,
      fileEncoding = encoding,
      na.strings = na,
      ...
    )

    names(dat) <- .gp3_face_repair_names(names(dat), trim_names = trim_names)

    detected_source <- if (identical(source, "auto")) {
      .gp3_face_detect_source(dat)
    } else {
      source
    }

    meta <- data.frame(
      gp3_face_file = basename(files[[i]]),
      gp3_face_path = normalizePath(files[[i]], winslash = "/", mustWork = FALSE),
      gp3_face_source = detected_source,
      gp3_face_participant_id = participant_id[[i]],
      gp3_face_session_id = session_id[[i]],
      stringsAsFactors = FALSE
    )

    out[[i]] <- cbind(meta, dat, stringsAsFactors = FALSE)
  }

  out <- .gp3_face_bind_rows(out)
  out <- tibble::as_tibble(out)

  class(out) <- c("gp3_face_export", class(out))
  attr(out, "gp3_face_files") <- files
  attr(out, "gp3_face_settings") <- list(
    source = source,
    recursive = recursive,
    trim_names = trim_names
  )

  out
}


#' Standardise external facial-analysis columns
#'
#' Adds common gp3tools-friendly facial-analysis columns to external
#' face-analysis outputs. The function keeps the original columns by default and
#' prepends standard columns such as `face_frame`, `face_time_sec`,
#' `face_confidence`, `face_success`, and `face_valid`.
#'
#' @param data A data frame returned by `read_gazepoint_face_export()`, a plain
#'   data frame, or a CSV path readable by `read_gazepoint_face_export()`.
#' @param source Facial-analysis source. Use `"auto"` to infer a likely source.
#' @param participant_id_col Optional participant identifier column.
#' @param frame_col Optional frame-index column.
#' @param time_col Optional time column, preferably in seconds.
#' @param confidence_col Optional face-detection confidence column.
#' @param success_col Optional face-detection success column.
#' @param face_id_col Optional face identifier column.
#' @param file_col Optional file/source column.
#' @param confidence_threshold Minimum confidence required for `face_valid`
#'   when a confidence column is available.
#' @param keep_original_columns Should original facial-analysis columns be kept?
#'
#' @return A tibble with standardised facial-analysis columns. The returned
#'   object has class `gp3_face_data`.
#' @export
#'
#' @examples
#' face <- data.frame(
#'   frame = 1:2,
#'   timestamp = c(0, 0.033),
#'   confidence = c(0.98, 0.40),
#'   success = c(1, 1),
#'   AU12_r = c(0.1, 0.2)
#' )
#'
#' standardize_gazepoint_face_columns(face)
standardize_gazepoint_face_columns <- function(data,
                                               source = c(
                                                 "auto",
                                                 "openface",
                                                 "pyfeat",
                                                 "mediapipe",
                                                 "facereader",
                                                 "generic"
                                               ),
                                               participant_id_col = NULL,
                                               frame_col = NULL,
                                               time_col = NULL,
                                               confidence_col = NULL,
                                               success_col = NULL,
                                               face_id_col = NULL,
                                               file_col = NULL,
                                               confidence_threshold = 0.80,
                                               keep_original_columns = TRUE) {
  source <- match.arg(source)

  if (is.character(data) && length(data) == 1L && file.exists(data)) {
    data <- read_gazepoint_face_export(data, source = source)
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or a readable CSV path.", call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  names(data) <- .gp3_face_repair_names(names(data), trim_names = TRUE)

  detected_source <- if (identical(source, "auto")) {
    if ("gp3_face_source" %in% names(data)) {
      unique_sources <- unique(stats::na.omit(as.character(data$gp3_face_source)))
      if (length(unique_sources) == 1L) {
        unique_sources[[1L]]
      } else if (length(unique_sources) > 1L) {
        "mixed"
      } else {
        .gp3_face_detect_source(data)
      }
    } else {
      .gp3_face_detect_source(data)
    }
  } else {
    source
  }

  participant_id_col <- .gp3_face_choose_col(
    data,
    participant_id_col,
    c(
      "gp3_face_participant_id",
      "participant_id",
      "participant",
      "subject_id",
      "subject",
      "user",
      "USER"
    )
  )

  frame_col <- .gp3_face_choose_col(
    data,
    frame_col,
    c("frame", "Frame", "FRAME", "frame_id", "video_frame", "VID_FRAME")
  )

  time_col <- .gp3_face_choose_col(
    data,
    time_col,
    c(
      "timestamp",
      "Timestamp",
      "time",
      "Time",
      "TIME",
      "time_sec",
      "seconds",
      "sec",
      "time_seconds"
    )
  )

  confidence_col <- .gp3_face_choose_col(
    data,
    confidence_col,
    c(
      "confidence",
      "Confidence",
      "detection_confidence",
      "face_confidence",
      "tracking_confidence",
      "score"
    )
  )

  success_col <- .gp3_face_choose_col(
    data,
    success_col,
    c(
      "success",
      "Success",
      "detected",
      "face_detected",
      "tracking_success",
      "valid"
    )
  )

  face_id_col <- .gp3_face_choose_col(
    data,
    face_id_col,
    c("face_id", "FaceID", "face", "face_index", "person_id")
  )

  file_col <- .gp3_face_choose_col(
    data,
    file_col,
    c("gp3_face_file", "file", "filename", "video", "input")
  )

  face_confidence <- .gp3_face_numeric_col(data, confidence_col)
  face_success <- .gp3_face_logical_col(data, success_col)

  face_valid <- .gp3_face_validity(
    face_confidence = face_confidence,
    face_success = face_success,
    confidence_threshold = confidence_threshold
  )

  std <- data.frame(
    face_source = rep(detected_source, nrow(data)),
    face_file = .gp3_face_character_col(data, file_col),
    participant_id = .gp3_face_character_col(data, participant_id_col),
    face_id = .gp3_face_character_col(data, face_id_col),
    face_frame = .gp3_face_integer_col(data, frame_col),
    face_time_sec = .gp3_face_numeric_col(data, time_col),
    face_time_ms = .gp3_face_numeric_col(data, time_col) * 1000,
    face_confidence = face_confidence,
    face_success = face_success,
    face_valid = face_valid,
    stringsAsFactors = FALSE
  )

  pose <- .gp3_face_pose_columns(data)

  if (keep_original_columns) {
    original <- data[, setdiff(names(data), names(std)), drop = FALSE]
    out <- cbind(std, original, stringsAsFactors = FALSE)
  } else {
    out <- std
  }

  if (ncol(pose) > 0L) {
    pose <- pose[, setdiff(names(pose), names(out)), drop = FALSE]
    out <- cbind(out, pose, stringsAsFactors = FALSE)
  }

  out <- tibble::as_tibble(out)

  class(out) <- c("gp3_face_data", class(out))
  attr(out, "gp3_face_standardization") <- list(
    source = source,
    detected_source = detected_source,
    participant_id_col = participant_id_col,
    frame_col = frame_col,
    time_col = time_col,
    confidence_col = confidence_col,
    success_col = success_col,
    face_id_col = face_id_col,
    file_col = file_col,
    confidence_threshold = confidence_threshold
  )

  out
}


.gp3_face_list_files <- function(path, recursive = TRUE) {
  if (!is.character(path) || length(path) < 1L) {
    stop("`path` must be one or more file or directory paths.", call. = FALSE)
  }

  files <- character(0)

  for (p in path) {
    if (dir.exists(p)) {
      found <- list.files(
        p,
        pattern = "\\.csv$",
        recursive = recursive,
        full.names = TRUE,
        ignore.case = TRUE
      )
      files <- c(files, found)
    } else if (file.exists(p)) {
      files <- c(files, p)
    } else {
      stop("File or directory does not exist: ", p, call. = FALSE)
    }
  }

  files <- unique(files)

  if (length(files) < 1L) {
    stop("No CSV files were found in `path`.", call. = FALSE)
  }

  files
}


.gp3_face_repair_names <- function(x, trim_names = TRUE) {
  if (trim_names) {
    x <- trimws(x)
  }

  missing <- is.na(x) | x == ""
  x[missing] <- paste0("unnamed_face_column_", seq_len(sum(missing)))
  make.unique(x, sep = "_")
}


.gp3_face_detect_source <- function(data) {
  nms <- names(data)

  has_openface <- all(c("frame", "timestamp", "confidence", "success") %in% nms) &&
    any(grepl("^AU[0-9]{2}_[rc]$", nms))

  if (has_openface) {
    return("openface")
  }

  has_mediapipe <- any(grepl("blendshape|face_landmark|faceblendshape", nms, ignore.case = TRUE))

  if (has_mediapipe) {
    return("mediapipe")
  }

  has_pyfeat <- any(grepl("^AU[0-9]{2}$|^AU[0-9]{2}_r$", nms)) &&
    any(grepl("anger|happy|sad|fear|surprise|disgust|neutral|valence|arousal",
              nms,
              ignore.case = TRUE))

  if (has_pyfeat) {
    return("pyfeat")
  }

  has_facereader <- any(grepl("valence|arousal|neutral|happy|sad|angry|surprised",
                              nms,
                              ignore.case = TRUE)) &&
    any(grepl("quality|model|fit|head|orientation", nms, ignore.case = TRUE))

  if (has_facereader) {
    return("facereader")
  }

  "generic"
}


.gp3_face_recycle_metadata <- function(x, n, name) {
  if (is.null(x)) {
    return(rep(NA_character_, n))
  }

  x <- as.character(x)

  if (length(x) == 1L) {
    return(rep(x, n))
  }

  if (length(x) != n) {
    stop(
      "`",
      name,
      "` must have length 1 or the same length as the number of files.",
      call. = FALSE
    )
  }

  x
}


.gp3_face_bind_rows <- function(x) {
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


.gp3_face_choose_col <- function(data, supplied, candidates) {
  if (!is.null(supplied)) {
    if (!supplied %in% names(data)) {
      stop("Column not found: ", supplied, call. = FALSE)
    }
    return(supplied)
  }

  idx <- match(tolower(candidates), tolower(names(data)))
  idx <- idx[!is.na(idx)]

  if (length(idx) < 1L) {
    return(NULL)
  }

  names(data)[idx[[1L]]]
}


.gp3_face_numeric_col <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_real_, nrow(data)))
  }

  suppressWarnings(as.numeric(data[[col]]))
}


.gp3_face_integer_col <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_integer_, nrow(data)))
  }

  suppressWarnings(as.integer(as.numeric(data[[col]])))
}


.gp3_face_character_col <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA_character_, nrow(data)))
  }

  as.character(data[[col]])
}


.gp3_face_logical_col <- function(data, col) {
  if (is.null(col)) {
    return(rep(NA, nrow(data)))
  }

  x <- data[[col]]

  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    return(!is.na(x) & x > 0)
  }

  x <- tolower(trimws(as.character(x)))

  out <- rep(NA, length(x))
  out[x %in% c("1", "true", "t", "yes", "y", "success", "valid", "detected")] <- TRUE
  out[x %in% c("0", "false", "f", "no", "n", "fail", "failed", "invalid", "missing")] <- FALSE

  out
}


.gp3_face_validity <- function(face_confidence,
                               face_success,
                               confidence_threshold = 0.80) {
  has_conf <- !all(is.na(face_confidence))
  has_success <- !all(is.na(face_success))

  if (has_conf && has_success) {
    return(!is.na(face_success) &
             face_success &
             !is.na(face_confidence) &
             face_confidence >= confidence_threshold)
  }

  if (has_conf) {
    return(!is.na(face_confidence) & face_confidence >= confidence_threshold)
  }

  if (has_success) {
    return(!is.na(face_success) & face_success)
  }

  rep(NA, length(face_confidence))
}


.gp3_face_pose_columns <- function(data) {
  pose_map <- c(
    face_pose_tx = "pose_Tx",
    face_pose_ty = "pose_Ty",
    face_pose_tz = "pose_Tz",
    face_pose_rx = "pose_Rx",
    face_pose_ry = "pose_Ry",
    face_pose_rz = "pose_Rz"
  )

  out <- list()

  for (nm in names(pose_map)) {
    col <- .gp3_face_choose_col(data, NULL, pose_map[[nm]])
    if (!is.null(col)) {
      out[[nm]] <- suppressWarnings(as.numeric(data[[col]]))
    }
  }

  if (length(out) < 1L) {
    return(data.frame())
  }

  as.data.frame(out, stringsAsFactors = FALSE)
}
