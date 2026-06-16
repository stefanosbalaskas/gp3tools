#' Run optional eyetools fixation and saccade detection
#'
#' Prepare Gazepoint sample-level gaze data for the optional `eyetools` package
#' and, when `eyetools` is installed, run fixation and/or saccade detection using
#' `eyetools::fixation_dispersion()`, `eyetools::fixation_VTI()`, and/or
#' `eyetools::saccade_VTI()`.
#'
#' This helper is an optional external-detector branch. It does not replace the
#' main `gp3tools` summaries or AOI/transition workflows.
#'
#' @param data A data frame containing sample-level gaze data.
#' @param participant_col Participant/subject column. If `NULL`, common names are detected.
#' @param trial_col Trial column. If `NULL`, common names are detected.
#' @param time_col Time column. If `NULL`, common names are detected.
#' @param x_col Horizontal gaze coordinate column. If `NULL`, common names are detected.
#' @param y_col Vertical gaze coordinate column. If `NULL`, common names are detected.
#' @param condition_col Optional condition/group column.
#' @param stimulus_col Optional stimulus/media column.
#' @param method Detector branch. Options are `"dispersion"`, `"vti"`, `"saccade"`, and `"all"`.
#' @param sample_rate Optional sample rate passed to velocity-threshold functions.
#' @param threshold Velocity threshold passed to velocity-threshold functions.
#' @param min_dur Minimum fixation duration in milliseconds.
#' @param min_dur_sac Minimum saccade duration in milliseconds.
#' @param disp_tol Dispersion tolerance in pixels.
#' @param NA_tol Missing-data tolerance passed to `fixation_dispersion()`.
#' @param smooth Logical; passed to `fixation_VTI()`.
#' @param drop_missing Logical. If `TRUE`, rows with non-finite time, x, or y are removed before detector execution.
#' @param progress Logical. Passed to eyetools detector functions.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_eyetools_fixation_detection`.
#' @export
run_gazepoint_eyetools_fixation_detection <- function(
    data,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    x_col = NULL,
    y_col = NULL,
    condition_col = NULL,
    stimulus_col = NULL,
    method = c("dispersion", "vti", "saccade", "all"),
    sample_rate = NULL,
    threshold = 100,
    min_dur = 150,
    min_dur_sac = 20,
    disp_tol = 100,
    NA_tol = 0.25,
    smooth = FALSE,
    drop_missing = TRUE,
    progress = FALSE,
    name = "gazepoint_eyetools_fixation_detection"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  method <- match.arg(method)

  .gp3_eyetools_check_label(name, "name")
  .gp3_eyetools_check_positive_number(threshold, "threshold")
  .gp3_eyetools_check_positive_number(min_dur, "min_dur")
  .gp3_eyetools_check_positive_number(min_dur_sac, "min_dur_sac")
  .gp3_eyetools_check_positive_number(disp_tol, "disp_tol")
  .gp3_eyetools_check_prop(NA_tol, "NA_tol")
  .gp3_eyetools_check_logical(drop_missing, "drop_missing")
  .gp3_eyetools_check_logical(smooth, "smooth")
  .gp3_eyetools_check_logical(progress, "progress")

  if (!is.null(sample_rate)) {
    .gp3_eyetools_check_positive_number(sample_rate, "sample_rate")
  }

  names_data <- names(data)

  participant_col <- .gp3_eyetools_resolve_or_detect_col(
    participant_col,
    names_data,
    "participant_col",
    c("pID", "subject", "participant", "participant_id", "USER_FILE", "recording_id"),
    required = TRUE
  )

  trial_col <- .gp3_eyetools_resolve_or_detect_col(
    trial_col,
    names_data,
    "trial_col",
    c("trial", "trial_id", "trial_global", "trialNumber", "TRIAL_INDEX", "MEDIA_ID", "media_id"),
    required = TRUE
  )

  time_col <- .gp3_eyetools_resolve_or_detect_col(
    time_col,
    names_data,
    "time_col",
    c("time", "time_ms", "timestamp", "TIMESTAMP", "TIME", "sample_index", "CNT"),
    required = TRUE
  )

  x_col <- .gp3_eyetools_resolve_or_detect_col(
    x_col,
    names_data,
    "x_col",
    c("x", "X", "gaze_x", "FPOGX", "LPOGX", "RPOGX", "POGX"),
    required = TRUE
  )

  y_col <- .gp3_eyetools_resolve_or_detect_col(
    y_col,
    names_data,
    "y_col",
    c("y", "Y", "gaze_y", "FPOGY", "LPOGY", "RPOGY", "POGY"),
    required = TRUE
  )

  condition_col <- .gp3_eyetools_resolve_or_detect_col(
    condition_col,
    names_data,
    "condition_col",
    c("condition", "CONDITION", "group", "GROUP", "trial_type"),
    required = FALSE
  )

  stimulus_col <- .gp3_eyetools_resolve_or_detect_col(
    stimulus_col,
    names_data,
    "stimulus_col",
    c("stimulus", "stimulus_id", "stimulus_file", "media", "MEDIA_ID", "media_id"),
    required = FALSE
  )

  prepared_data <- tibble::tibble(
    pID = as.character(data[[participant_col]]),
    trial = as.character(data[[trial_col]]),
    time = suppressWarnings(as.numeric(data[[time_col]])),
    x = suppressWarnings(as.numeric(data[[x_col]])),
    y = suppressWarnings(as.numeric(data[[y_col]]))
  )

  if (!is.null(condition_col)) {
    prepared_data$condition <- as.character(data[[condition_col]])
  } else {
    prepared_data$condition <- NA_character_
  }

  if (!is.null(stimulus_col)) {
    prepared_data$stimulus <- as.character(data[[stimulus_col]])
  } else {
    prepared_data$stimulus <- NA_character_
  }

  prepared_data <- prepared_data |>
    dplyr::arrange(.data$pID, .data$trial, .data$time)

  n_prepared_before_drop <- nrow(prepared_data)

  if (isTRUE(drop_missing)) {
    prepared_data <- prepared_data |>
      dplyr::filter(
        is.finite(.data$time),
        is.finite(.data$x),
        is.finite(.data$y),
        !is.na(.data$pID),
        !is.na(.data$trial)
      )
  }

  if (nrow(prepared_data) == 0L) {
    stop("No valid rows remain after preparing eyetools input.", call. = FALSE)
  }

  eyetools_available <- .gp3_eyetools_namespace_available()

  function_audit <- .gp3_eyetools_function_audit(eyetools_available)

  settings <- tibble::tibble(
    setting = c(
      "participant_col",
      "trial_col",
      "time_col",
      "x_col",
      "y_col",
      "condition_col",
      "stimulus_col",
      "method",
      "sample_rate",
      "threshold",
      "min_dur",
      "min_dur_sac",
      "disp_tol",
      "NA_tol",
      "smooth",
      "drop_missing",
      "progress",
      "name"
    ),
    value = c(
      participant_col,
      trial_col,
      time_col,
      x_col,
      y_col,
      .gp3_eyetools_collapse_nullable(condition_col),
      .gp3_eyetools_collapse_nullable(stimulus_col),
      method,
      .gp3_eyetools_collapse_nullable(sample_rate),
      as.character(threshold),
      as.character(min_dur),
      as.character(min_dur_sac),
      as.character(disp_tol),
      as.character(NA_tol),
      as.character(smooth),
      as.character(drop_missing),
      as.character(progress),
      name
    )
  )

  if (!eyetools_available) {
    overview <- .gp3_eyetools_overview(
      name = name,
      prepared_data = prepared_data,
      n_prepared_before_drop = n_prepared_before_drop,
      status = "skipped_missing_package",
      message = "Optional package 'eyetools' is not installed.",
      method = method,
      fixation_dispersion_data = NULL,
      fixation_vti_data = NULL,
      saccade_data = NULL
    )

    out <- list(
      overview = overview,
      prepared_data = prepared_data,
      fixation_dispersion = NULL,
      fixation_vti = NULL,
      saccades = NULL,
      function_audit = function_audit,
      settings = settings
    )

    class(out) <- c("gp3_eyetools_fixation_detection", "list")
    return(out)
  }

  required_functions <- .gp3_eyetools_required_functions(method)
  missing_required <- required_functions[!vapply(required_functions, .gp3_eyetools_export_exists, logical(1))]

  if (length(missing_required) > 0L) {
    overview <- .gp3_eyetools_overview(
      name = name,
      prepared_data = prepared_data,
      n_prepared_before_drop = n_prepared_before_drop,
      status = "skipped_missing_eyetools_functions",
      message = paste(
        "Installed 'eyetools' package is missing required functions:",
        paste(missing_required, collapse = ", ")
      ),
      method = method,
      fixation_dispersion_data = NULL,
      fixation_vti_data = NULL,
      saccade_data = NULL
    )

    out <- list(
      overview = overview,
      prepared_data = prepared_data,
      fixation_dispersion = NULL,
      fixation_vti = NULL,
      saccades = NULL,
      function_audit = function_audit,
      settings = settings
    )

    class(out) <- c("gp3_eyetools_fixation_detection", "list")
    return(out)
  }

  fixation_dispersion_data <- NULL
  fixation_vti_data <- NULL
  saccade_data <- NULL
  statuses <- character(0)
  messages <- character(0)

  if (method %in% c("dispersion", "all")) {
    fixation_dispersion_fun <- .gp3_eyetools_get_export("fixation_dispersion")

    fixation_dispersion_data <- tryCatch({
      fixation_dispersion_fun(
        data = prepared_data,
        min_dur = min_dur,
        disp_tol = disp_tol,
        NA_tol = NA_tol,
        progress = progress
      )
    }, error = function(e) {
      .gp3_eyetools_error(conditionMessage(e))
    })

    if (inherits(fixation_dispersion_data, "gp3_eyetools_error")) {
      statuses <- c(statuses, "error_fixation_dispersion")
      messages <- c(messages, fixation_dispersion_data$error)
      fixation_dispersion_data <- NULL
    } else {
      fixation_dispersion_data <- tibble::as_tibble(fixation_dispersion_data)
      statuses <- c(statuses, "fixation_dispersion_complete")
    }
  }

  if (method %in% c("vti", "all")) {
    fixation_vti_fun <- .gp3_eyetools_get_export("fixation_VTI")

    fixation_vti_data <- .gp3_eyetools_fixation_vti_compat(
      fixation_vti_fun = fixation_vti_fun,
      prepared_data = prepared_data,
      sample_rate = sample_rate,
      threshold = threshold,
      min_dur = min_dur,
      min_dur_sac = min_dur_sac,
      disp_tol = disp_tol,
      smooth = smooth,
      progress = progress
    )

    if (inherits(fixation_vti_data, "gp3_eyetools_error")) {
      statuses <- c(statuses, "error_fixation_vti")
      messages <- c(messages, fixation_vti_data$error)
      fixation_vti_data <- NULL
    } else {
      fixation_vti_data <- tibble::as_tibble(fixation_vti_data)
      statuses <- c(statuses, "fixation_vti_complete")
    }
  }

  if (method %in% c("saccade", "all")) {
    saccade_fun <- .gp3_eyetools_get_export("saccade_VTI")

    saccade_data <- .gp3_eyetools_saccade_vti_compat(
      saccade_fun = saccade_fun,
      prepared_data = prepared_data,
      sample_rate = sample_rate,
      threshold = threshold,
      min_dur_sac = min_dur_sac
    )

    if (inherits(saccade_data, "gp3_eyetools_error")) {
      statuses <- c(statuses, "error_saccade_vti")
      messages <- c(messages, saccade_data$error)
      saccade_data <- NULL
    } else {
      saccade_data <- tibble::as_tibble(saccade_data)
      statuses <- c(statuses, "saccade_vti_complete")
    }
  }

  final_status <- if (length(statuses) == 0L) {
    "complete"
  } else if (all(grepl("^error", statuses))) {
    "error_all_detectors"
  } else if (any(grepl("^error", statuses))) {
    "partial_complete"
  } else {
    "complete"
  }

  final_message <- if (length(messages) == 0L) {
    paste("eyetools detector branch completed with status:", paste(statuses, collapse = ", "))
  } else {
    paste(
      paste("eyetools detector branch statuses:", paste(statuses, collapse = ", ")),
      paste("Errors:", paste(messages, collapse = " | ")),
      sep = " "
    )
  }

  overview <- .gp3_eyetools_overview(
    name = name,
    prepared_data = prepared_data,
    n_prepared_before_drop = n_prepared_before_drop,
    status = final_status,
    message = final_message,
    method = method,
    fixation_dispersion_data = fixation_dispersion_data,
    fixation_vti_data = fixation_vti_data,
    saccade_data = saccade_data
  )

  out <- list(
    overview = overview,
    prepared_data = prepared_data,
    fixation_dispersion = fixation_dispersion_data,
    fixation_vti = fixation_vti_data,
    saccades = saccade_data,
    function_audit = function_audit,
    settings = settings
  )

  class(out) <- c("gp3_eyetools_fixation_detection", "list")

  out
}

.gp3_eyetools_overview <- function(
    name,
    prepared_data,
    n_prepared_before_drop,
    status,
    message,
    method,
    fixation_dispersion_data,
    fixation_vti_data,
    saccade_data
) {
  tibble::tibble(
    object_name = name,
    method = method,
    n_input_rows_prepared = n_prepared_before_drop,
    n_rows_used = nrow(prepared_data),
    n_rows_dropped = n_prepared_before_drop - nrow(prepared_data),
    n_participants = dplyr::n_distinct(prepared_data$pID),
    n_trials = dplyr::n_distinct(paste(prepared_data$pID, prepared_data$trial, sep = "||")),
    n_fixations_dispersion = .gp3_eyetools_n_rows(fixation_dispersion_data),
    n_fixations_vti = .gp3_eyetools_n_rows(fixation_vti_data),
    n_saccades = .gp3_eyetools_n_rows(saccade_data),
    detector_status = status,
    message = message
  )
}

.gp3_eyetools_n_rows <- function(x) {
  if (is.null(x)) {
    return(NA_integer_)
  }

  nrow(x)
}

.gp3_eyetools_package_name <- function() {
  "eyetools"
}

.gp3_eyetools_namespace_available <- function() {
  requireNamespace(.gp3_eyetools_package_name(), quietly = TRUE)
}

.gp3_eyetools_get_export <- function(function_name) {
  getExportedValue(.gp3_eyetools_package_name(), function_name)
}

.gp3_eyetools_export_exists <- function(function_name) {
  if (!.gp3_eyetools_namespace_available()) {
    return(FALSE)
  }

  function_name %in% getNamespaceExports(.gp3_eyetools_package_name())
}

.gp3_eyetools_required_functions <- function(method) {
  switch(
    method,
    dispersion = "fixation_dispersion",
    vti = "fixation_VTI",
    saccade = "saccade_VTI",
    all = c("fixation_dispersion", "fixation_VTI", "saccade_VTI")
  )
}

.gp3_eyetools_function_audit <- function(eyetools_available) {
  functions <- tibble::tibble(
    function_name = c(
      "fixation_dispersion",
      "fixation_VTI",
      "saccade_VTI"
    ),
    purpose = c(
      "dispersion_fixation_detection",
      "velocity_threshold_fixation_detection",
      "velocity_threshold_saccade_detection"
    )
  )

  if (!eyetools_available) {
    functions$available <- FALSE
    return(functions)
  }

  functions$available <- vapply(
    functions$function_name,
    .gp3_eyetools_export_exists,
    logical(1)
  )

  functions
}


.gp3_eyetools_detector_inputs <- function(prepared_data) {
  minimal <- prepared_data |>
    dplyr::select(
      "pID",
      "trial",
      "time",
      "x",
      "y"
    )

  numeric_ids <- minimal |>
    dplyr::mutate(
      pID = as.integer(factor(.data$pID)),
      trial = as.integer(factor(.data$trial))
    )

  ordered_minimal <- minimal |>
    dplyr::arrange(.data$pID, .data$trial, .data$time)

  ordered_numeric_ids <- numeric_ids |>
    dplyr::arrange(.data$pID, .data$trial, .data$time)

  list(
    prepared_data = prepared_data,
    minimal = minimal,
    ordered_minimal = ordered_minimal,
    numeric_ids = numeric_ids,
    ordered_numeric_ids = ordered_numeric_ids
  )
}

.gp3_eyetools_fixation_vti_compat <- function(
    fixation_vti_fun,
    prepared_data,
    sample_rate,
    threshold,
    min_dur,
    min_dur_sac,
    disp_tol,
    smooth,
    progress
) {
  input_variants <- .gp3_eyetools_detector_inputs(prepared_data)

  attempts <- lapply(input_variants, function(input_data) {
    function() {
      fixation_vti_fun(
        data = input_data,
        sample_rate = sample_rate,
        threshold = threshold,
        min_dur = min_dur,
        min_dur_sac = min_dur_sac,
        disp_tol = disp_tol,
        smooth = smooth,
        progress = progress
      )
    }
  })

  .gp3_eyetools_run_attempts(
    attempts,
    label = "eyetools fixation_VTI call"
  )
}

.gp3_eyetools_saccade_vti_compat <- function(
    saccade_fun,
    prepared_data,
    sample_rate,
    threshold,
    min_dur_sac
) {
  input_variants <- .gp3_eyetools_detector_inputs(prepared_data)

  attempts <- lapply(input_variants, function(input_data) {
    function() {
      saccade_fun(
        data = input_data,
        sample_rate = sample_rate,
        threshold = threshold,
        min_dur = min_dur_sac
      )
    }
  })

  .gp3_eyetools_run_attempts(
    attempts,
    label = "eyetools saccade_VTI call"
  )
}

.gp3_eyetools_run_attempts <- function(attempts, label) {
  errors <- character(0)

  for (attempt in attempts) {
    result <- tryCatch({
      attempt()
    }, error = function(e) {
      .gp3_eyetools_error(conditionMessage(e))
    })

    if (!inherits(result, "gp3_eyetools_error")) {
      return(result)
    }

    errors <- c(errors, result$error)
  }

  errors <- unique(errors)
  errors <- errors[nzchar(errors)]

  .gp3_eyetools_error(
    paste(
      paste0(label, " failed across compatible input variants."),
      paste(errors, collapse = " | "),
      sep = " "
    )
  )
}

.gp3_eyetools_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
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
    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_eyetools_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a finite positive number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_check_prop <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x > 1) {
    stop("`", arg, "` must be a finite number between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_eyetools_collapse_nullable <- function(...) {
  values <- list(...)

  if (length(values) == 0L) {
    return(NA_character_)
  }

  values <- values[!vapply(values, is.null, logical(1))]

  if (length(values) == 0L) {
    return(NA_character_)
  }

  values <- unlist(values, use.names = FALSE)

  if (length(values) == 0L || all(is.na(values))) {
    return(NA_character_)
  }

  paste(as.character(values), collapse = ", ")
}

.gp3_eyetools_clean_error_message <- function(x) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.gp3_eyetools_error <- function(message) {
  structure(
    list(error = .gp3_eyetools_clean_error_message(message)),
    class = "gp3_eyetools_error"
  )
}
