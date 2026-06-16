#' Run an optional gazeR pupil-preprocessing cross-check
#'
#' Prepare Gazepoint pupil data for the optional `gazer` package and, when
#' `gazer` is installed, run a conservative pupil-preprocessing cross-check using
#' gazeR-style blink extension, smoothing/interpolation, optional baseline
#' correction, and optional downsampling.
#'
#' This helper is a cross-check branch. It is not intended to replace the main
#' `gp3tools` pupil preprocessing pipeline.
#'
#' @param data A data frame containing Gazepoint or gp3tools pupil time-series data.
#' @param participant_col Participant/subject column. If `NULL`, common names are detected.
#' @param trial_col Trial column. If `NULL`, common names are detected.
#' @param time_col Time column. If `NULL`, common names are detected.
#' @param pupil_col Pupil column. If `NULL`, common processed/raw pupil columns are detected.
#' @param condition_col Optional condition column. If `NULL`, common names are detected; otherwise `"all_data"` is used.
#' @param message_col Optional message/event column.
#' @param blink_col Optional blink/trackloss column.
#' @param hz Sampling rate passed to gazeR functions.
#' @param fillback Blink-extension window before missing/blink samples, in ms.
#' @param fillforward Blink-extension window after missing/blink samples, in ms.
#' @param smooth_n Smoothing window parameter passed to `gazer::smooth_interpolate_pupil()`.
#' @param step_first Processing order passed to `gazer::smooth_interpolate_pupil()`.
#' @param interpolation_type Interpolation type passed to `gazer::smooth_interpolate_pupil()`.
#' @param maxgap Maximum gap passed to `gazer::smooth_interpolate_pupil()`.
#' @param baseline_window Optional numeric vector of length 2 passed to `gazer::baseline_correction_pupil()`.
#' @param baseline_event Optional event label passed to `gazer::baseline_correction_pupil_msg()`.
#' @param baseline_dur Baseline duration used with `baseline_event`.
#' @param baseline_method Baseline method used with `baseline_event`.
#' @param bin_length Optional bin length passed to `gazer::downsample_gaze()`.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_gazer_crosscheck`.
#' @export
run_gazepoint_gazer_crosscheck <- function(
    data,
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    pupil_col = NULL,
    condition_col = NULL,
    message_col = NULL,
    blink_col = NULL,
    hz = 60,
    fillback = 100,
    fillforward = 100,
    smooth_n = 5,
    step_first = c("smooth", "interpolate"),
    interpolation_type = "linear",
    maxgap = Inf,
    baseline_window = NULL,
    baseline_event = NULL,
    baseline_dur = 100,
    baseline_method = "sub",
    bin_length = NULL,
    name = "gazepoint_gazer_crosscheck"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  step_first <- match.arg(step_first)

  .gp3_gazer_check_label(name, "name")
  .gp3_gazer_check_positive_number(hz, "hz")
  .gp3_gazer_check_non_negative_number(fillback, "fillback")
  .gp3_gazer_check_non_negative_number(fillforward, "fillforward")
  .gp3_gazer_check_positive_integer(smooth_n, "smooth_n")
  .gp3_gazer_check_positive_number(baseline_dur, "baseline_dur")

  if (!is.null(bin_length)) {
    .gp3_gazer_check_positive_number(bin_length, "bin_length")
  }

  if (!is.null(baseline_window)) {
    if (
      !is.numeric(baseline_window) ||
      length(baseline_window) != 2L ||
      anyNA(baseline_window) ||
      any(!is.finite(baseline_window))
    ) {
      stop("`baseline_window` must be a finite numeric vector of length 2.", call. = FALSE)
    }
  }

  names_data <- names(data)

  participant_col <- .gp3_gazer_resolve_or_detect_col(
    participant_col,
    names_data,
    "participant_col",
    c(
      "subject",
      "participant",
      "participant_id",
      "pID",
      "USER_FILE",
      "user",
      "recording_id"
    ),
    required = TRUE
  )

  trial_col <- .gp3_gazer_resolve_or_detect_col(
    trial_col,
    names_data,
    "trial_col",
    c(
      "trial_global",
      "trial",
      "trial_id",
      "TRIAL_INDEX",
      "item_id",
      "media_id",
      "MEDIA_ID"
    ),
    required = TRUE
  )

  time_col <- .gp3_gazer_resolve_or_detect_col(
    time_col,
    names_data,
    "time_col",
    c(
      "time",
      "time_ms",
      "timestamp",
      "TIMESTAMP",
      "TIME",
      "sample_index",
      "CNT"
    ),
    required = TRUE
  )

  pupil_col <- .gp3_gazer_resolve_or_detect_col(
    pupil_col,
    names_data,
    "pupil_col",
    c(
      "pupil_bc_processed",
      "pupil_smoothed",
      "pupil_interpolated",
      "pupil_clean",
      "pupil_for_preprocessing",
      "pupil_raw",
      "mean_pupil",
      "pupil",
      "LPD",
      "RPD"
    ),
    required = TRUE
  )

  condition_col <- .gp3_gazer_resolve_or_detect_col(
    condition_col,
    names_data,
    "condition_col",
    c(
      "condition",
      "CONDITION",
      "group",
      "GROUP",
      "trial_type"
    ),
    required = FALSE
  )

  message_col <- .gp3_gazer_resolve_or_detect_col(
    message_col,
    names_data,
    "message_col",
    c(
      "message",
      "event_label",
      "event",
      "event_type",
      "USER",
      "USER_DATA"
    ),
    required = FALSE
  )

  blink_col <- .gp3_gazer_resolve_or_detect_col(
    blink_col,
    names_data,
    "blink_col",
    c(
      "blink",
      "trackloss",
      "Trackloss",
      "missing_pupil",
      "missing_gaze",
      "artifact_flag",
      "pupil_bad_sample_basic"
    ),
    required = FALSE
  )

  prepared_data <- tibble::tibble(
    subject = as.character(data[[participant_col]]),
    trial = as.character(data[[trial_col]]),
    time = suppressWarnings(as.numeric(data[[time_col]])),
    pupil = suppressWarnings(as.numeric(data[[pupil_col]]))
  )

  if (!is.null(condition_col)) {
    prepared_data$condition <- as.character(data[[condition_col]])
  } else {
    prepared_data$condition <- "all_data"
  }

  if (!is.null(message_col)) {
    prepared_data$message <- as.character(data[[message_col]])
  } else {
    prepared_data$message <- NA_character_
  }

  if (!is.null(blink_col)) {
    prepared_data$blink <- as.logical(data[[blink_col]])
  } else {
    prepared_data$blink <- is.na(prepared_data$pupil)
  }

  prepared_data$condition[
    is.na(prepared_data$condition) | !nzchar(prepared_data$condition)
  ] <- "all_data"
  prepared_data$message[is.na(prepared_data$message)] <- NA_character_
  prepared_data$blink[is.na(prepared_data$blink)] <- FALSE

  prepared_data <- prepared_data |>
    dplyr::arrange(.data$subject, .data$trial, .data$time)

  gazer_available <- .gp3_gazer_namespace_available()

  settings <- tibble::tibble(
    setting = c(
      "participant_col",
      "trial_col",
      "time_col",
      "pupil_col",
      "condition_col",
      "message_col",
      "blink_col",
      "hz",
      "fillback",
      "fillforward",
      "smooth_n",
      "step_first",
      "interpolation_type",
      "maxgap",
      "baseline_window",
      "baseline_event",
      "baseline_dur",
      "baseline_method",
      "bin_length",
      "name"
    ),
    value = c(
      participant_col,
      trial_col,
      time_col,
      pupil_col,
      .gp3_gazer_collapse_nullable(condition_col),
      .gp3_gazer_collapse_nullable(message_col),
      .gp3_gazer_collapse_nullable(blink_col),
      as.character(hz),
      as.character(fillback),
      as.character(fillforward),
      as.character(smooth_n),
      step_first,
      interpolation_type,
      as.character(maxgap),
      .gp3_gazer_collapse_nullable(baseline_window),
      .gp3_gazer_collapse_nullable(baseline_event),
      as.character(baseline_dur),
      baseline_method,
      .gp3_gazer_collapse_nullable(bin_length),
      name
    )
  )

  if (!gazer_available) {
    overview <- .gp3_gazer_overview(
      name = name,
      prepared_data = prepared_data,
      status = "skipped_missing_package",
      message = "Optional package 'gazer' is not installed."
    )

    out <- list(
      overview = overview,
      prepared_data = prepared_data,
      extended_data = NULL,
      processed_data = NULL,
      baseline_data = NULL,
      downsampled_data = NULL,
      function_audit = .gp3_gazer_function_audit(gazer_available = FALSE),
      settings = settings
    )

    class(out) <- c("gp3_gazer_crosscheck", "list")
    return(out)
  }

  function_audit <- .gp3_gazer_function_audit(gazer_available = TRUE)
  missing_required <- function_audit$function_name[
    function_audit$required & !function_audit$available
  ]

  if (length(missing_required) > 0L) {
    overview <- .gp3_gazer_overview(
      name = name,
      prepared_data = prepared_data,
      status = "skipped_missing_gazer_functions",
      message = paste(
        "Installed 'gazer' package is missing required functions:",
        paste(missing_required, collapse = ", ")
      )
    )

    out <- list(
      overview = overview,
      prepared_data = prepared_data,
      extended_data = NULL,
      processed_data = NULL,
      baseline_data = NULL,
      downsampled_data = NULL,
      function_audit = function_audit,
      settings = settings
    )

    class(out) <- c("gp3_gazer_crosscheck", "list")
    return(out)
  }

  .gp3_gazer_attach_dplyr_for_gazer()

  extend_blinks_fun <- .gp3_gazer_get_export("extend_blinks")
  smooth_interp_fun <- .gp3_gazer_get_export("smooth_interpolate_pupil")

  extended_data <- tryCatch({
    prepared_data |>
      dplyr::group_by(.data$subject, .data$trial) |>
      dplyr::mutate(
        gazer_extendpupil = extend_blinks_fun(
          .data$pupil,
          fillback = fillback,
          fillforward = fillforward,
          hz = hz
        )
      ) |>
      dplyr::ungroup()
  }, error = function(e) {
    .gp3_gazer_error(conditionMessage(e))
  })

  if (inherits(extended_data, "gp3_gazer_error")) {
    overview <- .gp3_gazer_overview(
      name = name,
      prepared_data = prepared_data,
      status = "error_extend_blinks",
      message = extended_data$error
    )

    out <- list(
      overview = overview,
      prepared_data = prepared_data,
      extended_data = NULL,
      processed_data = NULL,
      baseline_data = NULL,
      downsampled_data = NULL,
      function_audit = function_audit,
      settings = settings
    )

    class(out) <- c("gp3_gazer_crosscheck", "list")
    return(out)
  }

  processed_data <- .gp3_gazer_smooth_interpolate_compat(
    smooth_interp_fun = smooth_interp_fun,
    extended_data = extended_data,
    step_first = step_first,
    maxgap = maxgap,
    interpolation_type = interpolation_type,
    hz = hz,
    smooth_n = smooth_n
  )

  if (inherits(processed_data, "gp3_gazer_error")) {
    overview <- .gp3_gazer_overview(
      name = name,
      prepared_data = prepared_data,
      status = "error_smooth_interpolate",
      message = processed_data$error
    )

    out <- list(
      overview = overview,
      prepared_data = prepared_data,
      extended_data = extended_data,
      processed_data = NULL,
      baseline_data = NULL,
      downsampled_data = NULL,
      function_audit = function_audit,
      settings = settings
    )

    class(out) <- c("gp3_gazer_crosscheck", "list")
    return(out)
  }

  baseline_data <- processed_data
  baseline_status <- "not_requested"

  if (!is.null(baseline_event)) {
    if (.gp3_gazer_export_exists("baseline_correction_pupil_msg")) {
      baseline_msg_fun <- .gp3_gazer_get_export("baseline_correction_pupil_msg")

      baseline_data <- .gp3_gazer_baseline_event_compat(
        baseline_msg_fun = baseline_msg_fun,
        processed_data = processed_data,
        pupil_col = .gp3_gazer_choose_pupil_output_col(processed_data),
        baseline_event = baseline_event,
        baseline_dur = baseline_dur,
        baseline_method = baseline_method
      )

      baseline_status <- if (inherits(baseline_data, "gp3_gazer_error")) {
        "error_baseline_event"
      } else {
        "baseline_event_applied"
      }
    } else {
      baseline_status <- "skipped_missing_baseline_event_function"
    }
  } else if (!is.null(baseline_window)) {
    if (.gp3_gazer_export_exists("baseline_correction_pupil")) {
      baseline_fun <- .gp3_gazer_get_export("baseline_correction_pupil")

      baseline_data <- .gp3_gazer_baseline_window_compat(
        baseline_fun = baseline_fun,
        processed_data = processed_data,
        pupil_col = .gp3_gazer_choose_pupil_output_col(processed_data),
        baseline_window = baseline_window,
        baseline_method = baseline_method
      )

      baseline_status <- if (inherits(baseline_data, "gp3_gazer_error")) {
        "error_baseline_window"
      } else {
        "baseline_window_applied"
      }
    } else {
      baseline_status <- "skipped_missing_baseline_window_function"
    }
  }

  baseline_error_message <- NA_character_
  if (inherits(baseline_data, "gp3_gazer_error")) {
    baseline_error_message <- baseline_data$error
    baseline_data <- NULL
  }

  downsampled_data <- NULL
  downsample_status <- "not_requested"

  if (!is.null(bin_length)) {
    if (.gp3_gazer_export_exists("downsample_gaze")) {
      downsample_fun <- .gp3_gazer_get_export("downsample_gaze")
      downsample_input <- if (!is.null(baseline_data)) {
        baseline_data
      } else {
        processed_data
      }

      downsampled_data <- tryCatch({
        downsample_fun(
          downsample_input,
          bin.length = bin_length,
          timevar = "time",
          aggvars = c("subject", "condition", "timebins"),
          type = "pupil"
        )
      }, error = function(e) {
        .gp3_gazer_error(conditionMessage(e))
      })

      downsample_status <- if (inherits(downsampled_data, "gp3_gazer_error")) {
        "error_downsample"
      } else {
        "downsample_applied"
      }
    } else {
      downsample_status <- "skipped_missing_downsample_function"
    }
  }

  downsample_error_message <- NA_character_
  if (inherits(downsampled_data, "gp3_gazer_error")) {
    downsample_error_message <- downsampled_data$error
    downsampled_data <- NULL
  }

  final_status <- dplyr::case_when(
    grepl("^error", baseline_status) ~ baseline_status,
    grepl("^error", downsample_status) ~ downsample_status,
    TRUE ~ "complete"
  )

  final_message <- paste(
    c(
      paste0("baseline_status=", baseline_status),
      if (!is.na(baseline_error_message)) {
        paste0("baseline_error=", baseline_error_message)
      },
      paste0("downsample_status=", downsample_status),
      if (!is.na(downsample_error_message)) {
        paste0("downsample_error=", downsample_error_message)
      }
    ),
    collapse = "; "
  )

  overview <- .gp3_gazer_overview(
    name = name,
    prepared_data = prepared_data,
    status = final_status,
    message = final_message
  )

  out <- list(
    overview = overview,
    prepared_data = prepared_data,
    extended_data = extended_data,
    processed_data = processed_data,
    baseline_data = baseline_data,
    downsampled_data = downsampled_data,
    function_audit = function_audit,
    settings = settings
  )

  class(out) <- c("gp3_gazer_crosscheck", "list")

  out
}

.gp3_gazer_overview <- function(name, prepared_data, status, message) {
  tibble::tibble(
    object_name = name,
    n_rows_prepared = nrow(prepared_data),
    n_subjects = dplyr::n_distinct(prepared_data$subject),
    n_trials = dplyr::n_distinct(
      paste(prepared_data$subject, prepared_data$trial, sep = "||")
    ),
    n_conditions = dplyr::n_distinct(prepared_data$condition),
    prop_missing_pupil = mean(is.na(prepared_data$pupil), na.rm = TRUE),
    prop_blink_or_missing = mean(
      prepared_data$blink | is.na(prepared_data$pupil),
      na.rm = TRUE
    ),
    crosscheck_status = status,
    message = message
  )
}

.gp3_gazer_package_name <- function() {
  "gazer"
}

.gp3_gazer_namespace_available <- function() {
  pkg <- .gp3_gazer_package_name()
  requireNamespace(pkg, quietly = TRUE)
}

.gp3_gazer_get_export <- function(function_name) {
  getExportedValue(.gp3_gazer_package_name(), function_name)
}

.gp3_gazer_export_exists <- function(function_name) {
  if (!.gp3_gazer_namespace_available()) {
    return(FALSE)
  }

  function_name %in% getNamespaceExports(.gp3_gazer_package_name())
}

.gp3_gazer_function_audit <- function(gazer_available) {
  functions <- tibble::tibble(
    function_name = c(
      "extend_blinks",
      "smooth_interpolate_pupil",
      "baseline_correction_pupil",
      "baseline_correction_pupil_msg",
      "downsample_gaze"
    ),
    required = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )

  if (!gazer_available) {
    functions$available <- FALSE
    return(functions)
  }

  functions$available <- vapply(
    functions$function_name,
    .gp3_gazer_export_exists,
    logical(1)
  )

  functions
}

.gp3_gazer_choose_pupil_output_col <- function(data) {
  candidates <- c(
    "interp",
    "pup_interp",
    "smooth_interp",
    "pupil_interp",
    "movingavgpup",
    "pupil"
  )

  found <- candidates[candidates %in% names(data)]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  "pupil"
}

.gp3_gazer_smooth_interpolate_compat <- function(
    smooth_interp_fun,
    extended_data,
    step_first,
    maxgap,
    interpolation_type,
    hz,
    smooth_n
) {
  vector_call <- tryCatch({
    smooth_interp_fun(
      extended_data,
      pupil = suppressWarnings(as.numeric(extended_data$pupil)),
      extendpupil = suppressWarnings(as.numeric(extended_data$gazer_extendpupil)),
      extendblinks = TRUE,
      step.first = step_first,
      maxgap = maxgap,
      type = interpolation_type,
      hz = hz,
      n = smooth_n
    )
  }, error = function(e) {
    .gp3_gazer_error(conditionMessage(e))
  })

  if (!inherits(vector_call, "gp3_gazer_error")) {
    return(vector_call)
  }

  column_name_call <- tryCatch({
    smooth_interp_fun(
      extended_data,
      pupil = "pupil",
      extendpupil = "gazer_extendpupil",
      extendblinks = TRUE,
      step.first = step_first,
      maxgap = maxgap,
      type = interpolation_type,
      hz = hz,
      n = smooth_n
    )
  }, error = function(e) {
    .gp3_gazer_error(conditionMessage(e))
  })

  if (!inherits(column_name_call, "gp3_gazer_error")) {
    return(column_name_call)
  }

  .gp3_gazer_error(
    paste(
      paste0("Vector-style gazeR call failed: ", vector_call$error),
      paste0("Column-name gazeR call failed: ", column_name_call$error),
      sep = " | "
    )
  )
}

.gp3_gazer_baseline_window_compat <- function(
    baseline_fun,
    processed_data,
    pupil_col,
    baseline_window,
    baseline_method
) {
  pupil_vec <- suppressWarnings(as.numeric(processed_data[[pupil_col]]))

  attempts <- list(
    function() baseline_fun(
      processed_data,
      pupil_colname = pupil_col,
      baseline_window = baseline_window,
      baseline_method = baseline_method
    ),
    function() baseline_fun(
      processed_data,
      pupil_colname = pupil_col,
      baseline_window = baseline_window
    ),
    function() baseline_fun(
      processed_data,
      pupil_colnames = pupil_col,
      baseline_window = baseline_window,
      baseline_method = baseline_method
    ),
    function() baseline_fun(
      processed_data,
      pupil_colnames = pupil_col,
      baseline_window = baseline_window
    ),
    function() baseline_fun(
      processed_data,
      pupil = pupil_col,
      baseline_window = baseline_window,
      baseline_method = baseline_method
    ),
    function() baseline_fun(
      processed_data,
      pupil = pupil_col,
      baseline_window = baseline_window
    ),
    function() baseline_fun(
      processed_data,
      pupil = pupil_vec,
      baseline_window = baseline_window,
      baseline_method = baseline_method
    ),
    function() baseline_fun(
      processed_data,
      pupil = pupil_vec,
      baseline_window = baseline_window
    ),
    function() baseline_fun(
      processed_data,
      pupil_col,
      baseline_window
    ),
    function() baseline_fun(
      processed_data,
      pupil_vec,
      baseline_window
    )
  )

  .gp3_gazer_run_attempts(
    attempts,
    label = "gazeR baseline-window call"
  )
}

.gp3_gazer_baseline_event_compat <- function(
    baseline_msg_fun,
    processed_data,
    pupil_col,
    baseline_event,
    baseline_dur,
    baseline_method
) {
  pupil_vec <- suppressWarnings(as.numeric(processed_data[[pupil_col]]))

  attempts <- list(
    function() baseline_msg_fun(
      processed_data,
      pupil_colname = pupil_col,
      baseline_dur = baseline_dur,
      event = baseline_event,
      baseline_method = baseline_method
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil_colname = pupil_col,
      baseline_dur = baseline_dur,
      event = baseline_event
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil_colnames = pupil_col,
      baseline_dur = baseline_dur,
      event = baseline_event,
      baseline_method = baseline_method
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil_colnames = pupil_col,
      baseline_dur = baseline_dur,
      event = baseline_event
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil = pupil_col,
      baseline_dur = baseline_dur,
      event = baseline_event,
      baseline_method = baseline_method
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil = pupil_col,
      baseline_dur = baseline_dur,
      event = baseline_event
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil = pupil_vec,
      baseline_dur = baseline_dur,
      event = baseline_event,
      baseline_method = baseline_method
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil = pupil_vec,
      baseline_dur = baseline_dur,
      event = baseline_event
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil_col,
      baseline_dur,
      baseline_event,
      baseline_method
    ),
    function() baseline_msg_fun(
      processed_data,
      pupil_vec,
      baseline_dur,
      baseline_event,
      baseline_method
    )
  )

  .gp3_gazer_run_attempts(
    attempts,
    label = "gazeR baseline-event call"
  )
}

.gp3_gazer_run_attempts <- function(attempts, label) {
  errors <- character(0)

  for (attempt in attempts) {
    result <- tryCatch({
      attempt()
    }, error = function(e) {
      .gp3_gazer_error(conditionMessage(e))
    })

    if (!inherits(result, "gp3_gazer_error")) {
      return(result)
    }

    errors <- c(errors, result$error)
  }

  errors <- unique(errors)
  errors <- errors[nzchar(errors)]

  .gp3_gazer_error(
    paste(
      paste0(label, " failed across compatible signatures."),
      paste(errors, collapse = " | "),
      sep = " "
    )
  )
}

.gp3_gazer_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_gazer_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_gazer_resolve_col(col, names_data, arg))
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

.gp3_gazer_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_gazer_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a finite positive number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_gazer_check_non_negative_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`", arg, "` must be a finite non-negative number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_gazer_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  if (x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_gazer_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}

.gp3_gazer_attach_dplyr_for_gazer <- function() {
  if (!"package:dplyr" %in% search()) {
    suppressPackageStartupMessages(
      base::library("dplyr", character.only = TRUE)
    )
  }

  invisible(TRUE)
}

.gp3_gazer_clean_error_message <- function(x) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.gp3_gazer_error <- function(message) {
  structure(
    list(error = .gp3_gazer_clean_error_message(message)),
    class = "gp3_gazer_error"
  )
}
