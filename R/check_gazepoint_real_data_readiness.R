#' Check real-data readiness before Gazepoint analysis
#'
#' Create an explicit final readiness gate for real Gazepoint or gp3tools master
#' data before confirmatory analysis. The gate checks required identifiers,
#' trial/time structure, analysis-specific columns, missingness, basic design
#' balance, and optional upstream audit objects.
#'
#' This helper is a final decision wrapper. It complements, but does not replace,
#' `validate_gazepoint_master()` or `create_gazepoint_analysis_decision_audit()`.
#'
#' @param data A Gazepoint or gp3tools data frame.
#' @param analysis_type Analysis target. Options are `"general"`, `"pupil"`,
#'   `"aoi"`, and `"combined"`.
#' @param participant_col Optional participant column. If `NULL`, common names
#'   are detected.
#' @param trial_col Optional trial column. If `NULL`, common names are detected.
#' @param time_col Optional time column. If `NULL`, common names are detected.
#' @param condition_col Optional condition/group column. If `NULL`, common names
#'   are detected when present.
#' @param stimulus_col Optional stimulus/media column. If `NULL`, common names
#'   are detected when present.
#' @param aoi_col Optional AOI/state column. If `NULL`, common names are detected
#'   when present.
#' @param pupil_col Optional pupil column. If `NULL`, common names are detected
#'   when present.
#' @param gaze_x_col Optional horizontal gaze coordinate column.
#' @param gaze_y_col Optional vertical gaze coordinate column.
#' @param tracking_valid_col Optional tracking-validity column.
#' @param required_cols Additional required columns.
#' @param audit_objects Optional list of upstream audit/validation objects.
#' @param min_rows Minimum acceptable number of rows.
#' @param min_participants Minimum acceptable number of participants.
#' @param min_trials Minimum acceptable number of participant-trial units.
#' @param max_missing_pupil_prop Maximum acceptable pupil missingness proportion
#'   when pupil data are required.
#' @param max_missing_gaze_prop Maximum acceptable gaze-coordinate missingness
#'   proportion when gaze coordinate columns are present.
#' @param max_condition_imbalance_ratio Warning threshold for condition imbalance.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_real_data_readiness_gate`.
#' @export
check_gazepoint_real_data_readiness <- function(
    data,
    analysis_type = c("general", "pupil", "aoi", "combined"),
    participant_col = NULL,
    trial_col = NULL,
    time_col = NULL,
    condition_col = NULL,
    stimulus_col = NULL,
    aoi_col = NULL,
    pupil_col = NULL,
    gaze_x_col = NULL,
    gaze_y_col = NULL,
    tracking_valid_col = NULL,
    required_cols = NULL,
    audit_objects = NULL,
    min_rows = 1L,
    min_participants = 1L,
    min_trials = 1L,
    max_missing_pupil_prop = 0.40,
    max_missing_gaze_prop = 0.40,
    max_condition_imbalance_ratio = 3,
    name = "gazepoint_real_data_readiness_gate"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  analysis_type <- match.arg(analysis_type)

  .gp3_rdr_check_label(name, "name")
  .gp3_rdr_check_positive_integer(min_rows, "min_rows")
  .gp3_rdr_check_positive_integer(min_participants, "min_participants")
  .gp3_rdr_check_positive_integer(min_trials, "min_trials")
  .gp3_rdr_check_prop(max_missing_pupil_prop, "max_missing_pupil_prop")
  .gp3_rdr_check_prop(max_missing_gaze_prop, "max_missing_gaze_prop")
  .gp3_rdr_check_positive_number(max_condition_imbalance_ratio, "max_condition_imbalance_ratio")

  names_data <- names(data)

  required_cols <- .gp3_rdr_check_required_cols(required_cols)

  participant_col <- .gp3_rdr_resolve_or_detect_col(
    col = participant_col,
    names_data = names_data,
    arg = "participant_col",
    candidates = c(
      "subject",
      "participant",
      "participant_id",
      "pID",
      "USER_FILE",
      "user",
      "recording_id"
    )
  )

  trial_col <- .gp3_rdr_resolve_or_detect_col(
    col = trial_col,
    names_data = names_data,
    arg = "trial_col",
    candidates = c(
      "trial_global",
      "trial",
      "trial_id",
      "TRIAL_INDEX",
      "item_id",
      "media_id",
      "MEDIA_ID"
    )
  )

  time_col <- .gp3_rdr_resolve_or_detect_col(
    col = time_col,
    names_data = names_data,
    arg = "time_col",
    candidates = c(
      "time",
      "time_ms",
      "timestamp",
      "TIMESTAMP",
      "TIME",
      "sample_index",
      "CNT"
    )
  )

  condition_col <- .gp3_rdr_resolve_or_detect_col(
    col = condition_col,
    names_data = names_data,
    arg = "condition_col",
    candidates = c(
      "condition",
      "CONDITION",
      "group",
      "GROUP",
      "trial_type"
    )
  )

  stimulus_col <- .gp3_rdr_resolve_or_detect_col(
    col = stimulus_col,
    names_data = names_data,
    arg = "stimulus_col",
    candidates = c(
      "stimulus",
      "stimulus_id",
      "stimulus_file",
      "image_file",
      "image",
      "media",
      "media_id",
      "MEDIA_ID"
    )
  )

  aoi_col <- .gp3_rdr_resolve_or_detect_col(
    col = aoi_col,
    names_data = names_data,
    arg = "aoi_col",
    candidates = c(
      "aoi",
      "AOI",
      "aoi_label",
      "AOI_LABEL",
      "aoi_name",
      "AOI_NAME",
      "CURRENT_FIX_INTEREST_AREA_LABEL"
    )
  )

  pupil_col <- .gp3_rdr_resolve_or_detect_col(
    col = pupil_col,
    names_data = names_data,
    arg = "pupil_col",
    candidates = c(
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
    )
  )

  gaze_x_col <- .gp3_rdr_resolve_or_detect_col(
    col = gaze_x_col,
    names_data = names_data,
    arg = "gaze_x_col",
    candidates = c(
      "gaze_x",
      "x",
      "X",
      "FPOGX",
      "LPOGX",
      "RPOGX",
      "POGX"
    )
  )

  gaze_y_col <- .gp3_rdr_resolve_or_detect_col(
    col = gaze_y_col,
    names_data = names_data,
    arg = "gaze_y_col",
    candidates = c(
      "gaze_y",
      "y",
      "Y",
      "FPOGY",
      "LPOGY",
      "RPOGY",
      "POGY"
    )
  )

  tracking_valid_col <- .gp3_rdr_resolve_or_detect_col(
    col = tracking_valid_col,
    names_data = names_data,
    arg = "tracking_valid_col",
    candidates = c(
      "tracking_valid",
      "valid_gaze",
      "is_valid",
      "valid",
      "FPOGV",
      "LPOGV",
      "RPOGV",
      "POGV"
    )
  )

  detected_columns <- tibble::tibble(
    role = c(
      "participant_col",
      "trial_col",
      "time_col",
      "condition_col",
      "stimulus_col",
      "aoi_col",
      "pupil_col",
      "gaze_x_col",
      "gaze_y_col",
      "tracking_valid_col"
    ),
    column = c(
      .gp3_rdr_collapse_nullable(participant_col),
      .gp3_rdr_collapse_nullable(trial_col),
      .gp3_rdr_collapse_nullable(time_col),
      .gp3_rdr_collapse_nullable(condition_col),
      .gp3_rdr_collapse_nullable(stimulus_col),
      .gp3_rdr_collapse_nullable(aoi_col),
      .gp3_rdr_collapse_nullable(pupil_col),
      .gp3_rdr_collapse_nullable(gaze_x_col),
      .gp3_rdr_collapse_nullable(gaze_y_col),
      .gp3_rdr_collapse_nullable(tracking_valid_col)
    ),
    detected = !is.na(c(
      .gp3_rdr_collapse_nullable(participant_col),
      .gp3_rdr_collapse_nullable(trial_col),
      .gp3_rdr_collapse_nullable(time_col),
      .gp3_rdr_collapse_nullable(condition_col),
      .gp3_rdr_collapse_nullable(stimulus_col),
      .gp3_rdr_collapse_nullable(aoi_col),
      .gp3_rdr_collapse_nullable(pupil_col),
      .gp3_rdr_collapse_nullable(gaze_x_col),
      .gp3_rdr_collapse_nullable(gaze_y_col),
      .gp3_rdr_collapse_nullable(tracking_valid_col)
    ))
  )

  checks <- list()

  checks <- .gp3_rdr_add_check(
    checks,
    check_id = "data_non_empty",
    check_area = "structure",
    status = if (nrow(data) > 0L) "pass" else "fail",
    severity = "blocking",
    message = if (nrow(data) > 0L) {
      "Data contain at least one row."
    } else {
      "Data contain no rows."
    },
    observed = nrow(data),
    threshold = 1
  )

  checks <- .gp3_rdr_add_check(
    checks,
    check_id = "minimum_rows",
    check_area = "structure",
    status = if (nrow(data) >= min_rows) "pass" else "fail",
    severity = "blocking",
    message = paste0("Rows available: ", nrow(data), ". Minimum required: ", min_rows, "."),
    observed = nrow(data),
    threshold = min_rows
  )

  required_main <- .gp3_rdr_required_roles(analysis_type)

  role_values <- list(
    participant_col = participant_col,
    trial_col = trial_col,
    time_col = time_col,
    aoi_col = aoi_col,
    pupil_col = pupil_col
  )

  for (role in names(required_main)) {
    role_required <- required_main[[role]]
    role_col <- role_values[[role]]

    checks <- .gp3_rdr_add_check(
      checks,
      check_id = paste0("required_", role),
      check_area = "required_columns",
      status = if (!is.null(role_col)) "pass" else if (isTRUE(role_required)) "fail" else "info",
      severity = if (isTRUE(role_required)) "blocking" else "informational",
      message = if (!is.null(role_col)) {
        paste0("Detected required role `", role, "` as column `", role_col, "`.")
      } else if (isTRUE(role_required)) {
        paste0("Required role `", role, "` was not detected.")
      } else {
        paste0("Optional role `", role, "` was not detected.")
      },
      observed = if (!is.null(role_col)) 1 else 0,
      threshold = if (isTRUE(role_required)) 1 else 0
    )
  }

  missing_required_cols <- setdiff(required_cols, names_data)

  checks <- .gp3_rdr_add_check(
    checks,
    check_id = "user_required_columns",
    check_area = "required_columns",
    status = if (length(missing_required_cols) == 0L) "pass" else "fail",
    severity = "blocking",
    message = if (length(missing_required_cols) == 0L) {
      "All user-specified required columns are present."
    } else {
      paste("Missing user-specified required columns:", paste(missing_required_cols, collapse = ", "))
    },
    observed = length(required_cols) - length(missing_required_cols),
    threshold = length(required_cols)
  )

  n_participants <- .gp3_rdr_n_distinct_col(data, participant_col)
  n_trials <- .gp3_rdr_n_trial_units(data, participant_col, trial_col)
  n_conditions <- .gp3_rdr_n_distinct_col(data, condition_col)
  n_stimuli <- .gp3_rdr_n_distinct_col(data, stimulus_col)

  checks <- .gp3_rdr_add_check(
    checks,
    check_id = "minimum_participants",
    check_area = "sample_structure",
    status = if (!is.na(n_participants) && n_participants >= min_participants) "pass" else "fail",
    severity = "blocking",
    message = paste0("Participants available: ", .gp3_rdr_format_missing_number(n_participants),
                     ". Minimum required: ", min_participants, "."),
    observed = n_participants,
    threshold = min_participants
  )

  checks <- .gp3_rdr_add_check(
    checks,
    check_id = "minimum_trials",
    check_area = "sample_structure",
    status = if (!is.na(n_trials) && n_trials >= min_trials) "pass" else "fail",
    severity = "blocking",
    message = paste0("Participant-trial units available: ", .gp3_rdr_format_missing_number(n_trials),
                     ". Minimum required: ", min_trials, "."),
    observed = n_trials,
    threshold = min_trials
  )

  if (!is.null(time_col)) {
    time_values <- suppressWarnings(as.numeric(data[[time_col]]))
    prop_bad_time <- mean(is.na(time_values) | !is.finite(time_values))

    checks <- .gp3_rdr_add_check(
      checks,
      check_id = "finite_time_values",
      check_area = "time_structure",
      status = if (prop_bad_time == 0) "pass" else "fail",
      severity = "blocking",
      message = paste0("Proportion of missing/non-finite time values: ", round(prop_bad_time, 4), "."),
      observed = prop_bad_time,
      threshold = 0
    )

    if (!is.null(participant_col) && !is.null(trial_col)) {
      duplicate_prop <- .gp3_rdr_duplicate_time_prop(
        data = data,
        participant_col = participant_col,
        trial_col = trial_col,
        time_col = time_col
      )

      checks <- .gp3_rdr_add_check(
        checks,
        check_id = "duplicate_participant_trial_time",
        check_area = "time_structure",
        status = if (duplicate_prop == 0) "pass" else "warn",
        severity = "warning",
        message = paste0("Proportion of duplicated participant-trial-time keys: ", round(duplicate_prop, 4), "."),
        observed = duplicate_prop,
        threshold = 0
      )
    }
  }

  if (!is.null(pupil_col)) {
    pupil_values <- suppressWarnings(as.numeric(data[[pupil_col]]))
    prop_missing_pupil <- mean(is.na(pupil_values) | !is.finite(pupil_values))
    pupil_required <- analysis_type %in% c("pupil", "combined")

    checks <- .gp3_rdr_add_check(
      checks,
      check_id = "pupil_missingness",
      check_area = "signal_quality",
      status = if (prop_missing_pupil <= max_missing_pupil_prop) {
        "pass"
      } else if (pupil_required) {
        "fail"
      } else {
        "warn"
      },
      severity = if (pupil_required) "blocking" else "warning",
      message = paste0(
        "Pupil missingness/non-finite proportion: ",
        round(prop_missing_pupil, 4),
        ". Threshold: ",
        max_missing_pupil_prop,
        "."
      ),
      observed = prop_missing_pupil,
      threshold = max_missing_pupil_prop
    )
  }

  if (!is.null(gaze_x_col) && !is.null(gaze_y_col)) {
    gaze_x <- suppressWarnings(as.numeric(data[[gaze_x_col]]))
    gaze_y <- suppressWarnings(as.numeric(data[[gaze_y_col]]))
    prop_missing_gaze <- mean(
      is.na(gaze_x) | !is.finite(gaze_x) |
        is.na(gaze_y) | !is.finite(gaze_y)
    )

    checks <- .gp3_rdr_add_check(
      checks,
      check_id = "gaze_coordinate_missingness",
      check_area = "signal_quality",
      status = if (prop_missing_gaze <= max_missing_gaze_prop) "pass" else "warn",
      severity = "warning",
      message = paste0(
        "Gaze-coordinate missingness/non-finite proportion: ",
        round(prop_missing_gaze, 4),
        ". Threshold: ",
        max_missing_gaze_prop,
        "."
      ),
      observed = prop_missing_gaze,
      threshold = max_missing_gaze_prop
    )
  } else if (!is.null(gaze_x_col) || !is.null(gaze_y_col)) {
    checks <- .gp3_rdr_add_check(
      checks,
      check_id = "paired_gaze_coordinates",
      check_area = "signal_quality",
      status = "warn",
      severity = "warning",
      message = "Only one gaze-coordinate column was detected; both x and y are preferable for gaze-quality checks.",
      observed = 1,
      threshold = 2
    )
  }

  if (!is.null(tracking_valid_col)) {
    valid_values <- .gp3_rdr_as_logical_valid(data[[tracking_valid_col]])
    prop_invalid <- mean(!valid_values, na.rm = TRUE)

    checks <- .gp3_rdr_add_check(
      checks,
      check_id = "tracking_validity",
      check_area = "signal_quality",
      status = if (is.na(prop_invalid) || prop_invalid <= max_missing_gaze_prop) "pass" else "warn",
      severity = "warning",
      message = paste0(
        "Tracking-invalid proportion: ",
        .gp3_rdr_format_missing_number(round(prop_invalid, 4)),
        ". Threshold: ",
        max_missing_gaze_prop,
        "."
      ),
      observed = prop_invalid,
      threshold = max_missing_gaze_prop
    )
  }

  if (!is.null(condition_col)) {
    condition_summary <- .gp3_rdr_condition_summary(data, condition_col)

    if (nrow(condition_summary) <= 1L) {
      checks <- .gp3_rdr_add_check(
        checks,
        check_id = "condition_count",
        check_area = "design_balance",
        status = "warn",
        severity = "warning",
        message = "Only one condition/group was detected.",
        observed = nrow(condition_summary),
        threshold = 2
      )
    } else {
      imbalance_ratio <- max(condition_summary$n_rows) / min(condition_summary$n_rows)

      checks <- .gp3_rdr_add_check(
        checks,
        check_id = "condition_imbalance",
        check_area = "design_balance",
        status = if (imbalance_ratio <= max_condition_imbalance_ratio) "pass" else "warn",
        severity = "warning",
        message = paste0(
          "Condition row-count imbalance ratio: ",
          round(imbalance_ratio, 4),
          ". Threshold: ",
          max_condition_imbalance_ratio,
          "."
        ),
        observed = imbalance_ratio,
        threshold = max_condition_imbalance_ratio
      )
    }
  } else {
    condition_summary <- tibble::tibble(
      condition = character(0),
      n_rows = integer(0),
      proportion = numeric(0)
    )

    checks <- .gp3_rdr_add_check(
      checks,
      check_id = "condition_detected",
      check_area = "design_balance",
      status = "info",
      severity = "informational",
      message = "No condition/group column was detected.",
      observed = 0,
      threshold = 0
    )
  }

  audit_checks <- .gp3_rdr_audit_object_checks(audit_objects)

  checks_tbl <- dplyr::bind_rows(checks, audit_checks)

  checks_tbl <- checks_tbl |>
    dplyr::mutate(
      status = factor(
        .data$status,
        levels = c("fail", "warn", "pass", "info"),
        ordered = TRUE
      )
    ) |>
    dplyr::arrange(.data$status, .data$check_area, .data$check_id) |>
    dplyr::mutate(status = as.character(.data$status))

  n_fail <- sum(checks_tbl$status == "fail")
  n_warn <- sum(checks_tbl$status == "warn")
  n_pass <- sum(checks_tbl$status == "pass")
  n_info <- sum(checks_tbl$status == "info")

  readiness_status <- if (n_fail > 0L) {
    "fail"
  } else if (n_warn > 0L) {
    "warn"
  } else {
    "pass"
  }

  gate_decision <- tibble::tibble(
    object_name = name,
    analysis_type = analysis_type,
    readiness_status = readiness_status,
    ready_for_real_data_analysis = n_fail == 0L,
    n_fail = n_fail,
    n_warn = n_warn,
    n_pass = n_pass,
    n_info = n_info,
    decision_message = .gp3_rdr_decision_message(readiness_status, n_fail, n_warn)
  )

  data_summary <- tibble::tibble(
    object_name = name,
    n_rows = nrow(data),
    n_columns = ncol(data),
    n_participants = n_participants,
    n_trial_units = n_trials,
    n_conditions = n_conditions,
    n_stimuli = n_stimuli,
    analysis_type = analysis_type
  )

  overview <- tibble::tibble(
    object_name = name,
    analysis_type = analysis_type,
    readiness_status = readiness_status,
    ready_for_real_data_analysis = n_fail == 0L,
    n_rows = nrow(data),
    n_participants = n_participants,
    n_trial_units = n_trials,
    n_fail = n_fail,
    n_warn = n_warn,
    n_pass = n_pass,
    n_info = n_info
  )

  settings <- tibble::tibble(
    setting = c(
      "analysis_type",
      "participant_col",
      "trial_col",
      "time_col",
      "condition_col",
      "stimulus_col",
      "aoi_col",
      "pupil_col",
      "gaze_x_col",
      "gaze_y_col",
      "tracking_valid_col",
      "required_cols",
      "min_rows",
      "min_participants",
      "min_trials",
      "max_missing_pupil_prop",
      "max_missing_gaze_prop",
      "max_condition_imbalance_ratio",
      "name"
    ),
    value = c(
      analysis_type,
      .gp3_rdr_collapse_nullable(participant_col),
      .gp3_rdr_collapse_nullable(trial_col),
      .gp3_rdr_collapse_nullable(time_col),
      .gp3_rdr_collapse_nullable(condition_col),
      .gp3_rdr_collapse_nullable(stimulus_col),
      .gp3_rdr_collapse_nullable(aoi_col),
      .gp3_rdr_collapse_nullable(pupil_col),
      .gp3_rdr_collapse_nullable(gaze_x_col),
      .gp3_rdr_collapse_nullable(gaze_y_col),
      .gp3_rdr_collapse_nullable(tracking_valid_col),
      .gp3_rdr_collapse_nullable(required_cols),
      as.character(min_rows),
      as.character(min_participants),
      as.character(min_trials),
      as.character(max_missing_pupil_prop),
      as.character(max_missing_gaze_prop),
      as.character(max_condition_imbalance_ratio),
      name
    )
  )

  out <- list(
    overview = overview,
    gate_decision = gate_decision,
    checks = checks_tbl,
    detected_columns = detected_columns,
    data_summary = data_summary,
    condition_summary = condition_summary,
    settings = settings
  )

  class(out) <- c("gp3_real_data_readiness_gate", "list")

  out
}

.gp3_rdr_required_roles <- function(analysis_type) {
  switch(
    analysis_type,
    general = list(
      participant_col = TRUE,
      trial_col = TRUE,
      time_col = FALSE,
      aoi_col = FALSE,
      pupil_col = FALSE
    ),
    pupil = list(
      participant_col = TRUE,
      trial_col = TRUE,
      time_col = TRUE,
      aoi_col = FALSE,
      pupil_col = TRUE
    ),
    aoi = list(
      participant_col = TRUE,
      trial_col = TRUE,
      time_col = FALSE,
      aoi_col = TRUE,
      pupil_col = FALSE
    ),
    combined = list(
      participant_col = TRUE,
      trial_col = TRUE,
      time_col = TRUE,
      aoi_col = TRUE,
      pupil_col = TRUE
    )
  )
}

.gp3_rdr_add_check <- function(
    checks,
    check_id,
    check_area,
    status,
    severity,
    message,
    observed,
    threshold
) {
  checks[[length(checks) + 1L]] <- tibble::tibble(
    check_id = check_id,
    check_area = check_area,
    status = status,
    severity = severity,
    message = message,
    observed = suppressWarnings(as.numeric(observed)),
    threshold = suppressWarnings(as.numeric(threshold))
  )

  checks
}

.gp3_rdr_decision_message <- function(readiness_status, n_fail, n_warn) {
  if (identical(readiness_status, "fail")) {
    return(paste0(
      "Not ready for real-data analysis: ",
      n_fail,
      " blocking issue(s) must be resolved."
    ))
  }

  if (identical(readiness_status, "warn")) {
    return(paste0(
      "Conditionally ready: no blocking issues, but ",
      n_warn,
      " warning-level issue(s) should be reviewed."
    ))
  }

  "Ready for real-data analysis: no blocking or warning-level issues detected."
}

.gp3_rdr_audit_object_checks <- function(audit_objects) {
  if (is.null(audit_objects)) {
    return(tibble::tibble(
      check_id = character(0),
      check_area = character(0),
      status = character(0),
      severity = character(0),
      message = character(0),
      observed = numeric(0),
      threshold = numeric(0)
    ))
  }

  if (is.data.frame(audit_objects)) {
    audit_objects <- list(audit_objects)
  }

  if (!is.list(audit_objects)) {
    stop("`audit_objects` must be NULL, a data frame, or a list.", call. = FALSE)
  }

  out <- vector("list", length(audit_objects))

  for (i in seq_along(audit_objects)) {
    out[[i]] <- .gp3_rdr_single_audit_check(audit_objects[[i]], i)
  }

  dplyr::bind_rows(out)
}

.gp3_rdr_single_audit_check <- function(audit_object, index) {
  source_name <- paste0("audit_object_", index)

  if (is.list(audit_object) && !is.null(audit_object$overview) && is.data.frame(audit_object$overview)) {
    overview <- audit_object$overview
    source_name <- class(audit_object)[[1]]
  } else if (is.data.frame(audit_object)) {
    overview <- audit_object
  } else {
    return(tibble::tibble(
      check_id = paste0("audit_object_", index),
      check_area = "upstream_audits",
      status = "info",
      severity = "informational",
      message = paste0("Audit object ", index, " could not be interpreted as a data-frame overview."),
      observed = NA_real_,
      threshold = NA_real_
    ))
  }

  status_cols <- grep(
    "status|decision|ready",
    names(overview),
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(status_cols) == 0L || nrow(overview) == 0L) {
    return(tibble::tibble(
      check_id = paste0("audit_object_", index),
      check_area = "upstream_audits",
      status = "info",
      severity = "informational",
      message = paste0("Audit object `", source_name, "` has no interpretable status columns."),
      observed = NA_real_,
      threshold = NA_real_
    ))
  }

  status_values <- unlist(overview[status_cols], use.names = FALSE)
  status_values <- as.character(status_values)
  status_values <- status_values[!is.na(status_values)]

  has_fail <- any(grepl(
    "fail|error|invalid|not_ready|not ready|blocked|missing_required|stop",
    status_values,
    ignore.case = TRUE
  ))

  has_warn <- any(grepl(
    "warn|review|partial|caution|skipped|singular|imbalance|unavailable",
    status_values,
    ignore.case = TRUE
  ))

  has_pass <- any(grepl(
    "pass|ready|complete|ok|valid",
    status_values,
    ignore.case = TRUE
  ))

  status <- if (has_fail) {
    "fail"
  } else if (has_warn) {
    "warn"
  } else if (has_pass) {
    "pass"
  } else {
    "info"
  }

  tibble::tibble(
    check_id = paste0("audit_object_", index),
    check_area = "upstream_audits",
    status = status,
    severity = if (identical(status, "fail")) {
      "blocking"
    } else if (identical(status, "warn")) {
      "warning"
    } else {
      "informational"
    },
    message = paste0(
      "Upstream audit `",
      source_name,
      "` interpreted as status `",
      status,
      "`."
    ),
    observed = NA_real_,
    threshold = NA_real_
  )
}

.gp3_rdr_condition_summary <- function(data, condition_col) {
  out <- data |>
    dplyr::mutate(.gp3_condition = as.character(.data[[condition_col]])) |>
    dplyr::filter(!is.na(.data$.gp3_condition), nzchar(.data$.gp3_condition)) |>
    dplyr::count(.data$.gp3_condition, name = "n_rows") |>
    dplyr::mutate(
      proportion = .data$n_rows / sum(.data$n_rows)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_rows))

  names(out)[names(out) == ".gp3_condition"] <- "condition"

  out
}

.gp3_rdr_duplicate_time_prop <- function(data, participant_col, trial_col, time_col) {
  key_data <- tibble::tibble(
    participant = as.character(data[[participant_col]]),
    trial = as.character(data[[trial_col]]),
    time = as.character(data[[time_col]])
  )

  n_duplicate_rows <- sum(duplicated(key_data))

  n_duplicate_rows / nrow(key_data)
}

.gp3_rdr_n_distinct_col <- function(data, col) {
  if (is.null(col)) {
    return(NA_integer_)
  }

  values <- data[[col]]
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return(0L)
  }

  dplyr::n_distinct(values)
}

.gp3_rdr_n_trial_units <- function(data, participant_col, trial_col) {
  if (is.null(participant_col) || is.null(trial_col)) {
    return(NA_integer_)
  }

  key <- paste(
    as.character(data[[participant_col]]),
    as.character(data[[trial_col]]),
    sep = "||"
  )

  dplyr::n_distinct(key[!is.na(key)])
}

.gp3_rdr_as_logical_valid <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    return(!is.na(x) & x > 0)
  }

  x_chr <- tolower(trimws(as.character(x)))

  x_chr %in% c("true", "t", "yes", "y", "1", "valid", "ok")
}

.gp3_rdr_check_required_cols <- function(required_cols) {
  if (is.null(required_cols)) {
    return(character(0))
  }

  if (!is.character(required_cols) || anyNA(required_cols)) {
    stop("`required_cols` must be a character vector.", call. = FALSE)
  }

  required_cols[nzchar(required_cols)]
}

.gp3_rdr_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_rdr_resolve_or_detect_col <- function(col, names_data, arg, candidates) {
  if (!is.null(col)) {
    return(.gp3_rdr_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  NULL
}

.gp3_rdr_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_rdr_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  if (x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_rdr_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a finite positive number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_rdr_check_prop <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x > 1) {
    stop("`", arg, "` must be a finite number between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_rdr_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}

.gp3_rdr_format_missing_number <- function(x) {
  if (length(x) == 0L || is.na(x)) {
    return("NA")
  }

  as.character(x)
}
