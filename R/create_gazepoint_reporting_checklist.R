#' Create a Gazepoint reporting checklist
#'
#' Create an auto-generated reporting checklist for Gazepoint/gp3tools analyses.
#' The checklist summarises whether key dataset, preprocessing, quality-control,
#' AOI, pupil, modelling, sensitivity, and reproducibility elements are present
#' or still need reporting.
#'
#' This helper is intended as a reporting aid. It does not replace the underlying
#' audit, preprocessing, modelling, or readiness-gate functions.
#'
#' @param data Optional Gazepoint or gp3tools data frame.
#' @param objects Optional list of gp3tools audit, model, workflow, readiness, or
#'   external-check objects.
#' @param analysis_type Analysis target. Options are `"general"`, `"pupil"`,
#'   `"aoi"`, and `"combined"`.
#' @param study_title Optional study title or short label.
#' @param required_sections Optional character vector of checklist item IDs that
#'   should be treated as required. If `NULL`, a default set is used.
#' @param include_optional Logical. If `TRUE`, include optional advanced-methods
#'   reporting items.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_reporting_checklist`.
#' @export
create_gazepoint_reporting_checklist <- function(
    data = NULL,
    objects = NULL,
    analysis_type = c("general", "pupil", "aoi", "combined"),
    study_title = NULL,
    required_sections = NULL,
    include_optional = TRUE,
    name = "gazepoint_reporting_checklist"
) {
  analysis_type <- match.arg(analysis_type)

  .gp3_reporting_check_label(name, "name")
  .gp3_reporting_check_logical(include_optional, "include_optional")

  if (!is.null(study_title)) {
    .gp3_reporting_check_label(study_title, "study_title")
  }

  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be NULL or a data frame.", call. = FALSE)
  }

  objects <- .gp3_reporting_normalise_objects(objects)

  if (!is.null(required_sections)) {
    if (!is.character(required_sections) || anyNA(required_sections)) {
      stop("`required_sections` must be NULL or a character vector.", call. = FALSE)
    }

    required_sections <- unique(required_sections[nzchar(required_sections)])
  }

  object_summary <- .gp3_reporting_object_summary(objects)
  data_summary <- .gp3_reporting_data_summary(data, analysis_type, name)

  item_list <- list()

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "study_identification",
    item_id = "study_title",
    item = "Study title or short study label is available.",
    status = if (!is.null(study_title)) "pass" else "warn",
    evidence = if (!is.null(study_title)) study_title else "No study title supplied.",
    recommendation = if (!is.null(study_title)) {
      "Report the study title/label consistently across outputs."
    } else {
      "Supply `study_title` or report a clear study label in the manuscript/report."
    },
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "study_identification",
    item_id = "analysis_type",
    item = "Analysis type is declared.",
    status = "pass",
    evidence = analysis_type,
    recommendation = paste0("Report this as a ", analysis_type, " Gazepoint/gp3tools analysis."),
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "data_structure",
    item_id = "dataset_available",
    item = "Dataset/data frame was supplied to the checklist.",
    status = if (!is.null(data)) "pass" else "warn",
    evidence = if (!is.null(data)) {
      paste0(nrow(data), " rows and ", ncol(data), " columns.")
    } else {
      "No data frame supplied."
    },
    recommendation = if (!is.null(data)) {
      "Report the analytic row count and key data columns."
    } else {
      "Supply the final analysis data frame to document row/column structure."
    },
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "data_structure",
    item_id = "participant_trial_structure",
    item = "Participant and trial structure can be reported.",
    status = .gp3_reporting_participant_trial_status(data),
    evidence = .gp3_reporting_participant_trial_evidence(data),
    recommendation = "Report participant count, trial count, and participant-trial units.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "readiness_gate",
    item_id = "real_data_readiness_gate",
    item = "Explicit real-data readiness gate is available.",
    status = .gp3_reporting_class_status(
      object_summary,
      class_pattern = "gp3_real_data_readiness_gate",
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_class_evidence(
      object_summary,
      class_pattern = "gp3_real_data_readiness_gate",
      missing_evidence = "No gp3_real_data_readiness_gate object supplied."
    ),
    recommendation = "Use check_gazepoint_real_data_readiness() before final confirmatory analysis.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "workflow_and_import",
    item_id = "workflow_or_file_pair_check",
    item = "Import/workflow/file-pair checks are documented.",
    status = .gp3_reporting_any_class_status(
      object_summary,
      class_patterns = c(
        "gp3_workflow",
        "gp3_file_pair",
        "gp3_workflow_summary",
        "gp3_master_audit",
        "gp3_master_validation"
      ),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_class_evidence(
      object_summary,
      class_patterns = c(
        "gp3_workflow",
        "gp3_file_pair",
        "gp3_workflow_summary",
        "gp3_master_audit",
        "gp3_master_validation"
      ),
      missing_evidence = "No workflow, file-pair, master-audit, or master-validation object supplied."
    ),
    recommendation = "Report source files, import workflow, master-table construction, and validation checks.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "sampling_and_tracking",
    item_id = "sampling_rate_reported",
    item = "Sampling-rate checks are available or should be reported.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("sampling", "check_sampling_rate"),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("sampling", "check_sampling_rate"),
      missing_evidence = "No sampling-rate object detected."
    ),
    recommendation = "Report expected and observed sampling rate, dropped samples, and timing irregularities.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "sampling_and_tracking",
    item_id = "tracking_quality_reported",
    item = "Tracking/gaze-signal quality checks are available or should be reported.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("tracking_quality", "gaze_signal_quality", "condition_quality_imbalance"),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("tracking_quality", "gaze_signal_quality", "condition_quality_imbalance"),
      missing_evidence = "No tracking-quality or gaze-signal-quality object detected."
    ),
    recommendation = "Report missing gaze, valid tracking, exclusions, and condition-level quality imbalance.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "design_and_exclusions",
    item_id = "design_balance_reported",
    item = "Design balance or post-exclusion balance is documented.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("design_balance", "post_exclusion_balance", "condition_imbalance"),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("design_balance", "post_exclusion_balance", "condition_imbalance"),
      missing_evidence = "No design-balance or post-exclusion-balance object detected."
    ),
    recommendation = "Report condition/sample balance before and after exclusions.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "design_and_exclusions",
    item_id = "exclusion_flow_reported",
    item = "Exclusion flow is documented.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("exclusion_flow"),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("exclusion_flow"),
      missing_evidence = "No exclusion-flow object detected."
    ),
    recommendation = "Report row, trial, participant, and condition losses due to exclusions.",
    required = TRUE
  )

  if (analysis_type %in% c("aoi", "combined")) {
    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "aoi_reporting",
      item_id = "aoi_definition_and_geometry",
      item = "AOI definitions, geometry, overlap, or verification are documented.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("aoi_geometry", "aoi_overlap", "aoi_coding", "aoi_verification", "aoi_margin"),
        missing_status = "warn"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("aoi_geometry", "aoi_overlap", "aoi_coding", "aoi_verification", "aoi_margin"),
        missing_evidence = "No AOI geometry/coding/verification object detected."
      ),
      recommendation = "Report AOI definitions, coordinates, overlap checks, and verification procedure.",
      required = TRUE
    )

    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "aoi_reporting",
      item_id = "aoi_outcomes_reported",
      item = "AOI outcome summaries or AOI model objects are available.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("aoi_trial_features", "aoi_windows", "aoi_glmm", "aoi_gamm", "aoi_transition"),
        missing_status = "warn"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("aoi_trial_features", "aoi_windows", "aoi_glmm", "aoi_gamm", "aoi_transition"),
        missing_evidence = "No AOI outcome/model object detected."
      ),
      recommendation = "Report AOI metrics, denominators, model family, link function, and random effects.",
      required = TRUE
    )
  }

  if (analysis_type %in% c("pupil", "combined")) {
    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "pupil_reporting",
      item_id = "pupil_preprocessing_reported",
      item = "Pupil preprocessing decisions are documented.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("pupil_preprocessing", "preprocessing_registry", "pupil_artifacts", "interpolate_gazepoint_pupil", "smooth_gazepoint_pupil", "baseline_correct"),
        missing_status = "warn"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("pupil_preprocessing", "preprocessing_registry", "pupil_artifacts", "interpolate_gazepoint_pupil", "smooth_gazepoint_pupil", "baseline_correct"),
        missing_evidence = "No pupil preprocessing/audit object detected."
      ),
      recommendation = "Report artifact rules, interpolation method, baseline correction, smoothing, and retained samples.",
      required = TRUE
    )

    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "pupil_reporting",
      item_id = "pupil_quality_audits_reported",
      item = "Pupil quality audits are documented.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("pupil_gaps", "pupil_baseline", "pupil_imbalance", "pupil_drift", "pupil_overlap", "pupil_reliability"),
        missing_status = "warn"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("pupil_gaps", "pupil_baseline", "pupil_imbalance", "pupil_drift", "pupil_overlap", "pupil_reliability"),
        missing_evidence = "No pupil quality-audit object detected."
      ),
      recommendation = "Report gap structure, baseline quality, drift, imbalance, overlap risk, and reliability if relevant.",
      required = TRUE
    )

    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "pupil_reporting",
      item_id = "stimulus_luminance_reported",
      item = "Stimulus luminance/brightness audit is available or discussed.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("stimulus_luminance", "luminance"),
        missing_status = "warn"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("stimulus_luminance", "luminance"),
        missing_evidence = "No stimulus luminance audit detected."
      ),
      recommendation = "Report luminance/brightness control or acknowledge it as a limitation for pupil outcomes.",
      required = TRUE
    )
  }

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "models_and_diagnostics",
    item_id = "model_results_reported",
    item = "Model summaries or fixed-effect tables are available.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("model", "fixed_effects", "emmeans", "glmm", "gamm", "gca", "lmm"),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("model", "fixed_effects", "emmeans", "glmm", "gamm", "gca", "lmm"),
      missing_evidence = "No model summary/fixed-effect object detected."
    ),
    recommendation = "Report model formula, family/link, fixed effects, random effects, diagnostics, and inference criterion.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "models_and_diagnostics",
    item_id = "model_diagnostics_reported",
    item = "Model diagnostics are available or should be reported.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("diagnose", "convergence", "singularity", "overdispersion", "model_diagnostic"),
      missing_status = "warn"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("diagnose", "convergence", "singularity", "overdispersion", "model_diagnostic"),
      missing_evidence = "No model diagnostic object detected."
    ),
    recommendation = "Report convergence, singularity, overdispersion, residual, and sensitivity diagnostics where relevant.",
    required = TRUE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "sensitivity_and_robustness",
    item_id = "sensitivity_analyses_reported",
    item = "Sensitivity, multiverse, or optional external cross-checks are available.",
    status = .gp3_reporting_any_name_or_class_status(
      object_summary,
      patterns = c("sensitivity", "multiverse", "crosscheck", "pchip", "gazer", "eyetools", "transition_count_nb", "time_varying_transition"),
      missing_status = "info"
    ),
    evidence = .gp3_reporting_any_name_or_class_evidence(
      object_summary,
      patterns = c("sensitivity", "multiverse", "crosscheck", "pchip", "gazer", "eyetools", "transition_count_nb", "time_varying_transition"),
      missing_evidence = "No optional sensitivity/external-cross-check object detected."
    ),
    recommendation = "Report optional sensitivity checks when used; otherwise state that they were not part of the planned analysis.",
    required = FALSE
  )

  item_list <- .gp3_reporting_add_item(
    item_list,
    reporting_area = "reproducibility",
    item_id = "package_workflow_reported",
    item = "Package workflow and software environment can be reported.",
    status = "pass",
    evidence = paste0("Checklist generated by gp3tools object `", name, "`."),
    recommendation = "Report gp3tools version, R version, key optional packages, and analysis script availability.",
    required = TRUE
  )

  if (isTRUE(include_optional)) {
    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "advanced_optional_methods",
      item_id = "advanced_sequence_or_transition_methods",
      item = "Advanced sequence or transition-method reporting is available if used.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("markovchain", "semimarkov", "hmm", "transition_matrix", "time_varying_transition"),
        missing_status = "info"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("markovchain", "semimarkov", "hmm", "transition_matrix", "time_varying_transition"),
        missing_evidence = "No advanced sequence/transition object detected."
      ),
      recommendation = "If sequence models are used, report state definitions, transition denominators, and model assumptions.",
      required = FALSE
    )

    item_list <- .gp3_reporting_add_item(
      item_list,
      reporting_area = "advanced_optional_methods",
      item_id = "external_detector_or_adapter_reporting",
      item = "External detector/package-adapter reporting is available if used.",
      status = .gp3_reporting_any_name_or_class_status(
        object_summary,
        patterns = c("adapter", "eyetrackingr", "pupillometryr", "gazer", "eyetools", "external"),
        missing_status = "info"
      ),
      evidence = .gp3_reporting_any_name_or_class_evidence(
        object_summary,
        patterns = c("adapter", "eyetrackingr", "pupillometryr", "gazer", "eyetools", "external"),
        missing_evidence = "No external detector/adapter object detected."
      ),
      recommendation = "If external packages are used, report package names, versions, input mapping, and any skipped/partial branches.",
      required = FALSE
    )
  }

  checklist <- do.call(rbind, item_list)
  checklist <- tibble::as_tibble(checklist)

  if (!is.null(required_sections) && length(required_sections) > 0L) {
    checklist$required <- checklist$item_id %in% required_sections | checklist$required
  }

  checklist$required <- as.logical(checklist$required)
  checklist$status <- as.character(checklist$status)
  checklist$status <- ifelse(
    checklist$status %in% c("pass", "warn", "fail", "info"),
    checklist$status,
    "info"
  )

  checklist <- checklist[order(
    factor(checklist$status, levels = c("fail", "warn", "pass", "info")),
    checklist$reporting_area,
    checklist$item_id
  ), , drop = FALSE]

  n_fail <- sum(checklist$status == "fail")
  n_warn <- sum(checklist$status == "warn")
  n_pass <- sum(checklist$status == "pass")
  n_info <- sum(checklist$status == "info")

  checklist_status <- if (n_fail > 0L) {
    "fail"
  } else if (n_warn > 0L) {
    "warn"
  } else {
    "pass"
  }

  section_summary <- .gp3_reporting_section_summary(checklist)

  text_summary <- .gp3_reporting_text_summary(
    name = name,
    study_title = study_title,
    analysis_type = analysis_type,
    checklist_status = checklist_status,
    n_fail = n_fail,
    n_warn = n_warn,
    n_pass = n_pass,
    n_info = n_info
  )

  overview <- tibble::tibble(
    object_name = name,
    study_title = .gp3_reporting_collapse_nullable(study_title),
    analysis_type = analysis_type,
    checklist_status = checklist_status,
    ready_for_reporting = n_fail == 0L,
    n_items = nrow(checklist),
    n_required_items = sum(checklist$required),
    n_fail = n_fail,
    n_warn = n_warn,
    n_pass = n_pass,
    n_info = n_info,
    n_objects_supplied = nrow(object_summary)
  )

  settings <- tibble::tibble(
    setting = c(
      "analysis_type",
      "study_title",
      "required_sections",
      "include_optional",
      "name"
    ),
    value = c(
      analysis_type,
      .gp3_reporting_collapse_nullable(study_title),
      .gp3_reporting_collapse_nullable(required_sections),
      as.character(include_optional),
      name
    )
  )

  out <- list(
    overview = overview,
    checklist = checklist,
    section_summary = section_summary,
    object_summary = object_summary,
    data_summary = data_summary,
    text_summary = text_summary,
    settings = settings
  )

  class(out) <- c("gp3_reporting_checklist", "list")

  out
}

.gp3_reporting_add_item <- function(
    items,
    reporting_area,
    item_id,
    item,
    status,
    evidence,
    recommendation,
    required
) {
  items[[length(items) + 1L]] <- data.frame(
    reporting_area = reporting_area,
    item_id = item_id,
    item = item,
    status = status,
    evidence = evidence,
    recommendation = recommendation,
    required = required,
    stringsAsFactors = FALSE
  )

  items
}

.gp3_reporting_normalise_objects <- function(objects) {
  if (is.null(objects)) {
    return(list())
  }

  if (is.data.frame(objects)) {
    return(list(object_1 = objects))
  }

  if (!is.list(objects)) {
    stop("`objects` must be NULL, a data frame, or a list.", call. = FALSE)
  }

  if (length(objects) == 0L) {
    return(list())
  }

  if (is.null(names(objects))) {
    names(objects) <- paste0("object_", seq_along(objects))
  } else {
    missing_names <- !nzchar(names(objects)) | is.na(names(objects))
    names(objects)[missing_names] <- paste0("object_", which(missing_names))
  }

  objects
}

.gp3_reporting_object_summary <- function(objects) {
  if (length(objects) == 0L) {
    return(tibble::tibble(
      object_label = character(0),
      class = character(0),
      object_name = character(0),
      status = character(0),
      message = character(0),
      n_rows_overview = integer(0)
    ))
  }

  rows <- lapply(seq_along(objects), function(i) {
    object <- objects[[i]]
    overview <- .gp3_reporting_extract_overview(object)

    object_name <- .gp3_reporting_extract_object_name(overview, names(objects)[[i]])
    status <- .gp3_reporting_extract_status(overview)
    message <- .gp3_reporting_extract_message(overview)
    first_class <- class(object)[[1]]

    tibble::tibble(
      object_label = names(objects)[[i]],
      class = first_class,
      object_name = object_name,
      status = status,
      message = message,
      n_rows_overview = if (is.null(overview)) 0L else nrow(overview)
    )
  })

  do.call(rbind, rows)
}

.gp3_reporting_extract_overview <- function(object) {
  if (is.data.frame(object)) {
    return(object)
  }

  if (
    is.list(object) &&
    "overview" %in% names(object) &&
    is.data.frame(object[["overview"]])
  ) {
    return(object[["overview"]])
  }

  NULL
}

.gp3_reporting_extract_object_name <- function(overview, fallback) {
  if (!is.null(overview) && "object_name" %in% names(overview) && nrow(overview) > 0L) {
    value <- as.character(overview$object_name[[1]])
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }

  fallback
}

.gp3_reporting_extract_status <- function(overview) {
  if (is.null(overview) || nrow(overview) == 0L) {
    return("info")
  }

  status_cols <- grep(
    "status|decision|ready|complete|valid",
    names(overview),
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(status_cols) == 0L) {
    return("info")
  }

  values <- unlist(overview[status_cols], use.names = FALSE)
  values <- as.character(values)
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return("info")
  }

  .gp3_reporting_status_from_values(values)
}

.gp3_reporting_extract_message <- function(overview) {
  if (is.null(overview) || nrow(overview) == 0L) {
    return(NA_character_)
  }

  message_cols <- grep(
    "message|note|reason|comment",
    names(overview),
    ignore.case = TRUE,
    value = TRUE
  )

  if (length(message_cols) == 0L) {
    return(NA_character_)
  }

  values <- unlist(overview[message_cols], use.names = FALSE)
  values <- as.character(values)
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return(NA_character_)
  }

  paste(unique(values), collapse = " | ")
}

.gp3_reporting_status_from_values <- function(values) {
  values <- tolower(paste(values, collapse = " "))

  if (grepl("fail|error|invalid|not_ready|not ready|blocked|missing_required", values)) {
    return("fail")
  }

  if (grepl("warn|review|partial|caution|skipped|singular|imbalance|unavailable", values)) {
    return("warn")
  }

  if (grepl("pass|ready|complete|ok|valid|true", values)) {
    return("pass")
  }

  "info"
}

.gp3_reporting_data_summary <- function(data, analysis_type, name) {
  if (is.null(data)) {
    return(tibble::tibble(
      object_name = name,
      analysis_type = analysis_type,
      n_rows = NA_integer_,
      n_columns = NA_integer_,
      participant_col = NA_character_,
      trial_col = NA_character_,
      time_col = NA_character_,
      condition_col = NA_character_,
      pupil_col = NA_character_,
      aoi_col = NA_character_,
      n_participants = NA_integer_,
      n_trial_units = NA_integer_
    ))
  }

  names_data <- names(data)

  participant_col <- .gp3_reporting_detect_col(
    names_data,
    c("subject", "participant", "participant_id", "pID", "USER_FILE", "user", "recording_id")
  )

  trial_col <- .gp3_reporting_detect_col(
    names_data,
    c("trial_global", "trial", "trial_id", "TRIAL_INDEX", "item_id", "media_id", "MEDIA_ID")
  )

  time_col <- .gp3_reporting_detect_col(
    names_data,
    c("time", "time_ms", "timestamp", "TIMESTAMP", "TIME", "sample_index", "CNT")
  )

  condition_col <- .gp3_reporting_detect_col(
    names_data,
    c("condition", "CONDITION", "group", "GROUP", "trial_type")
  )

  pupil_col <- .gp3_reporting_detect_col(
    names_data,
    c("pupil_bc_processed", "pupil_smoothed", "pupil_interpolated", "pupil_clean", "pupil", "LPD", "RPD")
  )

  aoi_col <- .gp3_reporting_detect_col(
    names_data,
    c("aoi", "AOI", "aoi_label", "AOI_LABEL", "aoi_name", "AOI_NAME", "CURRENT_FIX_INTEREST_AREA_LABEL")
  )

  tibble::tibble(
    object_name = name,
    analysis_type = analysis_type,
    n_rows = nrow(data),
    n_columns = ncol(data),
    participant_col = .gp3_reporting_collapse_nullable(participant_col),
    trial_col = .gp3_reporting_collapse_nullable(trial_col),
    time_col = .gp3_reporting_collapse_nullable(time_col),
    condition_col = .gp3_reporting_collapse_nullable(condition_col),
    pupil_col = .gp3_reporting_collapse_nullable(pupil_col),
    aoi_col = .gp3_reporting_collapse_nullable(aoi_col),
    n_participants = .gp3_reporting_n_distinct_col(data, participant_col),
    n_trial_units = .gp3_reporting_n_trial_units(data, participant_col, trial_col)
  )
}

.gp3_reporting_participant_trial_status <- function(data) {
  if (is.null(data)) {
    return("warn")
  }

  summary <- .gp3_reporting_data_summary(data, "general", "tmp")

  if (!is.na(summary$n_participants[[1]]) && !is.na(summary$n_trial_units[[1]])) {
    return("pass")
  }

  "warn"
}

.gp3_reporting_participant_trial_evidence <- function(data) {
  if (is.null(data)) {
    return("No data supplied.")
  }

  summary <- .gp3_reporting_data_summary(data, "general", "tmp")

  paste0(
    "Participants: ",
    .gp3_reporting_format_number(summary$n_participants[[1]]),
    "; participant-trial units: ",
    .gp3_reporting_format_number(summary$n_trial_units[[1]]),
    "."
  )
}

.gp3_reporting_class_status <- function(object_summary, class_pattern, missing_status) {
  if (nrow(object_summary) == 0L) {
    return(missing_status)
  }

  rows <- grepl(class_pattern, object_summary$class, ignore.case = TRUE)

  if (!any(rows)) {
    return(missing_status)
  }

  .gp3_reporting_status_from_values(object_summary$status[rows])
}

.gp3_reporting_class_evidence <- function(object_summary, class_pattern, missing_evidence) {
  if (nrow(object_summary) == 0L) {
    return(missing_evidence)
  }

  rows <- grepl(class_pattern, object_summary$class, ignore.case = TRUE)

  if (!any(rows)) {
    return(missing_evidence)
  }

  paste(
    paste0(
      object_summary$object_label[rows],
      " [",
      object_summary$class[rows],
      ": ",
      object_summary$status[rows],
      "]"
    ),
    collapse = "; "
  )
}

.gp3_reporting_any_class_status <- function(object_summary, class_patterns, missing_status) {
  if (nrow(object_summary) == 0L) {
    return(missing_status)
  }

  pattern <- paste(class_patterns, collapse = "|")
  rows <- grepl(pattern, object_summary$class, ignore.case = TRUE)

  if (!any(rows)) {
    return(missing_status)
  }

  .gp3_reporting_status_from_values(object_summary$status[rows])
}

.gp3_reporting_any_class_evidence <- function(object_summary, class_patterns, missing_evidence) {
  if (nrow(object_summary) == 0L) {
    return(missing_evidence)
  }

  pattern <- paste(class_patterns, collapse = "|")
  rows <- grepl(pattern, object_summary$class, ignore.case = TRUE)

  if (!any(rows)) {
    return(missing_evidence)
  }

  paste(
    paste0(
      object_summary$object_label[rows],
      " [",
      object_summary$class[rows],
      ": ",
      object_summary$status[rows],
      "]"
    ),
    collapse = "; "
  )
}

.gp3_reporting_any_name_or_class_status <- function(object_summary, patterns, missing_status) {
  if (nrow(object_summary) == 0L) {
    return(missing_status)
  }

  pattern <- paste(patterns, collapse = "|")
  haystack <- paste(
    object_summary$object_label,
    object_summary$class,
    object_summary$object_name,
    object_summary$message
  )

  rows <- grepl(pattern, haystack, ignore.case = TRUE)

  if (!any(rows)) {
    return(missing_status)
  }

  .gp3_reporting_status_from_values(object_summary$status[rows])
}

.gp3_reporting_any_name_or_class_evidence <- function(object_summary, patterns, missing_evidence) {
  if (nrow(object_summary) == 0L) {
    return(missing_evidence)
  }

  pattern <- paste(patterns, collapse = "|")
  haystack <- paste(
    object_summary$object_label,
    object_summary$class,
    object_summary$object_name,
    object_summary$message
  )

  rows <- grepl(pattern, haystack, ignore.case = TRUE)

  if (!any(rows)) {
    return(missing_evidence)
  }

  paste(
    paste0(
      object_summary$object_label[rows],
      " [",
      object_summary$class[rows],
      ": ",
      object_summary$status[rows],
      "]"
    ),
    collapse = "; "
  )
}

.gp3_reporting_section_summary <- function(checklist) {
  split_items <- split(checklist, checklist$reporting_area)

  rows <- lapply(names(split_items), function(area) {
    x <- split_items[[area]]

    tibble::tibble(
      reporting_area = area,
      n_items = nrow(x),
      n_required = sum(x$required),
      n_fail = sum(x$status == "fail"),
      n_warn = sum(x$status == "warn"),
      n_pass = sum(x$status == "pass"),
      n_info = sum(x$status == "info"),
      area_status = if (any(x$status == "fail")) {
        "fail"
      } else if (any(x$status == "warn")) {
        "warn"
      } else {
        "pass"
      }
    )
  })

  out <- do.call(rbind, rows)
  out[order(out$reporting_area), , drop = FALSE]
}

.gp3_reporting_text_summary <- function(
    name,
    study_title,
    analysis_type,
    checklist_status,
    n_fail,
    n_warn,
    n_pass,
    n_info
) {
  title_text <- if (is.null(study_title)) {
    "Untitled Gazepoint study"
  } else {
    study_title
  }

  decision <- if (identical(checklist_status, "fail")) {
    paste0(
      "Reporting checklist is incomplete: ",
      n_fail,
      " blocking reporting item(s) require attention."
    )
  } else if (identical(checklist_status, "warn")) {
    paste0(
      "Reporting checklist is conditionally complete: no blocking reporting failures, but ",
      n_warn,
      " warning-level item(s) should be reviewed."
    )
  } else {
    "Reporting checklist is complete: no blocking or warning-level reporting items were detected."
  }

  tibble::tibble(
    object_name = name,
    study_title = title_text,
    analysis_type = analysis_type,
    checklist_status = checklist_status,
    text = paste0(
      title_text,
      " was checked as a ",
      analysis_type,
      " Gazepoint/gp3tools analysis. ",
      decision,
      " Item counts: ",
      n_pass,
      " pass, ",
      n_warn,
      " warn, ",
      n_fail,
      " fail, ",
      n_info,
      " info."
    )
  )
}

.gp3_reporting_detect_col <- function(names_data, candidates) {
  found <- candidates[candidates %in% names_data]

  if (length(found) == 0L) {
    return(NULL)
  }

  found[[1]]
}

.gp3_reporting_n_distinct_col <- function(data, col) {
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

.gp3_reporting_n_trial_units <- function(data, participant_col, trial_col) {
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

.gp3_reporting_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_reporting_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_reporting_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}

.gp3_reporting_format_number <- function(x) {
  if (length(x) == 0L || is.na(x)) {
    return("NA")
  }

  as.character(x)
}
