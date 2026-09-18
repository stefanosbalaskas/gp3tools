.gp3_require_eyeprocess_survival <- function() {
  if (!requireNamespace("eyeprocess", quietly = TRUE)) {
    stop(
      "eyeprocess is required for Gazepoint gaze-survival analysis. Install a version that exports the gaze-survival API.",
      call. = FALSE
    )
  }
  required <- c(
    "prepare_gaze_survival_data", "validate_gaze_survival_data",
    "summarise_gaze_censoring", "estimate_gaze_survival",
    "fit_gaze_mixed_cox_model", "fit_gaze_aft_model",
    "check_gaze_proportional_hazards", "compare_gaze_survival_models",
    "report_gaze_survival_model"
  )
  missing <- required[
    !vapply(
      required,
      exists,
      logical(1),
      envir = asNamespace("eyeprocess"),
      inherits = FALSE
    )
  ]
  if (length(missing)) {
    stop(
      "The installed eyeprocess version does not provide the required gaze-survival API: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Prepare Gazepoint gaze-survival data via eyeprocess
#'
#' A thin Gazepoint convenience adapter. Scientific event/censor construction,
#' validation, provenance, and survival semantics are implemented by
#' eyeprocess. This wrapper does not infer censoring from a missing Gazepoint
#' field.
#'
#' @param trials Trial-window table accepted by eyeprocess.
#' @param events Fixation/AOI-visit/event table accepted by eyeprocess.
#' @param ... Arguments forwarded unchanged to
#'   eyeprocess::prepare_gaze_survival_data().
#'
#' @return The canonical survival-ready data frame returned by eyeprocess.
#' @export
prepare_gazepoint_survival_data <- function(trials, events = NULL, ...) {
  .gp3_require_eyeprocess_survival()
  eyeprocess::prepare_gaze_survival_data(
    trials = trials,
    events = events,
    ...
  )
}

#' Run a Gazepoint gaze-latency survival workflow
#'
#' Runs a deliberately thin end-to-end convenience workflow through the
#' vendor-neutral eyeprocess survival engine. The Cox repeated-observation
#' structure and AFT family must both be named explicitly; gp3tools never
#' chooses either estimator for the analyst.
#'
#' @param trials Trial-window table accepted by eyeprocess.
#' @param events Fixation/AOI-visit/event table accepted by eyeprocess.
#' @param target_aoi Target AOI identifier.
#' @param formula Model right-hand-side formula/string accepted by eyeprocess.
#' @param participant_col Participant identifier column.
#' @param cox_structure Required: "cluster_robust" or "frailty".
#' @param aft_distribution Required: "weibull" or "lognormal".
#' @param km_group Optional grouping column for descriptive Kaplan-Meier curves.
#' @param ... Preparation arguments forwarded unchanged to
#'   eyeprocess::prepare_gaze_survival_data().
#'
#' @return A gazepoint_survival_analysis list containing the delegated
#'   canonical data, validation, censoring summary, Kaplan-Meier estimate,
#'   Cox and AFT fits, diagnostics, comparison table, reporting bundle, and
#'   explicit analysis specification.
#' @export
run_gazepoint_latency_analysis <- function(
    trials,
    events = NULL,
    target_aoi,
    formula,
    participant_col = "participant_id",
    cox_structure = NULL,
    aft_distribution = NULL,
    km_group = NULL,
    ...) {
  if (is.null(cox_structure)) {
    stop(
      "cox_structure must be specified explicitly as 'cluster_robust' or 'frailty'.",
      call. = FALSE
    )
  }
  cox_structure <- match.arg(
    cox_structure,
    c("cluster_robust", "frailty")
  )

  if (is.null(aft_distribution)) {
    stop(
      "aft_distribution must be specified explicitly as 'weibull' or 'lognormal'.",
      call. = FALSE
    )
  }
  aft_distribution <- match.arg(
    aft_distribution,
    c("weibull", "lognormal")
  )
  .gp3_require_eyeprocess_survival()

  data <- eyeprocess::prepare_gaze_survival_data(
    trials = trials,
    events = events,
    target_aoi = target_aoi,
    ...
  )
  validation <- eyeprocess::validate_gaze_survival_data(
    data,
    raise_on_error = FALSE
  )
  censoring <- eyeprocess::summarise_gaze_censoring(data)
  kaplan_meier <- eyeprocess::estimate_gaze_survival(
    data,
    group = km_group
  )
  cox <- eyeprocess::fit_gaze_mixed_cox_model(
    data,
    formula = formula,
    participant_col = participant_col,
    structure = cox_structure
  )
  aft <- eyeprocess::fit_gaze_aft_model(
    data,
    formula = formula,
    distribution = aft_distribution
  )

  ph_diagnostics <- if (identical(cox$backend, "coxme::coxme")) {
    data.frame(
      status = "not_available_for_frailty_backend",
      guidance = paste(
        "Run and report the corresponding marginal Cox model",
        "for survival::cox.zph diagnostics."
      ),
      stringsAsFactors = FALSE
    )
  } else {
    eyeprocess::check_gaze_proportional_hazards(cox)
  }

  comparison <- eyeprocess::compare_gaze_survival_models(cox, aft)
  out <- list(
    data = data,
    validation = validation,
    censoring = censoring,
    kaplan_meier = kaplan_meier,
    cox = cox,
    aft = aft,
    ph_diagnostics = ph_diagnostics,
    comparison = comparison,
    report = eyeprocess::report_gaze_survival_model(cox),
    specification = list(
      target_aoi = target_aoi,
      formula = formula,
      participant_col = participant_col,
      cox_structure = cox_structure,
      aft_distribution = aft_distribution,
      km_group = km_group,
      engine = "eyeprocess"
    )
  )
  class(out) <- c("gazepoint_survival_analysis", "list")
  out
}
