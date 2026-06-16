#' Run sensitivity models for confirmatory pupil-window analyses
#'
#' Run a compact set of sensitivity models for confirmatory pupil-window
#' analyses. Supported model families are the main linear mixed model, a
#' weighted linear mixed model, a fixed-effects linear model, and a weighted
#' fixed-effects linear model. Weighted models use the prepared valid-sample
#' count column as weights by default.
#'
#' @param data Pupil-window model data, usually produced by
#'   `prepare_gazepoint_pupil_window_model_data()`.
#' @param outcome_col Outcome column.
#' @param subject_col Subject column.
#' @param condition_col Condition column.
#' @param window_col Window column.
#' @param weights_col Optional weights column.
#' @param model_types Character vector of model types to fit. Supported values
#'   are `"lmm"`, `"weighted_lmm"`, `"lm"`, and `"weighted_lm"`.
#' @param include_condition Logical. Include condition fixed effects when more
#'   than one condition level is available.
#' @param include_window Logical. Include window fixed effects when more than
#'   one window level is available.
#' @param include_interaction Logical. Include the condition-by-window
#'   interaction when both condition and window are used.
#' @param random_intercept Logical. Include a subject random intercept for LMM
#'   model types when feasible.
#' @param random_window_slopes Logical. Attempt subject-level random window
#'   slopes for LMM model types when feasible.
#' @param fallback_on_singular Logical. If `TRUE`, LMM model types may fall back
#'   from random-window-slope models to random-intercept models when needed.
#' @param REML Logical. Passed to `lme4::lmer()`.
#' @param optimizer Optimizer passed to `lme4::lmerControl()`.
#' @param maxfun Maximum optimizer iterations passed to `lme4::lmerControl()`.
#' @param drop_missing Logical. If `TRUE`, rows with missing or non-finite
#'   model inputs are removed before fitting.
#' @param ... Additional arguments passed to
#'   `fit_gazepoint_pupil_window_lmm()`.
#'
#' @return A list containing fitted models, formulas, fixed effects, a
#'   comparison table, settings, and model-status information.
#'
#' @export
fit_gazepoint_pupil_window_sensitivity <- function(
    data,
    outcome_col = "pupil_model_outcome",
    subject_col = "pupil_model_subject",
    condition_col = "pupil_model_condition",
    window_col = "pupil_model_window",
    weights_col = "pupil_model_weight",
    model_types = c("lmm", "weighted_lmm", "lm", "weighted_lm"),
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = TRUE,
    random_intercept = TRUE,
    random_window_slopes = FALSE,
    fallback_on_singular = TRUE,
    REML = FALSE,
    optimizer = "bobyqa",
    maxfun = 2e5,
    drop_missing = TRUE,
    ...
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_column <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_optional_column <- function(x, arg) {
    if (!is.null(x) &&
        (!is.character(x) ||
         length(x) != 1L ||
         is.na(x) ||
         !nzchar(x))) {
      stop(
        "`", arg, "` must be NULL or a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_logical <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  valid_column(outcome_col, "outcome_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_optional_column(window_col, "window_col")
  valid_optional_column(weights_col, "weights_col")

  valid_logical(include_condition, "include_condition")
  valid_logical(include_window, "include_window")
  valid_logical(include_interaction, "include_interaction")
  valid_logical(random_intercept, "random_intercept")
  valid_logical(random_window_slopes, "random_window_slopes")
  valid_logical(fallback_on_singular, "fallback_on_singular")
  valid_logical(REML, "REML")
  valid_logical(drop_missing, "drop_missing")

  if (!is.character(model_types) ||
      length(model_types) < 1L ||
      any(is.na(model_types)) ||
      any(!nzchar(model_types))) {
    stop(
      "`model_types` must be a non-empty character vector.",
      call. = FALSE
    )
  }

  allowed_model_types <- c("lmm", "weighted_lmm", "lm", "weighted_lm")
  bad_model_types <- setdiff(model_types, allowed_model_types)

  if (length(bad_model_types) > 0L) {
    stop(
      "Unsupported model type(s): ",
      paste(bad_model_types, collapse = ", "),
      ". Supported model types are: ",
      paste(allowed_model_types, collapse = ", "),
      call. = FALSE
    )
  }

  model_types <- unique(model_types)

  if (!is.character(optimizer) ||
      length(optimizer) != 1L ||
      is.na(optimizer) ||
      !nzchar(optimizer)) {
    stop("`optimizer` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!is.numeric(maxfun) ||
      length(maxfun) != 1L ||
      is.na(maxfun) ||
      !is.finite(maxfun) ||
      maxfun <= 0) {
    stop("`maxfun` must be a positive finite numeric scalar.", call. = FALSE)
  }

  fit_one_model <- function(model_type) {
    use_weights <- model_type %in% c("weighted_lmm", "weighted_lm")
    use_random_intercept <- model_type %in% c("lmm", "weighted_lmm") &&
      random_intercept

    fit <- tryCatch(
      fit_gazepoint_pupil_window_lmm(
        data = data,
        outcome_col = outcome_col,
        subject_col = subject_col,
        condition_col = condition_col,
        window_col = window_col,
        weights_col = weights_col,
        use_weights = use_weights,
        include_condition = include_condition,
        include_window = include_window,
        include_interaction = include_interaction,
        random_intercept = use_random_intercept,
        random_window_slopes = if (use_random_intercept) {
          random_window_slopes
        } else {
          FALSE
        },
        fallback_on_singular = fallback_on_singular,
        REML = REML,
        optimizer = optimizer,
        maxfun = maxfun,
        drop_missing = drop_missing,
        ...
      ),
      error = function(e) {
        list(
          model = NULL,
          attempted_model = NULL,
          formula = NULL,
          attempted_formula = NULL,
          fallback_formula = NULL,
          data = NULL,
          comparison = tibble::tibble(
            model = "attempted",
            formula = NA_character_,
            engine = NA_character_,
            model_status = "error",
            singular_fit = NA,
            n = NA_integer_,
            n_subjects = NA_integer_,
            n_conditions = NA_integer_,
            n_windows = NA_integer_,
            AIC = NA_real_,
            BIC = NA_real_,
            logLik = NA_real_,
            warnings = "",
            error_message = conditionMessage(e)
          ),
          fixed_effects = tibble::tibble(
            term = character(0),
            estimate = numeric(0),
            std_error = numeric(0),
            statistic = numeric(0),
            p_value = numeric(0)
          ),
          settings = list(),
          random_effect_structure = NA_character_,
          attempted_random_effect_structure = NA_character_,
          fallback_used = FALSE,
          singular_fit = NA,
          attempted_singular_fit = NA,
          model_status = "error",
          error_message = conditionMessage(e),
          warnings = character(0)
        )
      }
    )

    fit
  }

  fits <- stats::setNames(
    lapply(model_types, fit_one_model),
    model_types
  )

  models <- lapply(fits, function(x) x$model)
  formulas <- lapply(fits, function(x) x$formula)

  first_data <- NULL

  for (fit_name in names(fits)) {
    if (!is.null(fits[[fit_name]]$data)) {
      first_data <- fits[[fit_name]]$data
      break
    }
  }

  comparison <- dplyr::bind_rows(
    lapply(names(fits), function(model_type) {
      comp <- fits[[model_type]]$comparison

      comp$model_type <- model_type
      comp$final_model_status <- fits[[model_type]]$model_status
      comp$fallback_used <- fits[[model_type]]$fallback_used
      comp$random_effect_structure <- fits[[model_type]]$random_effect_structure

      comp[, c(
        "model_type",
        "model",
        "formula",
        "engine",
        "model_status",
        "final_model_status",
        "singular_fit",
        "fallback_used",
        "random_effect_structure",
        "n",
        "n_subjects",
        "n_conditions",
        "n_windows",
        "AIC",
        "BIC",
        "logLik",
        "warnings",
        "error_message"
      )]
    })
  )

  fixed_effects <- dplyr::bind_rows(
    lapply(names(fits), function(model_type) {
      fe <- fits[[model_type]]$fixed_effects

      if (nrow(fe) == 0L) {
        return(tibble::tibble(
          model_type = character(0),
          term = character(0),
          estimate = numeric(0),
          std_error = numeric(0),
          statistic = numeric(0),
          p_value = numeric(0)
        ))
      }

      fe$model_type <- model_type
      fe[, c(
        "model_type",
        "term",
        "estimate",
        "std_error",
        "statistic",
        "p_value"
      )]
    })
  )

  final_statuses <- vapply(
    fits,
    function(x) x$model_status,
    character(1)
  )

  error_messages <- vapply(
    fits,
    function(x) {
      if (is.null(x$error_message) || is.na(x$error_message)) {
        ""
      } else {
        x$error_message
      }
    },
    character(1)
  )

  model_status <- if (all(final_statuses == "error")) {
    "error"
  } else if (any(final_statuses == "error")) {
    "partial_error"
  } else if (any(final_statuses %in% c(
    "singular_fit",
    "fallback_singular_fit"
  ))) {
    "singular_fit"
  } else {
    "ok"
  }

  out <- list(
    models = models,
    fits = fits,
    formulas = formulas,
    data = first_data,
    comparison = comparison,
    fixed_effects = fixed_effects,
    settings = list(
      outcome_col = outcome_col,
      subject_col = subject_col,
      condition_col = condition_col,
      window_col = window_col,
      weights_col = weights_col,
      model_types = model_types,
      include_condition = include_condition,
      include_window = include_window,
      include_interaction = include_interaction,
      random_intercept = random_intercept,
      random_window_slopes = random_window_slopes,
      fallback_on_singular = fallback_on_singular,
      REML = REML,
      optimizer = optimizer,
      maxfun = maxfun,
      drop_missing = drop_missing
    ),
    model_status = model_status,
    model_status_by_type = final_statuses,
    error_message = paste(
      error_messages[nzchar(error_messages)],
      collapse = " | "
    )
  )

  class(out) <- c("gp3_pupil_window_sensitivity", class(out))

  out
}
