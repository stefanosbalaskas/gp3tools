#' Fit an AOI-window binomial GLMM
#'
#' Fit a confirmatory AOI-window mixed-effects logistic regression from data
#' prepared by `prepare_gazepoint_aoi_glmm_data()`.
#'
#' @param data AOI GLMM data returned by `prepare_gazepoint_aoi_glmm_data()`.
#' @param success_col Success-count column.
#' @param failure_col Failure-count column.
#' @param subject_col Subject factor/column.
#' @param condition_col Condition factor/column.
#' @param window_col AOI-window factor/column.
#' @param include_condition Logical. Include condition fixed effects when at
#'   least two conditions are available.
#' @param include_window Logical. Include window fixed effects when at least two
#'   windows are available.
#' @param include_interaction Logical. Include condition-by-window interaction
#'   when both condition and window fixed effects are included.
#' @param random_intercept Logical. Include subject random intercept.
#' @param random_window_slopes Logical. Attempt subject-level random slopes for
#'   AOI window.
#' @param fallback_on_singular Logical. If `TRUE`, fall back to a simpler random
#'   intercept model when a random-slope model is singular or fails.
#' @param optimizer Optimizer passed to `lme4::glmerControl()`.
#' @param maxfun Maximum optimizer evaluations.
#' @param nAGQ Number of adaptive Gauss-Hermite quadrature points.
#' @param drop_missing Logical. Drop rows with missing model variables before
#'   fitting.
#'
#' @return A list with fitted model, attempted model, formulas, comparison table,
#'   settings, status fields, and model data.
#'
#' @export
fit_gazepoint_aoi_window_glmm <- function(
    data,
    success_col = "aoi_glmm_success",
    failure_col = "aoi_glmm_failure",
    subject_col = "aoi_glmm_subject",
    condition_col = "aoi_glmm_condition",
    window_col = "aoi_glmm_window",
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = TRUE,
    random_intercept = TRUE,
    random_window_slopes = FALSE,
    fallback_on_singular = TRUE,
    optimizer = "bobyqa",
    maxfun = 2e5,
    nAGQ = 0,
    drop_missing = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop(
      "Package `lme4` is required to fit AOI-window GLMMs.",
      call. = FALSE
    )
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

  valid_logical <- function(x, arg) {
    if (!is.logical(x) ||
        length(x) != 1L ||
        is.na(x)) {
      stop(
        "`", arg, "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }

  valid_column(success_col, "success_col")
  valid_column(failure_col, "failure_col")
  valid_column(subject_col, "subject_col")
  valid_column(condition_col, "condition_col")
  valid_column(window_col, "window_col")

  valid_logical(include_condition, "include_condition")
  valid_logical(include_window, "include_window")
  valid_logical(include_interaction, "include_interaction")
  valid_logical(random_intercept, "random_intercept")
  valid_logical(random_window_slopes, "random_window_slopes")
  valid_logical(fallback_on_singular, "fallback_on_singular")
  valid_logical(drop_missing, "drop_missing")

  if (!is.character(optimizer) ||
      length(optimizer) != 1L ||
      is.na(optimizer) ||
      !nzchar(optimizer)) {
    stop(
      "`optimizer` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.numeric(maxfun) ||
      length(maxfun) != 1L ||
      is.na(maxfun) ||
      !is.finite(maxfun) ||
      maxfun <= 0) {
    stop(
      "`maxfun` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.numeric(nAGQ) ||
      length(nAGQ) != 1L ||
      is.na(nAGQ) ||
      !is.finite(nAGQ) ||
      nAGQ < 0) {
    stop(
      "`nAGQ` must be a non-negative finite numeric scalar.",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  required_cols <- c(
    success_col,
    failure_col,
    subject_col,
    condition_col,
    window_col
  )

  missing_cols <- setdiff(required_cols, names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat$.gp3_success <- suppressWarnings(as.numeric(dat[[success_col]]))
  dat$.gp3_failure <- suppressWarnings(as.numeric(dat[[failure_col]]))
  dat$.gp3_subject <- factor(dat[[subject_col]])
  dat$.gp3_condition <- factor(dat[[condition_col]])
  dat$.gp3_window <- factor(dat[[window_col]])

  model_vars <- c(
    ".gp3_success",
    ".gp3_failure",
    ".gp3_subject",
    ".gp3_condition",
    ".gp3_window"
  )

  if (drop_missing) {
    dat <- dat |>
      dplyr::filter(
        stats::complete.cases(dplyr::across(dplyr::all_of(model_vars)))
      )
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after removing missing AOI-window GLMM variables.",
      call. = FALSE
    )
  }

  invalid_counts <- !is.finite(dat$.gp3_success) |
    !is.finite(dat$.gp3_failure) |
    dat$.gp3_success < 0 |
    dat$.gp3_failure < 0

  if (any(invalid_counts)) {
    stop(
      "AOI-window GLMM success and failure counts must be finite and non-negative.",
      call. = FALSE
    )
  }

  if (dplyr::n_distinct(dat$.gp3_subject) < 2L) {
    stop(
      "At least two subjects are required to fit an AOI-window GLMM.",
      call. = FALSE
    )
  }

  n_conditions <- dplyr::n_distinct(dat$.gp3_condition)
  n_windows <- dplyr::n_distinct(dat$.gp3_window)

  use_condition <- include_condition && n_conditions > 1L
  use_window <- include_window && n_windows > 1L
  use_interaction <- include_interaction && use_condition && use_window

  fixed_terms <- character(0)

  if (use_condition && use_window && use_interaction) {
    fixed_terms <- ".gp3_condition * .gp3_window"
  } else {
    if (use_condition) {
      fixed_terms <- c(fixed_terms, ".gp3_condition")
    }

    if (use_window) {
      fixed_terms <- c(fixed_terms, ".gp3_window")
    }
  }

  if (length(fixed_terms) == 0L) {
    fixed_part <- "1"
  } else {
    fixed_part <- paste(fixed_terms, collapse = " + ")
  }

  if (random_intercept) {
    if (random_window_slopes && n_windows > 1L) {
      random_part <- "(1 + .gp3_window | .gp3_subject)"
      random_structure <- "random_intercept_and_window_slopes"
      fallback_random_part <- "(1 | .gp3_subject)"
      fallback_random_structure <- "random_intercept"
    } else {
      random_part <- "(1 | .gp3_subject)"
      random_structure <- "random_intercept"
      fallback_random_part <- NULL
      fallback_random_structure <- NULL
    }
  } else {
    random_part <- NULL
    random_structure <- "fixed_effects_only"
    fallback_random_part <- NULL
    fallback_random_structure <- NULL
  }

  rhs <- fixed_part

  if (!is.null(random_part)) {
    rhs <- paste(rhs, random_part, sep = " + ")
  }

  attempted_formula <- stats::as.formula(
    paste0("cbind(.gp3_success, .gp3_failure) ~ ", rhs)
  )

  fallback_formula <- NULL

  if (!is.null(fallback_random_part)) {
    fallback_formula <- stats::as.formula(
      paste0(
        "cbind(.gp3_success, .gp3_failure) ~ ",
        fixed_part,
        " + ",
        fallback_random_part
      )
    )
  }

  control <- lme4::glmerControl(
    optimizer = optimizer,
    optCtrl = list(maxfun = maxfun)
  )

  fit_glmer <- function(formula) {
    args <- list(
      formula = formula,
      data = dat,
      family = stats::binomial(),
      control = control,
      nAGQ = as.integer(nAGQ)
    )

    tryCatch(
      do.call(lme4::glmer, args),
      error = function(e) e
    )
  }

  attempted_model <- fit_glmer(attempted_formula)

  attempted_error <- inherits(attempted_model, "error")
  attempted_singular <- FALSE

  if (!attempted_error && inherits(attempted_model, "merMod")) {
    attempted_singular <- lme4::isSingular(attempted_model)
  }

  final_model <- attempted_model
  final_formula <- attempted_formula
  final_random_structure <- random_structure
  fallback_used <- FALSE
  model_status <- "ok"
  error_message <- NA_character_

  if (attempted_error) {
    error_message <- conditionMessage(attempted_model)
    model_status <- "fit_failed"

    if (!is.null(fallback_formula) && fallback_on_singular) {
      fallback_model <- fit_glmer(fallback_formula)

      if (!inherits(fallback_model, "error")) {
        final_model <- fallback_model
        final_formula <- fallback_formula
        final_random_structure <- fallback_random_structure
        fallback_used <- TRUE
        model_status <- "fallback_after_fit_failure"
        error_message <- NA_character_
      }
    }
  } else if (attempted_singular) {
    model_status <- "singular_fit"

    if (!is.null(fallback_formula) && fallback_on_singular) {
      fallback_model <- fit_glmer(fallback_formula)

      if (!inherits(fallback_model, "error")) {
        final_model <- fallback_model
        final_formula <- fallback_formula
        final_random_structure <- fallback_random_structure
        fallback_used <- TRUE
        model_status <- "fallback_after_singular_fit"
      } else {
        model_status <- "singular_random_slope_model_fallback_failed"
        error_message <- conditionMessage(fallback_model)
      }
    }
  }

  final_error <- inherits(final_model, "error")

  if (final_error) {
    final_model_for_output <- NULL
  } else {
    final_model_for_output <- final_model
  }

  singular_fit <- FALSE

  if (!final_error && inherits(final_model, "merMod")) {
    singular_fit <- lme4::isSingular(final_model)
  }

  model_comparison <- tibble::tibble(
    model = c("attempted_model", "final_model"),
    random_effect_structure = c(random_structure, final_random_structure),
    model_status = c(
      if (attempted_error) {
        "fit_failed"
      } else if (attempted_singular) {
        "singular_fit"
      } else {
        "ok"
      },
      model_status
    ),
    n_obs = c(
      nrow(dat),
      nrow(dat)
    ),
    AIC = c(
      if (attempted_error) NA_real_ else stats::AIC(attempted_model),
      if (final_error) NA_real_ else stats::AIC(final_model)
    ),
    BIC = c(
      if (attempted_error) NA_real_ else stats::BIC(attempted_model),
      if (final_error) NA_real_ else stats::BIC(final_model)
    ),
    logLik = c(
      if (attempted_error) NA_real_ else as.numeric(stats::logLik(attempted_model)),
      if (final_error) NA_real_ else as.numeric(stats::logLik(final_model))
    )
  )

  output <- list(
    model = final_model_for_output,
    attempted_model = if (attempted_error) NULL else attempted_model,
    formula = final_formula,
    attempted_formula = attempted_formula,
    fallback_formula = fallback_formula,
    data = dat,
    comparison = model_comparison,
    settings = list(
      success_col = success_col,
      failure_col = failure_col,
      subject_col = subject_col,
      condition_col = condition_col,
      window_col = window_col,
      include_condition = include_condition,
      include_window = include_window,
      include_interaction = include_interaction,
      random_intercept = random_intercept,
      random_window_slopes = random_window_slopes,
      fallback_on_singular = fallback_on_singular,
      optimizer = optimizer,
      maxfun = maxfun,
      nAGQ = nAGQ,
      drop_missing = drop_missing,
      n_conditions = n_conditions,
      n_windows = n_windows
    ),
    random_effect_structure = final_random_structure,
    attempted_random_effect_structure = random_structure,
    fallback_used = fallback_used,
    singular_fit = singular_fit,
    attempted_singular_fit = attempted_singular,
    model_status = model_status,
    error_message = error_message
  )

  class(output) <- c("gp3_aoi_window_glmm", "list")

  output
}
