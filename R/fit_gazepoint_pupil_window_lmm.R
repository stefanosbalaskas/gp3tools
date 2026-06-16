#' Fit confirmatory pupil-window linear mixed models
#'
#' Fit the main confirmatory trial/window-level pupil model from data prepared
#' with `prepare_gazepoint_pupil_window_model_data()`. The default model is a
#' linear mixed model with pupil outcome as the continuous dependent variable,
#' condition and/or window fixed effects when available, and a subject random
#' intercept when feasible.
#'
#' @param data Pupil-window model data, usually produced by
#'   `prepare_gazepoint_pupil_window_model_data()`.
#' @param formula Optional model formula. If `NULL`, a formula is constructed
#'   automatically.
#' @param outcome_col Outcome column.
#' @param subject_col Subject column.
#' @param condition_col Condition column.
#' @param window_col Window column.
#' @param weights_col Optional weights column.
#' @param use_weights Logical. If `TRUE`, use `weights_col` as model weights.
#' @param include_condition Logical. Include condition fixed effects when more
#'   than one condition level is available.
#' @param include_window Logical. Include window fixed effects when more than
#'   one window level is available.
#' @param include_interaction Logical. Include the condition-by-window
#'   interaction when both condition and window are used.
#' @param random_intercept Logical. Include a subject random intercept when
#'   feasible.
#' @param random_window_slopes Logical. Attempt subject-level random window
#'   slopes when feasible.
#' @param fallback_on_singular Logical. If `TRUE`, fall back from a random-slope
#'   model to a random-intercept model when the attempted model is singular or
#'   fails.
#' @param REML Logical. Passed to `lme4::lmer()`.
#' @param optimizer Optimizer passed to `lme4::lmerControl()`.
#' @param maxfun Maximum optimizer iterations passed to `lme4::lmerControl()`.
#' @param drop_missing Logical. If `TRUE`, rows with missing or non-finite
#'   model inputs are removed before fitting.
#' @param ... Additional arguments passed to `lme4::lmer()` or `stats::lm()`.
#'
#' @return A list containing the fitted model, formula, attempted model,
#'   fallback information, fixed effects, comparison table, settings, and
#'   model diagnostics.
#'
#' @export
fit_gazepoint_pupil_window_lmm <- function(
    data,
    formula = NULL,
    outcome_col = "pupil_model_outcome",
    subject_col = "pupil_model_subject",
    condition_col = "pupil_model_condition",
    window_col = "pupil_model_window",
    weights_col = "pupil_model_weight",
    use_weights = FALSE,
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

  if (!is.null(formula) && !inherits(formula, "formula")) {
    stop("`formula` must be NULL or a formula.", call. = FALSE)
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

  valid_logical(use_weights, "use_weights")
  valid_logical(include_condition, "include_condition")
  valid_logical(include_window, "include_window")
  valid_logical(include_interaction, "include_interaction")
  valid_logical(random_intercept, "random_intercept")
  valid_logical(random_window_slopes, "random_window_slopes")
  valid_logical(fallback_on_singular, "fallback_on_singular")
  valid_logical(REML, "REML")
  valid_logical(drop_missing, "drop_missing")

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

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop(
      "Package `lme4` is required to fit pupil-window LMMs. ",
      "Install it with install.packages(\"lme4\").",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  required_cols <- c(outcome_col, subject_col)

  missing_cols <- setdiff(required_cols, names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (use_weights) {
    if (is.null(weights_col)) {
      stop(
        "`weights_col` must be provided when `use_weights = TRUE`.",
        call. = FALSE
      )
    }

    if (!weights_col %in% names(dat)) {
      stop(
        "Missing required weights column: ",
        weights_col,
        call. = FALSE
      )
    }
  }

  if (drop_missing && "pupil_model_status" %in% names(dat)) {
    dat <- dat[dat$pupil_model_status == "ok", , drop = FALSE]
  }

  dat$.gp3_outcome <- suppressWarnings(as.numeric(dat[[outcome_col]]))

  dat$.gp3_subject <- as.character(dat[[subject_col]])
  dat$.gp3_subject <- trimws(dat$.gp3_subject)
  dat$.gp3_subject[
    is.na(dat$.gp3_subject) |
      !nzchar(dat$.gp3_subject)
  ] <- NA_character_

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    dat$.gp3_condition <- as.character(dat[[condition_col]])
    dat$.gp3_condition <- trimws(dat$.gp3_condition)
    dat$.gp3_condition[
      is.na(dat$.gp3_condition) |
        !nzchar(dat$.gp3_condition)
    ] <- "all_data"
  } else {
    dat$.gp3_condition <- "all_data"
  }

  if (!is.null(window_col) && window_col %in% names(dat)) {
    if (is.factor(dat[[window_col]])) {
      window_levels <- levels(droplevels(dat[[window_col]]))
      dat$.gp3_window <- factor(
        as.character(dat[[window_col]]),
        levels = window_levels
      )
    } else {
      dat$.gp3_window <- as.character(dat[[window_col]])
      dat$.gp3_window <- trimws(dat$.gp3_window)
      dat$.gp3_window[
        is.na(dat$.gp3_window) |
          !nzchar(dat$.gp3_window)
      ] <- "all_windows"
      dat$.gp3_window <- factor(
        dat$.gp3_window,
        levels = unique(dat$.gp3_window)
      )
    }
  } else {
    dat$.gp3_window <- factor("all_windows")
  }

  dat$.gp3_subject <- factor(dat$.gp3_subject)
  dat$.gp3_condition <- factor(dat$.gp3_condition)

  if (use_weights) {
    dat$.gp3_weights <- suppressWarnings(as.numeric(dat[[weights_col]]))
  } else {
    dat$.gp3_weights <- NA_real_
  }

  valid_rows <- is.finite(dat$.gp3_outcome) &
    !is.na(dat$.gp3_subject) &
    !is.na(dat$.gp3_condition) &
    !is.na(dat$.gp3_window)

  if (use_weights) {
    valid_rows <- valid_rows &
      is.finite(dat$.gp3_weights) &
      dat$.gp3_weights > 0
  }

  if (drop_missing) {
    dat <- dat[valid_rows, , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop("No rows are available for pupil-window LMM fitting.", call. = FALSE)
  }

  dat$.gp3_subject <- droplevels(dat$.gp3_subject)
  dat$.gp3_condition <- droplevels(dat$.gp3_condition)
  dat$.gp3_window <- droplevels(dat$.gp3_window)

  n_subjects <- length(unique(stats::na.omit(dat$.gp3_subject)))
  n_conditions <- length(unique(stats::na.omit(dat$.gp3_condition)))
  n_windows <- length(unique(stats::na.omit(dat$.gp3_window)))

  condition_used <- include_condition && n_conditions > 1L
  window_used <- include_window && n_windows > 1L

  fixed_part <- "1"

  if (condition_used && window_used && include_interaction) {
    fixed_part <- ".gp3_condition * .gp3_window"
  } else if (condition_used && window_used) {
    fixed_part <- ".gp3_condition + .gp3_window"
  } else if (condition_used) {
    fixed_part <- ".gp3_condition"
  } else if (window_used) {
    fixed_part <- ".gp3_window"
  }

  can_use_random_intercept <- random_intercept && n_subjects > 1L
  can_use_random_window_slopes <- can_use_random_intercept &&
    random_window_slopes &&
    window_used

  make_formula <- function(use_random_slopes = FALSE) {
    random_part <- character(0)

    if (can_use_random_intercept) {
      if (use_random_slopes) {
        random_part <- "(1 + .gp3_window | .gp3_subject)"
      } else {
        random_part <- "(1 | .gp3_subject)"
      }
    }

    rhs <- paste(c(fixed_part, random_part), collapse = " + ")

    stats::as.formula(
      paste(".gp3_outcome ~", rhs),
      env = parent.frame()
    )
  }

  if (is.null(formula)) {
    attempted_formula <- make_formula(
      use_random_slopes = can_use_random_window_slopes
    )

    fallback_formula <- NULL

    if (can_use_random_window_slopes) {
      fallback_formula <- make_formula(use_random_slopes = FALSE)
    }
  } else {
    attempted_formula <- formula
    fallback_formula <- NULL
  }

  collapse_formula <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }

    paste(deparse(x), collapse = "")
  }

  has_random_effect_formula <- function(fm) {
    grepl("\\|", paste(deparse(fm), collapse = ""))
  }

  random_effect_label <- function(fm) {
    formula_text <- paste(deparse(fm), collapse = "")

    if (!has_random_effect_formula(fm)) {
      return("none")
    }

    matches <- regmatches(
      formula_text,
      gregexpr("\\([^()]*\\|[^()]*\\)", formula_text)
    )[[1L]]

    if (length(matches) == 0L || identical(matches, character(0))) {
      return("random_effect")
    }

    matches <- gsub("^\\(|\\)$", "", matches)

    paste(matches, collapse = " + ")
  }

  safe_metric <- function(model, fun) {
    if (is.null(model)) {
      return(NA_real_)
    }

    out <- tryCatch(
      suppressWarnings(fun(model)),
      error = function(e) NA_real_
    )

    as.numeric(out)[1L]
  }

  fit_once <- function(fm) {
    warnings <- character(0)
    error_message <- NA_character_
    has_random_effect <- has_random_effect_formula(fm)
    dots <- list(...)

    model <- tryCatch(
      withCallingHandlers(
        {
          if (has_random_effect) {
            args <- list(
              formula = fm,
              data = dat,
              REML = REML,
              control = lme4::lmerControl(
                optimizer = optimizer,
                optCtrl = list(maxfun = maxfun)
              )
            )

            if (use_weights) {
              args$weights <- dat$.gp3_weights
            }

            args <- c(args, dots)

            do.call(lme4::lmer, args)
          } else {
            args <- list(
              formula = fm,
              data = dat
            )

            if (use_weights) {
              args$weights <- dat$.gp3_weights
            }

            args <- c(args, dots)

            do.call(stats::lm, args)
          }
        },
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    )

    singular_fit <- NA

    if (!is.null(model) && inherits(model, "merMod")) {
      singular_fit <- lme4::isSingular(model)
    }

    list(
      model = model,
      formula = fm,
      engine = if (has_random_effect) "lme4::lmer" else "stats::lm",
      singular_fit = singular_fit,
      warnings = unique(warnings),
      error_message = error_message,
      status = if (is.null(model)) "error" else "ok"
    )
  }

  attempted_fit <- fit_once(attempted_formula)
  fallback_fit <- NULL
  fallback_used <- FALSE

  should_try_fallback <- !is.null(fallback_formula) &&
    fallback_on_singular &&
    (
      is.null(attempted_fit$model) ||
        isTRUE(attempted_fit$singular_fit)
    )

  if (should_try_fallback) {
    fallback_fit <- fit_once(fallback_formula)

    if (!is.null(fallback_fit$model)) {
      fallback_used <- TRUE
    }
  }

  final_fit <- attempted_fit

  if (fallback_used) {
    final_fit <- fallback_fit
  }

  extract_fixed_effects <- function(model) {
    empty_fixed <- tibble::tibble(
      term = character(0),
      estimate = numeric(0),
      std_error = numeric(0),
      statistic = numeric(0),
      p_value = numeric(0)
    )

    if (is.null(model)) {
      return(empty_fixed)
    }

    coef_mat <- tryCatch(
      {
        sm <- summary(model)

        if (!is.null(sm$coefficients)) {
          sm$coefficients
        } else {
          stats::coef(sm)
        }
      },
      error = function(e) NULL
    )

    if (is.null(coef_mat)) {
      return(empty_fixed)
    }

    coef_df <- as.data.frame(coef_mat)

    statistic_col <- intersect(
      c("t value", "z value"),
      names(coef_df)
    )

    p_col <- intersect(
      c("Pr(>|t|)", "Pr(>|z|)"),
      names(coef_df)
    )

    tibble::tibble(
      term = rownames(coef_df),
      estimate = as.numeric(coef_df[["Estimate"]]),
      std_error = as.numeric(coef_df[["Std. Error"]]),
      statistic = if (length(statistic_col) > 0L) {
        as.numeric(coef_df[[statistic_col[[1L]]]])
      } else {
        rep(NA_real_, nrow(coef_df))
      },
      p_value = if (length(p_col) > 0L) {
        as.numeric(coef_df[[p_col[[1L]]]])
      } else {
        rep(NA_real_, nrow(coef_df))
      }
    )
  }

  comparison_row <- function(label, fit) {
    tibble::tibble(
      model = label,
      formula = collapse_formula(fit$formula),
      engine = fit$engine,
      model_status = fit$status,
      singular_fit = fit$singular_fit,
      n = nrow(dat),
      n_subjects = n_subjects,
      n_conditions = n_conditions,
      n_windows = n_windows,
      AIC = safe_metric(fit$model, stats::AIC),
      BIC = safe_metric(fit$model, stats::BIC),
      logLik = safe_metric(fit$model, stats::logLik),
      warnings = paste(fit$warnings, collapse = " | "),
      error_message = fit$error_message
    )
  }

  comparison <- comparison_row("attempted", attempted_fit)

  if (!is.null(fallback_fit)) {
    comparison <- dplyr::bind_rows(
      comparison,
      comparison_row("fallback", fallback_fit)
    )
  }

  model_status <- if (is.null(final_fit$model)) {
    "error"
  } else if (fallback_used && isTRUE(final_fit$singular_fit)) {
    "fallback_singular_fit"
  } else if (fallback_used) {
    "fallback_ok"
  } else if (isTRUE(final_fit$singular_fit)) {
    "singular_fit"
  } else {
    "ok"
  }

  out <- list(
    model = final_fit$model,
    attempted_model = attempted_fit$model,
    formula = final_fit$formula,
    attempted_formula = attempted_formula,
    fallback_formula = fallback_formula,
    data = dat,
    comparison = comparison,
    fixed_effects = extract_fixed_effects(final_fit$model),
    settings = list(
      outcome_col = outcome_col,
      subject_col = subject_col,
      condition_col = condition_col,
      window_col = window_col,
      weights_col = weights_col,
      use_weights = use_weights,
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
    random_effect_structure = random_effect_label(final_fit$formula),
    attempted_random_effect_structure = random_effect_label(attempted_formula),
    fallback_used = fallback_used,
    singular_fit = final_fit$singular_fit,
    attempted_singular_fit = attempted_fit$singular_fit,
    model_status = model_status,
    error_message = final_fit$error_message,
    warnings = final_fit$warnings
  )

  class(out) <- c("gp3_pupil_window_lmm", class(out))

  out
}
