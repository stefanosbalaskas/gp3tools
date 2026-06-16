#' Fit a Gazepoint Growth Curve Analysis mixed model
#'
#' Fit a Growth Curve Analysis (GCA) mixed model to prepared pupil time-course
#' data. The function first attempts a random-intercept plus random-time-slopes
#' model and, if the model fails or is singular, falls back to a random-intercept
#' model.
#'
#' @param data A data frame created by `prepare_gazepoint_gca_data()`.
#' @param outcome_col Name of the GCA outcome column.
#' @param subject_col Name of the subject column.
#' @param condition_col Name of the condition column.
#' @param time_terms Optional character vector of polynomial time-term columns.
#'   If `NULL`, terms named `time_poly_1`, `time_poly_2`, ... are detected.
#' @param degree Optional number of polynomial terms to use. If supplied and
#'   `time_terms = NULL`, the function uses `time_poly_1` through
#'   `time_poly_degree`.
#' @param weights_col Optional weights column. Use `NULL` for unweighted models.
#' @param use_weights Logical. If `TRUE`, uses `weights_col` when available.
#' @param random_slopes Logical. If `TRUE`, first attempts random slopes for all
#'   polynomial time terms.
#' @param fallback_on_singular Logical. If `TRUE`, falls back to a
#'   random-intercept model when the random-slope model is singular.
#' @param REML Logical passed to `lme4::lmer()`.
#' @param optimizer Optimizer passed to `lme4::lmerControl()`.
#' @param maxfun Maximum optimizer function evaluations.
#' @param drop_missing Logical. If `TRUE`, rows with missing model variables are
#'   removed before fitting.
#'
#' @return A list of class `gp3_gca_model` containing the fitted model, attempted
#'   and final formulas, model comparison information, settings, and status.
#'
#' @export
fit_gazepoint_gca <- function(
    data,
    outcome_col = "gca_pupil",
    subject_col = "subject",
    condition_col = "condition",
    time_terms = NULL,
    degree = NULL,
    weights_col = "gca_weight",
    use_weights = TRUE,
    random_slopes = TRUE,
    fallback_on_singular = TRUE,
    REML = FALSE,
    optimizer = "bobyqa",
    maxfun = 2e5,
    drop_missing = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop(
      "Package `lme4` is required to fit GCA mixed models. Please install it first.",
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

  valid_column(outcome_col, "outcome_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_optional_column(weights_col, "weights_col")

  if (!is.null(time_terms) &&
      (!is.character(time_terms) ||
       length(time_terms) < 1L ||
       any(is.na(time_terms)) ||
       any(!nzchar(time_terms)) ||
       anyDuplicated(time_terms))) {
    stop(
      "`time_terms` must be NULL or a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.null(degree) &&
      (!is.numeric(degree) ||
       length(degree) != 1L ||
       is.na(degree) ||
       !is.finite(degree) ||
       degree < 1)) {
    stop(
      "`degree` must be NULL or a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.null(degree)) {
    degree <- as.integer(degree)
  }

  if (!is.logical(use_weights) ||
      length(use_weights) != 1L ||
      is.na(use_weights)) {
    stop("`use_weights` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(random_slopes) ||
      length(random_slopes) != 1L ||
      is.na(random_slopes)) {
    stop("`random_slopes` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(fallback_on_singular) ||
      length(fallback_on_singular) != 1L ||
      is.na(fallback_on_singular)) {
    stop("`fallback_on_singular` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(REML) ||
      length(REML) != 1L ||
      is.na(REML)) {
    stop("`REML` must be TRUE or FALSE.", call. = FALSE)
  }

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

  if (!is.logical(drop_missing) ||
      length(drop_missing) != 1L ||
      is.na(drop_missing)) {
    stop("`drop_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  dat <- tibble::as_tibble(data)

  if (is.null(time_terms)) {
    if (!is.null(degree)) {
      time_terms <- paste0("time_poly_", seq_len(degree))
    } else {
      detected_terms <- grep(
        "^time_poly_[0-9]+$",
        names(dat),
        value = TRUE
      )

      if (length(detected_terms) > 0L) {
        term_numbers <- suppressWarnings(
          as.integer(sub("^time_poly_", "", detected_terms))
        )

        time_terms <- detected_terms[order(term_numbers)]
      }
    }
  }

  if (is.null(time_terms) || length(time_terms) == 0L) {
    stop(
      "Could not detect polynomial time terms. Please provide `time_terms`.",
      call. = FALSE
    )
  }

  required_cols <- c(outcome_col, subject_col, time_terms)

  if (!is.null(condition_col)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (use_weights && !is.null(weights_col)) {
    required_cols <- c(required_cols, weights_col)
  }

  missing_cols <- setdiff(unique(required_cols), names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  dat$.gp3_gca_outcome <- as_numeric_safe(dat[[outcome_col]])
  dat$.gp3_gca_subject <- as.factor(dat[[subject_col]])

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    condition_values <- as.character(dat[[condition_col]])
    condition_values <- trimws(condition_values)
    condition_values[
      is.na(condition_values) |
        !nzchar(condition_values)
    ] <- "all_data"

    dat$.gp3_gca_condition <- as.factor(condition_values)
  } else {
    dat$.gp3_gca_condition <- factor("all_data")
  }

  for (i in seq_along(time_terms)) {
    dat[[paste0(".gp3_time_poly_", i)]] <- as_numeric_safe(dat[[time_terms[[i]]]])
  }

  internal_time_terms <- paste0(".gp3_time_poly_", seq_along(time_terms))

  if (use_weights && !is.null(weights_col)) {
    dat$.gp3_gca_weights <- as_numeric_safe(dat[[weights_col]])
  } else {
    dat$.gp3_gca_weights <- NA_real_
  }

  if (drop_missing) {
    keep <- is.finite(dat$.gp3_gca_outcome) &
      !is.na(dat$.gp3_gca_subject) &
      !is.na(dat$.gp3_gca_condition)

    for (term in internal_time_terms) {
      keep <- keep & is.finite(dat[[term]])
    }

    if (use_weights && !is.null(weights_col)) {
      keep <- keep &
        is.finite(dat$.gp3_gca_weights) &
        dat$.gp3_gca_weights > 0
    }

    dat <- dat[keep, , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after removing missing GCA model variables.",
      call. = FALSE
    )
  }

  n_subjects <- length(unique(dat$.gp3_gca_subject))
  n_conditions <- length(unique(dat$.gp3_gca_condition))

  if (n_subjects < 2L) {
    stop(
      "At least two subjects are required to fit a GCA mixed model.",
      call. = FALSE
    )
  }

  fixed_time_rhs <- paste(internal_time_terms, collapse = " + ")

  if (n_conditions > 1L) {
    fixed_rhs <- paste0(
      ".gp3_gca_condition * (",
      fixed_time_rhs,
      ")"
    )
  } else {
    fixed_rhs <- fixed_time_rhs
  }

  random_intercept_term <- "(1 | .gp3_gca_subject)"

  random_slope_term <- paste0(
    "(1 + ",
    fixed_time_rhs,
    " | .gp3_gca_subject)"
  )

  attempted_random_structure <- if (random_slopes) {
    "random_intercept_and_time_slopes"
  } else {
    "random_intercept"
  }

  attempted_formula <- stats::as.formula(
    paste(
      ".gp3_gca_outcome ~",
      fixed_rhs,
      "+",
      if (random_slopes) random_slope_term else random_intercept_term
    )
  )

  fallback_formula <- stats::as.formula(
    paste(
      ".gp3_gca_outcome ~",
      fixed_rhs,
      "+",
      random_intercept_term
    )
  )

  control <- lme4::lmerControl(
    optimizer = optimizer,
    optCtrl = list(maxfun = maxfun)
  )

  fit_lmer <- function(model_formula) {
    args <- list(
      formula = model_formula,
      data = dat,
      REML = REML,
      control = control
    )

    if (use_weights && !is.null(weights_col)) {
      args$weights <- dat$.gp3_gca_weights
    }

    tryCatch(
      do.call(lme4::lmer, args),
      error = function(e) e
    )
  }

  attempted_model <- fit_lmer(attempted_formula)

  attempted_failed <- inherits(attempted_model, "error")
  attempted_singular <- FALSE

  if (!attempted_failed) {
    attempted_singular <- isTRUE(
      lme4::isSingular(attempted_model, tol = 1e-4)
    )
  }

  fallback_used <- FALSE
  final_model <- attempted_model
  final_formula <- attempted_formula
  final_random_structure <- attempted_random_structure
  model_status <- "ok"
  error_message <- NA_character_

  if (attempted_failed) {
    fallback_used <- TRUE
    fallback_model <- fit_lmer(fallback_formula)

    if (inherits(fallback_model, "error")) {
      final_model <- NULL
      final_formula <- fallback_formula
      final_random_structure <- "random_intercept"
      model_status <- "fit_failed"
      error_message <- paste(
        "Random-slope model failed:",
        conditionMessage(attempted_model),
        "| Random-intercept fallback failed:",
        conditionMessage(fallback_model)
      )
    } else {
      final_model <- fallback_model
      final_formula <- fallback_formula
      final_random_structure <- "random_intercept"
      model_status <- "fallback_after_fit_failure"
      error_message <- conditionMessage(attempted_model)
    }
  } else if (attempted_singular && fallback_on_singular && random_slopes) {
    fallback_used <- TRUE
    fallback_model <- fit_lmer(fallback_formula)

    if (inherits(fallback_model, "error")) {
      final_model <- attempted_model
      final_formula <- attempted_formula
      final_random_structure <- attempted_random_structure
      model_status <- "singular_random_slope_model_fallback_failed"
      error_message <- conditionMessage(fallback_model)
    } else {
      final_model <- fallback_model
      final_formula <- fallback_formula
      final_random_structure <- "random_intercept"
      model_status <- "fallback_after_singular_fit"
      error_message <- NA_character_
    }
  } else if (attempted_singular) {
    model_status <- "singular_fit"
  }

  final_singular <- FALSE

  if (!is.null(final_model)) {
    final_singular <- isTRUE(
      lme4::isSingular(final_model, tol = 1e-4)
    )
  }

  extract_stats <- function(model, model_type) {
    if (is.null(model) || inherits(model, "error")) {
      return(
        tibble::tibble(
          model_type = model_type,
          n = NA_integer_,
          AIC = NA_real_,
          BIC = NA_real_,
          logLik = NA_real_
        )
      )
    }

    tibble::tibble(
      model_type = model_type,
      n = stats::nobs(model),
      AIC = stats::AIC(model),
      BIC = stats::BIC(model),
      logLik = as.numeric(stats::logLik(model))
    )
  }

  comparison <- dplyr::bind_rows(
    extract_stats(
      if (attempted_failed) NULL else attempted_model,
      "attempted_model"
    ),
    extract_stats(final_model, "final_model")
  )

  if (all(is.finite(comparison$AIC))) {
    attempted_aic <- comparison$AIC[
      comparison$model_type == "attempted_model"
    ][[1]]

    attempted_bic <- comparison$BIC[
      comparison$model_type == "attempted_model"
    ][[1]]

    comparison <- comparison |>
      dplyr::mutate(
        delta_AIC_from_attempted = .data[["AIC"]] - attempted_aic,
        delta_BIC_from_attempted = .data[["BIC"]] - attempted_bic
      )
  } else {
    comparison <- comparison |>
      dplyr::mutate(
        delta_AIC_from_attempted = NA_real_,
        delta_BIC_from_attempted = NA_real_
      )
  }

  out <- list(
    model = final_model,
    attempted_model = if (attempted_failed) NULL else attempted_model,
    attempted_formula = attempted_formula,
    formula = final_formula,
    fallback_formula = fallback_formula,
    data = dat,
    comparison = comparison,
    settings = list(
      outcome_col = outcome_col,
      subject_col = subject_col,
      condition_col = condition_col,
      time_terms = time_terms,
      internal_time_terms = internal_time_terms,
      degree = if (is.null(degree)) length(time_terms) else degree,
      weights_col = weights_col,
      use_weights = use_weights,
      random_slopes = random_slopes,
      fallback_on_singular = fallback_on_singular,
      REML = REML,
      optimizer = optimizer,
      maxfun = maxfun
    ),
    random_effect_structure = final_random_structure,
    attempted_random_effect_structure = attempted_random_structure,
    fallback_used = fallback_used,
    singular_fit = final_singular,
    attempted_singular_fit = attempted_singular,
    model_status = model_status,
    error_message = error_message
  )

  class(out) <- c("gp3_gca_model", class(out))

  out
}
