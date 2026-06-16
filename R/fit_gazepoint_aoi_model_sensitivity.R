#' Fit AOI-window model-family sensitivity checks
#'
#' Fit a compact set of sensitivity models for AOI-window outcomes. The main
#' model is a binomial GLMM. Additional checks can include an empirical-logit
#' LMM, a weighted proportion LMM, and a fixed-effects quasibinomial GLM.
#'
#' @param data AOI GLMM data returned by `prepare_gazepoint_aoi_glmm_data()`.
#' @param success_col Success-count column.
#' @param failure_col Failure-count column.
#' @param denominator_col Denominator column.
#' @param proportion_col Proportion column.
#' @param subject_col Subject column.
#' @param condition_col Condition column.
#' @param window_col Window column.
#' @param model_types Character vector of model types. Supported values are
#'   `"binomial_glmm"`, `"empirical_logit_lmm"`, `"proportion_lmm"`, and
#'   `"quasibinomial_glm"`.
#' @param include_condition Logical. Include condition fixed effect when possible.
#' @param include_window Logical. Include window fixed effect when possible.
#' @param include_interaction Logical. Include condition-by-window interaction
#'   when both condition and window are included.
#' @param random_intercept Logical. Include subject random intercept in mixed
#'   sensitivity models.
#' @param optimizer Optimizer for `lme4` mixed models.
#' @param maxfun Maximum optimizer evaluations.
#' @param nAGQ Number of adaptive Gauss-Hermite quadrature points for the
#'   binomial GLMM.
#' @param empirical_logit_correction Small correction added to success and
#'   failure counts for empirical-logit models.
#' @param drop_missing Logical. Drop rows with missing model variables.
#'
#' @return A list containing fitted models, formulas, comparison table, fixed
#'   effects table, settings, and status information.
#'
#' @export
fit_gazepoint_aoi_model_sensitivity <- function(
    data,
    success_col = "aoi_glmm_success",
    failure_col = "aoi_glmm_failure",
    denominator_col = "aoi_glmm_denominator",
    proportion_col = "aoi_glmm_prop",
    subject_col = "aoi_glmm_subject",
    condition_col = "aoi_glmm_condition",
    window_col = "aoi_glmm_window",
    model_types = c(
      "binomial_glmm",
      "empirical_logit_lmm",
      "proportion_lmm",
      "quasibinomial_glm"
    ),
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = TRUE,
    random_intercept = TRUE,
    optimizer = "bobyqa",
    maxfun = 2e5,
    nAGQ = 0,
    empirical_logit_correction = 0.5,
    drop_missing = TRUE
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
  valid_column(denominator_col, "denominator_col")
  valid_column(proportion_col, "proportion_col")
  valid_column(subject_col, "subject_col")
  valid_column(condition_col, "condition_col")
  valid_column(window_col, "window_col")

  valid_logical(include_condition, "include_condition")
  valid_logical(include_window, "include_window")
  valid_logical(include_interaction, "include_interaction")
  valid_logical(random_intercept, "random_intercept")
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

  supported_model_types <- c(
    "binomial_glmm",
    "empirical_logit_lmm",
    "proportion_lmm",
    "quasibinomial_glm"
  )

  bad_model_types <- setdiff(model_types, supported_model_types)

  if (length(bad_model_types) > 0L) {
    stop(
      "Unsupported model type(s): ",
      paste(bad_model_types, collapse = ", "),
      call. = FALSE
    )
  }

  model_types <- unique(model_types)

  mixed_model_types <- intersect(
    model_types,
    c("binomial_glmm", "empirical_logit_lmm", "proportion_lmm")
  )

  if (length(mixed_model_types) > 0L &&
      !requireNamespace("lme4", quietly = TRUE)) {
    stop(
      "Package `lme4` is required for mixed AOI sensitivity models.",
      call. = FALSE
    )
  }

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

  if (!is.numeric(empirical_logit_correction) ||
      length(empirical_logit_correction) != 1L ||
      is.na(empirical_logit_correction) ||
      !is.finite(empirical_logit_correction) ||
      empirical_logit_correction <= 0) {
    stop(
      "`empirical_logit_correction` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  required_cols <- c(
    success_col,
    failure_col,
    denominator_col,
    proportion_col,
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
  dat$.gp3_denominator <- suppressWarnings(as.numeric(dat[[denominator_col]]))
  dat$.gp3_prop <- suppressWarnings(as.numeric(dat[[proportion_col]]))
  dat$.gp3_subject <- factor(dat[[subject_col]])
  dat$.gp3_condition <- factor(dat[[condition_col]])
  dat$.gp3_window <- factor(dat[[window_col]])

  model_vars <- c(
    ".gp3_success",
    ".gp3_failure",
    ".gp3_denominator",
    ".gp3_prop",
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
      "No rows remain after removing missing AOI sensitivity-model variables.",
      call. = FALSE
    )
  }

  invalid_counts <- !is.finite(dat$.gp3_success) |
    !is.finite(dat$.gp3_failure) |
    !is.finite(dat$.gp3_denominator) |
    dat$.gp3_success < 0 |
    dat$.gp3_failure < 0 |
    dat$.gp3_denominator <= 0

  if (any(invalid_counts)) {
    stop(
      "AOI sensitivity-model counts must be finite and non-negative, with positive denominators.",
      call. = FALSE
    )
  }

  if (any(dat$.gp3_success + dat$.gp3_failure != dat$.gp3_denominator)) {
    stop(
      "For AOI sensitivity models, success + failure must equal the denominator.",
      call. = FALSE
    )
  }

  dat$.gp3_empirical_logit <- log(
    (dat$.gp3_success + empirical_logit_correction) /
      (dat$.gp3_failure + empirical_logit_correction)
  )

  if (length(mixed_model_types) > 0L &&
      dplyr::n_distinct(dat$.gp3_subject) < 2L) {
    stop(
      "At least two subjects are required for mixed AOI sensitivity models.",
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

  fixed_part <- if (length(fixed_terms) == 0L) {
    "1"
  } else {
    paste(fixed_terms, collapse = " + ")
  }

  random_part <- if (random_intercept) {
    "(1 | .gp3_subject)"
  } else {
    NULL
  }

  mixed_rhs <- if (!is.null(random_part)) {
    paste(fixed_part, random_part, sep = " + ")
  } else {
    fixed_part
  }

  formulas <- list(
    binomial_glmm = stats::as.formula(
      paste0("cbind(.gp3_success, .gp3_failure) ~ ", mixed_rhs)
    ),
    empirical_logit_lmm = stats::as.formula(
      paste0(".gp3_empirical_logit ~ ", mixed_rhs)
    ),
    proportion_lmm = stats::as.formula(
      paste0(".gp3_prop ~ ", mixed_rhs)
    ),
    quasibinomial_glm = stats::as.formula(
      paste0("cbind(.gp3_success, .gp3_failure) ~ ", fixed_part)
    )
  )

  control_glmer <- if ("binomial_glmm" %in% model_types) {
    lme4::glmerControl(
      optimizer = optimizer,
      optCtrl = list(maxfun = maxfun)
    )
  } else {
    NULL
  }

  control_lmer <- if (any(c(
    "empirical_logit_lmm",
    "proportion_lmm"
  ) %in% model_types)) {
    lme4::lmerControl(
      optimizer = optimizer,
      optCtrl = list(maxfun = maxfun)
    )
  } else {
    NULL
  }

  fit_one <- function(model_type) {
    tryCatch(
      {
        if (identical(model_type, "binomial_glmm")) {
          lme4::glmer(
            formula = formulas[[model_type]],
            data = dat,
            family = stats::binomial(),
            control = control_glmer,
            nAGQ = as.integer(nAGQ)
          )
        } else if (identical(model_type, "empirical_logit_lmm")) {
          lme4::lmer(
            formula = formulas[[model_type]],
            data = dat,
            weights = dat$.gp3_denominator,
            control = control_lmer,
            REML = FALSE
          )
        } else if (identical(model_type, "proportion_lmm")) {
          lme4::lmer(
            formula = formulas[[model_type]],
            data = dat,
            weights = dat$.gp3_denominator,
            control = control_lmer,
            REML = FALSE
          )
        } else if (identical(model_type, "quasibinomial_glm")) {
          stats::glm(
            formula = formulas[[model_type]],
            data = dat,
            family = stats::quasibinomial()
          )
        } else {
          stop("Unsupported model type.", call. = FALSE)
        }
      },
      error = function(e) e
    )
  }

  models <- stats::setNames(
    lapply(model_types, fit_one),
    model_types
  )

  model_status <- vapply(
    models,
    function(x) {
      if (inherits(x, "error")) {
        "fit_failed"
      } else if (inherits(x, "merMod") && lme4::isSingular(x)) {
        "singular_fit"
      } else {
        "ok"
      }
    },
    character(1)
  )

  error_message <- vapply(
    models,
    function(x) {
      if (inherits(x, "error")) {
        conditionMessage(x)
      } else {
        NA_character_
      }
    },
    character(1)
  )

  model_objects <- models
  model_objects[
    vapply(model_objects, inherits, logical(1), what = "error")
  ] <- list(NULL)

  safe_aic <- function(x) {
    if (is.null(x) || inherits(x, "error")) {
      NA_real_
    } else {
      suppressWarnings(
        tryCatch(stats::AIC(x), error = function(e) NA_real_)
      )
    }
  }

  safe_bic <- function(x) {
    if (is.null(x) || inherits(x, "error")) {
      NA_real_
    } else {
      suppressWarnings(
        tryCatch(stats::BIC(x), error = function(e) NA_real_)
      )
    }
  }

  safe_loglik <- function(x) {
    if (is.null(x) || inherits(x, "error")) {
      NA_real_
    } else {
      suppressWarnings(
        tryCatch(as.numeric(stats::logLik(x)), error = function(e) NA_real_)
      )
    }
  }

  comparison <- tibble::tibble(
    model_type = model_types,
    model_status = unname(model_status),
    n_obs = nrow(dat),
    AIC = vapply(model_objects, safe_aic, numeric(1)),
    BIC = vapply(model_objects, safe_bic, numeric(1)),
    logLik = vapply(model_objects, safe_loglik, numeric(1)),
    error_message = unname(error_message)
  )

  extract_fixed <- function(model_type, model) {
    if (is.null(model) || inherits(model, "error")) {
      return(tibble::tibble(
        model_type = model_type,
        term = character(0),
        estimate = numeric(0),
        std_error = numeric(0),
        statistic = numeric(0),
        p_value = numeric(0)
      ))
    }

    coef_table <- tryCatch(
      stats::coef(summary(model)),
      error = function(e) NULL
    )

    if (is.null(coef_table) || nrow(coef_table) == 0L) {
      return(tibble::tibble(
        model_type = model_type,
        term = character(0),
        estimate = numeric(0),
        std_error = numeric(0),
        statistic = numeric(0),
        p_value = numeric(0)
      ))
    }

    coef_df <- as.data.frame(coef_table)
    coef_names <- colnames(coef_df)

    estimate_col <- coef_names[1]
    std_error_col <- coef_names[2]
    statistic_col <- coef_names[min(3, length(coef_names))]
    p_col <- grep("^Pr\\(", coef_names, value = TRUE)

    tibble::tibble(
      model_type = model_type,
      term = rownames(coef_df),
      estimate = as.numeric(coef_df[[estimate_col]]),
      std_error = as.numeric(coef_df[[std_error_col]]),
      statistic = as.numeric(coef_df[[statistic_col]]),
      p_value = if (length(p_col) > 0L) {
        as.numeric(coef_df[[p_col[1]]])
      } else {
        NA_real_
      }
    )

  }

  fixed_effects <- dplyr::bind_rows(
    Map(extract_fixed, names(model_objects), model_objects)
  )

  output <- list(
    models = model_objects,
    formulas = formulas[model_types],
    data = dat,
    comparison = comparison,
    fixed_effects = fixed_effects,
    settings = list(
      success_col = success_col,
      failure_col = failure_col,
      denominator_col = denominator_col,
      proportion_col = proportion_col,
      subject_col = subject_col,
      condition_col = condition_col,
      window_col = window_col,
      model_types = model_types,
      include_condition = include_condition,
      include_window = include_window,
      include_interaction = include_interaction,
      random_intercept = random_intercept,
      optimizer = optimizer,
      maxfun = maxfun,
      nAGQ = nAGQ,
      empirical_logit_correction = empirical_logit_correction,
      drop_missing = drop_missing,
      n_conditions = n_conditions,
      n_windows = n_windows
    ),
    model_status = comparison$model_status,
    error_message = comparison$error_message
  )

  class(output) <- c("gp3_aoi_model_sensitivity", "list")

  output
}
