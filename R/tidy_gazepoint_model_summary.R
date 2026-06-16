#' Create a tidy model summary for manuscript tables
#'
#' Create a compact model-summary object from common fitted models used in
#' `gp3tools` workflows. The function combines model metadata, fixed-effect
#' summaries, and optional model diagnostics into one structured object.
#'
#' @param model A fitted model object, or a `gp3tools` fit object containing
#'   a `$model` element.
#' @param model_name Optional model label used in returned tables.
#' @param conf_level Confidence level for Wald confidence intervals.
#' @param exponentiate Logical. If `TRUE`, exponentiate fixed-effect estimates
#'   and confidence intervals.
#' @param drop_intercept Logical. If `TRUE`, remove the intercept from the
#'   fixed-effect table.
#' @param include_diagnostics Logical. If `TRUE`, include model diagnostics
#'   when supported.
#' @param use_dharma Logical. If `TRUE`, request optional DHARMa diagnostics.
#' @param dharma_simulations Number of DHARMa simulations.
#' @param seed Random seed used before DHARMa simulation.
#'
#' @return A list with overview, model_info, fixed_effects, diagnostics, and
#'   settings. The returned object has class `gp3_model_summary`.
#' @export
tidy_gazepoint_model_summary <- function(
    model,
    model_name = NULL,
    conf_level = 0.95,
    exponentiate = FALSE,
    drop_intercept = FALSE,
    include_diagnostics = TRUE,
    use_dharma = FALSE,
    dharma_simulations = 250,
    seed = 123
) {
  if (is.null(model)) {
    stop("`model` must not be NULL.", call. = FALSE)
  }

  check_character_scalar <- function(x, arg, allow_null = FALSE) {
    if (is.null(x) && allow_null) {
      return(invisible(TRUE))
    }


    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }

    invisible(TRUE)


  }

  check_logical_scalar <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }


    invisible(TRUE)


  }

  check_conf_level <- function(x) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x <= 0 ||
        x >= 1) {
      stop(
        "`conf_level` must be a finite numeric scalar between 0 and 1.",
        call. = FALSE
      )
    }


    invisible(TRUE)


  }

  check_positive_numeric <- function(x, arg) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x <= 0) {
      stop(
        "`", arg, "` must be a positive finite numeric scalar.",
        call. = FALSE
      )
    }


    invisible(TRUE)


  }

  check_seed <- function(x) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x)) {
      stop("`seed` must be a finite numeric scalar.", call. = FALSE)
    }


    invisible(TRUE)


  }

  check_character_scalar(model_name, "model_name", allow_null = TRUE)
  check_conf_level(conf_level)
  check_logical_scalar(exponentiate, "exponentiate")
  check_logical_scalar(drop_intercept, "drop_intercept")
  check_logical_scalar(include_diagnostics, "include_diagnostics")
  check_logical_scalar(use_dharma, "use_dharma")
  check_positive_numeric(dharma_simulations, "dharma_simulations")
  check_seed(seed)

  extracted <- .gp3_extract_model_for_diagnostics(model)

  fitted_model <- extracted$model

  if (is.null(model_name)) {
    model_name <- extracted$model_name
  }

  model_class <- paste(class(fitted_model), collapse = "/")

  fixed_effects <- summarise_gazepoint_fixed_effects(
    fitted_model,
    model_name = model_name,
    conf_level = conf_level,
    exponentiate = exponentiate,
    drop_intercept = drop_intercept
  )

  model_info <- .gp3_model_info_table(
    fitted_model,
    model_name = model_name
  )

  diagnostics <- if (include_diagnostics) {
    .gp3_model_summary_diagnostics(
      fitted_model,
      model_name = model_name,
      use_dharma = use_dharma,
      dharma_simulations = dharma_simulations,
      seed = seed
    )
  } else {
    list(
      overview = tibble::tibble(
        model_name = model_name,
        model_class = model_class,
        diagnostic_status = "skipped_disabled",
        message = "Model diagnostics were disabled."
      )
    )
  }

  diagnostics_overview <- diagnostics$overview

  if (!is.data.frame(diagnostics_overview) ||
      nrow(diagnostics_overview) == 0L ||
      !"diagnostic_status" %in% names(diagnostics_overview)) {
    diagnostics_status <- "not_available"
    diagnostics_message <- "Diagnostics overview was not available."
  } else {
    diagnostics_status <- diagnostics_overview$diagnostic_status[[1L]]
    diagnostics_message <- if ("message" %in% names(diagnostics_overview)) {
      diagnostics_overview$message[[1L]]
    } else {
      NA_character_
    }
  }

  fixed_status <- .gp3_summary_table_status(fixed_effects)

  overview_status <- .gp3_model_summary_status(
    fixed_status = fixed_status,
    diagnostics_status = diagnostics_status,
    include_diagnostics = include_diagnostics
  )

  overview <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    model_family = model_info$model_family[[1L]],
    model_link = model_info$model_link[[1L]],
    n_observations = model_info$n_observations[[1L]],
    n_fixed_effects = sum(fixed_effects$diagnostic_status == "ok", na.rm = TRUE),
    fixed_effects_status = fixed_status,
    diagnostics_status = diagnostics_status,
    summary_status = overview_status,
    message = .gp3_collapse_messages(c(
      unique(fixed_effects$message),
      diagnostics_message
    ))
  )

  out <- list(
    overview = overview,
    model_info = model_info,
    fixed_effects = fixed_effects,
    diagnostics = diagnostics,
    settings = list(
      conf_level = conf_level,
      exponentiate = exponentiate,
      drop_intercept = drop_intercept,
      include_diagnostics = include_diagnostics,
      use_dharma = use_dharma,
      dharma_simulations = as.integer(dharma_simulations),
      seed = as.integer(seed)
    )
  )

  class(out) <- c("gp3_model_summary", "list")

  out
}

.gp3_model_info_table <- function(model, model_name) {
  model_class <- paste(class(model), collapse = "/")

  scalar_character <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
      return(NA_character_)
    }


    as.character(x[[1L]])


  }

  scalar_integer <- function(x) {
    x <- suppressWarnings(as.integer(x))


    if (length(x) == 0L || is.na(x[[1L]])) {
      return(NA_integer_)
    }

    x[[1L]]


  }

  scalar_numeric <- function(x) {
    x <- suppressWarnings(as.numeric(x))


    if (length(x) == 0L || is.na(x[[1L]]) || !is.finite(x[[1L]])) {
      return(NA_real_)
    }

    x[[1L]]


  }

  family_info <- tryCatch(
    stats::family(model),
    error = function(e) NULL
  )

  model_family <- if (!is.null(family_info)) {
    scalar_character(family_info$family)
  } else {
    NA_character_
  }

  model_link <- if (!is.null(family_info)) {
    scalar_character(family_info$link)
  } else {
    NA_character_
  }

  formula_text <- tryCatch(
    paste(deparse(stats::formula(model)), collapse = " "),
    error = function(e) NA_character_
  )

  n_observations <- tryCatch(
    stats::nobs(model),
    error = function(e) NA_integer_
  )

  aic_value <- tryCatch(
    stats::AIC(model),
    error = function(e) NA_real_
  )

  bic_value <- tryCatch(
    stats::BIC(model),
    error = function(e) NA_real_
  )

  loglik_value <- tryCatch(
    as.numeric(stats::logLik(model)),
    error = function(e) NA_real_
  )

  df_residual <- tryCatch(
    stats::df.residual(model),
    error = function(e) NA_real_
  )

  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    model_family = model_family,
    model_link = model_link,
    formula = scalar_character(formula_text),
    n_observations = scalar_integer(n_observations),
    df_residual = scalar_numeric(df_residual),
    aic = scalar_numeric(aic_value),
    bic = scalar_numeric(bic_value),
    log_lik = scalar_numeric(loglik_value)
  )
}

.gp3_model_summary_diagnostics <- function(
    model,
    model_name,
    use_dharma,
    dharma_simulations,
    seed
) {
  if (inherits(model, "gam")) {
    return(diagnose_gazepoint_gamm(
      model,
      model_name = model_name,
      use_dharma = use_dharma,
      dharma_simulations = dharma_simulations,
      seed = seed
    ))
  }

  if (inherits(model, c("lm", "glm", "merMod"))) {
    return(diagnose_gazepoint_glmm(
      model,
      model_name = model_name,
      use_dharma = use_dharma,
      dharma_simulations = dharma_simulations,
      seed = seed
    ))
  }

  model_class <- paste(class(model), collapse = "/")

  list(
    overview = tibble::tibble(
      model_name = model_name,
      model_class = model_class,
      diagnostic_status = "unsupported_model_class",
      message = "Unsupported model class for model diagnostics."
    )
  )
}

.gp3_summary_table_status <- function(tab) {
  if (!is.data.frame(tab) ||
      !"diagnostic_status" %in% names(tab) ||
      nrow(tab) == 0L) {
    return("not_available")
  }

  statuses <- unique(stats::na.omit(as.character(tab$diagnostic_status)))

  if (length(statuses) == 0L) {
    return("not_available")
  }

  if (any(statuses %in% c("error", "unsupported_model_class"))) {
    return("error")
  }

  if (all(statuses == "ok")) {
    return("ok")
  }

  if (any(statuses %in% c("not_available"))) {
    return("not_available")
  }

  statuses[[1L]]
}

.gp3_model_summary_status <- function(
    fixed_status,
    diagnostics_status,
    include_diagnostics
) {
  if (fixed_status == "error") {
    return("error")
  }

  if (fixed_status %in% c("not_available")) {
    return("not_available")
  }

  if (!include_diagnostics) {
    return(fixed_status)
  }

  if (diagnostics_status %in% c("error", "unsupported_model_class")) {
    return("diagnostic_error")
  }

  if (diagnostics_status %in% c(
    "diagnostic_warning",
    "convergence_warning",
    "singular_fit",
    "overdispersed",
    "basis_warning"
  )) {
    return("diagnostic_warning")
  }

  if (fixed_status == "ok") {
    return("ok")
  }

  "not_available"
}

.gp3_collapse_messages <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  x <- unique(x)

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}
