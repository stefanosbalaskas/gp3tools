#' Diagnose GLMM, LMM, and GLM models
#'
#' Run a compact diagnostics bundle for model objects used in `gp3tools`
#' workflows. The function combines convergence, singularity, overdispersion,
#' and optional DHARMa simulation-based residual diagnostics.
#'
#' The function accepts raw fitted models, `gp3tools` fit objects containing a
#' `$model` element, or a named list of fitted models. DHARMa diagnostics are
#' optional and are skipped cleanly when DHARMa is not installed.
#'
#' @param model A fitted model object, a `gp3tools` fit object containing
#'   `$model`, or a named list of fitted model objects.
#' @param model_name Optional model label used in returned tables.
#' @param check_convergence Logical. If `TRUE`, run convergence diagnostics.
#' @param check_singularity Logical. If `TRUE`, run singularity diagnostics.
#' @param check_overdispersion Logical. If `TRUE`, run overdispersion
#'   diagnostics.
#' @param use_dharma Logical. If `TRUE`, try to run optional DHARMa diagnostics.
#' @param dharma_simulations Number of DHARMa simulations.
#' @param seed Random seed used before DHARMa simulation.
#'
#' @return A list with overview, convergence, singularity, overdispersion,
#'   DHARMa diagnostics, and settings.
#' @export
diagnose_gazepoint_glmm <- function(
    model,
    model_name = NULL,
    check_convergence = TRUE,
    check_singularity = TRUE,
    check_overdispersion = TRUE,
    use_dharma = TRUE,
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
  check_logical_scalar(check_convergence, "check_convergence")
  check_logical_scalar(check_singularity, "check_singularity")
  check_logical_scalar(check_overdispersion, "check_overdispersion")
  check_logical_scalar(use_dharma, "use_dharma")
  check_positive_numeric(dharma_simulations, "dharma_simulations")
  check_seed(seed)

  dharma_simulations <- as.integer(dharma_simulations)
  seed <- as.integer(seed)

  if (.gp3_is_model_collection_for_diagnostics(model)) {
    model_names <- names(model)


    if (is.null(model_names)) {
      model_names <- rep("", length(model))
    }

    if (is.null(model_name)) {
      model_names <- ifelse(
        nzchar(model_names),
        model_names,
        paste0("model_", seq_along(model))
      )
    } else {
      suffix <- ifelse(
        nzchar(model_names),
        model_names,
        seq_along(model)
      )

      model_names <- paste0(model_name, "_", suffix)
    }

    diagnostics <- lapply(seq_along(model), function(i) {
      diagnose_gazepoint_glmm(
        model = model[[i]],
        model_name = model_names[[i]],
        check_convergence = check_convergence,
        check_singularity = check_singularity,
        check_overdispersion = check_overdispersion,
        use_dharma = use_dharma,
        dharma_simulations = dharma_simulations,
        seed = seed
      )
    })

    out <- list(
      overview = dplyr::bind_rows(lapply(diagnostics, `[[`, "overview")),
      convergence = dplyr::bind_rows(lapply(diagnostics, `[[`, "convergence")),
      singularity = dplyr::bind_rows(lapply(diagnostics, `[[`, "singularity")),
      overdispersion = dplyr::bind_rows(lapply(diagnostics, `[[`, "overdispersion")),
      dharma = dplyr::bind_rows(lapply(diagnostics, `[[`, "dharma")),
      settings = list(
        check_convergence = check_convergence,
        check_singularity = check_singularity,
        check_overdispersion = check_overdispersion,
        use_dharma = use_dharma,
        dharma_simulations = dharma_simulations,
        seed = seed,
        n_models = length(model)
      )
    )

    class(out) <- c("gp3_model_diagnostics", "list")

    return(out)


  }

  extracted <- .gp3_extract_model_for_diagnostics(model)

  fitted_model <- extracted$model

  if (is.null(model_name)) {
    model_name <- extracted$model_name
  }

  model_class <- paste(class(fitted_model), collapse = "/")

  convergence <- if (check_convergence) {
    check_gazepoint_model_convergence(
      fitted_model,
      model_name = model_name
    )
  } else {
    .gp3_skipped_diagnostic_row(
      model_name = model_name,
      model_class = model_class,
      diagnostic = "convergence",
      status = "skipped_disabled",
      message = "Convergence diagnostics were disabled."
    )
  }

  singularity <- if (check_singularity) {
    check_gazepoint_model_singularity(
      fitted_model,
      model_name = model_name
    )
  } else {
    .gp3_skipped_diagnostic_row(
      model_name = model_name,
      model_class = model_class,
      diagnostic = "singularity",
      status = "skipped_disabled",
      message = "Singularity diagnostics were disabled."
    )
  }

  overdispersion <- if (check_overdispersion) {
    check_gazepoint_model_overdispersion(
      fitted_model,
      model_name = model_name
    )
  } else {
    .gp3_skipped_diagnostic_row(
      model_name = model_name,
      model_class = model_class,
      diagnostic = "overdispersion",
      status = "skipped_disabled",
      message = "Overdispersion diagnostics were disabled."
    )
  }

  dharma <- .gp3_run_dharma_diagnostics(
    model = fitted_model,
    model_name = model_name,
    model_class = model_class,
    use_dharma = use_dharma,
    dharma_simulations = dharma_simulations,
    seed = seed
  )

  convergence_status <- convergence$diagnostic_status[[1L]]
  singularity_status <- singularity$diagnostic_status[[1L]]
  overdispersion_status <- overdispersion$diagnostic_status[[1L]]
  dharma_status <- dharma$dharma_status[[1L]]

  overview_status <- .gp3_combine_diagnostic_status(
    statuses = c(
      convergence_status,
      singularity_status,
      overdispersion_status,
      dharma$diagnostic_status[[1L]]
    )
  )

  overview_message <- .gp3_combine_diagnostic_messages(
    convergence = convergence,
    singularity = singularity,
    overdispersion = overdispersion,
    dharma = dharma
  )

  overview <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic_status = overview_status,
    converged = .gp3_extract_logical_value(convergence, "converged"),
    singular_fit = .gp3_extract_logical_value(singularity, "singular_fit"),
    overdispersed = .gp3_extract_logical_value(overdispersion, "overdispersed"),
    dharma_status = dharma_status,
    message = overview_message
  )

  out <- list(
    overview = overview,
    convergence = convergence,
    singularity = singularity,
    overdispersion = overdispersion,
    dharma = dharma,
    settings = list(
      check_convergence = check_convergence,
      check_singularity = check_singularity,
      check_overdispersion = check_overdispersion,
      use_dharma = use_dharma,
      dharma_simulations = dharma_simulations,
      seed = seed,
      n_models = 1L
    )
  )

  class(out) <- c("gp3_model_diagnostics", "list")

  out
}

.gp3_is_model_collection_for_diagnostics <- function(x) {
  if (!is.list(x) ||
      inherits(x, c("lm", "glm", "gam", "merMod")) ||
      "model" %in% names(x) ||
      length(x) == 0L) {
    return(FALSE)
  }

  all(vapply(
    x,
    function(z) {
      inherits(z, c("lm", "glm", "gam", "merMod")) ||
        (is.list(z) && "model" %in% names(z) && !is.null(z$model))
    },
    logical(1)
  ))
}

.gp3_skipped_diagnostic_row <- function(
    model_name,
    model_class,
    diagnostic,
    status,
    message
) {
  if (identical(diagnostic, "convergence")) {
    return(tibble::tibble(
      model_name = model_name,
      model_class = model_class,
      diagnostic = diagnostic,
      converged = NA,
      diagnostic_status = status,
      message = message
    ))
  }

  if (identical(diagnostic, "singularity")) {
    return(tibble::tibble(
      model_name = model_name,
      model_class = model_class,
      diagnostic = diagnostic,
      singular_fit = NA,
      tolerance = NA_real_,
      diagnostic_status = status,
      message = message
    ))
  }

  if (identical(diagnostic, "overdispersion")) {
    return(tibble::tibble(
      model_name = model_name,
      model_class = model_class,
      diagnostic = diagnostic,
      dispersion_ratio = NA_real_,
      pearson_chisq = NA_real_,
      residual_df = NA_real_,
      overdispersed = NA,
      ratio_threshold = NA_real_,
      diagnostic_status = status,
      message = message
    ))
  }

  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic = diagnostic,
    diagnostic_status = status,
    message = message
  )
}

.gp3_run_dharma_diagnostics <- function(
    model,
    model_name,
    model_class,
    use_dharma,
    dharma_simulations,
    seed
) {
  base <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic = "dharma",
    uniformity_p = NA_real_,
    dispersion_p = NA_real_,
    zero_inflation_p = NA_real_,
    dharma_status = "not_available",
    diagnostic_status = "not_available",
    message = NA_character_,
    n_simulations = dharma_simulations
  )

  if (!use_dharma) {
    base$dharma_status <- "skipped_disabled"
    base$diagnostic_status <- "skipped_disabled"
    base$message <- "DHARMa diagnostics were disabled."
    return(base)
  }

  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    base$dharma_status <- "skipped_missing_package"
    base$diagnostic_status <- "skipped_missing_package"
    base$message <- "Package `DHARMa` is not installed."
    return(base)
  }


  simulate_residuals <- getExportedValue("DHARMa", "simulateResiduals")
  test_uniformity <- getExportedValue("DHARMa", "testUniformity")
  test_dispersion <- getExportedValue("DHARMa", "testDispersion")
  test_zero_inflation <- getExportedValue("DHARMa", "testZeroInflation")

  simulate_args <- list(
    fittedModel = model,
    n = dharma_simulations
  )

  if (!is.null(seed)) {
    simulate_args$seed <- seed
  }

  sim <- tryCatch(
    do.call(simulate_residuals, simulate_args),
    error = function(e) {
      base$dharma_status <<- "error"
      base$diagnostic_status <<- "error"
      base$message <<- paste0(
        "DHARMa residual simulation failed: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(sim)) {
    return(base)
  }

  uniformity <- tryCatch(
    suppressMessages(
      test_uniformity(sim, plot = FALSE)
    ),
    error = function(e) e
  )

  dispersion <- tryCatch(
    suppressMessages(
      test_dispersion(sim, plot = FALSE)
    ),
    error = function(e) e
  )

  zero_inflation <- tryCatch(
    suppressMessages(
      test_zero_inflation(sim, plot = FALSE)
    ),
    error = function(e) e
  )

  extract_p <- function(x) {
    if (inherits(x, "error")) {
      return(NA_real_)
    }


    if (!is.null(x$p.value)) {
      return(as.numeric(x$p.value))
    }

    NA_real_


  }

  base$uniformity_p <- extract_p(uniformity)
  base$dispersion_p <- extract_p(dispersion)
  base$zero_inflation_p <- extract_p(zero_inflation)

  errors <- c(
    if (inherits(uniformity, "error")) {
      paste0("uniformity: ", conditionMessage(uniformity))
    },
    if (inherits(dispersion, "error")) {
      paste0("dispersion: ", conditionMessage(dispersion))
    },
    if (inherits(zero_inflation, "error")) {
      paste0("zero inflation: ", conditionMessage(zero_inflation))
    }
  )

  if (length(errors) > 0L) {
    base$dharma_status <- "partial_error"
    base$diagnostic_status <- "partial_error"
    base$message <- paste(errors, collapse = " | ")
    return(base)
  }

  p_values <- c(
    base$uniformity_p,
    base$dispersion_p,
    base$zero_inflation_p
  )

  if (any(is.finite(p_values) & p_values < 0.05)) {
    base$dharma_status <- "diagnostic_warning"
    base$diagnostic_status <- "diagnostic_warning"
    base$message <- "At least one DHARMa diagnostic test returned p < .05."
  } else {
    base$dharma_status <- "ok"
    base$diagnostic_status <- "ok"
    base$message <- "DHARMa diagnostics completed without p < .05."
  }

  base
}

.gp3_extract_logical_value <- function(tab, col) {
  if (!is.data.frame(tab) || !col %in% names(tab) || nrow(tab) == 0L) {
    return(NA)
  }

  value <- tab[[col]][[1L]]

  if (is.na(value)) {
    return(NA)
  }

  isTRUE(value)
}

.gp3_combine_diagnostic_status <- function(statuses) {
  statuses <- statuses[!is.na(statuses)]

  if (length(statuses) == 0L) {
    return("not_available")
  }

  warning_statuses <- c(
    "convergence_warning",
    "singular_fit",
    "overdispersed",
    "diagnostic_warning",
    "partial_error"
  )

  error_statuses <- c(
    "error",
    "unsupported_model_class"
  )

  if (any(statuses %in% error_statuses)) {
    return("error")
  }

  if (any(statuses %in% warning_statuses)) {
    return("diagnostic_warning")
  }

  if (all(statuses %in% c(
    "ok",
    "not_applicable",
    "skipped_disabled",
    "skipped_missing_package"
  ))) {
    return("ok")
  }

  "not_available"
}

.gp3_combine_diagnostic_messages <- function(...) {
  tabs <- list(...)

  messages <- unlist(lapply(tabs, function(tab) {
    if (!is.data.frame(tab) || !"message" %in% names(tab)) {
      return(character())
    }


    as.character(tab$message)


  }))

  messages <- messages[
    !is.na(messages) &
      nzchar(messages)
  ]

  messages <- unique(messages)

  if (length(messages) == 0L) {
    return(NA_character_)
  }

  paste(messages, collapse = " | ")
}
