#' Diagnose GAM and BAM models
#'
#' Run a compact diagnostics bundle for `mgcv` GAM/BAM models used in
#' `gp3tools` workflows. The function combines convergence, basis-dimension
#' checks, overdispersion checks, and optional DHARMa simulation-based residual
#' diagnostics.
#'
#' The function accepts raw `mgcv::gam()` / `mgcv::bam()` model objects,
#' `gp3tools` fit objects containing a `$model` element, or a named list of
#' fitted model objects.
#'
#' @param model A fitted GAM/BAM object, a `gp3tools` fit object containing
#'   `$model`, or a named list of fitted model objects.
#' @param model_name Optional model label used in returned tables.
#' @param check_convergence Logical. If `TRUE`, run convergence diagnostics.
#' @param check_basis Logical. If `TRUE`, run `mgcv::k.check()` basis-dimension
#'   diagnostics when available.
#' @param check_overdispersion Logical. If `TRUE`, run overdispersion
#'   diagnostics when meaningful for the model family.
#' @param use_dharma Logical. If `TRUE`, try to run optional DHARMa diagnostics.
#' @param dharma_simulations Number of DHARMa simulations.
#' @param seed Random seed used before DHARMa simulation.
#'
#' @return A list with overview, convergence, basis, overdispersion, DHARMa
#'   diagnostics, and settings.
#' @export
diagnose_gazepoint_gamm <- function(
    model,
    model_name = NULL,
    check_convergence = TRUE,
    check_basis = TRUE,
    check_overdispersion = TRUE,
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
  check_logical_scalar(check_basis, "check_basis")
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
      diagnose_gazepoint_gamm(
        model = model[[i]],
        model_name = model_names[[i]],
        check_convergence = check_convergence,
        check_basis = check_basis,
        check_overdispersion = check_overdispersion,
        use_dharma = use_dharma,
        dharma_simulations = dharma_simulations,
        seed = seed
      )
    })

    out <- list(
      overview = dplyr::bind_rows(lapply(diagnostics, `[[`, "overview")),
      convergence = dplyr::bind_rows(lapply(diagnostics, `[[`, "convergence")),
      basis = dplyr::bind_rows(lapply(diagnostics, `[[`, "basis")),
      overdispersion = dplyr::bind_rows(lapply(diagnostics, `[[`, "overdispersion")),
      dharma = dplyr::bind_rows(lapply(diagnostics, `[[`, "dharma")),
      settings = list(
        check_convergence = check_convergence,
        check_basis = check_basis,
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

  basis <- if (check_basis) {
    .gp3_check_gamm_basis(
      fitted_model,
      model_name = model_name
    )
  } else {
    .gp3_skipped_gamm_basis_row(
      model_name = model_name,
      model_class = model_class,
      status = "skipped_disabled",
      message = "GAM basis diagnostics were disabled."
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

  basis_status <- .gp3_summarise_basis_status(basis)

  overview_status <- .gp3_combine_diagnostic_status(
    statuses = c(
      convergence$diagnostic_status[[1L]],
      basis_status,
      overdispersion$diagnostic_status[[1L]],
      dharma$diagnostic_status[[1L]]
    )
  )

  overview_message <- .gp3_combine_diagnostic_messages(
    convergence = convergence,
    basis = basis,
    overdispersion = overdispersion,
    dharma = dharma
  )

  overview <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic_status = overview_status,
    converged = .gp3_extract_logical_value(convergence, "converged"),
    basis_status = basis_status,
    overdispersed = .gp3_extract_logical_value(overdispersion, "overdispersed"),
    dharma_status = dharma$dharma_status[[1L]],
    message = overview_message
  )

  out <- list(
    overview = overview,
    convergence = convergence,
    basis = basis,
    overdispersion = overdispersion,
    dharma = dharma,
    settings = list(
      check_convergence = check_convergence,
      check_basis = check_basis,
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

.gp3_check_gamm_basis <- function(model, model_name) {
  model_class <- paste(class(model), collapse = "/")

  base <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic = "basis",
    smooth = NA_character_,
    k_index = NA_real_,
    edf = NA_real_,
    k_prime = NA_real_,
    p_value = NA_real_,
    basis_status = "not_available",
    diagnostic_status = "not_available",
    message = NA_character_
  )

  if (!inherits(model, "gam")) {
    base$basis_status <- "not_applicable"
    base$diagnostic_status <- "not_applicable"
    base$message <- "GAM basis diagnostics are only applicable to mgcv GAM/BAM objects."
    return(base)
  }

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    base$basis_status <- "skipped_missing_package"
    base$diagnostic_status <- "skipped_missing_package"
    base$message <- "Package `mgcv` is required for GAM basis diagnostics."
    return(base)
  }

  k_check <- tryCatch(
    suppressWarnings(
      suppressMessages(
        mgcv::k.check(model)
      )
    ),
    error = function(e) {
      base$basis_status <<- "error"
      base$diagnostic_status <<- "error"
      base$message <<- paste0(
        "mgcv::k.check() failed: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(k_check)) {
    return(base)
  }

  if (is.null(dim(k_check)) || nrow(k_check) == 0L) {
    base$basis_status <- "not_available"
    base$diagnostic_status <- "not_available"
    base$message <- "mgcv::k.check() returned no smooth-level basis diagnostics."
    return(base)
  }

  k_check <- as.data.frame(k_check)
  k_check$smooth <- rownames(k_check)
  rownames(k_check) <- NULL

  get_numeric_column <- function(tab, candidates) {
    matched <- intersect(candidates, names(tab))


    if (length(matched) == 0L) {
      return(rep(NA_real_, nrow(tab)))
    }

    suppressWarnings(as.numeric(tab[[matched[[1L]]]]))


  }

  out <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic = "basis",
    smooth = as.character(k_check$smooth),
    k_index = get_numeric_column(k_check, c("k-index", "k_index", "k.index")),
    edf = get_numeric_column(k_check, c("edf", "EDF")),
    k_prime = get_numeric_column(k_check, c("k'", "k_prime", "k.prime")),
    p_value = get_numeric_column(k_check, c("p-value", "p_value", "p.value"))
  )

  out$basis_status <- dplyr::case_when(
    is.finite(out$p_value) & out$p_value < 0.05 ~ "basis_warning",
    TRUE ~ "ok"
  )

  out$diagnostic_status <- out$basis_status

  out$message <- dplyr::case_when(
    out$basis_status == "basis_warning" ~
      "Basis-dimension check returned p < .05.",
    TRUE ~
      "Basis-dimension check did not return p < .05."
  )

  out
}

.gp3_skipped_gamm_basis_row <- function(
    model_name,
    model_class,
    status,
    message
) {
  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic = "basis",
    smooth = NA_character_,
    k_index = NA_real_,
    edf = NA_real_,
    k_prime = NA_real_,
    p_value = NA_real_,
    basis_status = status,
    diagnostic_status = status,
    message = message
  )
}

.gp3_summarise_basis_status <- function(basis) {
  if (!is.data.frame(basis) ||
      !"basis_status" %in% names(basis) ||
      nrow(basis) == 0L) {
    return("not_available")
  }

  statuses <- unique(stats::na.omit(as.character(basis$basis_status)))

  if (length(statuses) == 0L) {
    return("not_available")
  }

  if (any(statuses %in% c("error"))) {
    return("error")
  }

  if (any(statuses %in% c("basis_warning"))) {
    return("basis_warning")
  }

  if (all(statuses %in% c(
    "ok",
    "not_applicable",
    "skipped_disabled",
    "skipped_missing_package"
  ))) {
    if (any(statuses == "ok")) {
      return("ok")
    }


    return(statuses[[1L]])


  }

  "not_available"
}
