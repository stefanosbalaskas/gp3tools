#' Compare nested Gazepoint models
#'
#' Compare a sequence of nested models, such as null, main-effect, time,
#' condition, and interaction models. The helper returns model-level fit
#' indices, likelihood-ratio comparisons, ranking information, and extraction
#' statuses.
#'
#' This helper is useful for GCA, confirmatory LMM/GLMM workflows, AOI GLMMs,
#' pupil LMMs, and other fitted model workflows where reviewers expect explicit
#' model-comparison evidence.
#'
#' @param models A list of fitted model objects.
#' @param model_names Optional character vector of model names. If `NULL`, names
#'   are taken from `models` or generated as `model_1`, `model_2`, etc.
#' @param comparison Comparison strategy. `"sequential"` compares each model
#'   with the previous model. `"against_first"` compares each model with the
#'   first model.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_nested_model_comparison`.
#' @export
compare_gazepoint_nested_models <- function(
    models,
    model_names = NULL,
    comparison = c("sequential", "against_first"),
    name = "gazepoint_nested_model_comparison"
) {
  if (!is.list(models) || length(models) == 0L) {
    stop("`models` must be a non-empty list of fitted model objects.", call. = FALSE)
  }

  comparison <- match.arg(comparison)

  .gp3_nested_check_label(name, "name")

  model_names <- .gp3_nested_resolve_model_names(
    models = models,
    model_names = model_names
  )

  model_table <- .gp3_nested_model_table(
    models = models,
    model_names = model_names
  )

  lrt_table <- .gp3_nested_lrt_table(
    model_table = model_table,
    comparison = comparison
  )

  ranking_table <- .gp3_nested_ranking_table(model_table)

  n_complete <- sum(model_table$extraction_status == "complete")
  n_errors <- sum(model_table$extraction_status != "complete")

  lrt_complete <- if (nrow(lrt_table) == 0L) {
    0L
  } else {
    sum(lrt_table$comparison_status == "complete")
  }

  lrt_problem <- if (nrow(lrt_table) == 0L) {
    0L
  } else {
    sum(lrt_table$comparison_status != "complete")
  }

  comparison_status <- if (length(models) < 2L) {
    "not_enough_models"
  } else if (n_complete < 2L) {
    "failed"
  } else if (n_errors == 0L && lrt_problem == 0L) {
    "complete"
  } else {
    "partial_complete"
  }

  best_aic_model <- ranking_table$model_name[ranking_table$aic_rank == 1L]
  best_bic_model <- ranking_table$model_name[ranking_table$bic_rank == 1L]

  if (length(best_aic_model) == 0L) {
    best_aic_model <- NA_character_
  }

  if (length(best_bic_model) == 0L) {
    best_bic_model <- NA_character_
  }

  overview <- tibble::tibble(
    object_name = name,
    comparison_status = comparison_status,
    comparison = comparison,
    n_models = length(models),
    n_complete_models = n_complete,
    n_model_extraction_errors = n_errors,
    n_lrt_comparisons = nrow(lrt_table),
    n_lrt_complete = lrt_complete,
    n_lrt_problem = lrt_problem,
    best_aic_model = best_aic_model[[1]],
    best_bic_model = best_bic_model[[1]]
  )

  settings <- tibble::tibble(
    setting = c(
      "model_names",
      "comparison",
      "name"
    ),
    value = c(
      paste(model_names, collapse = ", "),
      comparison,
      name
    )
  )

  out <- list(
    overview = overview,
    model_table = model_table,
    lrt_table = lrt_table,
    ranking_table = ranking_table,
    settings = settings
  )

  class(out) <- c("gp3_nested_model_comparison", "list")

  out
}

.gp3_nested_resolve_model_names <- function(models, model_names = NULL) {
  if (!is.null(model_names)) {
    if (!is.character(model_names) || length(model_names) != length(models) || anyNA(model_names) || any(!nzchar(model_names))) {
      stop("`model_names` must be NULL or a non-missing character vector with one name per model.", call. = FALSE)
    }

    if (anyDuplicated(model_names)) {
      stop("`model_names` must be unique.", call. = FALSE)
    }

    return(model_names)
  }

  model_names <- names(models)

  if (is.null(model_names)) {
    model_names <- rep("", length(models))
  }

  missing_names <- is.na(model_names) | !nzchar(model_names)
  model_names[missing_names] <- paste0("model_", which(missing_names))

  if (anyDuplicated(model_names)) {
    stop("Model names must be unique. Supply `model_names` to disambiguate them.", call. = FALSE)
  }

  model_names
}

.gp3_nested_model_table <- function(models, model_names) {
  rows <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    model_name <- model_names[[i]]
    model_class <- paste(class(model), collapse = ", ")

    loglik_result <- .gp3_nested_try_loglik(model)
    aic_result <- .gp3_nested_try_numeric(stats::AIC, model, "AIC")
    bic_result <- .gp3_nested_try_numeric(stats::BIC, model, "BIC")
    nobs_result <- .gp3_nested_try_numeric(stats::nobs, model, "nobs")

    errors <- c(
      loglik_result$message,
      aic_result$message,
      bic_result$message,
      nobs_result$message
    )
    errors <- errors[!is.na(errors) & nzchar(errors)]

    extraction_status <- if (length(errors) == 0L) {
      "complete"
    } else if (!is.na(loglik_result$logLik) && !is.na(loglik_result$df)) {
      "partial_complete"
    } else {
      "extraction_error"
    }

    tibble::tibble(
      model_index = i,
      model_name = model_name,
      model_class = model_class,
      nobs = nobs_result$value,
      df = loglik_result$df,
      logLik = loglik_result$logLik,
      AIC = aic_result$value,
      BIC = bic_result$value,
      extraction_status = extraction_status,
      message = if (length(errors) == 0L) NA_character_ else paste(errors, collapse = " | ")
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_nested_lrt_table <- function(model_table, comparison) {
  if (nrow(model_table) < 2L) {
    return(tibble::tibble(
      comparison_index = integer(0),
      model_0 = character(0),
      model_1 = character(0),
      df_0 = numeric(0),
      df_1 = numeric(0),
      df_diff = numeric(0),
      logLik_0 = numeric(0),
      logLik_1 = numeric(0),
      chisq = numeric(0),
      p_value = numeric(0),
      comparison_status = character(0),
      message = character(0)
    ))
  }

  comparison_pairs <- switch(
    comparison,
    sequential = tibble::tibble(
      comparison_index = seq_len(nrow(model_table) - 1L),
      model_0_index = seq_len(nrow(model_table) - 1L),
      model_1_index = seq_len(nrow(model_table) - 1L) + 1L
    ),
    against_first = tibble::tibble(
      comparison_index = seq_len(nrow(model_table) - 1L),
      model_0_index = 1L,
      model_1_index = seq_len(nrow(model_table) - 1L) + 1L
    )
  )

  rows <- lapply(seq_len(nrow(comparison_pairs)), function(i) {
    pair <- comparison_pairs[i, , drop = FALSE]
    m0 <- model_table[pair$model_0_index[[1]], , drop = FALSE]
    m1 <- model_table[pair$model_1_index[[1]], , drop = FALSE]

    df_diff <- m1$df - m0$df
    chisq <- 2 * (m1$logLik - m0$logLik)

    status <- "complete"
    message <- NA_character_
    p_value <- NA_real_

    if (m0$extraction_status == "extraction_error" || m1$extraction_status == "extraction_error") {
      status <- "model_extraction_error"
      message <- "At least one model did not provide logLik/df information."
    } else if (!is.finite(df_diff) || !is.finite(chisq)) {
      status <- "missing_lrt_components"
      message <- "Likelihood-ratio components could not be computed."
    } else if (df_diff <= 0) {
      status <- "nonpositive_df_difference"
      message <- "The comparison model did not have more degrees of freedom than the reference model."
    } else if (chisq < 0) {
      status <- "negative_lrt_statistic"
      message <- "The comparison model had a lower log-likelihood than the reference model."
    } else {
      p_value <- stats::pchisq(chisq, df = df_diff, lower.tail = FALSE)
    }

    tibble::tibble(
      comparison_index = pair$comparison_index[[1]],
      model_0 = m0$model_name,
      model_1 = m1$model_name,
      df_0 = m0$df,
      df_1 = m1$df,
      df_diff = df_diff,
      logLik_0 = m0$logLik,
      logLik_1 = m1$logLik,
      chisq = chisq,
      p_value = p_value,
      comparison_status = status,
      message = message
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_nested_ranking_table <- function(model_table) {
  out <- model_table |>
    dplyr::mutate(
      delta_AIC = .data$AIC - suppressWarnings(min(.data$AIC, na.rm = TRUE)),
      delta_BIC = .data$BIC - suppressWarnings(min(.data$BIC, na.rm = TRUE)),
      aic_rank = rank(.data$AIC, ties.method = "min", na.last = "keep"),
      bic_rank = rank(.data$BIC, ties.method = "min", na.last = "keep")
    ) |>
    dplyr::select(
      "model_index",
      "model_name",
      "AIC",
      "delta_AIC",
      "aic_rank",
      "BIC",
      "delta_BIC",
      "bic_rank",
      "logLik",
      "df",
      "nobs",
      "extraction_status"
    ) |>
    dplyr::arrange(.data$aic_rank, .data$bic_rank, .data$model_index)

  out$delta_AIC[!is.finite(out$delta_AIC)] <- NA_real_
  out$delta_BIC[!is.finite(out$delta_BIC)] <- NA_real_

  out
}

.gp3_nested_try_loglik <- function(model) {
  out <- tryCatch(
    stats::logLik(model),
    error = function(e) .gp3_nested_error(conditionMessage(e))
  )

  if (inherits(out, "gp3_nested_error")) {
    if (
      is.list(model) &&
      !is.null(model[["logLik"]]) &&
      !is.null(model[["df"]])
    ) {
      loglik_value <- suppressWarnings(as.numeric(model[["logLik"]][[1]]))
      df_value <- suppressWarnings(as.numeric(model[["df"]][[1]]))

      if (
        length(loglik_value) == 1L &&
        length(df_value) == 1L &&
        is.finite(loglik_value) &&
        is.finite(df_value)
      ) {
        return(list(
          logLik = loglik_value,
          df = df_value,
          message = NA_character_
        ))
      }
    }

    return(list(
      logLik = NA_real_,
      df = NA_real_,
      message = paste0("logLik: ", out$error)
    ))
  }

  list(
    logLik = as.numeric(out),
    df = as.numeric(attr(out, "df")),
    message = NA_character_
  )
}

.gp3_nested_try_numeric <- function(fun, model, label) {
  out <- tryCatch(
    fun(model),
    error = function(e) .gp3_nested_error(conditionMessage(e))
  )

  if (inherits(out, "gp3_nested_error")) {
    fallback_name <- switch(
      label,
      AIC = "AIC",
      BIC = "BIC",
      nobs = "nobs",
      label
    )

    if (is.list(model) && !is.null(model[[fallback_name]])) {
      fallback_value <- suppressWarnings(as.numeric(model[[fallback_name]][[1]]))

      if (length(fallback_value) == 1L && is.finite(fallback_value)) {
        return(list(
          value = fallback_value,
          message = NA_character_
        ))
      }
    }

    return(list(
      value = NA_real_,
      message = paste0(label, ": ", out$error)
    ))
  }

  value <- suppressWarnings(as.numeric(out[[1]]))

  if (length(value) == 0L || !is.finite(value)) {
    return(list(
      value = NA_real_,
      message = paste0(label, ": could not extract a finite numeric value")
    ))
  }

  list(
    value = value,
    message = NA_character_
  )
}


.gp3_nested_error <- function(message) {
  structure(
    list(error = .gp3_nested_clean_error_message(message)),
    class = "gp3_nested_error"
  )
}

.gp3_nested_clean_error_message <- function(x) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.gp3_nested_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}
