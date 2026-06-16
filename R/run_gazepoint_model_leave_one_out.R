#' Run leave-one-unit model sensitivity analysis
#'
#' Refit the same model repeatedly while removing one participant, item,
#' stimulus, trial, or other analysis unit at a time. The helper compares
#' leave-one-out estimates with the full-data model to assess whether a key
#' effect is driven by a single unit.
#'
#' This is a generic robustness wrapper. It can be used with linear models,
#' GLMs, mixed models, GAMMs, GCA models, AOI GLMMs, pupil LMMs, or any custom
#' model as long as a fitting function is supplied.
#'
#' @param data A data frame used for model fitting.
#' @param unit_col Column identifying the unit to leave out, for example subject,
#'   participant, item, stimulus, or trial.
#' @param fit_function Function that takes one data frame argument and returns a
#'   fitted model.
#' @param extract_function Optional function that takes a fitted model and
#'   returns a data frame of effects. If `NULL`, a default coefficient extractor
#'   is used for common model objects.
#' @param effect_terms Optional character vector of terms/effects to retain in
#'   the sensitivity summary.
#' @param min_rows Minimum number of rows required after leaving one unit out.
#' @param keep_models Logical. If `TRUE`, keep the full model and refitted models.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_model_leave_one_out_sensitivity`.
#' @export
run_gazepoint_model_leave_one_out <- function(
    data,
    unit_col,
    fit_function,
    extract_function = NULL,
    effect_terms = NULL,
    min_rows = 2L,
    keep_models = FALSE,
    name = "gazepoint_model_leave_one_out"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_loo_check_col(unit_col, names(data), "unit_col")
  .gp3_loo_check_function(fit_function, "fit_function")

  if (!is.null(extract_function)) {
    .gp3_loo_check_function(extract_function, "extract_function")
  }

  if (!is.null(effect_terms)) {
    if (!is.character(effect_terms) || anyNA(effect_terms)) {
      stop("`effect_terms` must be NULL or a character vector.", call. = FALSE)
    }

    effect_terms <- unique(effect_terms[nzchar(effect_terms)])
  }

  .gp3_loo_check_positive_integer(min_rows, "min_rows")
  .gp3_loo_check_logical(keep_models, "keep_models")
  .gp3_loo_check_label(name, "name")

  unit_values <- as.character(data[[unit_col]])
  unit_values <- sort(unique(unit_values[!is.na(unit_values) & nzchar(unit_values)]))

  if (length(unit_values) < 2L) {
    stop("`unit_col` must contain at least two non-missing units.", call. = FALSE)
  }

  extractor <- if (is.null(extract_function)) {
    .gp3_loo_default_extract
  } else {
    extract_function
  }

  full_fit <- .gp3_loo_fit_model(
    data = data,
    fit_function = fit_function
  )

  if (inherits(full_fit, "gp3_loo_error")) {
    stop(
      "The full-data model could not be fitted: ",
      full_fit$error,
      call. = FALSE
    )
  }

  full_effects <- .gp3_loo_extract_effects(
    model = full_fit,
    extract_function = extractor,
    effect_terms = effect_terms
  )

  if (inherits(full_effects, "gp3_loo_error")) {
    stop(
      "Effects could not be extracted from the full-data model: ",
      full_effects$error,
      call. = FALSE
    )
  }

  full_effects$model_scope <- "full_data"
  full_effects$left_out_unit <- NA_character_
  full_effects$n_rows_used <- nrow(data)

  refit_rows <- vector("list", length(unit_values))
  refit_effects <- vector("list", length(unit_values))
  refit_models <- if (isTRUE(keep_models)) vector("list", length(unit_values)) else NULL

  for (i in seq_along(unit_values)) {
    unit <- unit_values[[i]]
    analysis_data <- data[as.character(data[[unit_col]]) != unit, , drop = FALSE]
    n_rows_removed <- nrow(data) - nrow(analysis_data)

    if (nrow(analysis_data) < min_rows) {
      refit_rows[[i]] <- tibble::tibble(
        left_out_unit = unit,
        n_rows_removed = n_rows_removed,
        n_rows_used = nrow(analysis_data),
        model_status = "skipped_too_few_rows",
        message = paste0("Fewer than ", min_rows, " rows remained after leaving this unit out.")
      )

      refit_effects[[i]] <- tibble::tibble(
        left_out_unit = character(0),
        term = character(0),
        estimate = numeric(0),
        std_error = numeric(0),
        statistic = numeric(0),
        p_value = numeric(0),
        conf_low = numeric(0),
        conf_high = numeric(0),
        model_scope = character(0),
        n_rows_used = integer(0)
      )

      if (isTRUE(keep_models)) {
        refit_models[[i]] <- NULL
      }

      next
    }

    fit <- .gp3_loo_fit_model(
      data = analysis_data,
      fit_function = fit_function
    )

    if (inherits(fit, "gp3_loo_error")) {
      refit_rows[[i]] <- tibble::tibble(
        left_out_unit = unit,
        n_rows_removed = n_rows_removed,
        n_rows_used = nrow(analysis_data),
        model_status = "fit_error",
        message = fit$error
      )

      refit_effects[[i]] <- tibble::tibble(
        left_out_unit = character(0),
        term = character(0),
        estimate = numeric(0),
        std_error = numeric(0),
        statistic = numeric(0),
        p_value = numeric(0),
        conf_low = numeric(0),
        conf_high = numeric(0),
        model_scope = character(0),
        n_rows_used = integer(0)
      )

      if (isTRUE(keep_models)) {
        refit_models[[i]] <- NULL
      }

      next
    }

    effects <- .gp3_loo_extract_effects(
      model = fit,
      extract_function = extractor,
      effect_terms = effect_terms
    )

    if (inherits(effects, "gp3_loo_error")) {
      refit_rows[[i]] <- tibble::tibble(
        left_out_unit = unit,
        n_rows_removed = n_rows_removed,
        n_rows_used = nrow(analysis_data),
        model_status = "extract_error",
        message = effects$error
      )

      refit_effects[[i]] <- tibble::tibble(
        left_out_unit = character(0),
        term = character(0),
        estimate = numeric(0),
        std_error = numeric(0),
        statistic = numeric(0),
        p_value = numeric(0),
        conf_low = numeric(0),
        conf_high = numeric(0),
        model_scope = character(0),
        n_rows_used = integer(0)
      )

      if (isTRUE(keep_models)) {
        refit_models[[i]] <- fit
      }

      next
    }

    effects$model_scope <- "leave_one_out"
    effects$left_out_unit <- unit
    effects$n_rows_used <- nrow(analysis_data)

    refit_rows[[i]] <- tibble::tibble(
      left_out_unit = unit,
      n_rows_removed = n_rows_removed,
      n_rows_used = nrow(analysis_data),
      model_status = "complete",
      message = NA_character_
    )

    refit_effects[[i]] <- effects

    if (isTRUE(keep_models)) {
      refit_models[[i]] <- fit
    }
  }

  names(refit_models) <- if (isTRUE(keep_models)) unit_values else NULL

  leave_one_results <- dplyr::bind_rows(refit_rows)
  leave_one_effects <- dplyr::bind_rows(refit_effects)

  effect_summary <- .gp3_loo_effect_summary(
    full_effects = full_effects,
    leave_one_effects = leave_one_effects
  )

  n_complete <- sum(leave_one_results$model_status == "complete")
  n_fit_error <- sum(leave_one_results$model_status == "fit_error")
  n_extract_error <- sum(leave_one_results$model_status == "extract_error")
  n_skipped <- sum(leave_one_results$model_status == "skipped_too_few_rows")

  sensitivity_status <- if (n_complete == length(unit_values)) {
    "complete"
  } else if (n_complete > 0L) {
    "partial_complete"
  } else {
    "failed"
  }

  overview <- tibble::tibble(
    object_name = name,
    sensitivity_status = sensitivity_status,
    unit_col = unit_col,
    n_input_rows = nrow(data),
    n_units = length(unit_values),
    n_complete = n_complete,
    n_fit_error = n_fit_error,
    n_extract_error = n_extract_error,
    n_skipped = n_skipped,
    n_effect_terms = length(unique(full_effects$term)),
    keep_models = keep_models
  )

  settings <- tibble::tibble(
    setting = c(
      "unit_col",
      "effect_terms",
      "min_rows",
      "keep_models",
      "name"
    ),
    value = c(
      unit_col,
      .gp3_loo_collapse_nullable(effect_terms),
      as.character(min_rows),
      as.character(keep_models),
      name
    )
  )

  out <- list(
    overview = overview,
    full_effects = full_effects,
    leave_one_results = leave_one_results,
    leave_one_effects = leave_one_effects,
    effect_summary = effect_summary,
    full_model = if (isTRUE(keep_models)) full_fit else NULL,
    refit_models = if (isTRUE(keep_models)) refit_models else NULL,
    settings = settings
  )

  class(out) <- c("gp3_model_leave_one_out_sensitivity", "list")

  out
}

.gp3_loo_fit_model <- function(data, fit_function) {
  tryCatch(
    fit_function(data),
    error = function(e) .gp3_loo_error(conditionMessage(e))
  )
}

.gp3_loo_extract_effects <- function(model, extract_function, effect_terms) {
  effects <- tryCatch(
    extract_function(model),
    error = function(e) .gp3_loo_error(conditionMessage(e))
  )

  if (inherits(effects, "gp3_loo_error")) {
    return(effects)
  }

  effects <- .gp3_loo_standardise_effects(effects)

  if (!is.null(effect_terms) && length(effect_terms) > 0L) {
    effects <- effects |>
      dplyr::filter(.data$term %in% effect_terms)
  }

  effects
}

.gp3_loo_default_extract <- function(model) {
  model_summary <- summary(model)

  coef_mat <- NULL

  if (!is.null(model_summary$coefficients)) {
    coef_mat <- model_summary$coefficients
  } else if (!is.null(model_summary$coef)) {
    coef_mat <- model_summary$coef
  }

  if (is.null(coef_mat)) {
    stop(
      "Could not locate a coefficient matrix in `summary(model)`; supply `extract_function`.",
      call. = FALSE
    )
  }

  coef_df <- as.data.frame(coef_mat)
  coef_df$term <- rownames(coef_mat)
  rownames(coef_df) <- NULL

  estimate_col <- names(coef_df)[[1]]
  se_col <- .gp3_loo_first_matching_col(
    names(coef_df),
    c("Std. Error", "Std.Error", "SE", "Std_Error", "std.error")
  )
  statistic_col <- .gp3_loo_first_matching_col(
    names(coef_df),
    c("t value", "z value", "t.value", "z.value", "statistic")
  )
  p_col <- .gp3_loo_first_matching_col(
    names(coef_df),
    c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p_value", "P>|t|", "P>|z|")
  )

  estimate <- suppressWarnings(as.numeric(coef_df[[estimate_col]]))
  std_error <- if (!is.null(se_col)) suppressWarnings(as.numeric(coef_df[[se_col]])) else NA_real_
  statistic <- if (!is.null(statistic_col)) suppressWarnings(as.numeric(coef_df[[statistic_col]])) else NA_real_
  p_value <- if (!is.null(p_col)) suppressWarnings(as.numeric(coef_df[[p_col]])) else NA_real_

  tibble::tibble(
    term = as.character(coef_df$term),
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = p_value,
    conf_low = estimate - stats::qnorm(0.975) * std_error,
    conf_high = estimate + stats::qnorm(0.975) * std_error
  )
}

.gp3_loo_standardise_effects <- function(effects) {
  if (!is.data.frame(effects)) {
    stop("Effect extraction must return a data frame.", call. = FALSE)
  }

  names_effects <- names(effects)

  term_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("term", "effect", "parameter", "coefficient")
  )

  estimate_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("estimate", "Estimate", "coef", "coefficient", "beta")
  )

  if (is.null(term_col)) {
    stop("Extracted effects must include a term/effect/parameter column.", call. = FALSE)
  }

  if (is.null(estimate_col)) {
    stop("Extracted effects must include an estimate/coef/beta column.", call. = FALSE)
  }

  se_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("std_error", "std.error", "SE", "Std. Error", "Std.Error")
  )

  statistic_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("statistic", "t", "z", "t.value", "z.value", "t value", "z value")
  )

  p_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("p_value", "p.value", "p", "Pr(>|t|)", "Pr(>|z|)")
  )

  conf_low_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("conf_low", "conf.low", "lower", "lower_ci", "ci_low")
  )

  conf_high_col <- .gp3_loo_first_matching_col(
    names_effects,
    c("conf_high", "conf.high", "upper", "upper_ci", "ci_high")
  )

  estimate <- suppressWarnings(as.numeric(effects[[estimate_col]]))
  std_error <- if (!is.null(se_col)) suppressWarnings(as.numeric(effects[[se_col]])) else NA_real_

  conf_low <- if (!is.null(conf_low_col)) {
    suppressWarnings(as.numeric(effects[[conf_low_col]]))
  } else {
    estimate - stats::qnorm(0.975) * std_error
  }

  conf_high <- if (!is.null(conf_high_col)) {
    suppressWarnings(as.numeric(effects[[conf_high_col]]))
  } else {
    estimate + stats::qnorm(0.975) * std_error
  }

  tibble::tibble(
    term = as.character(effects[[term_col]]),
    estimate = estimate,
    std_error = std_error,
    statistic = if (!is.null(statistic_col)) suppressWarnings(as.numeric(effects[[statistic_col]])) else NA_real_,
    p_value = if (!is.null(p_col)) suppressWarnings(as.numeric(effects[[p_col]])) else NA_real_,
    conf_low = conf_low,
    conf_high = conf_high
  )
}

.gp3_loo_effect_summary <- function(full_effects, leave_one_effects) {
  if (nrow(leave_one_effects) == 0L) {
    return(tibble::tibble(
      term = full_effects$term,
      full_estimate = full_effects$estimate,
      n_refits_complete = 0L,
      mean_leave_one_estimate = NA_real_,
      min_leave_one_estimate = NA_real_,
      max_leave_one_estimate = NA_real_,
      sd_leave_one_estimate = NA_real_,
      max_abs_change = NA_real_,
      max_abs_percent_change = NA_real_,
      largest_change_unit = NA_character_,
      sign_flip = NA
    ))
  }

  rows <- lapply(seq_len(nrow(full_effects)), function(i) {
    term_i <- full_effects$term[[i]]
    full_estimate <- full_effects$estimate[[i]]

    loo <- leave_one_effects |>
      dplyr::filter(.data$term == term_i)

    if (nrow(loo) == 0L) {
      return(tibble::tibble(
        term = term_i,
        full_estimate = full_estimate,
        n_refits_complete = 0L,
        mean_leave_one_estimate = NA_real_,
        min_leave_one_estimate = NA_real_,
        max_leave_one_estimate = NA_real_,
        sd_leave_one_estimate = NA_real_,
        max_abs_change = NA_real_,
        max_abs_percent_change = NA_real_,
        largest_change_unit = NA_character_,
        sign_flip = NA
      ))
    }

    change <- loo$estimate - full_estimate
    abs_change <- abs(change)

    largest_index <- which.max(abs_change)

    tibble::tibble(
      term = term_i,
      full_estimate = full_estimate,
      n_refits_complete = nrow(loo),
      mean_leave_one_estimate = mean(loo$estimate, na.rm = TRUE),
      min_leave_one_estimate = min(loo$estimate, na.rm = TRUE),
      max_leave_one_estimate = max(loo$estimate, na.rm = TRUE),
      sd_leave_one_estimate = stats::sd(loo$estimate, na.rm = TRUE),
      max_abs_change = max(abs_change, na.rm = TRUE),
      max_abs_percent_change = if (isTRUE(all.equal(full_estimate, 0))) {
        NA_real_
      } else {
        max(abs_change / abs(full_estimate) * 100, na.rm = TRUE)
      },
      largest_change_unit = loo$left_out_unit[[largest_index]],
      sign_flip = any(sign(loo$estimate) != sign(full_estimate), na.rm = TRUE)
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_loo_first_matching_col <- function(names_x, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names_x) {
      return(candidate)
    }
  }

  NULL
}

.gp3_loo_error <- function(message) {
  structure(
    list(error = .gp3_loo_clean_error_message(message)),
    class = "gp3_loo_error"
  )
}

.gp3_loo_clean_error_message <- function(x) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.gp3_loo_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_loo_check_function <- function(x, arg) {
  if (!is.function(x)) {
    stop("`", arg, "` must be a function.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_loo_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_loo_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_loo_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_loo_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
