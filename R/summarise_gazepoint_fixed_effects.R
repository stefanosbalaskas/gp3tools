#' Summarise fixed effects from fitted models
#'
#' Create a compact manuscript-ready fixed-effect summary table from common
#' models used in `gp3tools` workflows.
#'
#' The function supports `lm`, `glm`, `lme4` mixed models, and `mgcv` GAM/BAM
#' objects. It can also accept a `gp3tools` fit object containing a `$model`
#' element. Confidence intervals are computed using a Wald approximation from
#' the estimate and standard error so that the function remains lightweight and
#' fast for mixed models.
#'
#' @param model A fitted model object, or a `gp3tools` fit object containing
#'   a `$model` element.
#' @param model_name Optional model label used in the returned table.
#' @param conf_level Confidence level for Wald confidence intervals.
#' @param exponentiate Logical. If `TRUE`, exponentiate estimates and
#'   confidence intervals. This is useful for logistic models when reporting
#'   odds ratios.
#' @param drop_intercept Logical. If `TRUE`, remove the intercept row.
#'
#' @return A tibble with fixed-effect estimates, standard errors, test
#'   statistics, p-values when available, confidence intervals, significance
#'   stars, and status fields.
#' @export
summarise_gazepoint_fixed_effects <- function(
    model,
    model_name = NULL,
    conf_level = 0.95,
    exponentiate = FALSE,
    drop_intercept = FALSE
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

  check_character_scalar(model_name, "model_name", allow_null = TRUE)
  check_conf_level(conf_level)
  check_logical_scalar(exponentiate, "exponentiate")
  check_logical_scalar(drop_intercept, "drop_intercept")

  extracted <- .gp3_extract_model_for_diagnostics(model)

  fitted_model <- extracted$model

  if (is.null(model_name)) {
    model_name <- extracted$model_name
  }

  model_class <- paste(class(fitted_model), collapse = "/")

  unsupported <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    term = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    statistic_type = NA_character_,
    df = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    response_scale = if (isTRUE(exponentiate)) {
      "exponentiated"
    } else {
      "link_or_original"
    },
    significance = NA_character_,
    diagnostic_status = "unsupported_model_class",
    message = "Unsupported model class for fixed-effect summaries."
  )

  if (!inherits(fitted_model, c("lm", "glm", "merMod", "gam"))) {
    return(unsupported)
  }

  coef_table <- tryCatch(
    .gp3_get_fixed_effect_matrix(fitted_model),
    error = function(e) {
      attr(unsupported, "error_message") <- conditionMessage(e)
      NULL
    }
  )

  if (is.null(coef_table)) {
    unsupported$diagnostic_status <- "error"
    unsupported$message <- "Could not extract fixed-effect coefficient table."
    return(unsupported)
  }

  if (nrow(coef_table) == 0L) {
    unsupported$diagnostic_status <- "not_available"
    unsupported$message <- "No fixed-effect coefficients were available."
    return(unsupported)
  }

  coef_df <- as.data.frame(coef_table)
  coef_df$term <- rownames(coef_table)
  rownames(coef_df) <- NULL

  estimate <- .gp3_numeric_column(
    coef_df,
    c("Estimate", "estimate")
  )

  std_error <- .gp3_numeric_column(
    coef_df,
    c("Std. Error", "Std Error", "SE", "se", "std.error")
  )

  statistic <- .gp3_numeric_column(
    coef_df,
    c("t value", "z value", "F", "statistic")
  )

  p_value <- .gp3_numeric_column(
    coef_df,
    c("Pr(>|t|)", "Pr(>|z|)", "Pr(>F)", "p-value", "p_value", "p.value")
  )

  statistic_type <- .gp3_statistic_type(names(coef_df))

  df <- rep(NA_real_, nrow(coef_df))

  if (inherits(fitted_model, "lm") && !inherits(fitted_model, "glm")) {
    df_value <- tryCatch(
      stats::df.residual(fitted_model),
      error = function(e) NA_real_
    )


    df <- rep(suppressWarnings(as.numeric(df_value)), nrow(coef_df))


  }

  alpha <- 1 - conf_level
  z_value <- stats::qnorm(1 - alpha / 2)

  conf_low <- estimate - z_value * std_error
  conf_high <- estimate + z_value * std_error

  response_scale <- "link_or_original"

  if (isTRUE(exponentiate)) {
    estimate <- exp(estimate)
    conf_low <- exp(conf_low)
    conf_high <- exp(conf_high)
    response_scale <- "exponentiated"
  }

  out <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    term = as.character(coef_df$term),
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    statistic_type = statistic_type,
    df = df,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high,
    response_scale = response_scale,
    significance = .gp3_p_stars(p_value),
    diagnostic_status = "ok",
    message = "Fixed-effect summary extracted."
  )

  if (isTRUE(drop_intercept)) {
    out <- dplyr::filter(
      out,
      !.data$term %in% c("(Intercept)", "Intercept", "intercept")
    )
  }

  if (nrow(out) == 0L) {
    return(tibble::tibble(
      model_name = model_name,
      model_class = model_class,
      term = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      statistic_type = NA_character_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      response_scale = response_scale,
      significance = NA_character_,
      diagnostic_status = "not_available",
      message = "No fixed-effect rows remained after filtering."
    ))
  }

  out
}

.gp3_get_fixed_effect_matrix <- function(model) {
  if (inherits(model, "gam")) {
    model_summary <- summary(model)


    if (!is.null(model_summary$p.table)) {
      return(model_summary$p.table)
    }

    stop("The mgcv model summary did not contain a parametric coefficient table.",
         call. = FALSE)


  }

  model_summary <- summary(model)

  if (!is.null(model_summary$coefficients)) {
    return(model_summary$coefficients)
  }

  if (!is.null(model_summary$coef)) {
    return(model_summary$coef)
  }

  stop("The model summary did not contain a coefficient table.", call. = FALSE)
}

.gp3_numeric_column <- function(tab, candidates) {
  matched <- intersect(candidates, names(tab))

  if (length(matched) == 0L) {
    return(rep(NA_real_, nrow(tab)))
  }

  suppressWarnings(as.numeric(tab[[matched[[1L]]]]))
}

.gp3_statistic_type <- function(column_names) {
  if ("t value" %in% column_names) {
    return("t")
  }

  if ("z value" %in% column_names) {
    return("z")
  }

  if ("F" %in% column_names) {
    return("F")
  }

  if ("statistic" %in% column_names) {
    return("statistic")
  }

  NA_character_
}

.gp3_p_stars <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.10 ~ ".",
    TRUE ~ ""
  )
}
