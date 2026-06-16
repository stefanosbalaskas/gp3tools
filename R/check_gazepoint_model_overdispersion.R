#' Check model overdispersion
#'
#' Compute a Pearson-residual overdispersion diagnostic for models where this is
#' meaningful, especially binomial and count models.
#'
#' This helper supports `glm`, `lme4` GLMMs, and `mgcv` GAM/GAMM objects when
#' their family is binomial, quasibinomial, Poisson, quasipoisson, or
#' negative-binomial-like. Gaussian `lm`, `lmer`, and Gaussian GAM models return
#' a structured `not_applicable` diagnostic row.
#'
#' @param model A fitted model object, or a `gp3tools` fit object containing
#'   a `$model` element.
#' @param ratio_threshold Numeric threshold above which the model is flagged as
#'   overdispersed.
#' @param model_name Optional model label used in the returned table.
#'
#' @return A tibble with Pearson chi-square, residual degrees of freedom,
#'   dispersion ratio, overdispersion flag, diagnostic status, and message.
#' @export
check_gazepoint_model_overdispersion <- function(
    model,
    ratio_threshold = 1.2,
    model_name = NULL
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

  check_positive_numeric(ratio_threshold, "ratio_threshold")
  check_character_scalar(model_name, "model_name", allow_null = TRUE)

  extracted <- .gp3_extract_model_for_diagnostics(model)

  model <- extracted$model

  if (is.null(model_name)) {
    model_name <- extracted$model_name
  }

  model_class <- paste(class(model), collapse = "/")

  out <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    diagnostic = "overdispersion",
    dispersion_ratio = NA_real_,
    pearson_chisq = NA_real_,
    residual_df = NA_real_,
    overdispersed = NA,
    ratio_threshold = ratio_threshold,
    diagnostic_status = "not_available",
    message = NA_character_
  )

  is_supported_class <- inherits(model, c("glm", "merMod", "gam"))

  if (!is_supported_class) {
    if (inherits(model, "lm")) {
      out$diagnostic_status <- "not_applicable"
      out$message <- "Overdispersion diagnostics are not applicable to ordinary lm objects."
      return(out)
    }


    out$diagnostic_status <- "unsupported_model_class"
    out$message <- "Unsupported model class for overdispersion diagnostics."
    return(out)


  }

  family_info <- tryCatch(
    stats::family(model),
    error = function(e) NULL
  )

  if (is.null(family_info) ||
      is.null(family_info$family) ||
      is.na(family_info$family)) {
    out$diagnostic_status <- "not_available"
    out$message <- "Could not determine model family."
    return(out)
  }

  family_name <- tolower(as.character(family_info$family))

  applicable <- family_name %in% c(
    "binomial",
    "quasibinomial",
    "poisson",
    "quasipoisson"
  ) ||
    grepl("negative binomial", family_name, fixed = TRUE) ||
    grepl("nb", family_name, fixed = TRUE)

  if (!applicable) {
    out$diagnostic_status <- "not_applicable"
    out$message <- paste0(
      "Overdispersion diagnostics are not applied to family `",
      family_info$family,
      "`."
    )
    return(out)
  }

  pearson_residuals <- tryCatch(
    stats::residuals(model, type = "pearson"),
    error = function(e) {
      out$diagnostic_status <<- "error"
      out$message <<- paste0(
        "Could not compute Pearson residuals: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(pearson_residuals)) {
    return(out)
  }

  residual_df <- tryCatch(
    stats::df.residual(model),
    error = function(e) NA_real_
  )

  residual_df <- suppressWarnings(as.numeric(residual_df))

  if (length(residual_df) != 1L ||
      is.na(residual_df) ||
      !is.finite(residual_df) ||
      residual_df <= 0) {
    out$diagnostic_status <- "insufficient_residual_df"
    out$message <- "Residual degrees of freedom are missing, non-finite, or non-positive."
    return(out)
  }

  pearson_chisq <- sum(pearson_residuals^2, na.rm = TRUE)
  pearson_chisq <- suppressWarnings(as.numeric(pearson_chisq))

  if (length(pearson_chisq) != 1L ||
      is.na(pearson_chisq) ||
      !is.finite(pearson_chisq)) {
    out$diagnostic_status <- "not_available"
    out$message <- "Pearson chi-square statistic is missing or non-finite."
    return(out)
  }

  dispersion_ratio <- pearson_chisq / residual_df
  overdispersed <- dispersion_ratio > ratio_threshold

  out$pearson_chisq <- pearson_chisq
  out$residual_df <- residual_df
  out$dispersion_ratio <- dispersion_ratio
  out$overdispersed <- overdispersed

  if (isTRUE(overdispersed)) {
    out$diagnostic_status <- "overdispersed"
    out$message <- paste0(
      "Dispersion ratio exceeds threshold ",
      ratio_threshold,
      "."
    )
  } else {
    out$diagnostic_status <- "ok"
    out$message <- paste0(
      "Dispersion ratio does not exceed threshold ",
      ratio_threshold,
      "."
    )
  }

  out
}
