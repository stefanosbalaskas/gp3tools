#' Check model singularity
#'
#' Check whether a fitted mixed model has a singular random-effects structure.
#'
#' This helper is primarily intended for `lme4` mixed models. For model classes
#' where singularity is not meaningful, such as `lm`, `glm`, and `mgcv` GAM
#' objects, it returns a structured `not_applicable` diagnostic row.
#'
#' @param model A fitted model object, or a `gp3tools` fit object containing
#'   a `$model` element.
#' @param tolerance Numeric tolerance passed to `lme4::isSingular()`.
#' @param model_name Optional model label used in the returned table.
#'
#' @return A tibble with model class, singular-fit status, diagnostic status,
#'   and message.
#' @export
check_gazepoint_model_singularity <- function(
    model,
    tolerance = 1e-4,
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

  check_positive_numeric(tolerance, "tolerance")
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
    diagnostic = "singularity",
    singular_fit = NA,
    tolerance = tolerance,
    diagnostic_status = "not_available",
    message = NA_character_
  )

  if (inherits(model, "merMod")) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      out$diagnostic_status <- "skipped_missing_package"
      out$message <- "Package `lme4` is required for singularity diagnostics."
      return(out)
    }


    singular_fit <- tryCatch(
      lme4::isSingular(model, tol = tolerance),
      error = function(e) {
        out$diagnostic_status <<- "error"
        out$message <<- conditionMessage(e)
        NA
      }
    )

    if (is.na(singular_fit)) {
      return(out)
    }

    out$singular_fit <- isTRUE(singular_fit)

    if (isTRUE(out$singular_fit)) {
      out$diagnostic_status <- "singular_fit"
      out$message <- "lme4 reports a singular random-effects structure."
    } else {
      out$diagnostic_status <- "ok"
      out$message <- "No singular random-effects structure detected."
    }

    return(out)


  }

  if (inherits(model, c("lm", "glm", "gam"))) {
    out$singular_fit <- NA
    out$diagnostic_status <- "not_applicable"
    out$message <- paste0(
      "Singularity diagnostics are not applicable to ",
      class(model)[[1L]],
      " objects."
    )


    return(out)


  }

  out$diagnostic_status <- "unsupported_model_class"
  out$message <- "Unsupported model class for singularity diagnostics."
  out
}
