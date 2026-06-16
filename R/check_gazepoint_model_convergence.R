#' Check model convergence
#'
#' Check convergence status for fitted models used in `gp3tools` workflows.
#'
#' This helper supports `lme4` mixed models, `mgcv` GAM/GAMM objects,
#' `glm` objects, and `lm` objects where convergence is meaningful. It returns
#' a compact diagnostic table instead of printing model-specific messages.
#'
#' @param model A fitted model object, or a `gp3tools` fit object containing
#'   a `$model` element.
#' @param model_name Optional model label used in the returned table.
#'
#' @return A tibble with model class, convergence status, diagnostic status,
#'   and message.
#' @export
check_gazepoint_model_convergence <- function(model, model_name = NULL) {
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
      stop("`", arg, "` must be a non-missing character scalar.",
           call. = FALSE)
    }

    invisible(TRUE)


  }

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
    diagnostic = "convergence",
    converged = NA,
    diagnostic_status = "not_available",
    message = NA_character_
  )

  if (inherits(model, "merMod")) {
    optinfo <- tryCatch(
      model@optinfo,
      error = function(e) NULL
    )


    if (is.null(optinfo)) {
      out$diagnostic_status <- "not_available"
      out$message <- "Could not access lme4 optimizer information."
      return(out)
    }

    opt_code <- NA_integer_

    if (!is.null(optinfo$conv$opt)) {
      opt_code <- suppressWarnings(as.integer(optinfo$conv$opt))
    }

    lme4_messages <- optinfo$conv$lme4$messages

    if (is.null(lme4_messages)) {
      lme4_messages <- character()
    }

    lme4_messages <- as.character(lme4_messages)

    has_bad_code <- is.finite(opt_code) && opt_code != 0L
    has_messages <- length(lme4_messages) > 0L

    out$converged <- !(has_bad_code || has_messages)

    if (isTRUE(out$converged)) {
      out$diagnostic_status <- "ok"
      out$message <- "No lme4 convergence problems detected."
    } else {
      out$diagnostic_status <- "convergence_warning"

      msg <- character()

      if (has_bad_code) {
        msg <- c(msg, paste0("Optimizer convergence code: ", opt_code, "."))
      }

      if (has_messages) {
        msg <- c(msg, lme4_messages)
      }

      out$message <- paste(unique(msg), collapse = " | ")
    }

    return(out)


  }

  if (inherits(model, "gam")) {
    if (!is.null(model$converged)) {
      out$converged <- isTRUE(model$converged)


      if (isTRUE(out$converged)) {
        out$diagnostic_status <- "ok"
        out$message <- "mgcv model reports convergence."
      } else {
        out$diagnostic_status <- "convergence_warning"
        out$message <- "mgcv model reports non-convergence."
      }

      return(out)
    }

    out$diagnostic_status <- "not_available"
    out$message <- "The mgcv model does not expose a `converged` field."
    return(out)


  }

  if (inherits(model, "glm")) {
    if (!is.null(model$converged)) {
      out$converged <- isTRUE(model$converged)


      if (isTRUE(out$converged)) {
        out$diagnostic_status <- "ok"
        out$message <- "glm model reports convergence."
      } else {
        out$diagnostic_status <- "convergence_warning"
        out$message <- "glm model reports non-convergence."
      }

      return(out)
    }

    out$diagnostic_status <- "not_available"
    out$message <- "The glm object does not expose a `converged` field."
    return(out)


  }

  if (inherits(model, "lm")) {
    out$converged <- NA
    out$diagnostic_status <- "not_applicable"
    out$message <- "Convergence diagnostics are not applicable to ordinary lm objects."
    return(out)
  }

  out$diagnostic_status <- "unsupported_model_class"
  out$message <- "Unsupported model class for convergence diagnostics."
  out
}

.gp3_extract_model_for_diagnostics <- function(model) {
  if (is.null(model)) {
    stop("`model` must not be NULL.", call. = FALSE)
  }

  if (is.data.frame(model) || is.matrix(model) || is.atomic(model)) {
    stop("`model` must be a fitted model object or a gp3tools fit object.",
         call. = FALSE)
  }

  if (is.list(model) &&
      !inherits(model, c("lm", "glm", "gam", "merMod")) &&
      "model" %in% names(model)) {
    model_name <- if (!is.null(model$model_name) &&
                      is.character(model$model_name) &&
                      length(model$model_name) == 1L &&
                      !is.na(model$model_name) &&
                      nzchar(model$model_name)) {
      model$model_name
    } else {
      "model"
    }


    if (is.null(model$model)) {
      stop("`model$model` must not be NULL.", call. = FALSE)
    }

    return(list(model = model$model, model_name = model_name))


  }

  list(model = model, model_name = "model")
}
