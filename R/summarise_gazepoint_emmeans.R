#' Summarise estimated marginal means and contrasts
#'
#' Create manuscript-ready estimated marginal means and pairwise contrast tables
#' from fitted models used in `gp3tools` workflows.
#'
#' The function uses the optional `emmeans` package. If `emmeans` is not
#' installed, the function returns structured skipped tables rather than
#' failing. This keeps `emmeans` as an optional suggested dependency.
#'
#' @param model A fitted model object, or a `gp3tools` fit object containing
#'   a `$model` element.
#' @param specs Character vector or formula passed to `emmeans::emmeans()`.
#' @param by Optional character vector of grouping variables passed to
#'   `emmeans::emmeans()`.
#' @param model_name Optional model label used in returned tables.
#' @param type Scale passed to `emmeans` summaries. Common values are `"link"`
#'   and `"response"`.
#' @param contrast_method Contrast method passed to `emmeans::contrast()`.
#' @param adjust Multiplicity adjustment for contrasts.
#' @param conf_level Confidence level.
#' @param include_contrasts Logical. If `TRUE`, compute contrasts.
#'
#' @return A list with overview, emmeans, contrasts, and settings. The returned
#'   object has class `gp3_emmeans_summary`.
#' @export
summarise_gazepoint_emmeans <- function(
    model,
    specs,
    by = NULL,
    model_name = NULL,
    type = "response",
    contrast_method = "pairwise",
    adjust = "tukey",
    conf_level = 0.95,
    include_contrasts = TRUE
) {
  if (is.null(model)) {
    stop("`model` must not be NULL.", call. = FALSE)
  }

  if (missing(specs) || is.null(specs)) {
    stop("`specs` must be supplied.", call. = FALSE)
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

  check_character_vector <- function(x, arg, allow_null = FALSE) {
    if (is.null(x) && allow_null) {
      return(invisible(TRUE))
    }


    if (!is.character(x) ||
        length(x) < 1L ||
        any(is.na(x)) ||
        any(!nzchar(x))) {
      stop(
        "`", arg, "` must be a non-empty character vector.",
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
  check_character_scalar(type, "type")
  check_character_scalar(contrast_method, "contrast_method")
  check_character_scalar(adjust, "adjust")
  check_character_vector(by, "by", allow_null = TRUE)
  check_logical_scalar(include_contrasts, "include_contrasts")
  check_conf_level(conf_level)

  extracted <- .gp3_extract_model_for_diagnostics(model)

  fitted_model <- extracted$model

  if (is.null(model_name)) {
    model_name <- extracted$model_name
  }

  model_class <- paste(class(fitted_model), collapse = "/")
  specs_label <- .gp3_emmeans_label(specs)
  by_label <- .gp3_emmeans_by_label(by)

  if (!requireNamespace("emmeans", quietly = TRUE)) {
    out <- list(
      overview = tibble::tibble(
        model_name = model_name,
        model_class = model_class,
        specs = specs_label,
        by = by_label,
        n_emmeans = 0L,
        n_contrasts = 0L,
        emmeans_status = "skipped_missing_package",
        contrasts_status = "skipped_missing_package",
        summary_status = "skipped_missing_package",
        message = "Package `emmeans` is not installed."
      ),
      emmeans = .gp3_empty_emmeans_table(
        model_name = model_name,
        model_class = model_class,
        specs = specs_label,
        by = by_label,
        status = "skipped_missing_package",
        message = "Package `emmeans` is not installed."
      ),
      contrasts = .gp3_empty_contrasts_table(
        model_name = model_name,
        model_class = model_class,
        specs = specs_label,
        by = by_label,
        status = "skipped_missing_package",
        message = "Package `emmeans` is not installed."
      ),
      settings = list(
        specs = specs,
        by = by,
        type = type,
        contrast_method = contrast_method,
        adjust = adjust,
        conf_level = conf_level,
        include_contrasts = include_contrasts
      )
    )


    class(out) <- c("gp3_emmeans_summary", "list")
    return(out)


  }

  emmeans_fun <- getExportedValue("emmeans", "emmeans")
  contrast_fun <- getExportedValue("emmeans", "contrast")

  emm_grid <- tryCatch(
    suppressMessages(
      emmeans_fun(
        object = fitted_model,
        specs = specs,
        by = by
      )
    ),
    error = function(e) e
  )

  if (inherits(emm_grid, "error")) {
    emmeans_table <- .gp3_empty_emmeans_table(
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      status = "error",
      message = paste0("emmeans::emmeans() failed: ", conditionMessage(emm_grid))
    )


    contrasts_table <- .gp3_empty_contrasts_table(
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      status = "not_available",
      message = "Contrasts were not computed because estimated marginal means failed."
    )

    overview <- tibble::tibble(
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      n_emmeans = 0L,
      n_contrasts = 0L,
      emmeans_status = "error",
      contrasts_status = "not_available",
      summary_status = "error",
      message = emmeans_table$message[[1L]]
    )

    out <- list(
      overview = overview,
      emmeans = emmeans_table,
      contrasts = contrasts_table,
      settings = list(
        specs = specs,
        by = by,
        type = type,
        contrast_method = contrast_method,
        adjust = adjust,
        conf_level = conf_level,
        include_contrasts = include_contrasts
      )
    )

    class(out) <- c("gp3_emmeans_summary", "list")
    return(out)


  }

  emm_summary <- tryCatch(
    suppressMessages(
      summary(
        emm_grid,
        infer = c(TRUE, TRUE),
        level = conf_level,
        type = type
      )
    ),
    error = function(e) e
  )

  if (inherits(emm_summary, "error")) {
    emmeans_table <- .gp3_empty_emmeans_table(
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      status = "error",
      message = paste0("summary.emmGrid() failed: ", conditionMessage(emm_summary))
    )
  } else {
    emmeans_table <- .gp3_format_emmeans_table(
      emm_summary,
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      type = type
    )
  }

  contrasts_table <- if (isTRUE(include_contrasts) &&
                         !identical(emmeans_table$diagnostic_status[[1L]], "error")) {
    contrast_grid <- tryCatch(
      suppressMessages(
        contrast_fun(
          emm_grid,
          method = contrast_method,
          adjust = adjust
        )
      ),
      error = function(e) e
    )


    if (inherits(contrast_grid, "error")) {
      .gp3_empty_contrasts_table(
        model_name = model_name,
        model_class = model_class,
        specs = specs_label,
        by = by_label,
        status = "error",
        message = paste0("emmeans::contrast() failed: ", conditionMessage(contrast_grid))
      )
    } else {
      contrast_summary <- tryCatch(
        suppressMessages(
          summary(
            contrast_grid,
            infer = c(TRUE, TRUE),
            level = conf_level,
            type = type
          )
        ),
        error = function(e) e
      )

      if (inherits(contrast_summary, "error")) {
        .gp3_empty_contrasts_table(
          model_name = model_name,
          model_class = model_class,
          specs = specs_label,
          by = by_label,
          status = "error",
          message = paste0("summary.emmGrid() for contrasts failed: ", conditionMessage(contrast_summary))
        )
      } else {
        .gp3_format_contrasts_table(
          contrast_summary,
          model_name = model_name,
          model_class = model_class,
          specs = specs_label,
          by = by_label,
          type = type,
          contrast_method = contrast_method,
          adjust = adjust
        )
      }
    }


  } else if (!isTRUE(include_contrasts)) {
    .gp3_empty_contrasts_table(
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      status = "skipped_disabled",
      message = "Contrast computation was disabled."
    )
  } else {
    .gp3_empty_contrasts_table(
      model_name = model_name,
      model_class = model_class,
      specs = specs_label,
      by = by_label,
      status = "not_available",
      message = "Contrasts were not computed because estimated marginal means failed."
    )
  }

  emmeans_status <- .gp3_summary_table_status(emmeans_table)
  contrasts_status <- .gp3_summary_table_status(contrasts_table)

  summary_status <- .gp3_emmeans_summary_status(
    emmeans_status = emmeans_status,
    contrasts_status = contrasts_status,
    include_contrasts = include_contrasts
  )

  overview <- tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    specs = specs_label,
    by = by_label,
    n_emmeans = sum(emmeans_table$diagnostic_status == "ok", na.rm = TRUE),
    n_contrasts = sum(contrasts_table$diagnostic_status == "ok", na.rm = TRUE),
    emmeans_status = emmeans_status,
    contrasts_status = contrasts_status,
    summary_status = summary_status,
    message = .gp3_collapse_messages(c(
      unique(emmeans_table$message),
      unique(contrasts_table$message)
    ))
  )

  out <- list(
    overview = overview,
    emmeans = emmeans_table,
    contrasts = contrasts_table,
    settings = list(
      specs = specs,
      by = by,
      type = type,
      contrast_method = contrast_method,
      adjust = adjust,
      conf_level = conf_level,
      include_contrasts = include_contrasts
    )
  )

  class(out) <- c("gp3_emmeans_summary", "list")

  out
}

.gp3_emmeans_label <- function(specs) {
  if (inherits(specs, "formula")) {
    return(paste(deparse(specs), collapse = " "))
  }

  paste(as.character(specs), collapse = ", ")
}

.gp3_emmeans_by_label <- function(by) {
  if (is.null(by)) {
    return(NA_character_)
  }

  paste(as.character(by), collapse = ", ")
}

.gp3_first_existing_column <- function(tab, candidates) {
  matched <- intersect(candidates, names(tab))

  if (length(matched) == 0L) {
    return(NULL)
  }

  matched[[1L]]
}

.gp3_format_emmeans_table <- function(
    tab,
    model_name,
    model_class,
    specs,
    by,
    type
) {
  tab <- as.data.frame(tab)

  estimate_col <- .gp3_first_existing_column(
    tab,
    c("response", "prob", "rate", "emmean", "estimate")
  )

  se_col <- .gp3_first_existing_column(
    tab,
    c("SE", "std.error", "std_error")
  )

  df_col <- .gp3_first_existing_column(
    tab,
    c("df")
  )

  lower_col <- .gp3_first_existing_column(
    tab,
    c("asymp.LCL", "lower.CL", "LCL")
  )

  upper_col <- .gp3_first_existing_column(
    tab,
    c("asymp.UCL", "upper.CL", "UCL")
  )

  statistic_col <- .gp3_first_existing_column(
    tab,
    c("z.ratio", "t.ratio", "statistic")
  )

  p_col <- .gp3_first_existing_column(
    tab,
    c("p.value", "p_value")
  )

  level_cols <- setdiff(
    names(tab),
    c(
      estimate_col,
      se_col,
      df_col,
      lower_col,
      upper_col,
      statistic_col,
      p_col
    )
  )

  level_cols <- setdiff(level_cols, ".wgt.")

  term <- if (length(level_cols) > 0L) {
    apply(tab[, level_cols, drop = FALSE], 1, function(row) {
      paste(paste(level_cols, row, sep = "="), collapse = "; ")
    })
  } else {
    rep(specs, nrow(tab))
  }

  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    table_type = "emmeans",
    specs = specs,
    by = by,
    term = as.character(term),
    estimate = .gp3_get_numeric_or_na(tab, estimate_col),
    std_error = .gp3_get_numeric_or_na(tab, se_col),
    df = .gp3_get_numeric_or_na(tab, df_col),
    statistic = .gp3_get_numeric_or_na(tab, statistic_col),
    p_value = .gp3_get_numeric_or_na(tab, p_col),
    conf_low = .gp3_get_numeric_or_na(tab, lower_col),
    conf_high = .gp3_get_numeric_or_na(tab, upper_col),
    response_scale = type,
    significance = .gp3_p_stars(.gp3_get_numeric_or_na(tab, p_col)),
    diagnostic_status = "ok",
    message = "Estimated marginal means extracted."
  )
}

.gp3_format_contrasts_table <- function(
    tab,
    model_name,
    model_class,
    specs,
    by,
    type,
    contrast_method,
    adjust
) {
  tab <- as.data.frame(tab)

  contrast_col <- .gp3_first_existing_column(
    tab,
    c("contrast")
  )

  estimate_col <- .gp3_first_existing_column(
    tab,
    c("estimate", "odds.ratio", "ratio")
  )

  se_col <- .gp3_first_existing_column(
    tab,
    c("SE", "std.error", "std_error")
  )

  df_col <- .gp3_first_existing_column(
    tab,
    c("df")
  )

  lower_col <- .gp3_first_existing_column(
    tab,
    c("asymp.LCL", "lower.CL", "LCL")
  )

  upper_col <- .gp3_first_existing_column(
    tab,
    c("asymp.UCL", "upper.CL", "UCL")
  )

  statistic_col <- .gp3_first_existing_column(
    tab,
    c("z.ratio", "t.ratio", "statistic")
  )

  p_col <- .gp3_first_existing_column(
    tab,
    c("p.value", "p_value")
  )

  contrast <- if (!is.null(contrast_col)) {
    as.character(tab[[contrast_col]])
  } else {
    rep(NA_character_, nrow(tab))
  }

  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    table_type = "contrasts",
    specs = specs,
    by = by,
    contrast = contrast,
    estimate = .gp3_get_numeric_or_na(tab, estimate_col),
    std_error = .gp3_get_numeric_or_na(tab, se_col),
    df = .gp3_get_numeric_or_na(tab, df_col),
    statistic = .gp3_get_numeric_or_na(tab, statistic_col),
    p_value = .gp3_get_numeric_or_na(tab, p_col),
    conf_low = .gp3_get_numeric_or_na(tab, lower_col),
    conf_high = .gp3_get_numeric_or_na(tab, upper_col),
    response_scale = type,
    contrast_method = contrast_method,
    adjustment = adjust,
    significance = .gp3_p_stars(.gp3_get_numeric_or_na(tab, p_col)),
    diagnostic_status = "ok",
    message = "Contrasts extracted."
  )
}

.gp3_get_numeric_or_na <- function(tab, col) {
  if (is.null(col) || !col %in% names(tab)) {
    return(rep(NA_real_, nrow(tab)))
  }

  suppressWarnings(as.numeric(tab[[col]]))
}

.gp3_empty_emmeans_table <- function(
    model_name,
    model_class,
    specs,
    by,
    status,
    message
) {
  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    table_type = "emmeans",
    specs = specs,
    by = by,
    term = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    df = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    response_scale = NA_character_,
    significance = NA_character_,
    diagnostic_status = status,
    message = message
  )
}

.gp3_empty_contrasts_table <- function(
    model_name,
    model_class,
    specs,
    by,
    status,
    message
) {
  tibble::tibble(
    model_name = model_name,
    model_class = model_class,
    table_type = "contrasts",
    specs = specs,
    by = by,
    contrast = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    df = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    response_scale = NA_character_,
    contrast_method = NA_character_,
    adjustment = NA_character_,
    significance = NA_character_,
    diagnostic_status = status,
    message = message
  )
}

.gp3_emmeans_summary_status <- function(
    emmeans_status,
    contrasts_status,
    include_contrasts
) {
  if (emmeans_status %in% c("error", "unsupported_model_class")) {
    return("error")
  }

  if (!include_contrasts) {
    return(emmeans_status)
  }

  if (contrasts_status %in% c("error", "unsupported_model_class")) {
    return("contrast_error")
  }

  if (emmeans_status == "ok" &&
      contrasts_status %in% c("ok", "skipped_disabled")) {
    return("ok")
  }

  if (emmeans_status == "skipped_missing_package") {
    return("skipped_missing_package")
  }

  "not_available"
}
