#' Fit optional negative-binomial transition-count sensitivity models
#'
#' Fit an optional negative-binomial sensitivity model for AOI/state transition
#' counts using `glmmTMB` when it is installed. This helper is intended as a
#' publication sensitivity branch for overdispersed transition-count outcomes.
#'
#' The helper keeps `glmmTMB` optional. If `glmmTMB` is not installed, it returns
#' a structured skipped object rather than failing.
#'
#' @param data A data frame containing transition-count rows.
#' @param count_col Transition-count outcome column. If `NULL`, common count
#'   columns are detected automatically.
#' @param from_col Transition origin column. If `NULL`, common origin columns are
#'   detected automatically.
#' @param to_col Transition destination column. If `NULL`, common destination
#'   columns are detected automatically.
#' @param condition_cols Optional fixed-effect condition columns.
#' @param random_effect_cols Optional random-intercept grouping columns.
#' @param exposure_col Optional positive exposure column. If supplied, the model
#'   includes `offset(log(exposure_col))`.
#' @param offset_col Optional numeric offset column. Use either `exposure_col` or
#'   `offset_col`, not both.
#' @param formula Optional model formula. If `NULL`, a formula is constructed
#'   from transition origin, destination, condition columns, optional offset, and
#'   random intercepts.
#' @param family Negative-binomial family. Options are `"nbinom2"` and
#'   `"nbinom1"`.
#' @param zero_inflation Logical. If `TRUE`, use `ziformula = ~1` unless
#'   `ziformula` is supplied.
#' @param ziformula Optional zero-inflation formula passed to `glmmTMB`.
#' @param dispformula Optional dispersion formula passed to `glmmTMB`.
#' @param control Optional control object passed to `glmmTMB`.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_transition_count_nb_sensitivity`.
#' @export
fit_gazepoint_transition_count_nb_sensitivity <- function(
    data,
    count_col = NULL,
    from_col = NULL,
    to_col = NULL,
    condition_cols = NULL,
    random_effect_cols = NULL,
    exposure_col = NULL,
    offset_col = NULL,
    formula = NULL,
    family = c("nbinom2", "nbinom1"),
    zero_inflation = FALSE,
    ziformula = NULL,
    dispformula = NULL,
    control = NULL,
    name = "gazepoint_transition_count_nb_sensitivity"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  family <- match.arg(family)

  if (!is.logical(zero_inflation) || length(zero_inflation) != 1L || is.na(zero_inflation)) {
    stop("`zero_inflation` must be TRUE or FALSE.", call. = FALSE)
  }

  .gp3_transition_nb_check_label(name, "name")

  names_data <- names(data)

  count_col <- .gp3_transition_nb_resolve_or_detect_col(
    col = count_col,
    names_data = names_data,
    arg = "count_col",
    candidates = c(
      "transition_count",
      "n_transitions",
      "transition_n",
      "count",
      "n",
      "freq",
      "frequency"
    ),
    required = TRUE
  )

  from_col <- .gp3_transition_nb_resolve_or_detect_col(
    col = from_col,
    names_data = names_data,
    arg = "from_col",
    candidates = c(
      "from_aoi",
      "from_state",
      "from",
      "origin",
      "previous_aoi",
      "previous_state",
      "AOI_from"
    ),
    required = TRUE
  )

  to_col <- .gp3_transition_nb_resolve_or_detect_col(
    col = to_col,
    names_data = names_data,
    arg = "to_col",
    candidates = c(
      "to_aoi",
      "to_state",
      "to",
      "destination",
      "next_aoi",
      "next_state",
      "AOI_to"
    ),
    required = TRUE
  )

  if (!is.null(condition_cols)) {
    condition_cols <- .gp3_transition_nb_resolve_cols_allow_empty(
      condition_cols,
      names_data,
      "condition_cols"
    )
  } else {
    condition_cols <- character(0)
  }

  if (!is.null(random_effect_cols)) {
    random_effect_cols <- .gp3_transition_nb_resolve_cols_allow_empty(
      random_effect_cols,
      names_data,
      "random_effect_cols"
    )
  } else {
    random_effect_cols <- .gp3_transition_nb_detect_random_cols(names_data)
  }

  if (!is.null(exposure_col)) {
    exposure_col <- .gp3_transition_nb_resolve_col(
      exposure_col,
      names_data,
      "exposure_col"
    )
  }

  if (!is.null(offset_col)) {
    offset_col <- .gp3_transition_nb_resolve_col(
      offset_col,
      names_data,
      "offset_col"
    )
  }

  if (!is.null(exposure_col) && !is.null(offset_col)) {
    stop("Use either `exposure_col` or `offset_col`, not both.", call. = FALSE)
  }

  model_data <- tibble::as_tibble(data)

  model_data$.gp3_transition_count_nb_response <- suppressWarnings(
    as.numeric(model_data[[count_col]])
  )

  .gp3_transition_nb_check_count_response(
    model_data$.gp3_transition_count_nb_response,
    "count_col"
  )

  model_data$.gp3_transition_from <- as.factor(model_data[[from_col]])
  model_data$.gp3_transition_to <- as.factor(model_data[[to_col]])
  model_data$.gp3_transition_pair <- interaction(
    model_data$.gp3_transition_from,
    model_data$.gp3_transition_to,
    sep = " -> ",
    drop = TRUE
  )

  for (col in condition_cols) {
    model_data[[col]] <- as.factor(model_data[[col]])
  }

  for (col in random_effect_cols) {
    model_data[[col]] <- as.factor(model_data[[col]])
  }

  if (!is.null(exposure_col)) {
    model_data$.gp3_transition_nb_offset <- log(
      suppressWarnings(as.numeric(model_data[[exposure_col]]))
    )

    if (
      anyNA(model_data$.gp3_transition_nb_offset) ||
      any(!is.finite(model_data$.gp3_transition_nb_offset))
    ) {
      stop("`exposure_col` must contain finite positive values.", call. = FALSE)
    }
  }

  if (!is.null(offset_col)) {
    model_data$.gp3_transition_nb_offset <- suppressWarnings(
      as.numeric(model_data[[offset_col]])
    )

    if (
      anyNA(model_data$.gp3_transition_nb_offset) ||
      any(!is.finite(model_data$.gp3_transition_nb_offset))
    ) {
      stop("`offset_col` must contain finite numeric values.", call. = FALSE)
    }
  }

  if (is.null(formula)) {
    formula <- .gp3_transition_nb_build_formula(
      condition_cols = condition_cols,
      random_effect_cols = random_effect_cols,
      has_offset = !is.null(exposure_col) || !is.null(offset_col)
    )
  } else {
    if (!inherits(formula, "formula")) {
      stop("`formula` must be a formula when supplied.", call. = FALSE)
    }
  }

  glmmTMB_available <- .gp3_transition_nb_namespace_available()

  settings <- tibble::tibble(
    setting = c(
      "count_col",
      "from_col",
      "to_col",
      "condition_cols",
      "random_effect_cols",
      "exposure_col",
      "offset_col",
      "formula",
      "family",
      "zero_inflation",
      "ziformula",
      "dispformula",
      "name"
    ),
    value = c(
      count_col,
      from_col,
      to_col,
      .gp3_transition_nb_collapse_nullable(condition_cols),
      .gp3_transition_nb_collapse_nullable(random_effect_cols),
      .gp3_transition_nb_collapse_nullable(exposure_col),
      .gp3_transition_nb_collapse_nullable(offset_col),
      paste(deparse(formula), collapse = " "),
      family,
      as.character(zero_inflation),
      .gp3_transition_nb_collapse_nullable(
        if (is.null(ziformula)) NULL else paste(deparse(ziformula), collapse = " ")
      ),
      .gp3_transition_nb_collapse_nullable(
        if (is.null(dispformula)) NULL else paste(deparse(dispformula), collapse = " ")
      ),
      name
    )
  )

  if (!glmmTMB_available) {
    overview <- .gp3_transition_nb_overview(
      name = name,
      model_data = model_data,
      formula = formula,
      family = family,
      status = "skipped_missing_package",
      message = "Optional package 'glmmTMB' is not installed.",
      model = NULL
    )

    out <- list(
      overview = overview,
      model = NULL,
      model_summary = NULL,
      fixed_effects = NULL,
      random_effects = NULL,
      model_data = model_data,
      settings = settings
    )

    class(out) <- c("gp3_transition_count_nb_sensitivity", "list")
    return(out)
  }

  glmmTMB_fun <- .gp3_transition_nb_get_export("glmmTMB")
  family_fun <- .gp3_transition_nb_get_export(family)

  if (is.null(ziformula)) {
    ziformula <- if (isTRUE(zero_inflation)) {
      stats::as.formula("~1")
    } else {
      stats::as.formula("~0")
    }
  }

  if (is.null(dispformula)) {
    dispformula <- stats::as.formula("~1")
  }

  model <- tryCatch({
    model_args <- list(
      formula = formula,
      data = model_data,
      family = family_fun(link = "log"),
      ziformula = ziformula,
      dispformula = dispformula
    )

    if (!is.null(control)) {
      model_args$control <- control
    }

    do.call(glmmTMB_fun, model_args)
  }, error = function(e) {
    .gp3_transition_nb_error(conditionMessage(e))
  })

  if (inherits(model, "gp3_transition_nb_error")) {
    overview <- .gp3_transition_nb_overview(
      name = name,
      model_data = model_data,
      formula = formula,
      family = family,
      status = "error_model_fit",
      message = model$error,
      model = NULL
    )

    out <- list(
      overview = overview,
      model = NULL,
      model_summary = NULL,
      fixed_effects = NULL,
      random_effects = NULL,
      model_data = model_data,
      settings = settings
    )

    class(out) <- c("gp3_transition_count_nb_sensitivity", "list")
    return(out)
  }

  model_summary <- tryCatch({
    summary(model)
  }, error = function(e) {
    .gp3_transition_nb_error(conditionMessage(e))
  })

  fixed_effects <- if (inherits(model_summary, "gp3_transition_nb_error")) {
    NULL
  } else {
    .gp3_transition_nb_fixed_effects(model_summary)
  }

  random_effects <- .gp3_transition_nb_random_effects(model)

  overview <- .gp3_transition_nb_overview(
    name = name,
    model_data = model_data,
    formula = formula,
    family = family,
    status = "complete",
    message = "Negative-binomial transition-count sensitivity model fitted.",
    model = model
  )

  out <- list(
    overview = overview,
    model = model,
    model_summary = if (inherits(model_summary, "gp3_transition_nb_error")) NULL else model_summary,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    model_data = model_data,
    settings = settings
  )

  class(out) <- c("gp3_transition_count_nb_sensitivity", "list")

  out
}

.gp3_transition_nb_build_formula <- function(
    condition_cols,
    random_effect_cols,
    has_offset
) {
  fixed_terms <- c(".gp3_transition_pair", condition_cols)

  if (has_offset) {
    fixed_terms <- c(fixed_terms, "offset(.gp3_transition_nb_offset)")
  }

  random_terms <- if (length(random_effect_cols) > 0L) {
    paste0("(1 | ", random_effect_cols, ")")
  } else {
    character(0)
  }

  rhs <- paste(c(fixed_terms, random_terms), collapse = " + ")

  stats::as.formula(
    paste(".gp3_transition_count_nb_response ~", rhs)
  )
}

.gp3_transition_nb_overview <- function(
    name,
    model_data,
    formula,
    family,
    status,
    message,
    model
) {
  tibble::tibble(
    object_name = name,
    n_rows = nrow(model_data),
    n_positive_counts = sum(model_data$.gp3_transition_count_nb_response > 0),
    n_zero_counts = sum(model_data$.gp3_transition_count_nb_response == 0),
    mean_count = mean(model_data$.gp3_transition_count_nb_response),
    variance_count = stats::var(model_data$.gp3_transition_count_nb_response),
    variance_to_mean_ratio = stats::var(model_data$.gp3_transition_count_nb_response) /
      mean(model_data$.gp3_transition_count_nb_response),
    formula = paste(deparse(formula), collapse = " "),
    family = family,
    model_status = status,
    message = message,
    AIC = .gp3_transition_nb_model_metric(model, "AIC"),
    BIC = .gp3_transition_nb_model_metric(model, "BIC"),
    logLik = .gp3_transition_nb_model_metric(model, "logLik")
  )
}

.gp3_transition_nb_model_metric <- function(model, metric) {
  if (is.null(model)) {
    return(NA_real_)
  }

  out <- tryCatch({
    switch(
      metric,
      AIC = stats::AIC(model),
      BIC = stats::BIC(model),
      logLik = as.numeric(stats::logLik(model)),
      NA_real_
    )
  }, error = function(e) {
    NA_real_
  })

  as.numeric(out)
}

.gp3_transition_nb_fixed_effects <- function(model_summary) {
  coef_obj <- tryCatch({
    model_summary$coefficients$cond
  }, error = function(e) {
    NULL
  })

  if (is.null(coef_obj)) {
    return(NULL)
  }

  coef_df <- as.data.frame(coef_obj)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL

  names(coef_df) <- gsub(" ", "_", names(coef_df), fixed = TRUE)

  tibble::as_tibble(coef_df) |>
    dplyr::select("term", dplyr::everything())
  }

.gp3_transition_nb_random_effects <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }

  vc <- tryCatch({
    if (
      .gp3_transition_nb_namespace_available() &&
      "VarCorr" %in% getNamespaceExports("glmmTMB")
    ) {
      varcorr_fun <- getExportedValue("glmmTMB", "VarCorr")
      varcorr_fun(model)
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })

  if (is.null(vc)) {
    return(NULL)
  }

  tibble::tibble(
    component = "VarCorr",
    summary = paste(utils::capture.output(print(vc)), collapse = "\n")
  )
}

.gp3_transition_nb_detect_random_cols <- function(names_data) {
  candidates <- c(
    "subject",
    "participant",
    "participant_id",
    "USER_FILE",
    "recording_id"
  )

  intersect(candidates, names_data)[1L] |>
    stats::na.omit() |>
    as.character()
}

.gp3_transition_nb_check_count_response <- function(x, arg) {
  if (anyNA(x) || any(!is.finite(x))) {
    stop("`", arg, "` must contain finite non-missing counts.", call. = FALSE)
  }

  if (any(x < 0)) {
    stop("`", arg, "` must contain non-negative counts.", call. = FALSE)
  }

  if (any(abs(x - round(x)) > .Machine$double.eps^0.5)) {
    stop("`", arg, "` must contain integer-valued counts.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_transition_nb_namespace_available <- function() {
  requireNamespace("glmmTMB", quietly = TRUE)
}

.gp3_transition_nb_get_export <- function(function_name) {
  getExportedValue("glmmTMB", function_name)
}

.gp3_transition_nb_resolve_cols_allow_empty <- function(cols, names_data, arg) {
  if (!is.character(cols) || anyNA(cols)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }

  if (length(cols) == 0L) {
    return(character(0))
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_transition_nb_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_transition_nb_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_transition_nb_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_transition_nb_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_transition_nb_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}

.gp3_transition_nb_clean_error_message <- function(x) {
  x <- gsub("\033\\[[0-9;]*m", "", x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.gp3_transition_nb_error <- function(message) {
  structure(
    list(error = .gp3_transition_nb_clean_error_message(message)),
    class = "gp3_transition_nb_error"
  )
}
