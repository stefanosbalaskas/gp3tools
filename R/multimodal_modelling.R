#' Prepare multimodal Gazepoint and external face-window data
#'
#' Joins facial-behaviour window summaries with optional Gazepoint-derived
#' summaries, response variables, or covariates. The helper is intentionally
#' conservative: it prepares transparent analysis tables and optional scaled
#' predictors, but it does not infer emotional states or causal effects.
#'
#' @param face_windows A data frame, usually returned by
#'   `summarize_gazepoint_face_windows()` or
#'   `summarize_gazepoint_face_reactivity()`.
#' @param gaze_data Optional Gazepoint-derived data frame to join.
#' @param response_data Optional response/outcome data frame to join.
#' @param by Character vector of join columns shared across tables. If `NULL`,
#'   common identifier-like columns are detected.
#' @param gaze_by Optional named join mapping passed to `merge()` for
#'   `gaze_data`. If `NULL`, `by` is used.
#' @param response_by Optional named join mapping passed to `merge()` for
#'   `response_data`. If `NULL`, `by` is used.
#' @param predictor_cols Optional predictor columns to mark for modelling.
#'   If `NULL`, numeric non-identifier columns from the joined table are used.
#' @param outcome_cols Optional outcome columns to mark for modelling.
#' @param covariate_cols Optional covariate columns to mark for modelling.
#' @param scale_predictors Should numeric predictor columns be z-scaled?
#' @param scaled_suffix Suffix for scaled predictor columns.
#' @param drop_missing_outcomes Should rows with missing values in any
#'   `outcome_cols` be dropped?
#' @param keep_all Should all rows from `face_windows` be retained during joins?
#'
#' @return A tibble with class `gp3_multimodal_data`. Attributes contain join
#'   settings, selected predictors, outcomes, covariates, and scaling metadata.
#' @export
#'
#' @examples
#' face_windows <- data.frame(
#'   participant_id = c("P001", "P002"),
#'   trial_id = c(1, 1),
#'   AU12_r_mean = c(0.2, 0.3),
#'   face_confidence_mean = c(0.95, 0.94)
#' )
#'
#' responses <- data.frame(
#'   participant_id = c("P001", "P002"),
#'   trial_id = c(1, 1),
#'   rating = c(4, 5)
#' )
#'
#' prepare_gazepoint_multimodal_data(
#'   face_windows,
#'   response_data = responses,
#'   by = c("participant_id", "trial_id"),
#'   outcome_cols = "rating",
#'   predictor_cols = "AU12_r_mean"
#' )
prepare_gazepoint_multimodal_data <- function(face_windows,
                                              gaze_data = NULL,
                                              response_data = NULL,
                                              by = NULL,
                                              gaze_by = NULL,
                                              response_by = NULL,
                                              predictor_cols = NULL,
                                              outcome_cols = NULL,
                                              covariate_cols = NULL,
                                              scale_predictors = TRUE,
                                              scaled_suffix = "_z",
                                              drop_missing_outcomes = FALSE,
                                              keep_all = TRUE) {
  if (!is.data.frame(face_windows)) {
    stop("`face_windows` must be a data frame.", call. = FALSE)
  }

  face_windows <- as.data.frame(face_windows, stringsAsFactors = FALSE)

  if (!is.null(gaze_data) && !is.data.frame(gaze_data)) {
    stop("`gaze_data` must be a data frame or `NULL`.", call. = FALSE)
  }

  if (!is.null(response_data) && !is.data.frame(response_data)) {
    stop("`response_data` must be a data frame or `NULL`.", call. = FALSE)
  }

  if (!is.null(gaze_data)) {
    gaze_data <- as.data.frame(gaze_data, stringsAsFactors = FALSE)
  }

  if (!is.null(response_data)) {
    response_data <- as.data.frame(response_data, stringsAsFactors = FALSE)
  }

  if (is.null(by)) {
    by <- .gp3_multimodal_detect_join_cols(
      face_windows = face_windows,
      gaze_data = gaze_data,
      response_data = response_data
    )
  }

  if (length(by) < 1L && (!is.null(gaze_data) || !is.null(response_data))) {
    stop(
      "No join columns could be detected. Please supply `by`.",
      call. = FALSE
    )
  }

  .gp3_multimodal_validate_cols(face_windows, by, "`by` in `face_windows`")

  out <- face_windows

  if (!is.null(gaze_data)) {
    join_by <- .gp3_multimodal_resolve_join_by(
      left = out,
      right = gaze_data,
      by = if (is.null(gaze_by)) by else gaze_by,
      arg_name = "`gaze_by`"
    )

    out <- .gp3_multimodal_merge(
      left = out,
      right = gaze_data,
      by = join_by,
      all_x = keep_all,
      suffixes = c("", "_gaze")
    )
  }

  if (!is.null(response_data)) {
    join_by <- .gp3_multimodal_resolve_join_by(
      left = out,
      right = response_data,
      by = if (is.null(response_by)) by else response_by,
      arg_name = "`response_by`"
    )

    out <- .gp3_multimodal_merge(
      left = out,
      right = response_data,
      by = join_by,
      all_x = keep_all,
      suffixes = c("", "_response")
    )
  }

  .gp3_multimodal_validate_cols(out, outcome_cols, "`outcome_cols`")
  .gp3_multimodal_validate_cols(out, covariate_cols, "`covariate_cols`")

  predictor_cols <- .gp3_multimodal_predictor_cols(
    data = out,
    supplied = predictor_cols,
    exclude = unique(c(by, outcome_cols, covariate_cols))
  )

  if (length(predictor_cols) > 0L) {
    non_numeric <- predictor_cols[
      !vapply(out[predictor_cols], is.numeric, logical(1))
    ]

    if (length(non_numeric) > 0L) {
      stop(
        "Predictor column(s) must be numeric: ",
        paste(non_numeric, collapse = ", "),
        call. = FALSE
      )
    }
  }

  scaling <- data.frame(
    predictor = character(0),
    scaled_column = character(0),
    center = numeric(0),
    scale = numeric(0),
    stringsAsFactors = FALSE
  )

  if (scale_predictors && length(predictor_cols) > 0L) {
    scaled <- .gp3_multimodal_scale_predictors(
      data = out,
      predictor_cols = predictor_cols,
      scaled_suffix = scaled_suffix
    )

    out <- scaled$data
    scaling <- scaled$scaling
  }

  if (drop_missing_outcomes && length(outcome_cols) > 0L) {
    keep <- stats::complete.cases(out[, outcome_cols, drop = FALSE])
    out <- out[keep, , drop = FALSE]
  }

  out <- tibble::as_tibble(out)

  class(out) <- c("gp3_multimodal_data", class(out))
  attr(out, "gp3_multimodal_settings") <- list(
    by = by,
    gaze_by = gaze_by,
    response_by = response_by,
    predictor_cols = predictor_cols,
    outcome_cols = outcome_cols,
    covariate_cols = covariate_cols,
    scale_predictors = scale_predictors,
    scaled_suffix = scaled_suffix,
    drop_missing_outcomes = drop_missing_outcomes,
    keep_all = keep_all
  )
  attr(out, "gp3_multimodal_scaling") <- tibble::as_tibble(scaling)

  out
}


#' Fit a facial-behaviour window mixed or linear model
#'
#' Fits an explicit model to a face-window or multimodal analysis table. If
#' `random_effects` is supplied, the model is fitted with `lme4::lmer()`;
#' otherwise `stats::lm()` is used. The helper is a modelling convenience wrapper
#' and does not interpret facial-behaviour variables as emotions.
#'
#' @param data A face-window or multimodal data frame.
#' @param outcome Outcome column name.
#' @param predictors Character vector of fixed-effect predictor columns.
#' @param covariates Optional character vector of covariate columns.
#' @param random_effects Optional random-effects formula component, for example
#'   `"(1 | participant_id)"`.
#' @param na_action Missing-data handling. One of `"na.omit"` or `"na.exclude"`.
#' @param REML Passed to `lme4::lmer()` when a mixed model is fitted.
#'
#' @return A list with model, formula, data, and settings. The object has class
#'   `gp3_face_window_lmm`.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   participant_id = c("P001", "P002", "P003"),
#'   AU12_r_mean = c(0.1, 0.2, 0.3),
#'   rating = c(3, 4, 5)
#' )
#'
#' fit_gazepoint_face_window_lmm(
#'   dat,
#'   outcome = "rating",
#'   predictors = "AU12_r_mean"
#' )
fit_gazepoint_face_window_lmm <- function(data,
                                          outcome,
                                          predictors,
                                          covariates = NULL,
                                          random_effects = NULL,
                                          na_action = c("na.omit", "na.exclude"),
                                          REML = FALSE) {
  na_action <- match.arg(na_action)

  fit <- .gp3_multimodal_fit_model(
    data = data,
    outcome = outcome,
    predictors = predictors,
    covariates = covariates,
    random_effects = random_effects,
    family = NULL,
    na_action = na_action,
    REML = REML,
    model_label = "face_window_lmm"
  )

  class(fit) <- c("gp3_face_window_lmm", class(fit))

  fit
}


#' Fit a multimodal response model
#'
#' Fits an explicit response model using multimodal predictors. Linear models,
#' generalised linear models, and mixed-effects models are supported depending on
#' `family` and `random_effects`. The helper prepares and fits the model only; it
#' does not make causal, diagnostic, or emotion-inference claims.
#'
#' @param data A multimodal analysis data frame, usually returned by
#'   `prepare_gazepoint_multimodal_data()`.
#' @param outcome Outcome column name.
#' @param predictors Character vector of fixed-effect predictor columns.
#' @param covariates Optional character vector of covariate columns.
#' @param random_effects Optional random-effects formula component, for example
#'   `"(1 | participant_id)"`.
#' @param family Optional model family. If `NULL`, `stats::lm()` or
#'   `lme4::lmer()` is used. If supplied, `stats::glm()` or `lme4::glmer()` is
#'   used.
#' @param na_action Missing-data handling. One of `"na.omit"` or `"na.exclude"`.
#' @param REML Passed to `lme4::lmer()` when a linear mixed model is fitted.
#' @param ... Additional arguments passed to the model-fitting function.
#'
#' @return A list with model, formula, data, and settings. The object has class
#'   `gp3_multimodal_response_model`.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   participant_id = c("P001", "P002", "P003"),
#'   AU12_r_mean = c(0.1, 0.2, 0.3),
#'   dwell_time = c(1.2, 1.4, 1.1),
#'   rating = c(3, 4, 5)
#' )
#'
#' fit_gazepoint_multimodal_response_model(
#'   dat,
#'   outcome = "rating",
#'   predictors = c("AU12_r_mean", "dwell_time")
#' )
fit_gazepoint_multimodal_response_model <- function(data,
                                                    outcome,
                                                    predictors,
                                                    covariates = NULL,
                                                    random_effects = NULL,
                                                    family = NULL,
                                                    na_action = c(
                                                      "na.omit",
                                                      "na.exclude"
                                                    ),
                                                    REML = FALSE,
                                                    ...) {
  na_action <- match.arg(na_action)

  fit <- .gp3_multimodal_fit_model(
    data = data,
    outcome = outcome,
    predictors = predictors,
    covariates = covariates,
    random_effects = random_effects,
    family = family,
    na_action = na_action,
    REML = REML,
    model_label = "multimodal_response_model",
    ...
  )

  class(fit) <- c("gp3_multimodal_response_model", class(fit))

  fit
}


.gp3_multimodal_detect_join_cols <- function(face_windows,
                                             gaze_data = NULL,
                                             response_data = NULL) {
  candidates <- c(
    "participant_id",
    "subject_id",
    "user_id",
    "USER",
    "session_id",
    "trial_id",
    "trial",
    "MEDIA_ID",
    "MEDIA_NAME",
    "face_window_id",
    "face_window_label",
    "window",
    "phase"
  )

  common <- intersect(candidates, names(face_windows))

  if (!is.null(gaze_data)) {
    common <- intersect(common, names(gaze_data))
  }

  if (!is.null(response_data)) {
    common <- intersect(common, names(response_data))
  }

  common
}


.gp3_multimodal_validate_cols <- function(data, cols, label) {
  if (is.null(cols) || length(cols) < 1L) {
    return(invisible(TRUE))
  }

  missing <- setdiff(cols, names(data))

  if (length(missing) > 0L) {
    stop(
      label,
      " column(s) not found: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.gp3_multimodal_resolve_join_by <- function(left,
                                            right,
                                            by,
                                            arg_name) {
  if (is.null(by) || length(by) < 1L) {
    stop(arg_name, " must contain at least one join column.", call. = FALSE)
  }

  if (!is.character(by)) {
    stop(arg_name, " must be a character vector.", call. = FALSE)
  }

  if (is.null(names(by)) || all(names(by) == "")) {
    left_cols <- by
    right_cols <- by
  } else {
    left_cols <- names(by)
    right_cols <- unname(by)
  }

  missing_left <- setdiff(left_cols, names(left))
  missing_right <- setdiff(right_cols, names(right))

  if (length(missing_left) > 0L) {
    stop(
      arg_name,
      " left column(s) not found: ",
      paste(missing_left, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_right) > 0L) {
    stop(
      arg_name,
      " right column(s) not found: ",
      paste(missing_right, collapse = ", "),
      call. = FALSE
    )
  }

  stats::setNames(right_cols, left_cols)
}


.gp3_multimodal_merge <- function(left,
                                  right,
                                  by,
                                  all_x = TRUE,
                                  suffixes = c("", "_joined")) {
  merge(
    left,
    right,
    by.x = names(by),
    by.y = unname(by),
    all.x = all_x,
    all.y = FALSE,
    sort = FALSE,
    suffixes = suffixes
  )
}


.gp3_multimodal_predictor_cols <- function(data, supplied, exclude) {
  if (!is.null(supplied)) {
    .gp3_multimodal_validate_cols(data, supplied, "`predictor_cols`")
    return(supplied)
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, exclude)

  metadata_patterns <- paste(
    c(
      "^n$",
      "^n_",
      "_n$",
      "row",
      "id$",
      "time",
      "frame",
      "window_start",
      "window_end",
      "confidence",
      "valid_percent"
    ),
    collapse = "|"
  )

  numeric_cols[
    !grepl(metadata_patterns, numeric_cols, ignore.case = TRUE)
  ]
}


.gp3_multimodal_scale_predictors <- function(data,
                                             predictor_cols,
                                             scaled_suffix = "_z") {
  scaling <- data.frame(
    predictor = character(0),
    scaled_column = character(0),
    center = numeric(0),
    scale = numeric(0),
    stringsAsFactors = FALSE
  )

  for (p in predictor_cols) {
    x <- suppressWarnings(as.numeric(data[[p]]))
    center <- mean(x, na.rm = TRUE)
    scale <- stats::sd(x, na.rm = TRUE)

    scaled_col <- paste0(p, scaled_suffix)

    if (!is.finite(scale) || is.na(scale) || scale == 0) {
      data[[scaled_col]] <- NA_real_
    } else {
      data[[scaled_col]] <- (x - center) / scale
    }

    scaling <- rbind(
      scaling,
      data.frame(
        predictor = p,
        scaled_column = scaled_col,
        center = center,
        scale = scale,
        stringsAsFactors = FALSE
      )
    )
  }

  list(data = data, scaling = scaling)
}


.gp3_multimodal_fit_model <- function(data,
                                      outcome,
                                      predictors,
                                      covariates = NULL,
                                      random_effects = NULL,
                                      family = NULL,
                                      na_action = "na.omit",
                                      REML = FALSE,
                                      model_label = "model",
                                      ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)

  if (missing(outcome) || length(outcome) != 1L) {
    stop("`outcome` must be a single column name.", call. = FALSE)
  }

  if (missing(predictors) || length(predictors) < 1L) {
    stop("`predictors` must contain at least one column name.", call. = FALSE)
  }

  .gp3_multimodal_validate_cols(data, outcome, "`outcome`")
  .gp3_multimodal_validate_cols(data, predictors, "`predictors`")
  .gp3_multimodal_validate_cols(data, covariates, "`covariates`")

  fixed_terms <- unique(c(predictors, covariates))
  fixed_part <- paste(.gp3_multimodal_backtick(fixed_terms), collapse = " + ")

  rhs <- fixed_part

  if (!is.null(random_effects) && nzchar(random_effects)) {
    rhs <- paste(rhs, "+", random_effects)
  }

  form <- stats::as.formula(
    paste(.gp3_multimodal_backtick(outcome), "~", rhs)
  )

  model_data <- data[, unique(c(outcome, predictors, covariates)), drop = FALSE]

  if (!is.null(random_effects) && nzchar(random_effects)) {
    random_vars <- all.vars(stats::as.formula(paste("~", random_effects)))
    .gp3_multimodal_validate_cols(data, random_vars, "`random_effects`")
    model_data <- data[, unique(c(names(model_data), random_vars)), drop = FALSE]
  }

  complete <- stats::complete.cases(model_data)
  analysis_data <- data[complete, , drop = FALSE]

  if (nrow(analysis_data) < 1L) {
    stop("No complete rows were available for model fitting.", call. = FALSE)
  }

  na_fun <- switch(
    na_action,
    na.omit = stats::na.omit,
    na.exclude = stats::na.exclude
  )

  if (is.null(random_effects) || !nzchar(random_effects)) {
    model <- if (is.null(family)) {
      stats::lm(form, data = analysis_data, na.action = na_fun, ...)
    } else {
      stats::glm(form, data = analysis_data, family = family,
                 na.action = na_fun, ...)
    }
  } else {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop(
        "Package `lme4` is required for mixed-effects models.",
        call. = FALSE
      )
    }

    model <- if (is.null(family)) {
      lme4::lmer(form, data = analysis_data, REML = REML,
                 na.action = na_fun, ...)
    } else {
      lme4::glmer(form, data = analysis_data, family = family,
                  na.action = na_fun, ...)
    }
  }

  out <- list(
    model = model,
    formula = form,
    data = tibble::as_tibble(analysis_data),
    settings = list(
      model_label = model_label,
      outcome = outcome,
      predictors = predictors,
      covariates = covariates,
      random_effects = random_effects,
      family = if (is.null(family)) NULL else family,
      na_action = na_action,
      REML = REML,
      n_rows_input = nrow(data),
      n_rows_model = nrow(analysis_data)
    )
  )

  class(out) <- c("gp3_multimodal_model", class(out))

  out
}


.gp3_multimodal_backtick <- function(x) {
  ifelse(grepl("^[A-Za-z.][A-Za-z0-9_.]*$", x), x, paste0("`", x, "`"))
}
