#' Fit AOI time-course GAMMs
#'
#' Fit binomial GAMMs for AOI target-looking time courses prepared by
#' `prepare_gazepoint_aoi_gamm_data()`. The model uses target-looking
#' successes and failures over time and can include condition effects,
#' condition-specific smooths, and subject random-effect smooths.
#'
#' This function is intended for AOI time-course modelling. It is separate from
#' confirmatory AOI-window GLMMs and from cluster-based permutation tests.
#'
#' @param data A data frame returned by `prepare_gazepoint_aoi_gamm_data()`.
#' @param include_condition Logical. If `TRUE`, include condition as a
#'   parametric fixed effect when two or more conditions are available.
#' @param condition_smooths Logical. If `TRUE`, fit condition-specific time
#'   smooths when two or more conditions are available.
#' @param random_subject Logical. If `TRUE`, include a subject random-effect
#'   smooth.
#' @param random_subject_time Logical. If `TRUE`, include subject-specific
#'   factor-smooth time deviations. This can be useful for repeated-measures
#'   time-course data but may be too heavy for very small datasets.
#' @param time_k Basis dimension for the main time smooth.
#' @param subject_time_k Basis dimension for subject-specific factor-smooth
#'   time deviations.
#' @param family Model family. Defaults to `stats::binomial()`.
#' @param method Smoothing-parameter estimation method passed to
#'   `mgcv::bam()`.
#' @param discrete Logical passed to `mgcv::bam()`.
#' @param select Logical passed to `mgcv::bam()`.
#' @param drop_non_ok Logical. If `TRUE`, keep only rows with
#'   `.gp3_aoi_gamm_status == "ok"` before fitting.
#' @param min_rows Minimum number of rows required for model fitting.
#' @param min_subjects Minimum number of subjects required for model fitting.
#' @param min_time_bins Minimum number of time bins required for model fitting.
#' @param ... Additional arguments passed to `mgcv::bam()`.
#'
#' @return A list containing the fitted model, formula, model status,
#'   diagnostics, parametric table, smooth table, and settings.
#'
#' @export
fit_gazepoint_aoi_gamm <- function(
    data,
    include_condition = TRUE,
    condition_smooths = TRUE,
    random_subject = TRUE,
    random_subject_time = FALSE,
    time_k = 10,
    subject_time_k = 5,
    family = stats::binomial(),
    method = "fREML",
    discrete = FALSE,
    select = FALSE,
    drop_non_ok = TRUE,
    min_rows = 10,
    min_subjects = 2,
    min_time_bins = 4,
    ...
) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop(
      "Package `mgcv` is required to fit AOI GAMMs.",
      call. = FALSE
    )
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  check_logical_scalar <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  check_positive_numeric <- function(x, arg) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x <= 0) {
      stop("`", arg, "` must be a positive finite numeric scalar.",
           call. = FALSE)
    }
  }

  check_logical_scalar(include_condition, "include_condition")
  check_logical_scalar(condition_smooths, "condition_smooths")
  check_logical_scalar(random_subject, "random_subject")
  check_logical_scalar(random_subject_time, "random_subject_time")
  check_logical_scalar(discrete, "discrete")
  check_logical_scalar(select, "select")
  check_logical_scalar(drop_non_ok, "drop_non_ok")

  check_positive_numeric(time_k, "time_k")
  check_positive_numeric(subject_time_k, "subject_time_k")
  check_positive_numeric(min_rows, "min_rows")
  check_positive_numeric(min_subjects, "min_subjects")
  check_positive_numeric(min_time_bins, "min_time_bins")

  time_k <- as.integer(time_k)
  subject_time_k <- as.integer(subject_time_k)
  min_rows <- as.integer(min_rows)
  min_subjects <- as.integer(min_subjects)
  min_time_bins <- as.integer(min_time_bins)

  required_cols <- c(
    ".gp3_aoi_gamm_subject",
    ".gp3_aoi_gamm_condition",
    ".gp3_aoi_gamm_time_bin",
    ".gp3_aoi_gamm_success",
    ".gp3_aoi_gamm_failure",
    ".gp3_aoi_gamm_denominator",
    ".gp3_aoi_gamm_status"
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  dat$.gp3_aoi_gamm_subject <- factor(dat$.gp3_aoi_gamm_subject)
  dat$.gp3_aoi_gamm_condition <- factor(dat$.gp3_aoi_gamm_condition)
  dat$.gp3_aoi_gamm_time_bin <-
    suppressWarnings(as.numeric(dat$.gp3_aoi_gamm_time_bin))
  dat$.gp3_aoi_gamm_success <-
    suppressWarnings(as.numeric(dat$.gp3_aoi_gamm_success))
  dat$.gp3_aoi_gamm_failure <-
    suppressWarnings(as.numeric(dat$.gp3_aoi_gamm_failure))
  dat$.gp3_aoi_gamm_denominator <-
    suppressWarnings(as.numeric(dat$.gp3_aoi_gamm_denominator))

  dat$.gp3_aoi_gamm_fit_status <- dplyr::case_when(
    is.na(dat$.gp3_aoi_gamm_subject) ~ "missing_subject",
    is.na(dat$.gp3_aoi_gamm_condition) ~ "missing_condition",
    is.na(dat$.gp3_aoi_gamm_time_bin) ~ "missing_time_bin",
    !is.finite(dat$.gp3_aoi_gamm_time_bin) ~ "non_finite_time_bin",
    is.na(dat$.gp3_aoi_gamm_success) ~ "missing_success",
    !is.finite(dat$.gp3_aoi_gamm_success) ~ "non_finite_success",
    is.na(dat$.gp3_aoi_gamm_failure) ~ "missing_failure",
    !is.finite(dat$.gp3_aoi_gamm_failure) ~ "non_finite_failure",
    is.na(dat$.gp3_aoi_gamm_denominator) ~ "missing_denominator",
    !is.finite(dat$.gp3_aoi_gamm_denominator) ~ "non_finite_denominator",
    dat$.gp3_aoi_gamm_success < 0 ~ "negative_success",
    dat$.gp3_aoi_gamm_failure < 0 ~ "negative_failure",
    dat$.gp3_aoi_gamm_denominator <= 0 ~ "zero_denominator",
    abs(
      dat$.gp3_aoi_gamm_success +
        dat$.gp3_aoi_gamm_failure -
        dat$.gp3_aoi_gamm_denominator
    ) > sqrt(.Machine$double.eps) ~ "inconsistent_denominator",
    drop_non_ok & dat$.gp3_aoi_gamm_status != "ok" ~ "non_ok_input_status",
    TRUE ~ "ok"
  )

  fit_data <- dat[
    dat$.gp3_aoi_gamm_fit_status == "ok",
    ,
    drop = FALSE
  ]

  if (nrow(fit_data) < min_rows) {
    stop(
      "Not enough valid rows are available for AOI-GAMM fitting.",
      call. = FALSE
    )
  }

  n_subjects <- dplyr::n_distinct(fit_data$.gp3_aoi_gamm_subject)
  n_conditions <- dplyr::n_distinct(fit_data$.gp3_aoi_gamm_condition)
  n_time_bins <- dplyr::n_distinct(fit_data$.gp3_aoi_gamm_time_bin)

  if (n_subjects < min_subjects) {
    stop(
      "Not enough subjects are available for AOI-GAMM fitting.",
      call. = FALSE
    )
  }

  if (n_time_bins < min_time_bins) {
    stop(
      "Not enough time bins are available for AOI-GAMM fitting.",
      call. = FALSE
    )
  }

  has_condition <- n_conditions >= 2L
  use_condition <- isTRUE(include_condition) && has_condition
  use_condition_smooths <- isTRUE(condition_smooths) && has_condition

  condition_status <- dplyr::case_when(
    n_conditions < 2L ~ "less_than_two_conditions",
    n_conditions == 2L ~ "two_conditions",
    TRUE ~ "more_than_two_conditions"
  )

  effective_time_k <- min(time_k, n_time_bins)
  effective_time_k <- max(3L, effective_time_k)

  effective_subject_time_k <- min(subject_time_k, n_time_bins)
  effective_subject_time_k <- max(3L, effective_subject_time_k)

  response_term <- "cbind(.gp3_aoi_gamm_success, .gp3_aoi_gamm_failure)"

  smooth_terms <- character()

  if (use_condition_smooths) {
    smooth_terms <- c(
      smooth_terms,
      paste0(
        "s(.gp3_aoi_gamm_time_bin, by = .gp3_aoi_gamm_condition, k = ",
        effective_time_k,
        ")"
      )
    )
  } else {
    smooth_terms <- c(
      smooth_terms,
      paste0(
        "s(.gp3_aoi_gamm_time_bin, k = ",
        effective_time_k,
        ")"
      )
    )
  }

  fixed_terms <- character()

  if (use_condition) {
    fixed_terms <- c(fixed_terms, ".gp3_aoi_gamm_condition")
  }

  random_terms <- character()

  if (random_subject) {
    random_terms <- c(
      random_terms,
      "s(.gp3_aoi_gamm_subject, bs = 're')"
    )
  }

  if (random_subject_time) {
    random_terms <- c(
      random_terms,
      paste0(
        "s(.gp3_aoi_gamm_time_bin, .gp3_aoi_gamm_subject, ",
        "bs = 'fs', k = ",
        effective_subject_time_k,
        ")"
      )
    )
  }

  rhs_terms <- c(fixed_terms, smooth_terms, random_terms)

  if (length(rhs_terms) == 0L) {
    rhs_terms <- "1"
  }

  formula_text <- paste(
    response_term,
    "~",
    paste(rhs_terms, collapse = " + ")
  )

  model_formula <- stats::as.formula(formula_text)

  fit <- NULL
  error_message <- NA_character_
  warning_message <- NA_character_
  model_status <- "ok"

  fit <- tryCatch(
    withCallingHandlers(
      mgcv::bam(
        formula = model_formula,
        family = family,
        data = fit_data,
        method = method,
        discrete = discrete,
        select = select,
        ...
      ),
      warning = function(w) {
        existing_warnings <- warning_message[!is.na(warning_message)]

        warning_message <<- paste(
          unique(c(existing_warnings, conditionMessage(w))),
          collapse = " | "
        )

        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_message <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(fit)) {
    model_status <- "error"
  }

  model_summary <- NULL
  parametric_table <- tibble::tibble()
  smooth_table <- tibble::tibble()
  diagnostics <- tibble::tibble(
    n_rows = nrow(fit_data),
    n_subjects = n_subjects,
    n_conditions = n_conditions,
    n_time_bins = n_time_bins,
    total_success = sum(fit_data$.gp3_aoi_gamm_success, na.rm = TRUE),
    total_failure = sum(fit_data$.gp3_aoi_gamm_failure, na.rm = TRUE),
    total_denominator = sum(fit_data$.gp3_aoi_gamm_denominator, na.rm = TRUE),
    condition_status = condition_status,
    used_condition = use_condition,
    used_condition_smooths = use_condition_smooths,
    used_random_subject = random_subject,
    used_random_subject_time = random_subject_time,
    effective_time_k = effective_time_k,
    effective_subject_time_k = effective_subject_time_k,
    model_status = model_status,
    warning_message = warning_message,
    error_message = error_message
  )

  if (!is.null(fit)) {
    model_summary <- summary(fit)


    if (!is.null(model_summary$p.table)) {
      parametric_table <- as.data.frame(model_summary$p.table)
      parametric_table$term <- rownames(parametric_table)
      rownames(parametric_table) <- NULL
      parametric_table <- tibble::as_tibble(parametric_table)
      parametric_table <- parametric_table[
        ,
        c("term", setdiff(names(parametric_table), "term")),
        drop = FALSE
      ]
    }

    if (!is.null(model_summary$s.table)) {
      smooth_table <- as.data.frame(model_summary$s.table)
      smooth_table$smooth <- rownames(smooth_table)
      rownames(smooth_table) <- NULL
      smooth_table <- tibble::as_tibble(smooth_table)
      smooth_table <- smooth_table[
        ,
        c("smooth", setdiff(names(smooth_table), "smooth")),
        drop = FALSE
      ]
    }

    diagnostics$edf <- sum(fit$edf, na.rm = TRUE)
    diagnostics$aic <- stats::AIC(fit)
    diagnostics$deviance_explained <- model_summary$dev.expl
    diagnostics$r_squared_adjusted <- model_summary$r.sq


  } else {
    diagnostics$edf <- NA_real_
    diagnostics$aic <- NA_real_
    diagnostics$deviance_explained <- NA_real_
    diagnostics$r_squared_adjusted <- NA_real_
  }

  settings <- list(
    include_condition = include_condition,
    condition_smooths = condition_smooths,
    random_subject = random_subject,
    random_subject_time = random_subject_time,
    time_k = time_k,
    subject_time_k = subject_time_k,
    effective_time_k = effective_time_k,
    effective_subject_time_k = effective_subject_time_k,
    family = family$family,
    link = family$link,
    method = method,
    discrete = discrete,
    select = select,
    drop_non_ok = drop_non_ok,
    min_rows = min_rows,
    min_subjects = min_subjects,
    min_time_bins = min_time_bins
  )

  out <- list(
    model = fit,
    formula = model_formula,
    formula_text = formula_text,
    model_summary = model_summary,
    parametric_table = parametric_table,
    smooth_table = smooth_table,
    diagnostics = diagnostics,
    fit_data = fit_data,
    row_status = dat[
      ,
      c(
        ".gp3_aoi_gamm_subject",
        ".gp3_aoi_gamm_condition",
        ".gp3_aoi_gamm_time_bin",
        ".gp3_aoi_gamm_status",
        ".gp3_aoi_gamm_fit_status"
      ),
      drop = FALSE
    ],
    settings = settings,
    model_status = model_status,
    condition_status = condition_status,
    warning_message = warning_message,
    error_message = error_message
  )

  class(out) <- c("gp3_aoi_gamm_fit", class(out))

  out
}
