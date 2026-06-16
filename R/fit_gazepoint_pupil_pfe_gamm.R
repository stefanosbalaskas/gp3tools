#' Fit a gaze-position-adjusted pupil GAMM sensitivity model
#'
#' Fit and compare a main pupil GAMM with a gaze-position-adjusted sensitivity
#' model. The adjusted model adds a two-dimensional tensor-product smooth over
#' mean gaze x/y position using `te(mean_x, mean_y)`.
#'
#' @param data A binned pupil time-course data frame, usually created by
#'   `prepare_gazepoint_pupil_gamm_data()`.
#' @param pupil_col Name of the dependent pupil column.
#' @param time_col Name of the time-bin centre column.
#' @param subject_col Name of the subject column.
#' @param condition_col Name of the condition column.
#' @param x_col Name of the mean gaze x-position column.
#' @param y_col Name of the mean gaze y-position column.
#' @param n_time_basis Basis dimension for time smooths.
#' @param n_position_basis Basis dimension for gaze-position smooths.
#' @param use_condition_smooths Logical. If `TRUE`, condition-specific time
#'   smooths are used when multiple conditions are present.
#' @param include_subject_random_effect Logical. If `TRUE`, adds a subject
#'   random-effect smooth.
#' @param family Model family. Use `"gaussian"` or `"scat"`.
#' @param method Smoothing-parameter estimation method passed to `mgcv::bam()`.
#' @param discrete Logical passed to `mgcv::bam()`.
#' @param rho Optional AR(1) correlation parameter passed to `mgcv::bam()`.
#' @param ar_start_col Optional AR-start column.
#' @param weights_col Optional weights column.
#' @param drop_missing Logical. If `TRUE`, rows with missing model variables are
#'   removed before fitting.
#'
#' @return A list of class `gp3_pupil_pfe_gamm` containing the main model, the
#'   gaze-position-adjusted model, formulas, comparison table, settings, and
#'   status information.
#'
#' @export
fit_gazepoint_pupil_pfe_gamm <- function(
    data,
    pupil_col = "mean_pupil",
    time_col = "time_bin_center_ms",
    subject_col = "subject",
    condition_col = "condition",
    x_col = "mean_x",
    y_col = "mean_y",
    n_time_basis = 10,
    n_position_basis = 8,
    use_condition_smooths = TRUE,
    include_subject_random_effect = TRUE,
    family = c("gaussian", "scat"),
    method = "fREML",
    discrete = TRUE,
    rho = NULL,
    ar_start_col = "AR.start",
    weights_col = NULL,
    drop_missing = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop(
      "Package `mgcv` is required to fit pupil GAMMs. Please install it first.",
      call. = FALSE
    )
  }

  family <- match.arg(family)

  valid_column <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_optional_column <- function(x, arg) {
    if (!is.null(x) &&
        (!is.character(x) ||
         length(x) != 1L ||
         is.na(x) ||
         !nzchar(x))) {
      stop(
        "`", arg, "` must be NULL or a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_column(pupil_col, "pupil_col")
  valid_column(time_col, "time_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_column(x_col, "x_col")
  valid_column(y_col, "y_col")
  valid_optional_column(ar_start_col, "ar_start_col")
  valid_optional_column(weights_col, "weights_col")

  if (!is.numeric(n_time_basis) ||
      length(n_time_basis) != 1L ||
      is.na(n_time_basis) ||
      !is.finite(n_time_basis) ||
      n_time_basis < 3) {
    stop(
      "`n_time_basis` must be a finite numeric scalar greater than or equal to 3.",
      call. = FALSE
    )
  }

  if (!is.numeric(n_position_basis) ||
      length(n_position_basis) != 1L ||
      is.na(n_position_basis) ||
      !is.finite(n_position_basis) ||
      n_position_basis < 3) {
    stop(
      "`n_position_basis` must be a finite numeric scalar greater than or equal to 3.",
      call. = FALSE
    )
  }

  n_time_basis <- as.integer(n_time_basis)
  n_position_basis <- as.integer(n_position_basis)

  if (!is.logical(use_condition_smooths) ||
      length(use_condition_smooths) != 1L ||
      is.na(use_condition_smooths)) {
    stop(
      "`use_condition_smooths` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(include_subject_random_effect) ||
      length(include_subject_random_effect) != 1L ||
      is.na(include_subject_random_effect)) {
    stop(
      "`include_subject_random_effect` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.character(method) ||
      length(method) != 1L ||
      is.na(method) ||
      !nzchar(method)) {
    stop(
      "`method` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.logical(discrete) ||
      length(discrete) != 1L ||
      is.na(discrete)) {
    stop(
      "`discrete` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(rho) &&
      (!is.numeric(rho) ||
       length(rho) != 1L ||
       is.na(rho) ||
       !is.finite(rho) ||
       rho < 0 ||
       rho >= 1)) {
    stop(
      "`rho` must be NULL or a finite numeric scalar in [0, 1).",
      call. = FALSE
    )
  }

  if (!is.logical(drop_missing) ||
      length(drop_missing) != 1L ||
      is.na(drop_missing)) {
    stop(
      "`drop_missing` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  required_cols <- c(pupil_col, time_col, subject_col, x_col, y_col)

  if (!is.null(condition_col)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (!is.null(ar_start_col) && !is.null(rho)) {
    required_cols <- c(required_cols, ar_start_col)
  }

  if (!is.null(weights_col)) {
    required_cols <- c(required_cols, weights_col)
  }

  missing_cols <- setdiff(unique(required_cols), names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat$.gp3_pupil_response <- suppressWarnings(as.numeric(dat[[pupil_col]]))
  dat$.gp3_time <- suppressWarnings(as.numeric(dat[[time_col]]))
  dat$.gp3_subject <- as.factor(dat[[subject_col]])
  dat$.gp3_x <- suppressWarnings(as.numeric(dat[[x_col]]))
  dat$.gp3_y <- suppressWarnings(as.numeric(dat[[y_col]]))

  if (!is.null(condition_col)) {
    condition_values <- as.character(dat[[condition_col]])
    condition_values <- trimws(condition_values)
    condition_values[
      is.na(condition_values) |
        !nzchar(condition_values)
    ] <- "all_data"

    dat$.gp3_condition <- as.factor(condition_values)
  } else {
    dat$.gp3_condition <- factor("all_data")
  }

  if (!is.null(ar_start_col) && ar_start_col %in% names(dat)) {
    dat$.gp3_ar_start <- as.logical(dat[[ar_start_col]])
    dat$.gp3_ar_start[is.na(dat$.gp3_ar_start)] <- FALSE
  } else {
    dat$.gp3_ar_start <- FALSE
  }

  if (!is.null(weights_col)) {
    dat$.gp3_weights <- suppressWarnings(as.numeric(dat[[weights_col]]))
  } else {
    dat$.gp3_weights <- NA_real_
  }

  if (drop_missing) {
    keep <- is.finite(dat$.gp3_pupil_response) &
      is.finite(dat$.gp3_time) &
      !is.na(dat$.gp3_subject) &
      !is.na(dat$.gp3_condition) &
      is.finite(dat$.gp3_x) &
      is.finite(dat$.gp3_y)

    if (!is.null(weights_col)) {
      keep <- keep & is.finite(dat$.gp3_weights) & dat$.gp3_weights > 0
    }

    dat <- dat[keep, , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after removing missing model or gaze-position variables.",
      call. = FALSE
    )
  }

  n_subjects <- length(unique(dat$.gp3_subject))
  n_conditions <- length(unique(dat$.gp3_condition))
  n_time_values <- length(unique(dat$.gp3_time))
  n_x_values <- length(unique(dat$.gp3_x))
  n_y_values <- length(unique(dat$.gp3_y))

  if (n_time_values < 3L) {
    stop(
      "At least three unique time values are required to fit a pupil GAMM.",
      call. = FALSE
    )
  }

  if (n_x_values < 3L || n_y_values < 3L) {
    main_fit <- fit_gazepoint_pupil_gamm(
      dat,
      pupil_col = ".gp3_pupil_response",
      time_col = ".gp3_time",
      subject_col = ".gp3_subject",
      condition_col = ".gp3_condition",
      n_time_basis = n_time_basis,
      use_condition_smooths = use_condition_smooths,
      include_subject_random_effect = include_subject_random_effect,
      family = family,
      method = method,
      discrete = discrete,
      rho = rho,
      ar_start_col = ".gp3_ar_start",
      weights_col = if (!is.null(weights_col)) ".gp3_weights" else NULL,
      drop_missing = FALSE
    )

    out <- list(
      main_model = main_fit$model,
      pfe_model = NULL,
      main_fit = main_fit,
      pfe_formula = NULL,
      comparison = tibble::tibble(),
      data = dat,
      settings = list(
        pupil_col = pupil_col,
        time_col = time_col,
        subject_col = subject_col,
        condition_col = condition_col,
        x_col = x_col,
        y_col = y_col,
        n_time_basis = n_time_basis,
        n_position_basis = n_position_basis,
        use_condition_smooths = use_condition_smooths,
        include_subject_random_effect = include_subject_random_effect,
        family = family,
        method = method,
        discrete = discrete,
        rho = rho,
        ar_start_col = ar_start_col,
        weights_col = weights_col
      ),
      sensitivity_status = "insufficient_gaze_position_variation",
      error_message = NA_character_
    )

    class(out) <- c("gp3_pupil_pfe_gamm", class(out))

    return(out)
  }

  effective_time_k <- min(n_time_basis, max(3L, n_time_values - 1L))
  effective_position_k <- min(
    n_position_basis,
    max(3L, floor(sqrt(nrow(dat))))
  )

  main_fit <- fit_gazepoint_pupil_gamm(
    dat,
    pupil_col = ".gp3_pupil_response",
    time_col = ".gp3_time",
    subject_col = ".gp3_subject",
    condition_col = ".gp3_condition",
    n_time_basis = n_time_basis,
    use_condition_smooths = use_condition_smooths,
    include_subject_random_effect = include_subject_random_effect,
    family = family,
    method = method,
    discrete = discrete,
    rho = rho,
    ar_start_col = ".gp3_ar_start",
    weights_col = if (!is.null(weights_col)) ".gp3_weights" else NULL,
    drop_missing = FALSE
  )

  fixed_terms <- c()

  if (n_conditions > 1L) {
    fixed_terms <- c(fixed_terms, ".gp3_condition")
  }

  smooth_terms <- c(
    paste0("s(.gp3_time, k = ", effective_time_k, ")")
  )

  if (use_condition_smooths && n_conditions > 1L) {
    smooth_terms <- c(
      smooth_terms,
      paste0(
        "s(.gp3_time, by = .gp3_condition, k = ",
        effective_time_k,
        ")"
      )
    )
  }

  if (include_subject_random_effect && n_subjects > 1L) {
    smooth_terms <- c(
      smooth_terms,
      "s(.gp3_subject, bs = 're')"
    )
  }

  position_term <- paste0(
    "te(.gp3_x, .gp3_y, k = c(",
    effective_position_k,
    ", ",
    effective_position_k,
    "))"
  )

  rhs <- paste(c(fixed_terms, smooth_terms, position_term), collapse = " + ")

  pfe_formula <- stats::as.formula(
    paste(".gp3_pupil_response ~", rhs)
  )

  model_family <- switch(
    family,
    gaussian = stats::gaussian(),
    scat = mgcv::scat()
  )

  pfe_args <- list(
    formula = pfe_formula,
    data = dat,
    family = model_family,
    method = method,
    discrete = discrete
  )

  if (!is.null(weights_col)) {
    pfe_args$weights <- dat$.gp3_weights
  }

  ar_used <- !is.null(rho)

  if (ar_used) {
    pfe_args$rho <- rho
    pfe_args$AR.start <- dat$.gp3_ar_start
  }

  pfe_fit <- tryCatch(
    do.call(mgcv::bam, pfe_args),
    error = function(e) e
  )

  extract_model_stats <- function(model, model_type) {
    if (is.null(model) || inherits(model, "error")) {
      return(
        tibble::tibble(
          model_type = model_type,
          n = NA_integer_,
          AIC = NA_real_,
          BIC = NA_real_,
          deviance_explained = NA_real_,
          adj_r_squared = NA_real_
        )
      )
    }

    sm <- summary(model)

    tibble::tibble(
      model_type = model_type,
      n = stats::nobs(model),
      AIC = stats::AIC(model),
      BIC = stats::BIC(model),
      deviance_explained = if (!is.null(sm$dev.expl)) {
        unname(sm$dev.expl)
      } else {
        NA_real_
      },
      adj_r_squared = if (!is.null(sm$r.sq)) {
        unname(sm$r.sq)
      } else {
        NA_real_
      }
    )
  }

  comparison <- dplyr::bind_rows(
    extract_model_stats(main_fit$model, "main_gamm"),
    extract_model_stats(
      if (inherits(pfe_fit, "error")) NULL else pfe_fit,
      "pfe_gamm"
    )
  )

  if (all(c("main_gamm", "pfe_gamm") %in% comparison$model_type) &&
      all(is.finite(comparison$AIC))) {
    main_aic <- comparison$AIC[comparison$model_type == "main_gamm"][[1]]
    main_bic <- comparison$BIC[comparison$model_type == "main_gamm"][[1]]
    main_dev <- comparison$deviance_explained[
      comparison$model_type == "main_gamm"
    ][[1]]

    comparison <- comparison |>
      dplyr::mutate(
        delta_AIC_from_main = .data[["AIC"]] - main_aic,
        delta_BIC_from_main = .data[["BIC"]] - main_bic,
        delta_deviance_explained_from_main =
          .data[["deviance_explained"]] - main_dev
      )
  } else {
    comparison <- comparison |>
      dplyr::mutate(
        delta_AIC_from_main = NA_real_,
        delta_BIC_from_main = NA_real_,
        delta_deviance_explained_from_main = NA_real_
      )
  }

  sensitivity_status <- dplyr::case_when(
    !identical(main_fit$model_status, "ok") ~ "main_model_failed",
    inherits(pfe_fit, "error") ~ "pfe_model_failed",
    TRUE ~ "ok"
  )

  out <- list(
    main_model = main_fit$model,
    pfe_model = if (inherits(pfe_fit, "error")) NULL else pfe_fit,
    main_fit = main_fit,
    pfe_formula = pfe_formula,
    comparison = comparison,
    data = dat,
    settings = list(
      pupil_col = pupil_col,
      time_col = time_col,
      subject_col = subject_col,
      condition_col = condition_col,
      x_col = x_col,
      y_col = y_col,
      n_time_basis = n_time_basis,
      n_position_basis = n_position_basis,
      effective_time_k = effective_time_k,
      effective_position_k = effective_position_k,
      use_condition_smooths = use_condition_smooths,
      include_subject_random_effect = include_subject_random_effect,
      family = family,
      method = method,
      discrete = discrete,
      rho = rho,
      ar_used = ar_used,
      ar_start_col = ar_start_col,
      weights_col = weights_col
    ),
    sensitivity_status = sensitivity_status,
    error_message = if (inherits(pfe_fit, "error")) {
      conditionMessage(pfe_fit)
    } else {
      NA_character_
    }
  )

  class(out) <- c("gp3_pupil_pfe_gamm", class(out))

  out
}
