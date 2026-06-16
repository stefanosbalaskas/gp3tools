#' Fit a Gazepoint pupil GAMM
#'
#' Fit a generalized additive mixed model for binned pupil time-course data using
#' `mgcv::bam()`. The function is designed to work with data prepared by
#' `prepare_gazepoint_pupil_gamm_data()`.
#'
#' @param data A binned pupil time-course data frame.
#' @param pupil_col Name of the dependent pupil column.
#' @param time_col Name of the time-bin centre column.
#' @param subject_col Name of the subject column.
#' @param condition_col Name of the condition column.
#' @param n_time_basis Basis dimension for smooth time terms.
#' @param use_condition_smooths Logical. If `TRUE`, condition-specific smooths
#'   are added when the condition column has more than one level.
#' @param include_subject_random_effect Logical. If `TRUE`, adds a subject
#'   random-effect smooth.
#' @param family Model family. Use `"gaussian"` for the default Gaussian model
#'   or `"scat"` for mgcv's scaled-t family.
#' @param method Smoothing-parameter estimation method passed to `mgcv::bam()`.
#' @param discrete Logical passed to `mgcv::bam()`.
#' @param rho Optional AR(1) correlation parameter passed to `mgcv::bam()`.
#' @param ar_start_col Optional AR-start column. If present and `rho` is not
#'   `NULL`, it is passed to `mgcv::bam()` as `AR.start`.
#' @param weights_col Optional weights column.
#' @param drop_missing Logical. If `TRUE`, rows with missing model variables are
#'   removed before fitting.
#'
#' @return A list of class `gp3_pupil_gamm` containing the fitted model,
#'   formula, data, settings, and status information.
#'
#' @export
fit_gazepoint_pupil_gamm <- function(
    data,
    pupil_col = "mean_pupil",
    time_col = "time_bin_center_ms",
    subject_col = "subject",
    condition_col = "condition",
    n_time_basis = 10,
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

  n_time_basis <- as.integer(n_time_basis)

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

  required_cols <- c(pupil_col, time_col, subject_col)

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
      !is.na(dat$.gp3_condition)

    if (!is.null(weights_col)) {
      keep <- keep & is.finite(dat$.gp3_weights) & dat$.gp3_weights > 0
    }

    dat <- dat[keep, , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after removing missing model variables.",
      call. = FALSE
    )
  }

  n_subjects <- length(unique(dat$.gp3_subject))
  n_conditions <- length(unique(dat$.gp3_condition))
  n_time_values <- length(unique(dat$.gp3_time))

  if (n_time_values < 3L) {
    stop(
      "At least three unique time values are required to fit a pupil GAMM.",
      call. = FALSE
    )
  }

  effective_k <- min(n_time_basis, max(3L, n_time_values - 1L))

  fixed_terms <- c()

  if (n_conditions > 1L) {
    fixed_terms <- c(fixed_terms, ".gp3_condition")
  }

  smooth_terms <- c(
    paste0("s(.gp3_time, k = ", effective_k, ")")
  )

  if (use_condition_smooths && n_conditions > 1L) {
    smooth_terms <- c(
      smooth_terms,
      paste0(
        "s(.gp3_time, by = .gp3_condition, k = ",
        effective_k,
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

  rhs <- paste(c(fixed_terms, smooth_terms), collapse = " + ")

  model_formula <- stats::as.formula(
    paste(".gp3_pupil_response ~", rhs)
  )

  model_family <- switch(
    family,
    gaussian = stats::gaussian(),
    scat = mgcv::scat()
  )

  model_args <- list(
    formula = model_formula,
    data = dat,
    family = model_family,
    method = method,
    discrete = discrete
  )

  if (!is.null(weights_col)) {
    model_args$weights <- dat$.gp3_weights
  }

  ar_used <- !is.null(rho)

  if (ar_used) {
    model_args$rho <- rho
    model_args$AR.start <- dat$.gp3_ar_start
  }

  fit <- tryCatch(
    do.call(mgcv::bam, model_args),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    out <- list(
      model = NULL,
      formula = model_formula,
      data = dat,
      settings = list(
        pupil_col = pupil_col,
        time_col = time_col,
        subject_col = subject_col,
        condition_col = condition_col,
        n_time_basis = n_time_basis,
        effective_k = effective_k,
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
      model_status = "fit_failed",
      error_message = conditionMessage(fit)
    )

    class(out) <- c("gp3_pupil_gamm", class(out))

    return(out)
  }

  out <- list(
    model = fit,
    formula = model_formula,
    data = dat,
    settings = list(
      pupil_col = pupil_col,
      time_col = time_col,
      subject_col = subject_col,
      condition_col = condition_col,
      n_time_basis = n_time_basis,
      effective_k = effective_k,
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
    model_status = "ok",
    error_message = NA_character_
  )

  class(out) <- c("gp3_pupil_gamm", class(out))

  out
}
