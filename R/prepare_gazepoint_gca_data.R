#' Prepare Gazepoint Growth Curve Analysis data
#'
#' Prepare binned pupil time-course data for Growth Curve Analysis (GCA).
#' The function creates orthogonal polynomial time terms, preserves subject and
#' condition information, and standardises key columns for later mixed-model
#' fitting.
#'
#' @param data A binned pupil time-course data frame, usually created by
#'   `prepare_gazepoint_pupil_gamm_data()`.
#' @param pupil_col Name of the pupil outcome column.
#' @param time_col Name of the time column.
#' @param subject_col Name of the subject column.
#' @param condition_col Name of the condition column. If unavailable or entirely
#'   missing, a single condition label is used.
#' @param degree Number of polynomial time terms to create.
#' @param orthogonal Logical. If `TRUE`, use orthogonal polynomial terms from
#'   `stats::poly()`. If `FALSE`, use raw powers of z-scored time.
#' @param time_window Optional numeric vector of length 2 giving the time window
#'   to retain.
#' @param valid_samples_col Optional column containing valid sample counts.
#' @param min_valid_samples Minimum valid samples required per row when
#'   `valid_samples_col` is available.
#' @param weights_col Optional weights column to preserve for later modelling.
#' @param missing_condition_label Label used when condition values are missing.
#' @param drop_missing Logical. If `TRUE`, rows with missing outcome/time/subject
#'   values are removed.
#'
#' @return A tibble of class `gp3_gca_data` with standard GCA columns and
#'   polynomial time terms.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_gca_data <- function(
    data,
    pupil_col = "mean_pupil",
    time_col = "time_bin_center_ms",
    subject_col = "subject",
    condition_col = "condition",
    degree = 3,
    orthogonal = TRUE,
    time_window = NULL,
    valid_samples_col = "n_valid_samples",
    min_valid_samples = 1,
    weights_col = NULL,
    missing_condition_label = "all_data",
    drop_missing = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

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
  valid_optional_column(valid_samples_col, "valid_samples_col")
  valid_optional_column(weights_col, "weights_col")

  if (!is.numeric(degree) ||
      length(degree) != 1L ||
      is.na(degree) ||
      !is.finite(degree) ||
      degree < 1) {
    stop(
      "`degree` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  degree <- as.integer(degree)

  if (!is.logical(orthogonal) ||
      length(orthogonal) != 1L ||
      is.na(orthogonal)) {
    stop("`orthogonal` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(time_window) &&
      (!is.numeric(time_window) ||
       length(time_window) != 2L ||
       any(is.na(time_window)) ||
       any(!is.finite(time_window)))) {
    stop(
      "`time_window` must be NULL or a finite numeric vector of length 2.",
      call. = FALSE
    )
  }

  if (!is.numeric(min_valid_samples) ||
      length(min_valid_samples) != 1L ||
      is.na(min_valid_samples) ||
      !is.finite(min_valid_samples) ||
      min_valid_samples < 1) {
    stop(
      "`min_valid_samples` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.character(missing_condition_label) ||
      length(missing_condition_label) != 1L ||
      is.na(missing_condition_label) ||
      !nzchar(missing_condition_label)) {
    stop(
      "`missing_condition_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.logical(drop_missing) ||
      length(drop_missing) != 1L ||
      is.na(drop_missing)) {
    stop("`drop_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  dat <- tibble::as_tibble(data)
  selected_pupil_col <- pupil_col
  selected_time_col <- time_col
  selected_subject_col <- subject_col
  selected_condition_col <- condition_col
  selected_valid_samples_col <- valid_samples_col
  selected_weights_col <- weights_col
  selected_degree <- degree
  selected_orthogonal <- orthogonal
  selected_time_window <- time_window
  selected_min_valid_samples <- min_valid_samples
  selected_missing_condition_label <- missing_condition_label
  required_cols <- c(pupil_col, time_col, subject_col)

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (!is.null(valid_samples_col) && valid_samples_col %in% names(dat)) {
    required_cols <- c(required_cols, valid_samples_col)
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

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  dat$.gp3_gca_pupil <- as_numeric_safe(dat[[pupil_col]])
  dat$.gp3_gca_time <- as_numeric_safe(dat[[time_col]])
  dat$.gp3_gca_subject <- as.character(dat[[subject_col]])

  dat$.gp3_gca_subject[
    is.na(dat$.gp3_gca_subject) |
      !nzchar(trimws(dat$.gp3_gca_subject))
  ] <- "unknown_subject"

  condition_status <- "ok"

  if (is.null(condition_col) || !condition_col %in% names(dat)) {
    dat$.gp3_gca_condition <- missing_condition_label
    condition_status <- "no_condition_column"
  } else {
    condition_values <- as.character(dat[[condition_col]])
    condition_values <- trimws(condition_values)

    condition_values[
      is.na(condition_values) |
        !nzchar(condition_values)
    ] <- missing_condition_label

    if (length(unique(condition_values)) == 1L &&
        unique(condition_values) == missing_condition_label) {
      condition_status <- "condition_missing_all_data"
    }

    dat$.gp3_gca_condition <- condition_values
  }

  if (!is.null(valid_samples_col) && valid_samples_col %in% names(dat)) {
    dat$.gp3_valid_samples <- as_numeric_safe(dat[[valid_samples_col]])
  } else {
    dat$.gp3_valid_samples <- NA_real_
  }

  if (!is.null(weights_col)) {
    dat$.gp3_gca_weights <- as_numeric_safe(dat[[weights_col]])
  } else if (!is.null(valid_samples_col) && valid_samples_col %in% names(dat)) {
    dat$.gp3_gca_weights <- dat$.gp3_valid_samples
  } else {
    dat$.gp3_gca_weights <- NA_real_
  }

  if (!is.null(time_window)) {
    lower <- min(time_window)
    upper <- max(time_window)

    dat <- dat |>
      dplyr::filter(
        .data[[".gp3_gca_time"]] >= lower,
        .data[[".gp3_gca_time"]] <= upper
      )
  }

  if (!is.null(valid_samples_col) && valid_samples_col %in% names(dat)) {
    dat <- dat |>
      dplyr::filter(
        is.na(.data[[".gp3_valid_samples"]]) |
          .data[[".gp3_valid_samples"]] >= min_valid_samples
      )
  }

  if (drop_missing) {
    dat <- dat |>
      dplyr::filter(
        is.finite(.data[[".gp3_gca_pupil"]]),
        is.finite(.data[[".gp3_gca_time"]]),
        !is.na(.data[[".gp3_gca_subject"]]),
        !is.na(.data[[".gp3_gca_condition"]])
      )
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after applying GCA data filters.",
      call. = FALSE
    )
  }

  n_time_values <- length(unique(dat$.gp3_gca_time))

  if (n_time_values <= degree) {
    stop(
      "The number of unique time values must be greater than `degree`.",
      call. = FALSE
    )
  }

  time_mean <- mean(dat$.gp3_gca_time, na.rm = TRUE)
  time_sd <- stats::sd(dat$.gp3_gca_time, na.rm = TRUE)

  if (!is.finite(time_sd) || time_sd <= 0) {
    stop(
      "Time values must have non-zero variation.",
      call. = FALSE
    )
  }

  dat$gca_time <- dat$.gp3_gca_time
  dat$gca_time_centered <- dat$.gp3_gca_time - time_mean
  dat$gca_time_z <- dat$gca_time_centered / time_sd

  if (orthogonal) {
    poly_matrix <- stats::poly(dat$.gp3_gca_time, degree = degree)
    poly_coefs <- attr(poly_matrix, "coefs")
  } else {
    poly_matrix <- sapply(
      seq_len(degree),
      function(power) dat$gca_time_z ^ power
    )
    poly_matrix <- as.matrix(poly_matrix)
    poly_coefs <- NULL
  }

  for (term_i in seq_len(degree)) {
    dat[[paste0("time_poly_", term_i)]] <- as.numeric(poly_matrix[, term_i])
  }

  out <- dat |>
    dplyr::mutate(
      subject = .data[[".gp3_gca_subject"]],
      condition = .data[[".gp3_gca_condition"]],
      gca_pupil = .data[[".gp3_gca_pupil"]],
      gca_weight = .data[[".gp3_gca_weights"]],
      pupil_col = selected_pupil_col,
      time_col = selected_time_col,
      degree = selected_degree,
      orthogonal = selected_orthogonal,
      condition_status = condition_status,
      gca_data_status = dplyr::case_when(
        condition_status != "ok" ~ condition_status,
        TRUE ~ "ok"
      )
    )

  selected_cols <- unique(
    c(
      "subject",
      "condition",
      "gca_time",
      "gca_time_centered",
      "gca_time_z",
      paste0("time_poly_", seq_len(degree)),
      "gca_pupil",
      "gca_weight",
      if (!is.null(valid_samples_col) &&
          valid_samples_col %in% names(out)) valid_samples_col else NULL,
      if ("n_samples" %in% names(out)) "n_samples" else NULL,
      if ("n_valid_samples" %in% names(out)) "n_valid_samples" else NULL,
      "pupil_col",
      "time_col",
      "degree",
      "orthogonal",
      "condition_status",
      "gca_data_status"
    )
  )

  out <- out |>
    dplyr::select(dplyr::all_of(selected_cols))

  attr(out, "poly_coefs") <- poly_coefs
  attr(out, "time_mean") <- time_mean
  attr(out, "time_sd") <- time_sd
  attr(out, "settings") <- list(
    pupil_col = selected_pupil_col,
    time_col = selected_time_col,
    subject_col = selected_subject_col,
    condition_col = selected_condition_col,
    degree = selected_degree,
    orthogonal = selected_orthogonal,
    time_window = selected_time_window,
    valid_samples_col = selected_valid_samples_col,
    min_valid_samples = selected_min_valid_samples,
    weights_col = selected_weights_col,
    missing_condition_label = selected_missing_condition_label
  )

  class(out) <- c("gp3_gca_data", class(out))

  out
}
