#' Prepare AOI-window data for binomial GLMMs
#'
#' Prepare AOI-window summaries for confirmatory binomial mixed-effects
#' modelling. The function creates success, failure, denominator, proportion,
#' subject, condition, and window columns from output produced by
#' `summarise_gazepoint_aoi_windows()`.
#'
#' @param data AOI-window summary data.
#' @param success_col Column containing the success count. For target-looking
#'   models this is usually `n_target_samples`.
#' @param denominator Denominator definition. Use `"valid"` for valid AOI-window
#'   denominator samples, `"all"` for all window samples, `"aoi"` for AOI-only
#'   samples, or `"custom"` with `denominator_col`.
#' @param denominator_col Custom denominator column when `denominator = "custom"`.
#' @param valid_denominator_col Column used when `denominator = "valid"`.
#' @param all_denominator_col Column used when `denominator = "all"`.
#' @param aoi_denominator_col Column used when `denominator = "aoi"`.
#' @param subject_col Subject/participant column.
#' @param condition_col Optional condition column.
#' @param window_col AOI-window label column.
#' @param window_start_col Optional window-start column.
#' @param window_end_col Optional window-end column.
#' @param group_cols Optional extra grouping columns to keep/check.
#' @param min_denominator_samples Minimum acceptable denominator.
#' @param drop_invalid Logical. If `TRUE`, rows with invalid binomial counts or
#'   too-small denominators are removed.
#' @param missing_condition_label Label used when condition is missing.
#' @param outcome_label Label stored in the output to identify the modelled AOI
#'   outcome.
#'
#' @return A tibble of GLMM-ready AOI-window rows.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_aoi_glmm_data <- function(
    data,
    success_col = "n_target_samples",
    denominator = c("valid", "all", "aoi", "custom"),
    denominator_col = NULL,
    valid_denominator_col = "n_valid_denominator_samples",
    all_denominator_col = "n_window_samples",
    aoi_denominator_col = "n_aoi_samples",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    group_cols = NULL,
    min_denominator_samples = 1,
    drop_invalid = TRUE,
    missing_condition_label = "all_data",
    outcome_label = "target"
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

  valid_column(success_col, "success_col")
  valid_optional_column(denominator_col, "denominator_col")
  valid_column(valid_denominator_col, "valid_denominator_col")
  valid_column(all_denominator_col, "all_denominator_col")
  valid_column(aoi_denominator_col, "aoi_denominator_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_column(window_col, "window_col")
  valid_optional_column(window_start_col, "window_start_col")
  valid_optional_column(window_end_col, "window_end_col")

  if (!is.null(group_cols) &&
      (!is.character(group_cols) ||
       any(is.na(group_cols)) ||
       any(!nzchar(group_cols)))) {
    stop(
      "`group_cols` must be NULL or a character vector of column names.",
      call. = FALSE
    )
  }

  if (!is.numeric(min_denominator_samples) ||
      length(min_denominator_samples) != 1L ||
      is.na(min_denominator_samples) ||
      !is.finite(min_denominator_samples) ||
      min_denominator_samples <= 0) {
    stop(
      "`min_denominator_samples` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.logical(drop_invalid) ||
      length(drop_invalid) != 1L ||
      is.na(drop_invalid)) {
    stop(
      "`drop_invalid` must be TRUE or FALSE.",
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

  if (!is.character(outcome_label) ||
      length(outcome_label) != 1L ||
      is.na(outcome_label) ||
      !nzchar(outcome_label)) {
    stop(
      "`outcome_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  denominator <- match.arg(denominator)

  if (identical(denominator, "custom") && is.null(denominator_col)) {
    stop(
      "`denominator_col` must be provided when `denominator = \"custom\"`.",
      call. = FALSE
    )
  }

  selected_denominator_col <- switch(
    denominator,
    valid = valid_denominator_col,
    all = all_denominator_col,
    aoi = aoi_denominator_col,
    custom = denominator_col
  )

  dat <- tibble::as_tibble(data)

  required_cols <- c(
    success_col,
    selected_denominator_col,
    subject_col,
    window_col
  )

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (!is.null(window_start_col) && window_start_col %in% names(dat)) {
    required_cols <- c(required_cols, window_start_col)
  }

  if (!is.null(window_end_col) && window_end_col %in% names(dat)) {
    required_cols <- c(required_cols, window_end_col)
  }

  if (!is.null(group_cols)) {
    required_cols <- c(required_cols, group_cols)
  }

  missing_cols <- setdiff(unique(required_cols), names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat$aoi_glmm_success <- suppressWarnings(as.numeric(dat[[success_col]]))
  dat$aoi_glmm_denominator <- suppressWarnings(
    as.numeric(dat[[selected_denominator_col]])
  )
  dat$aoi_glmm_failure <- dat$aoi_glmm_denominator - dat$aoi_glmm_success

  dat$aoi_glmm_subject <- as.character(dat[[subject_col]])
  dat$aoi_glmm_subject <- trimws(dat$aoi_glmm_subject)
  dat$aoi_glmm_subject[
    is.na(dat$aoi_glmm_subject) |
      !nzchar(dat$aoi_glmm_subject)
  ] <- "unknown_subject"

  dat$aoi_glmm_window <- as.character(dat[[window_col]])
  dat$aoi_glmm_window <- trimws(dat$aoi_glmm_window)
  dat$aoi_glmm_window[
    is.na(dat$aoi_glmm_window) |
      !nzchar(dat$aoi_glmm_window)
  ] <- "unknown_window"

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    dat$aoi_glmm_condition <- as.character(dat[[condition_col]])
    dat$aoi_glmm_condition <- trimws(dat$aoi_glmm_condition)
    dat$aoi_glmm_condition[
      is.na(dat$aoi_glmm_condition) |
        !nzchar(dat$aoi_glmm_condition)
    ] <- missing_condition_label
  } else {
    dat$aoi_glmm_condition <- missing_condition_label
  }

  if (!is.null(window_start_col) && window_start_col %in% names(dat)) {
    dat$aoi_glmm_window_start_ms <- suppressWarnings(
      as.numeric(dat[[window_start_col]])
    )
  } else {
    dat$aoi_glmm_window_start_ms <- NA_real_
  }

  if (!is.null(window_end_col) && window_end_col %in% names(dat)) {
    dat$aoi_glmm_window_end_ms <- suppressWarnings(
      as.numeric(dat[[window_end_col]])
    )
  } else {
    dat$aoi_glmm_window_end_ms <- NA_real_
  }

  dat$aoi_glmm_prop <- dplyr::if_else(
    is.finite(dat$aoi_glmm_denominator) &
      dat$aoi_glmm_denominator > 0,
    dat$aoi_glmm_success / dat$aoi_glmm_denominator,
    NA_real_
  )

  dat$aoi_glmm_weight <- dat$aoi_glmm_denominator
  dat$aoi_glmm_outcome <- outcome_label
  dat$aoi_glmm_denominator_type <- denominator
  dat$aoi_glmm_success_col <- success_col
  dat$aoi_glmm_denominator_col <- selected_denominator_col

  dat$aoi_glmm_success_zero <- is.finite(dat$aoi_glmm_success) &
    dat$aoi_glmm_success == 0

  dat$aoi_glmm_success_all <- is.finite(dat$aoi_glmm_success) &
    is.finite(dat$aoi_glmm_denominator) &
    dat$aoi_glmm_denominator > 0 &
    dat$aoi_glmm_success == dat$aoi_glmm_denominator

  dat$aoi_glmm_status <- dplyr::case_when(
    !is.finite(dat$aoi_glmm_success) ~ "missing_success",
    !is.finite(dat$aoi_glmm_denominator) ~ "missing_denominator",
    dat$aoi_glmm_success < 0 ~ "negative_success",
    dat$aoi_glmm_denominator < 0 ~ "negative_denominator",
    dat$aoi_glmm_success > dat$aoi_glmm_denominator ~ "success_exceeds_denominator",
    dat$aoi_glmm_denominator == 0 ~ "zero_denominator",
    dat$aoi_glmm_denominator < min_denominator_samples ~ "low_denominator",
    dat$aoi_glmm_failure < 0 ~ "negative_failure",
    TRUE ~ "ok"
  )

  if (drop_invalid) {
    dat <- dat |>
      dplyr::filter(.data[["aoi_glmm_status"]] == "ok")
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after preparing AOI GLMM data.",
      call. = FALSE
    )
  }

  dat$aoi_glmm_subject <- factor(dat$aoi_glmm_subject)
  dat$aoi_glmm_condition <- factor(dat$aoi_glmm_condition)
  dat$aoi_glmm_window <- factor(
    dat$aoi_glmm_window,
    levels = unique(dat$aoi_glmm_window[order(
      dat$aoi_glmm_window_start_ms,
      dat$aoi_glmm_window
    )])
  )

  dat <- dat |>
    dplyr::arrange(
      .data[["aoi_glmm_subject"]],
      .data[["aoi_glmm_condition"]],
      .data[["aoi_glmm_window_start_ms"]],
      .data[["aoi_glmm_window"]]
    )

  class(dat) <- c("gp3_aoi_glmm_data", class(dat))

  attr(dat, "settings") <- list(
    success_col = success_col,
    denominator = denominator,
    denominator_col = selected_denominator_col,
    subject_col = subject_col,
    condition_col = condition_col,
    window_col = window_col,
    window_start_col = window_start_col,
    window_end_col = window_end_col,
    group_cols = group_cols,
    min_denominator_samples = min_denominator_samples,
    drop_invalid = drop_invalid,
    missing_condition_label = missing_condition_label,
    outcome_label = outcome_label
  )

  dat
}
