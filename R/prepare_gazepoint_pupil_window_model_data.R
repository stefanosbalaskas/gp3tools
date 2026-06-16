#' Prepare pupil-window data for confirmatory mixed models
#'
#' Prepare pupil-window summaries or pupil trial-feature tables for
#' confirmatory window-level modelling. The function standardises subject,
#' condition, window, trial/media identifiers, outcome, valid-sample counts,
#' total-sample counts, valid-sample proportions, weights, and model-readiness
#' status columns.
#'
#' @param data Pupil-window summary data.
#' @param outcome_col Column containing the pupil outcome to model. The default
#'   is `mean_pupil`.
#' @param subject_col Subject/participant column.
#' @param condition_col Optional condition column. Common aliases such as
#'   `condition`, `Condition`, and `CONDITION` are detected when available.
#' @param window_col Pupil-window label column.
#' @param window_start_col Optional window-start column.
#' @param window_end_col Optional window-end column.
#' @param trial_col Optional trial identifier column.
#' @param media_col Optional media/stimulus identifier column. Common aliases
#'   such as `media_id` and `MEDIA_ID` are detected when available.
#' @param valid_samples_col Optional column containing the number of valid
#'   pupil samples in the window. Common aliases such as `n_valid_pupil` and
#'   `n_valid_samples` are detected when available.
#' @param total_samples_col Optional column containing the total number of
#'   samples in the window. Common aliases such as `n_samples` and
#'   `n_window_samples` are detected when available.
#' @param min_valid_samples Minimum acceptable number of valid pupil samples.
#' @param min_valid_prop Minimum acceptable valid-sample proportion.
#' @param drop_invalid Logical. If `TRUE`, rows with invalid or low-quality
#'   model inputs are removed.
#' @param missing_condition_label Label used when condition is missing.
#' @param outcome_label Label stored in the output to identify the modelled
#'   pupil outcome.
#'
#' @return A tibble of pupil-window rows prepared for confirmatory modelling.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_pupil_window_model_data <- function(
    data,
    outcome_col = "mean_pupil",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    trial_col = NULL,
    media_col = "media_id",
    valid_samples_col = "n_valid_pupil",
    total_samples_col = "n_samples",
    min_valid_samples = 5,
    min_valid_prop = 0.70,
    drop_invalid = TRUE,
    missing_condition_label = "all_data",
    outcome_label = "pupil"
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

  valid_column(outcome_col, "outcome_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_column(window_col, "window_col")
  valid_optional_column(window_start_col, "window_start_col")
  valid_optional_column(window_end_col, "window_end_col")
  valid_optional_column(trial_col, "trial_col")
  valid_optional_column(media_col, "media_col")
  valid_optional_column(valid_samples_col, "valid_samples_col")
  valid_optional_column(total_samples_col, "total_samples_col")

  if (!is.numeric(min_valid_samples) ||
      length(min_valid_samples) != 1L ||
      is.na(min_valid_samples) ||
      !is.finite(min_valid_samples) ||
      min_valid_samples < 0) {
    stop(
      "`min_valid_samples` must be a non-negative finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.numeric(min_valid_prop) ||
      length(min_valid_prop) != 1L ||
      is.na(min_valid_prop) ||
      !is.finite(min_valid_prop) ||
      min_valid_prop < 0 ||
      min_valid_prop > 1) {
    stop(
      "`min_valid_prop` must be a finite numeric scalar between 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.logical(drop_invalid) ||
      length(drop_invalid) != 1L ||
      is.na(drop_invalid)) {
    stop("`drop_invalid` must be TRUE or FALSE.", call. = FALSE)
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

  dat <- tibble::as_tibble(data)

  resolve_optional_column <- function(col, aliases) {
    if (is.null(col)) {
      return(NULL)
    }

    if (col %in% names(dat)) {
      return(col)
    }

    fallback <- aliases[aliases %in% names(dat)]

    if (length(fallback) > 0L) {
      return(fallback[[1L]])
    }

    col
  }

  condition_col <- resolve_optional_column(
    condition_col,
    c("condition", "Condition", "CONDITION")
  )

  media_col <- resolve_optional_column(
    media_col,
    c("media_id", "MEDIA_ID", "media", "MEDIA")
  )

  valid_samples_col <- resolve_optional_column(
    valid_samples_col,
    c("n_valid_pupil", "n_valid_samples", "valid_samples", "n_valid")
  )

  total_samples_col <- resolve_optional_column(
    total_samples_col,
    c("n_samples", "n_window_samples", "total_samples", "n_total_samples")
  )

  required_cols <- c(outcome_col, subject_col, window_col)

  missing_required_cols <- setdiff(required_cols, names(dat))

  if (length(missing_required_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat$pupil_model_outcome <- suppressWarnings(
    as.numeric(dat[[outcome_col]])
  )

  dat$pupil_model_subject <- as.character(dat[[subject_col]])
  dat$pupil_model_subject <- trimws(dat$pupil_model_subject)
  dat$pupil_model_subject[
    is.na(dat$pupil_model_subject) |
      !nzchar(dat$pupil_model_subject)
  ] <- "unknown_subject"

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    dat$pupil_model_condition <- as.character(dat[[condition_col]])
    dat$pupil_model_condition <- trimws(dat$pupil_model_condition)
    dat$pupil_model_condition[
      is.na(dat$pupil_model_condition) |
        !nzchar(dat$pupil_model_condition)
    ] <- missing_condition_label
  } else {
    dat$pupil_model_condition <- missing_condition_label
  }

  dat$pupil_model_window <- as.character(dat[[window_col]])
  dat$pupil_model_window <- trimws(dat$pupil_model_window)
  dat$pupil_model_window[
    is.na(dat$pupil_model_window) |
      !nzchar(dat$pupil_model_window)
  ] <- "unknown_window"

  if (!is.null(window_start_col) && window_start_col %in% names(dat)) {
    dat$pupil_model_window_start_ms <- suppressWarnings(
      as.numeric(dat[[window_start_col]])
    )
  } else {
    dat$pupil_model_window_start_ms <- NA_real_
  }

  if (!is.null(window_end_col) && window_end_col %in% names(dat)) {
    dat$pupil_model_window_end_ms <- suppressWarnings(
      as.numeric(dat[[window_end_col]])
    )
  } else {
    dat$pupil_model_window_end_ms <- NA_real_
  }

  if (!is.null(trial_col) && trial_col %in% names(dat)) {
    dat$pupil_model_trial <- as.character(dat[[trial_col]])
    dat$pupil_model_trial <- trimws(dat$pupil_model_trial)
    dat$pupil_model_trial[
      is.na(dat$pupil_model_trial) |
        !nzchar(dat$pupil_model_trial)
    ] <- NA_character_
  } else {
    dat$pupil_model_trial <- NA_character_
  }

  if (!is.null(media_col) && media_col %in% names(dat)) {
    dat$pupil_model_media <- as.character(dat[[media_col]])
    dat$pupil_model_media <- trimws(dat$pupil_model_media)
    dat$pupil_model_media[
      is.na(dat$pupil_model_media) |
        !nzchar(dat$pupil_model_media)
    ] <- NA_character_
  } else {
    dat$pupil_model_media <- NA_character_
  }

  if (!is.null(valid_samples_col) && valid_samples_col %in% names(dat)) {
    dat$pupil_model_valid_samples <- suppressWarnings(
      as.numeric(dat[[valid_samples_col]])
    )
  } else {
    dat$pupil_model_valid_samples <- NA_real_
  }

  if (!is.null(total_samples_col) && total_samples_col %in% names(dat)) {
    dat$pupil_model_total_samples <- suppressWarnings(
      as.numeric(dat[[total_samples_col]])
    )
  } else {
    dat$pupil_model_total_samples <- NA_real_
  }

  dat$pupil_model_valid_prop <- dplyr::if_else(
    is.finite(dat$pupil_model_valid_samples) &
      is.finite(dat$pupil_model_total_samples) &
      dat$pupil_model_total_samples > 0,
    dat$pupil_model_valid_samples / dat$pupil_model_total_samples,
    NA_real_
  )

  dat$pupil_model_weight <- dat$pupil_model_valid_samples
  dat$pupil_model_outcome_label <- outcome_label
  dat$pupil_model_outcome_col <- outcome_col

  dat$pupil_model_valid_samples_col <- if (is.null(valid_samples_col)) {
    NA_character_
  } else {
    valid_samples_col
  }

  dat$pupil_model_total_samples_col <- if (is.null(total_samples_col)) {
    NA_character_
  } else {
    total_samples_col
  }

  dat$pupil_model_status <- dplyr::case_when(
    is.na(dat$pupil_model_outcome) ~ "missing_outcome",
    !is.finite(dat$pupil_model_outcome) ~ "non_finite_outcome",
    !is.finite(dat$pupil_model_valid_samples) ~ "missing_valid_samples",
    !is.finite(dat$pupil_model_total_samples) ~ "missing_total_samples",
    dat$pupil_model_valid_samples < 0 ~ "negative_valid_samples",
    dat$pupil_model_total_samples < 0 ~ "negative_total_samples",
    dat$pupil_model_total_samples == 0 ~ "zero_total_samples",
    dat$pupil_model_valid_samples > dat$pupil_model_total_samples ~
      "valid_exceeds_total",
    dat$pupil_model_valid_samples < min_valid_samples ~ "low_valid_samples",
    !is.finite(dat$pupil_model_valid_prop) ~ "missing_valid_prop",
    dat$pupil_model_valid_prop < min_valid_prop ~ "low_valid_prop",
    TRUE ~ "ok"
  )

  if (drop_invalid) {
    dat <- dat |>
      dplyr::filter(.data[["pupil_model_status"]] == "ok")
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after preparing pupil-window model data.",
      call. = FALSE
    )
  }

  dat$pupil_model_subject <- factor(dat$pupil_model_subject)
  dat$pupil_model_condition <- factor(dat$pupil_model_condition)

  dat$pupil_model_window <- factor(
    dat$pupil_model_window,
    levels = unique(dat$pupil_model_window[order(
      dat$pupil_model_window_start_ms,
      dat$pupil_model_window
    )])
  )

  dat <- dat |>
    dplyr::arrange(
      .data[["pupil_model_subject"]],
      .data[["pupil_model_condition"]],
      .data[["pupil_model_window_start_ms"]],
      .data[["pupil_model_window"]]
    )

  class(dat) <- c("gp3_pupil_window_model_data", class(dat))

  attr(dat, "settings") <- list(
    outcome_col = outcome_col,
    subject_col = subject_col,
    condition_col = condition_col,
    window_col = window_col,
    window_start_col = window_start_col,
    window_end_col = window_end_col,
    trial_col = trial_col,
    media_col = media_col,
    valid_samples_col = valid_samples_col,
    total_samples_col = total_samples_col,
    min_valid_samples = min_valid_samples,
    min_valid_prop = min_valid_prop,
    drop_invalid = drop_invalid,
    missing_condition_label = missing_condition_label,
    outcome_label = outcome_label
  )

  dat
}
