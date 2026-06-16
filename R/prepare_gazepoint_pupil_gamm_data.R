#' Prepare Gazepoint pupil GAMM data
#'
#' Prepare binned pupil time-course data for GAMM modelling with `mgcv::bam()`.
#' The function aggregates processed sample-level pupil data into subject-by-
#' condition-by-time-bin rows and creates an `AR.start` indicator for
#' autoregressive GAMM models.
#'
#' @param data A Gazepoint sample-level data frame, usually after pupil
#'   preprocessing, interpolation, baseline correction, and optional smoothing.
#' @param pupil_col Name of the pupil column to aggregate. If `NULL`, the
#'   function tries common processed pupil columns such as `pupil_smoothed`,
#'   `pupil_baseline_corrected`, `pupil_interpolated`, `pupil_clean`, and
#'   `pupil_for_preprocessing`.
#' @param time_col Name of the time column in milliseconds. If the requested
#'   column is not available, the function tries common alternatives.
#' @param subject_col Name of the subject column. If unavailable, the function
#'   tries common participant identifiers.
#' @param condition_col Name of the condition column. If unavailable or entirely
#'   missing, a single condition label is used.
#' @param x_col Optional gaze x-coordinate column. If `NULL`, common x-coordinate
#'   columns are auto-detected when available.
#' @param y_col Optional gaze y-coordinate column. If `NULL`, common y-coordinate
#'   columns are auto-detected when available.
#' @param group_cols Columns defining independent time series before binning.
#'   Defaults to `c("subject", "condition")`.
#' @param bin_width_ms Width of time bins in milliseconds.
#' @param time_window Optional numeric vector of length 2 giving the time window
#'   to retain before binning.
#' @param min_valid_samples Minimum number of valid pupil samples required for a
#'   bin to be retained.
#' @param missing_condition_label Label used when condition values are missing or
#'   when no usable condition column is available.
#'
#' @return A tibble with binned pupil time-course data for GAMM modelling.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_pupil_gamm_data <- function(
    data,
    pupil_col = NULL,
    time_col = "time",
    subject_col = "subject",
    condition_col = "condition",
    x_col = NULL,
    y_col = NULL,
    group_cols = c("subject", "condition"),
    bin_width_ms = 50,
    time_window = NULL,
    min_valid_samples = 1,
    missing_condition_label = "all_data"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_optional_column <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          nzchar(x)
      )
  }

  if (!valid_optional_column(pupil_col)) {
    stop(
      "`pupil_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(time_col)) {
    stop(
      "`time_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(subject_col)) {
    stop(
      "`subject_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(condition_col)) {
    stop(
      "`condition_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(x_col)) {
    stop(
      "`x_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(y_col)) {
    stop(
      "`y_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.character(group_cols) ||
      length(group_cols) < 1L ||
      any(is.na(group_cols)) ||
      any(!nzchar(group_cols)) ||
      anyDuplicated(group_cols)) {
    stop(
      "`group_cols` must be a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.numeric(bin_width_ms) ||
      length(bin_width_ms) != 1L ||
      is.na(bin_width_ms) ||
      !is.finite(bin_width_ms) ||
      bin_width_ms <= 0) {
    stop(
      "`bin_width_ms` must be a positive finite numeric scalar.",
      call. = FALSE
    )
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

  dat <- tibble::as_tibble(data)

  first_existing <- function(candidates, names_available) {
    hit <- candidates[candidates %in% names_available]

    if (length(hit) == 0L) {
      return(NULL)
    }

    hit[[1]]
  }

  first_informative_existing <- function(candidates, dat) {
    hit <- candidates[candidates %in% names(dat)]

    if (length(hit) == 0L) {
      return(NULL)
    }

    for (col in hit) {
      values <- dat[[col]]
      values <- as.character(values)
      values <- trimws(values)
      values <- values[!is.na(values) & nzchar(values)]

      if (length(unique(values)) > 0L) {
        return(col)
      }
    }

    hit[[1]]
  }

  if (is.null(pupil_col)) {
    pupil_col <- first_existing(
      c(
        "pupil_smoothed",
        "pupil_baseline_corrected",
        "pupil_interpolated",
        "pupil_clean",
        "pupil_for_preprocessing",
        "pupil",
        "pupil_raw",
        "PUPIL",
        "LPD",
        "RPD"
      ),
      names(dat)
    )
  }

  if (is.null(pupil_col) || !pupil_col %in% names(dat)) {
    stop(
      "Could not automatically detect a pupil column. Please provide `pupil_col`.",
      call. = FALSE
    )
  }

  if (is.null(time_col) || !time_col %in% names(dat)) {
    time_col <- first_existing(
      c(
        "time",
        "time_orig",
        "TIME",
        "TIMETICK",
        "timestamp",
        "timestamp_ms"
      ),
      names(dat)
    )
  }

  if (is.null(time_col) || !time_col %in% names(dat)) {
    stop(
      "Could not automatically detect a time column. Please provide `time_col`.",
      call. = FALSE
    )
  }

  if (is.null(subject_col) || !subject_col %in% names(dat)) {
    subject_col <- first_informative_existing(
      c(
        "subject",
        "USER_FILE",
        "USER_ID",
        "USER",
        "user",
        "participant",
        "participant_id"
      ),
      dat
    )
  }

  if (is.null(subject_col) || !subject_col %in% names(dat)) {
    stop(
      "Could not automatically detect a subject column. Please provide `subject_col`.",
      call. = FALSE
    )
  }

  condition_status <- "ok"

  if (is.null(condition_col) || !condition_col %in% names(dat)) {
    dat$condition <- missing_condition_label
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

    dat$condition <- condition_values
  }

  dat$subject <- as.character(dat[[subject_col]])

  if (any(is.na(dat$subject) | !nzchar(trimws(dat$subject)))) {
    dat$subject[
      is.na(dat$subject) |
        !nzchar(trimws(dat$subject))
    ] <- "unknown_subject"
  }

  if (is.null(x_col)) {
    x_col <- first_existing(
      c(
        "mean_x",
        "gaze_x",
        "x",
        "X",
        "FPOGX",
        "BPOGX",
        "left_x",
        "right_x"
      ),
      names(dat)
    )
  }

  if (!is.null(x_col) && !x_col %in% names(dat)) {
    stop(
      "Missing required columns: ",
      x_col,
      call. = FALSE
    )
  }

  if (is.null(y_col)) {
    y_col <- first_existing(
      c(
        "mean_y",
        "gaze_y",
        "y",
        "Y",
        "FPOGY",
        "BPOGY",
        "left_y",
        "right_y"
      ),
      names(dat)
    )
  }

  if (!is.null(y_col) && !y_col %in% names(dat)) {
    stop(
      "Missing required columns: ",
      y_col,
      call. = FALSE
    )
  }

  missing_group_cols <- setdiff(group_cols, names(dat))

  if (length(missing_group_cols) > 0L) {
    stop(
      "Missing required grouping columns: ",
      paste(missing_group_cols, collapse = ", "),
      call. = FALSE
    )
  }

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[is.finite(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  median_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[is.finite(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    stats::median(x)
  }

  sd_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[is.finite(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    stats::sd(x)
  }

  min_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[is.finite(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    min(x)
  }

  max_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[is.finite(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x)
  }

  dat$.gp3_time_ms <- as_numeric_safe(dat[[time_col]])
  dat$.gp3_pupil <- as_numeric_safe(dat[[pupil_col]])

  dat$.gp3_x <- if (!is.null(x_col)) {
    as_numeric_safe(dat[[x_col]])
  } else {
    NA_real_
  }

  dat$.gp3_y <- if (!is.null(y_col)) {
    as_numeric_safe(dat[[y_col]])
  } else {
    NA_real_
  }

  dat <- dat |>
    dplyr::filter(!is.na(.data[[".gp3_time_ms"]]))

  if (!is.null(time_window)) {
    lower <- min(time_window)
    upper <- max(time_window)

    dat <- dat |>
      dplyr::filter(
        .data[[".gp3_time_ms"]] >= lower,
        .data[[".gp3_time_ms"]] <= upper
      )
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after applying time filtering.",
      call. = FALSE
    )
  }

  dat <- dat |>
    dplyr::mutate(
      .gp3_pupil_valid =
        is.finite(.data[[".gp3_pupil"]]) &
        !is.na(.data[[".gp3_pupil"]]),
      time_bin = floor(.data[[".gp3_time_ms"]] / bin_width_ms),
      time_bin_start_ms = .data[["time_bin"]] * bin_width_ms,
      time_bin_end_ms = .data[["time_bin_start_ms"]] + bin_width_ms,
      time_bin_center_ms =
        .data[["time_bin_start_ms"]] + (bin_width_ms / 2)
    )

  binned <- dat |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_cols, "time_bin")))
    ) |>
    dplyr::summarise(
      time_bin_start_ms = min_or_na(.data[["time_bin_start_ms"]]),
      time_bin_end_ms = max_or_na(.data[["time_bin_end_ms"]]),
      time_bin_center_ms = mean_or_na(.data[["time_bin_center_ms"]]),

      n_samples = dplyr::n(),
      n_valid_samples = sum(.data[[".gp3_pupil_valid"]], na.rm = TRUE),
      valid_sample_prop = .data[["n_valid_samples"]] / .data[["n_samples"]],

      mean_pupil = mean_or_na(
        .data[[".gp3_pupil"]][.data[[".gp3_pupil_valid"]]]
      ),
      median_pupil = median_or_na(
        .data[[".gp3_pupil"]][.data[[".gp3_pupil_valid"]]]
      ),
      sd_pupil = sd_or_na(
        .data[[".gp3_pupil"]][.data[[".gp3_pupil_valid"]]]
      ),
      min_pupil = min_or_na(
        .data[[".gp3_pupil"]][.data[[".gp3_pupil_valid"]]]
      ),
      max_pupil = max_or_na(
        .data[[".gp3_pupil"]][.data[[".gp3_pupil_valid"]]]
      ),

      mean_x = mean_or_na(
        .data[[".gp3_x"]][.data[[".gp3_pupil_valid"]]]
      ),
      mean_y = mean_or_na(
        .data[[".gp3_y"]][.data[[".gp3_pupil_valid"]]]
      ),

      .groups = "drop"
    ) |>
    dplyr::filter(
      .data[["n_valid_samples"]] >= min_valid_samples
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[["time_bin_start_ms"]]
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      AR.start = dplyr::row_number() == 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pupil_col = pupil_col,
      time_col = time_col,
      bin_width_ms = bin_width_ms,
      condition_status = condition_status,
      gamm_data_status = dplyr::case_when(
        .data[["n_valid_samples"]] < min_valid_samples ~
          "insufficient_valid_samples",
        condition_status != "ok" ~ condition_status,
        TRUE ~ "ok"
      )
    )

  if (nrow(binned) == 0L) {
    stop(
      "No pupil bins remain after applying `min_valid_samples`.",
      call. = FALSE
    )
  }

  binned |>
    dplyr::select(
      dplyr::all_of(
        unique(
          c(
            group_cols,
            "time_bin",
            "time_bin_start_ms",
            "time_bin_end_ms",
            "time_bin_center_ms",
            "mean_pupil",
            "median_pupil",
            "sd_pupil",
            "min_pupil",
            "max_pupil",
            "n_samples",
            "n_valid_samples",
            "valid_sample_prop",
            "mean_x",
            "mean_y",
            "AR.start",
            "pupil_col",
            "time_col",
            "bin_width_ms",
            "condition_status",
            "gamm_data_status"
          )
        )
      )
    )
}
