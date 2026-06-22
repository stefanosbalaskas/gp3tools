#' Summarise Gazepoint pupil data
#'
#' Creates compact pupil-quality and pupil-distribution summaries from a
#' Gazepoint master sample-level table created by [as_gazepoint_master()] or
#' [create_gazepoint_master()]. This function is intended as the first pupil
#' preprocessing gate before interpolation, filtering, baseline correction, or
#' pupil-based modelling.
#'
#' @param master A Gazepoint master sample-level table.
#' @param group_cols Character vector of grouping columns. Defaults to
#'   `c("subject", "media_id")` using internally standardised names. Use
#'   `character(0)` for an overall summary.
#' @param pupil_col Optional name of the pupil column to summarise. If `NULL`,
#'   the function detects one of `mean_pupil`, `pupil`, `pupil_raw`,
#'   `left_pupil`, or `right_pupil`.
#' @param time_col Optional name of the time column. If `NULL`, the function
#'   detects one of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.
#' @param missing_pupil_col Optional name of the missing-pupil flag column. If
#'   `NULL`, the function uses `missing_pupil` when available.
#' @param min_pupil Minimum plausible pupil value. Defaults to `0`.
#' @param max_pupil Maximum plausible pupil value. Defaults to `Inf`.
#'   Use narrower values, such as `1` and `9`, only when the pupil column is
#'   known to be measured in millimetres.
#' @param outlier_k Multiplier for IQR-based outlier detection. Defaults to
#'   `1.5`.
#'
#' @return A tibble with pupil-quality and pupil-distribution summaries.
#'
#' @examples
#' \donttest{
#' master <- gazepoint_example_master
#' master <- create_gazepoint_master(
#'   gaze_data = gazepoint_example_master,
#'   screen_width_px = 1920,
#'   screen_height_px = 1080
#' )
#'
#' summarise_gazepoint_pupil(master)
#' summarise_gazepoint_pupil(master, group_cols = "subject")
#' summarise_gazepoint_pupil(master, group_cols = character(0))
#' }
#'
#' @importFrom rlang .data
#'
#' @export
summarise_gazepoint_pupil <- function(
    master,
    group_cols = c("subject", "media_id"),
    pupil_col = NULL,
    time_col = NULL,
    missing_pupil_col = NULL,
    min_pupil = 0,
    max_pupil = Inf,
    outlier_k = 1.5
) {
  if (!is.data.frame(master)) {
    rlang::abort("`master` must be a data frame.")
  }

  if (!is.character(group_cols)) {
    rlang::abort("`group_cols` must be a character vector.")
  }

  if (!is.null(pupil_col) && (!is.character(pupil_col) || length(pupil_col) != 1)) {
    rlang::abort("`pupil_col` must be `NULL` or a single character string.")
  }

  if (!is.null(time_col) && (!is.character(time_col) || length(time_col) != 1)) {
    rlang::abort("`time_col` must be `NULL` or a single character string.")
  }

  if (
    !is.null(missing_pupil_col) &&
    (!is.character(missing_pupil_col) || length(missing_pupil_col) != 1)
  ) {
    rlang::abort("`missing_pupil_col` must be `NULL` or a single character string.")
  }

  if (!is.numeric(min_pupil) || length(min_pupil) != 1) {
    rlang::abort("`min_pupil` must be a single numeric value.")
  }

  if (!is.numeric(max_pupil) || length(max_pupil) != 1) {
    rlang::abort("`max_pupil` must be a single numeric value.")
  }

  if (!is.numeric(outlier_k) || length(outlier_k) != 1) {
    rlang::abort("`outlier_k` must be a single numeric value.")
  }

  if (max_pupil <= min_pupil) {
    rlang::abort("`max_pupil` must be greater than `min_pupil`.")
  }

  min_plausible_value <- min_pupil
  max_plausible_value <- max_pupil

  detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(master)]

    if (length(found) == 0) {
      return(NA_character_)
    }

    found[[1]]
  }

  subject_source <- detect_col(c("subject", "pID", "participant"))
  media_source <- detect_col(c("media_id", "MEDIA_ID"))

  if (is.null(pupil_col)) {
    pupil_source <- detect_col(c(
      "mean_pupil",
      "pupil",
      "pupil_raw",
      "left_pupil",
      "right_pupil"
    ))
  } else {
    pupil_source <- pupil_col
  }

  if (is.null(time_col)) {
    time_source <- detect_col(c("time_ms", "time", "time_orig", "time_orig_ms"))
  } else {
    time_source <- time_col
  }

  if (is.null(missing_pupil_col)) {
    missing_pupil_source <- detect_col(c("missing_pupil"))
  } else {
    missing_pupil_source <- missing_pupil_col
  }

  if (is.na(subject_source)) {
    rlang::abort("No subject column was found.")
  }

  if (is.na(media_source)) {
    rlang::abort("No media/stimulus column was found.")
  }

  if (is.na(pupil_source) || !pupil_source %in% names(master)) {
    rlang::abort("No pupil column was found.")
  }

  if (is.na(time_source) || !time_source %in% names(master)) {
    rlang::abort("No time column was found.")
  }

  if (!is.na(missing_pupil_source) && !missing_pupil_source %in% names(master)) {
    rlang::abort("`missing_pupil_col` was not found in `master`.")
  }

  data <- tibble::tibble(
    subject = as.character(master[[subject_source]]),
    media_id = as.character(master[[media_source]]),
    time_ms = suppressWarnings(as.numeric(master[[time_source]])),
    pupil = suppressWarnings(as.numeric(master[[pupil_source]])),
    missing_pupil = if (!is.na(missing_pupil_source)) {
      as.logical(master[[missing_pupil_source]])
    } else {
      is.na(suppressWarnings(as.numeric(master[[pupil_source]])))
    }
  ) |>
    dplyr::mutate(
      missing_pupil = dplyr::coalesce(.data$missing_pupil, is.na(.data$pupil)),
      pupil_valid = !.data$missing_pupil & !is.na(.data$pupil),
      pupil_for_summary = dplyr::if_else(
        .data$pupil_valid,
        .data$pupil,
        NA_real_
      )
    )

  allowed_group_cols <- c("subject", "media_id")

  invalid_group_cols <- setdiff(group_cols, allowed_group_cols)

  if (length(invalid_group_cols) > 0) {
    rlang::abort(
      paste0(
        "`group_cols` can only contain: ",
        paste(allowed_group_cols, collapse = ", ")
      )
    )
  }

  safe_mean <- function(x) {
    x <- suppressWarnings(as.numeric(x))

    if (length(x) == 0 || all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE)
  }

  safe_median <- function(x) {
    x <- suppressWarnings(as.numeric(x))

    if (length(x) == 0 || all(is.na(x))) {
      return(NA_real_)
    }

    stats::median(x, na.rm = TRUE)
  }

  safe_sd <- function(x) {
    x <- suppressWarnings(as.numeric(x))

    if (length(x) == 0 || sum(!is.na(x)) < 2) {
      return(NA_real_)
    }

    stats::sd(x, na.rm = TRUE)
  }

  safe_min <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    min(x)
  }

  safe_max <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    max(x)
  }

  safe_quantile <- function(x, prob) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    unname(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
  }

  pct <- function(n, d) {
    if (is.na(d) || d == 0) {
      return(NA_real_)
    }

    n / d * 100
  }

  count_iqr_outliers <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    x <- x[x >= min_plausible_value & x <= max_plausible_value]

    if (length(x) < 4) {
      return(0L)
    }

    qs <- stats::quantile(
      x,
      probs = c(0.25, 0.75),
      na.rm = TRUE,
      names = FALSE
    )

    iqr_value <- qs[[2]] - qs[[1]]

    if (!is.finite(iqr_value) || iqr_value == 0) {
      return(0L)
    }

    lower <- qs[[1]] - outlier_k * iqr_value
    upper <- qs[[2]] + outlier_k * iqr_value

    sum(x < lower | x > upper, na.rm = TRUE)
  }

  summarise_data <- function(data) {
    data |>
      dplyr::summarise(
        n_rows = dplyr::n(),
        time_min_ms = safe_min(.data$time_ms),
        time_max_ms = safe_max(.data$time_ms),
        time_span_ms = safe_max(.data$time_ms) - safe_min(.data$time_ms),
        n_pupil_samples = sum(.data$pupil_valid, na.rm = TRUE),
        n_missing_pupil = sum(!.data$pupil_valid, na.rm = TRUE),
        missing_pupil_pct = pct(
          sum(!.data$pupil_valid, na.rm = TRUE),
          dplyr::n()
        ),
        valid_pupil_pct = pct(
          sum(.data$pupil_valid, na.rm = TRUE),
          dplyr::n()
        ),
        mean_pupil = safe_mean(.data$pupil_for_summary),
        median_pupil = safe_median(.data$pupil_for_summary),
        sd_pupil = safe_sd(.data$pupil_for_summary),
        min_pupil = safe_min(.data$pupil_for_summary),
        max_pupil = safe_max(.data$pupil_for_summary),
        q05_pupil = safe_quantile(.data$pupil_for_summary, 0.05),
        q25_pupil = safe_quantile(.data$pupil_for_summary, 0.25),
        q75_pupil = safe_quantile(.data$pupil_for_summary, 0.75),
        q95_pupil = safe_quantile(.data$pupil_for_summary, 0.95),
        n_below_plausible = sum(
          .data$pupil_valid & .data$pupil < min_plausible_value,
          na.rm = TRUE
        ),
        n_above_plausible = sum(
          .data$pupil_valid & .data$pupil > max_plausible_value,
          na.rm = TRUE
        ),
        n_implausible =
          sum(
            .data$pupil_valid & .data$pupil < min_plausible_value,
            na.rm = TRUE
          ) +
          sum(
            .data$pupil_valid & .data$pupil > max_plausible_value,
            na.rm = TRUE
          ),
        implausible_pct = pct(
          sum(
            .data$pupil_valid & .data$pupil < min_plausible_value,
            na.rm = TRUE
          ) +
            sum(
              .data$pupil_valid & .data$pupil > max_plausible_value,
              na.rm = TRUE
            ),
          dplyr::n()
        ),
        n_iqr_outliers = count_iqr_outliers(.data$pupil_for_summary),
        iqr_outlier_pct = pct(
          count_iqr_outliers(.data$pupil_for_summary),
          sum(.data$pupil_valid, na.rm = TRUE)
        ),
        pupil_column = pupil_source,
        time_column = time_source,
        min_plausible = min_plausible_value,
        max_plausible = max_plausible_value,
        .groups = "drop"
      )
  }

  if (length(group_cols) == 0) {
    return(summarise_data(data))
  }

  data |>
    dplyr::group_by(!!!rlang::syms(group_cols)) |>
    summarise_data()
}
