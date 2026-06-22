#' Flag invalid, missing, implausible, and outlying Gazepoint pupil samples
#'
#' Adds pupil-quality flags to a Gazepoint master sample-level table created by
#' [as_gazepoint_master()] or [create_gazepoint_master()]. This function is
#' intended as a preprocessing step before interpolation, filtering, baseline
#' correction, or pupil-based modelling.
#'
#' @param master A Gazepoint master sample-level table.
#' @param pupil_col Optional name of the pupil column to flag. If `NULL`, the
#'   function detects one of `mean_pupil`, `pupil`, `pupil_raw`, `left_pupil`,
#'   or `right_pupil`.
#' @param time_col Optional name of the time column. If `NULL`, the function
#'   detects one of `time_ms`, `time`, `time_orig`, or `time_orig_ms`.
#' @param missing_pupil_col Optional name of the missing-pupil flag column. If
#'   `NULL`, the function uses `missing_pupil` when available.
#' @param group_cols Character vector of grouping columns used for IQR-based
#'   outlier detection. Defaults to `c("subject", "media_id")` using
#'   internally standardised names. Use `character(0)` for global outlier
#'   detection.
#' @param min_pupil Minimum plausible pupil value. Defaults to `0`.
#' @param max_pupil Maximum plausible pupil value. Defaults to `Inf`. Use
#'   narrower values, such as `1` and `9`, only when the pupil column is known
#'   to be measured in millimetres.
#' @param outlier_k Multiplier for IQR-based outlier detection. Defaults to
#'   `1.5`.
#' @param flag_iqr_outliers Logical. If `TRUE`, IQR-based outliers are flagged.
#'   Defaults to `TRUE`.
#'
#' @return A tibble containing the original master table plus pupil-flagging
#'   columns.
#'
#' @examples
#' \donttest{
#' master <- gazepoint_example_master
#' flagged <- flag_gazepoint_pupil(master)
#'
#' dplyr::count(flagged, pupil_flag_reason)
#' }
#'
#' @importFrom rlang .data
#'
#' @export
flag_gazepoint_pupil <- function(
    master,
    pupil_col = NULL,
    time_col = NULL,
    missing_pupil_col = NULL,
    group_cols = c("subject", "media_id"),
    min_pupil = 0,
    max_pupil = Inf,
    outlier_k = 1.5,
    flag_iqr_outliers = TRUE
) {
  if (!is.data.frame(master)) {
    rlang::abort("`master` must be a data frame.")
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

  if (!is.character(group_cols)) {
    rlang::abort("`group_cols` must be a character vector.")
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

  if (!is.logical(flag_iqr_outliers) || length(flag_iqr_outliers) != 1) {
    rlang::abort("`flag_iqr_outliers` must be `TRUE` or `FALSE`.")
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

  compute_iqr_flags <- function(x) {
    original_length <- length(x)
    x_numeric <- suppressWarnings(as.numeric(x))
    finite_values <- x_numeric[is.finite(x_numeric)]

    flags <- rep(FALSE, original_length)

    if (length(finite_values) < 4) {
      return(flags)
    }

    qs <- stats::quantile(
      finite_values,
      probs = c(0.25, 0.75),
      na.rm = TRUE,
      names = FALSE
    )

    iqr_value <- qs[[2]] - qs[[1]]

    if (!is.finite(iqr_value) || iqr_value == 0) {
      return(flags)
    }

    lower <- qs[[1]] - outlier_k * iqr_value
    upper <- qs[[2]] + outlier_k * iqr_value

    flags[is.finite(x_numeric)] <- x_numeric[is.finite(x_numeric)] < lower |
      x_numeric[is.finite(x_numeric)] > upper

    flags
  }

  work <- tibble::tibble(
    row_id = seq_len(nrow(master)),
    subject = as.character(master[[subject_source]]),
    media_id = as.character(master[[media_source]]),
    time_ms = suppressWarnings(as.numeric(master[[time_source]])),
    pupil_raw_value = suppressWarnings(as.numeric(master[[pupil_source]])),
    missing_pupil_source = if (!is.na(missing_pupil_source)) {
      as.logical(master[[missing_pupil_source]])
    } else {
      is.na(suppressWarnings(as.numeric(master[[pupil_source]])))
    }
  ) |>
    dplyr::mutate(
      pupil_flag_missing = dplyr::coalesce(
        .data$missing_pupil_source,
        is.na(.data$pupil_raw_value)
      ),
      pupil_flag_missing = .data$pupil_flag_missing | is.na(.data$pupil_raw_value),
      pupil_flag_nonfinite = !.data$pupil_flag_missing &
        !is.finite(.data$pupil_raw_value),
      pupil_flag_implausible_low = !.data$pupil_flag_missing &
        !.data$pupil_flag_nonfinite &
        .data$pupil_raw_value < min_plausible_value,
      pupil_flag_implausible_high = !.data$pupil_flag_missing &
        !.data$pupil_flag_nonfinite &
        .data$pupil_raw_value > max_plausible_value,
      pupil_flag_implausible = .data$pupil_flag_implausible_low |
        .data$pupil_flag_implausible_high,
      pupil_iqr_candidate = dplyr::if_else(
        !.data$pupil_flag_missing &
          !.data$pupil_flag_nonfinite &
          !.data$pupil_flag_implausible,
        .data$pupil_raw_value,
        NA_real_
      )
    )

  if (isTRUE(flag_iqr_outliers)) {
    if (length(group_cols) == 0) {
      work <- work |>
        dplyr::mutate(
          pupil_flag_iqr_outlier = compute_iqr_flags(.data$pupil_iqr_candidate)
        )
    } else {
      work <- work |>
        dplyr::group_by(!!!rlang::syms(group_cols)) |>
        dplyr::mutate(
          pupil_flag_iqr_outlier = compute_iqr_flags(.data$pupil_iqr_candidate)
        ) |>
        dplyr::ungroup()
    }
  } else {
    work <- work |>
      dplyr::mutate(
        pupil_flag_iqr_outlier = FALSE
      )
  }

  work <- work |>
    dplyr::mutate(
      pupil_flag_invalid = .data$pupil_flag_missing |
        .data$pupil_flag_nonfinite |
        .data$pupil_flag_implausible |
        .data$pupil_flag_iqr_outlier,
      pupil_flag_reason = dplyr::case_when(
        .data$pupil_flag_missing ~ "missing",
        .data$pupil_flag_nonfinite ~ "nonfinite",
        .data$pupil_flag_implausible_low ~ "implausible_low",
        .data$pupil_flag_implausible_high ~ "implausible_high",
        .data$pupil_flag_iqr_outlier ~ "iqr_outlier",
        TRUE ~ "valid"
      ),
      pupil_for_preprocessing = dplyr::if_else(
        .data$pupil_flag_invalid,
        NA_real_,
        .data$pupil_raw_value
      ),
      pupil_flag_pupil_column = pupil_source,
      pupil_flag_time_column = time_source,
      pupil_flag_min_plausible = min_plausible_value,
      pupil_flag_max_plausible = max_plausible_value,
      pupil_flag_outlier_k = outlier_k
    ) |>
    dplyr::arrange(.data$row_id)

  output_cols <- c(
    "pupil_raw_value",
    "pupil_flag_missing",
    "pupil_flag_nonfinite",
    "pupil_flag_implausible_low",
    "pupil_flag_implausible_high",
    "pupil_flag_implausible",
    "pupil_flag_iqr_outlier",
    "pupil_flag_invalid",
    "pupil_flag_reason",
    "pupil_for_preprocessing",
    "pupil_flag_pupil_column",
    "pupil_flag_time_column",
    "pupil_flag_min_plausible",
    "pupil_flag_max_plausible",
    "pupil_flag_outlier_k"
  )

  original <- tibble::as_tibble(master)
  original[intersect(names(original), output_cols)] <- NULL

  dplyr::bind_cols(
    original,
    work |>
      dplyr::select(dplyr::all_of(output_cols))
  )
}
