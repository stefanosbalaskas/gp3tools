#' Flag low-quality Gazepoint recordings
#'
#' Combines tracking-quality and sampling-rate summaries and flags rows with
#' low gaze validity, low pupil validity, abnormal sampling rate, or short duration.
#'
#' @param quality Tracking-quality table from `summarise_tracking_quality()`.
#' @param sampling Sampling-rate table from `check_sampling_rate()`.
#' @param by Columns used to join `quality` and `sampling`.
#' @param min_gaze_valid_pct Minimum acceptable FPOGV validity percentage.
#' @param min_pupil_valid_pct Minimum acceptable pupil validity percentage.
#' @param expected_hz Expected sampling rate.
#' @param hz_tolerance Allowed deviation from the expected sampling rate.
#' @param min_duration_sec Minimum acceptable recording duration in seconds.
#'
#' @return A tibble with quality, sampling, flag columns, and an overall review flag.
#' @export
flag_tracking_quality <- function(
    quality,
    sampling,
    by = c("USER_FILE", "MEDIA_ID"),
    min_gaze_valid_pct = 70,
    min_pupil_valid_pct = 70,
    expected_hz = 60,
    hz_tolerance = 5,
    min_duration_sec = NULL
) {
  if (!is.data.frame(quality)) {
    rlang::abort("`quality` must be a data frame or tibble.")
  }

  if (!is.data.frame(sampling)) {
    rlang::abort("`sampling` must be a data frame or tibble.")
  }

  missing_quality_cols <- setdiff(by, names(quality))
  missing_sampling_cols <- setdiff(by, names(sampling))

  if (length(missing_quality_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing join columns in `quality`: ",
        paste(missing_quality_cols, collapse = ", ")
      )
    )
  }

  if (length(missing_sampling_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing join columns in `sampling`: ",
        paste(missing_sampling_cols, collapse = ", ")
      )
    )
  }

  required_quality <- c("FPOGV_valid_pct")
  missing_required_quality <- setdiff(required_quality, names(quality))

  if (length(missing_required_quality) > 0) {
    rlang::abort(
      paste0(
        "Missing required quality columns: ",
        paste(missing_required_quality, collapse = ", ")
      )
    )
  }

  required_sampling <- c("estimated_hz", "duration_sec")
  missing_required_sampling <- setdiff(required_sampling, names(sampling))

  if (length(missing_required_sampling) > 0) {
    rlang::abort(
      paste0(
        "Missing required sampling columns: ",
        paste(missing_required_sampling, collapse = ", ")
      )
    )
  }

  out <- dplyr::left_join(
    quality,
    sampling,
    by = by
  )

  pupil_cols <- intersect(
    c("LPV_valid_pct", "RPV_valid_pct", "LPMMV_valid_pct", "RPMMV_valid_pct"),
    names(out)
  )

  if (length(pupil_cols) > 0) {
    out <- out |>
      dplyr::mutate(
        min_pupil_valid_pct_observed = do.call(
          pmin,
          c(dplyr::across(dplyr::all_of(pupil_cols)), na.rm = TRUE)
        )
      )
  } else {
    out <- out |>
      dplyr::mutate(
        min_pupil_valid_pct_observed = NA_real_
      )
  }

  out <- out |>
    dplyr::mutate(
      flag_low_gaze_validity = FPOGV_valid_pct < min_gaze_valid_pct,
      flag_low_pupil_validity = min_pupil_valid_pct_observed < min_pupil_valid_pct,
      flag_sampling_rate = abs(estimated_hz - expected_hz) > hz_tolerance
    )

  if (is.null(min_duration_sec)) {
    out <- out |>
      dplyr::mutate(
        flag_short_duration = FALSE
      )
  } else {
    out <- out |>
      dplyr::mutate(
        flag_short_duration = duration_sec < min_duration_sec
      )
  }

  out |>
    dplyr::mutate(
      review_required = flag_low_gaze_validity |
        flag_low_pupil_validity |
        flag_sampling_rate |
        flag_short_duration
    )
}
