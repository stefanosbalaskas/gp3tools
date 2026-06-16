#' Summarise Gazepoint AOI metrics from gaze and fixation exports
#'
#' Combines sample-level AOI viewing information from all-gaze data with
#' fixation-level AOI metrics from fixation data.
#'
#' @param gaze_data A Gazepoint all-gaze data frame imported with `read_gazepoint()`.
#' @param fixation_data A Gazepoint fixation data frame imported with `read_gazepoint()`.
#' @param user_col Name of the column identifying the user file. Default is `"USER_FILE"`.
#' @param sample_rate Assumed sampling rate used to approximate viewed time from sample counts.
#'
#' @return A tibble with one row per user, media, and AOI.
#' @export
summarise_gazepoint_aoi <- function(
    gaze_data,
    fixation_data,
    user_col = "USER_FILE",
    sample_rate = 60
) {
  required_gaze_cols <- c(user_col, "MEDIA_ID", "MEDIA_NAME", "AOI", "TIME")
  missing_gaze_cols <- setdiff(required_gaze_cols, names(gaze_data))

  if (length(missing_gaze_cols) > 0) {
    stop(
      "Missing required columns in `gaze_data`: ",
      paste(missing_gaze_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_fix_cols <- c(
    user_col,
    "MEDIA_ID",
    "MEDIA_NAME",
    "AOI",
    "FPOGD",
    "FPOGS"
  )

  missing_fix_cols <- setdiff(required_fix_cols, names(fixation_data))

  if (length(missing_fix_cols) > 0) {
    stop(
      "Missing required columns in `fixation_data`: ",
      paste(missing_fix_cols, collapse = ", "),
      call. = FALSE
    )
  }

  sample_summary <- gaze_data |>
    dplyr::filter(!is.na(AOI), AOI != "") |>
    dplyr::mutate(
      USER_ID = as.integer(stringr::str_extract(.data[[user_col]], "\\d+"))
    ) |>
    dplyr::group_by(USER_ID, MEDIA_ID, MEDIA_NAME, AOI) |>
    dplyr::summarise(
      sample_ttff_sec = min(TIME, na.rm = TRUE),
      sample_count = dplyr::n(),
      sample_time_viewed_sec = dplyr::n() / sample_rate,
      .groups = "drop"
    )

  fixation_summary <- fixation_data |>
    dplyr::filter(!is.na(AOI), AOI != "") |>
    dplyr::mutate(
      USER_ID = as.integer(stringr::str_extract(.data[[user_col]], "\\d+"))
    ) |>
    dplyr::group_by(USER_ID, MEDIA_ID, MEDIA_NAME, AOI) |>
    dplyr::summarise(
      fixation_count = dplyr::n(),
      fixation_duration_sum_sec = sum(FPOGD, na.rm = TRUE),
      fixation_duration_mean_ms = mean(FPOGD, na.rm = TRUE) * 1000,
      fixation_ttff_sec = min(FPOGS, na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::full_join(
    sample_summary,
    fixation_summary,
    by = c("USER_ID", "MEDIA_ID", "MEDIA_NAME", "AOI")
  ) |>
    dplyr::arrange(USER_ID, MEDIA_ID, AOI)
}
