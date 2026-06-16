#' Audit a Gazepoint master sample table
#'
#' Creates compact quality-audit tables from a master sample-level table created
#' by [as_gazepoint_master()] or [create_gazepoint_master()]. The audit
#' summarises missing gaze, missing pupil, off-screen gaze, AOI states, pupil
#' availability, coordinate ranges, and subject/media-level quality.
#'
#' @param master A master sample-level table created by [as_gazepoint_master()]
#'   or [create_gazepoint_master()].
#'
#' @return A named list of tibbles:
#' \describe{
#'   \item{overview}{One-row overview of the master table.}
#'   \item{by_subject}{Quality summary by participant/source.}
#'   \item{by_media}{Quality summary by media/stimulus.}
#'   \item{by_subject_media}{Quality summary by participant/source and media/stimulus.}
#'   \item{aoi_states}{Counts and percentages of AOI states.}
#'   \item{pupil_summary}{Pupil summary by participant/source and media/stimulus.}
#'   \item{coordinate_summary}{Coordinate range and off-screen summary.}
#' }
#'
#' @examples
#' \dontrun{
#' results <- run_gazepoint_workflow(
#'   export_dir = "C:/Users/YourName/Desktop/gp3_test_exports",
#'   output_dir = "C:/Users/YourName/Desktop/gp3_outputs"
#' )
#'
#' master <- create_gazepoint_master(
#'   gaze_data = results$all_gaze,
#'   screen_width_px = 1920,
#'   screen_height_px = 1080
#' )
#'
#' audit <- audit_gazepoint_master(master)
#'
#' audit$overview
#' audit$by_subject
#' audit$aoi_states
#' }
#'
#' @importFrom rlang .data
#'
#' @export
audit_gazepoint_master <- function(master) {
  if (!is.data.frame(master)) {
    rlang::abort("`master` must be a data frame created by `as_gazepoint_master()` or `create_gazepoint_master()`.")
  }

  detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(master)]

    if (length(found) == 0) {
      return(NA_character_)
    }

    found[[1]]
  }

  column_map <- tibble::tibble(
    role = c(
      "subject",
      "media_id",
      "time_ms",
      "x",
      "y",
      "valid_sample",
      "missing_gaze",
      "missing_pupil",
      "gaze_offscreen",
      "mean_pupil",
      "aoi_current",
      "aoi_count",
      "raw_x",
      "raw_y"
    ),
    column = c(
      detect_col(c("subject", "pID", "participant")),
      detect_col(c("media_id", "MEDIA_ID")),
      detect_col(c("time_ms", "time", "time_orig", "time_orig_ms")),
      detect_col(c("x", "gaze_x")),
      detect_col(c("y", "gaze_y")),
      detect_col(c("valid_sample")),
      detect_col(c("missing_gaze")),
      detect_col(c("missing_pupil")),
      detect_col(c("gaze_offscreen")),
      detect_col(c("mean_pupil", "pupil", "pupil_raw")),
      detect_col(c("aoi_current", "AOI")),
      detect_col(c("aoi_count")),
      detect_col(c("raw_x")),
      detect_col(c("raw_y"))
    )
  )

  required_roles <- c(
    "subject",
    "media_id",
    "time_ms",
    "x",
    "y",
    "valid_sample",
    "missing_gaze",
    "missing_pupil",
    "gaze_offscreen",
    "mean_pupil",
    "aoi_current",
    "aoi_count"
  )

  missing_roles <- required_roles[
    is.na(column_map$column[match(required_roles, column_map$role)])
  ]

  if (length(missing_roles) > 0) {
    rlang::abort(
      paste0(
        "`master` is missing required columns: ",
        paste(missing_roles, collapse = ", ")
      )
    )
  }

  get_role <- function(role) {
    col <- column_map$column[column_map$role == role]

    if (length(col) == 0 || is.na(col)) {
      return(NULL)
    }

    master[[col]]
  }

  master <- tibble::tibble(
    subject = as.character(get_role("subject")),
    media_id = as.character(get_role("media_id")),
    time_ms = suppressWarnings(as.numeric(get_role("time_ms"))),
    x = suppressWarnings(as.numeric(get_role("x"))),
    y = suppressWarnings(as.numeric(get_role("y"))),
    valid_sample = as.logical(get_role("valid_sample")),
    missing_gaze = as.logical(get_role("missing_gaze")),
    missing_pupil = as.logical(get_role("missing_pupil")),
    gaze_offscreen = as.logical(get_role("gaze_offscreen")),
    mean_pupil = suppressWarnings(as.numeric(get_role("mean_pupil"))),
    aoi_current = as.character(get_role("aoi_current")),
    aoi_count = suppressWarnings(as.integer(get_role("aoi_count"))),
    raw_x = if (!is.null(get_role("raw_x"))) {
      suppressWarnings(as.numeric(get_role("raw_x")))
    } else {
      NA_real_
    },
    raw_y = if (!is.null(get_role("raw_y"))) {
      suppressWarnings(as.numeric(get_role("raw_y")))
    } else {
      NA_real_
    }
  )

  prop_true_pct <- function(x) {
    x <- as.logical(x)

    if (length(x) == 0 || all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE) * 100
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

  n_distinct_non_missing <- function(x) {
    dplyr::n_distinct(x[!is.na(x)])
  }

  is_real_aoi <- function(x) {
    !is.na(x) &
      x != "" &
      !(x %in% c("missing", "offscreen", "non_aoi", "unclassified"))
  }

  time_min_ms <- safe_min(master$time_ms)
  time_max_ms <- safe_max(master$time_ms)

  overview <- tibble::tibble(
    n_rows = nrow(master),
    n_subjects = n_distinct_non_missing(master$subject),
    n_media = n_distinct_non_missing(master$media_id),
    n_subject_media = nrow(dplyr::distinct(master, .data$subject, .data$media_id)),
    time_min_ms = time_min_ms,
    time_max_ms = time_max_ms,
    time_span_ms = time_max_ms - time_min_ms,
    valid_sample_pct = prop_true_pct(master$valid_sample),
    missing_gaze_pct = prop_true_pct(master$missing_gaze),
    missing_pupil_pct = prop_true_pct(master$missing_pupil),
    offscreen_gaze_pct = prop_true_pct(master$gaze_offscreen),
    n_missing_gaze = sum(master$missing_gaze, na.rm = TRUE),
    n_missing_pupil = sum(master$missing_pupil, na.rm = TRUE),
    n_offscreen_gaze = sum(master$gaze_offscreen, na.rm = TRUE),
    has_pupil = any(!is.na(master$mean_pupil)),
    has_aoi = any(is_real_aoi(master$aoi_current)),
    n_aoi_samples = sum(is_real_aoi(master$aoi_current), na.rm = TRUE),
    n_missing_state = sum(master$aoi_current == "missing", na.rm = TRUE),
    n_offscreen_state = sum(master$aoi_current == "offscreen", na.rm = TRUE)
  )

  summarise_quality_by <- function(data, group_cols) {
    data |>
      dplyr::group_by(!!!rlang::syms(group_cols)) |>
      dplyr::summarise(
        n_rows = dplyr::n(),
        time_min_ms = safe_min(.data$time_ms),
        time_max_ms = safe_max(.data$time_ms),
        time_span_ms = time_max_ms - time_min_ms,
        valid_sample_pct = prop_true_pct(.data$valid_sample),
        missing_gaze_pct = prop_true_pct(.data$missing_gaze),
        missing_pupil_pct = prop_true_pct(.data$missing_pupil),
        offscreen_gaze_pct = prop_true_pct(.data$gaze_offscreen),
        n_missing_gaze = sum(.data$missing_gaze, na.rm = TRUE),
        n_missing_pupil = sum(.data$missing_pupil, na.rm = TRUE),
        n_offscreen_gaze = sum(.data$gaze_offscreen, na.rm = TRUE),
        n_aoi_samples = sum(is_real_aoi(.data$aoi_current), na.rm = TRUE),
        n_missing_state = sum(.data$aoi_current == "missing", na.rm = TRUE),
        n_offscreen_state = sum(.data$aoi_current == "offscreen", na.rm = TRUE),
        aoi_count_sum = sum(.data$aoi_count, na.rm = TRUE),
        has_pupil = any(!is.na(.data$mean_pupil)),
        .groups = "drop"
      )
  }

  by_subject <- summarise_quality_by(master, "subject")
  by_media <- summarise_quality_by(master, "media_id")
  by_subject_media <- summarise_quality_by(master, c("subject", "media_id"))

  aoi_states <- master |>
    dplyr::mutate(
      aoi_state = dplyr::if_else(
        is.na(.data$aoi_current) | .data$aoi_current == "",
        "unclassified",
        .data$aoi_current
      )
    ) |>
    dplyr::count(.data$aoi_state, name = "n_samples") |>
    dplyr::mutate(
      prop_samples = .data$n_samples / sum(.data$n_samples) * 100
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_samples))

  pupil_summary <- master |>
    dplyr::group_by(.data$subject, .data$media_id) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_pupil_samples = sum(!is.na(.data$mean_pupil)),
      missing_pupil_pct = prop_true_pct(.data$missing_pupil),
      mean_pupil = safe_mean(.data$mean_pupil),
      median_pupil = safe_median(.data$mean_pupil),
      sd_pupil = safe_sd(.data$mean_pupil),
      min_pupil = safe_min(.data$mean_pupil),
      max_pupil = safe_max(.data$mean_pupil),
      .groups = "drop"
    )

  coordinate_summary <- tibble::tibble(
    n_rows = nrow(master),
    x_min = safe_min(master$x),
    x_max = safe_max(master$x),
    y_min = safe_min(master$y),
    y_max = safe_max(master$y),
    raw_x_min = safe_min(master$raw_x),
    raw_x_max = safe_max(master$raw_x),
    raw_y_min = safe_min(master$raw_y),
    raw_y_max = safe_max(master$raw_y),
    n_offscreen_gaze = sum(master$gaze_offscreen, na.rm = TRUE),
    offscreen_gaze_pct = prop_true_pct(master$gaze_offscreen)
  )

  list(
    overview = overview,
    by_subject = by_subject,
    by_media = by_media,
    by_subject_media = by_subject_media,
    aoi_states = aoi_states,
    pupil_summary = pupil_summary,
    coordinate_summary = coordinate_summary
  )
}
