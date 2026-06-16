#' Create a master long-format dataset from Gazepoint all-gaze data
#'
#' Converts Gazepoint all-gaze exports imported with `read_gazepoint()` or
#' `read_gazepoint_folder()` into a master sample-level structure suitable for
#' quality checks, AOI summaries, pupil preprocessing, time-course analyses, and
#' publication reporting.
#'
#' @param gaze_data Gazepoint all-gaze data frame.
#' @param screen_width_px Optional screen width in pixels. If provided and gaze
#' coordinates are normalised, x-coordinates are converted to pixels.
#' @param screen_height_px Optional screen height in pixels. If provided and gaze
#' coordinates are normalised, y-coordinates are converted to pixels.
#' @param screen_width_cm Optional physical screen width in centimetres.
#' @param screen_height_cm Optional physical screen height in centimetres.
#' @param viewing_distance_cm Optional viewing distance in centimetres.
#' @param time_unit Unit of the Gazepoint `TIME` column. Usually `"seconds"`.
#' @param user_col Column identifying the source/user file.
#' @param media_col Column identifying the stimulus/media.
#' @param media_name_col Column identifying the media/stimulus name.
#' @param tracker_model Tracker label stored in the master data.
#' @param tracker_sampling_rate Expected tracker sampling rate.
#' @param event_latency_offset_ms Optional event-latency correction in milliseconds.
#' @param baseline_window Optional numeric vector of length 2 giving baseline
#' start and end in milliseconds.
#' @param analysis_window Optional numeric vector of length 2 giving analysis
#' start and end in milliseconds.
#'
#' @return A tibble with one row per Gazepoint sample and publication-oriented
#' master columns.
#' @export
create_gazepoint_master <- function(
    gaze_data,
    screen_width_px = NULL,
    screen_height_px = NULL,
    screen_width_cm = NULL,
    screen_height_cm = NULL,
    viewing_distance_cm = NULL,
    time_unit = c("seconds", "milliseconds"),
    user_col = "USER_FILE",
    media_col = "MEDIA_ID",
    media_name_col = "MEDIA_NAME",
    tracker_model = "Gazepoint",
    tracker_sampling_rate = 60,
    event_latency_offset_ms = 0,
    baseline_window = NULL,
    analysis_window = NULL
) {
  time_unit <- match.arg(time_unit)

  if (!is.data.frame(gaze_data)) {
    rlang::abort("`gaze_data` must be a data frame or tibble.")
  }

  required_cols <- c(user_col, media_col, "TIME")
  missing_required <- setdiff(required_cols, names(gaze_data))

  if (length(missing_required) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in `gaze_data`: ",
        paste(missing_required, collapse = ", ")
      )
    )
  }

  n_rows <- nrow(gaze_data)

  user_file <- as.character(gaze_data[[user_col]])
  subject <- gsub("_all_gaze\\.csv$", "", user_file)
  subject <- gsub("_fixations\\.csv$", "", subject)

  media_id <- gaze_data[[media_col]]

  media_name <- if (media_name_col %in% names(gaze_data)) {
    as.character(gaze_data[[media_name_col]])
  } else {
    rep(NA_character_, n_rows)
  }

  raw_time <- .gp3_get_numeric(gaze_data, "TIME")

  time_ms <- if (time_unit == "seconds") {
    raw_time * 1000
  } else {
    raw_time
  }

  time_ms <- time_ms + event_latency_offset_ms

  best_x_raw <- .gp3_get_numeric(
    gaze_data,
    c("BPOGX", "FPOGX", "LPOGX", "RPOGX")
  )

  best_y_raw <- .gp3_get_numeric(
    gaze_data,
    c("BPOGY", "FPOGY", "LPOGY", "RPOGY")
  )

  left_x_raw <- .gp3_get_numeric(gaze_data, c("LPOGX", "LPOGVX"))
  left_y_raw <- .gp3_get_numeric(gaze_data, c("LPOGY", "LPOGVY"))

  right_x_raw <- .gp3_get_numeric(gaze_data, c("RPOGX", "RPOGVX"))
  right_y_raw <- .gp3_get_numeric(gaze_data, c("RPOGY", "RPOGVY"))

  x <- .gp3_coordinate_to_pixels(best_x_raw, screen_width_px)
  y <- .gp3_coordinate_to_pixels(best_y_raw, screen_height_px)

  left_x <- .gp3_coordinate_to_pixels(left_x_raw, screen_width_px)
  left_y <- .gp3_coordinate_to_pixels(left_y_raw, screen_height_px)

  right_x <- .gp3_coordinate_to_pixels(right_x_raw, screen_width_px)
  right_y <- .gp3_coordinate_to_pixels(right_y_raw, screen_height_px)

  gaze_valid_raw <- .gp3_get_numeric(
    gaze_data,
    c("BPOGV", "FPOGV", "LPOGV", "RPOGV"),
    default = 1
  )

  left_valid_raw <- .gp3_get_numeric(
    gaze_data,
    c("LPOGV", "LPV", "LPUPILV", "LPMMV"),
    default = NA_real_
  )

  right_valid_raw <- .gp3_get_numeric(
    gaze_data,
    c("RPOGV", "RPV", "RPUPILV", "RPMMV"),
    default = NA_real_
  )

  left_pupil <- .gp3_get_numeric(
    gaze_data,
    c("LPUPILD", "LPD", "LPMM", "LPUPIL")
  )

  right_pupil <- .gp3_get_numeric(
    gaze_data,
    c("RPUPILD", "RPD", "RPMM", "RPUPIL")
  )

  left_valid <- ifelse(is.na(left_valid_raw), !is.na(left_pupil), left_valid_raw == 1)
  right_valid <- ifelse(is.na(right_valid_raw), !is.na(right_pupil), right_valid_raw == 1)

  left_pupil[!left_valid] <- NA_real_
  right_pupil[!right_valid] <- NA_real_

  mean_pupil <- rowMeans(
    cbind(left_pupil, right_pupil),
    na.rm = TRUE
  )

  mean_pupil[is.nan(mean_pupil)] <- NA_real_

  aoi_current <- if ("AOI" %in% names(gaze_data)) {
    as.character(gaze_data$AOI)
  } else {
    rep(NA_character_, n_rows)
  }

  valid_sample <- gaze_valid_raw == 1 & !is.na(x) & !is.na(y)

  missing_gaze <- !valid_sample | is.na(x) | is.na(y)
  missing_pupil <- is.na(left_pupil) & is.na(right_pupil)

  gaze_offscreen <- rep(NA, n_rows)

  if (!is.null(screen_width_px) && !is.null(screen_height_px)) {
    gaze_offscreen <- !missing_gaze &
      is.finite(x) &
      is.finite(y) &
      (
        x < 0 |
          x > screen_width_px |
          y < 0 |
          y > screen_height_px
      )
  }

  sample_index <- .gp3_get_numeric(gaze_data, c("CNT", "COUNTER"))

  master <- tibble::tibble(
    subject = subject,
    pID = subject,
    USER_FILE = user_file,
    MEDIA_ID = media_id,
    MEDIA_NAME = media_name,
    trial = media_id,
    trial_global = paste(subject, media_id, sep = "_M"),
    condition = NA_character_,
    group = NA_character_,
    item_id = NA_character_,
    stimulus_id = as.character(media_id),
    stimulus_file = media_name,

    time = time_ms,
    time_orig = time_ms,
    sample_index = sample_index,
    sampling_rate_hz = tracker_sampling_rate,
    time_bin_25ms = floor(time_ms / 25) * 25,
    time_bin_50ms = floor(time_ms / 50) * 50,
    time_bin_100ms = floor(time_ms / 100) * 100,

    baseline_window = .gp3_in_window(time_ms, baseline_window),
    analysis_window = .gp3_in_window(time_ms, analysis_window),

    x = x,
    y = y,
    left_x = left_x,
    left_y = left_y,
    right_x = right_x,
    right_y = right_y,

    left_pupil = left_pupil,
    right_pupil = right_pupil,
    mean_pupil = mean_pupil,
    pupil = mean_pupil,
    pupil_raw = mean_pupil,
    pupil_unit = .gp3_detect_pupil_unit(gaze_data),
    gaze_unit = ifelse(
      !is.null(screen_width_px) && !is.null(screen_height_px),
      "pixels",
      "tracker_units"
    ),

    valid_sample = valid_sample,
    left_valid = left_valid,
    right_valid = right_valid,
    missing_gaze = missing_gaze,
    missing_pupil = missing_pupil,
    trackloss = missing_gaze,
    Trackloss = missing_gaze,
    blink = missing_gaze & missing_pupil,
    gaze_offscreen = gaze_offscreen,

    interpolated = FALSE,
    filtered = FALSE,
    artifact_flag = missing_gaze | missing_pupil,
    artifact_reason = dplyr::case_when(
      missing_gaze & missing_pupil ~ "missing_gaze_and_pupil",
      missing_gaze ~ "missing_gaze",
      missing_pupil ~ "missing_pupil",
      TRUE ~ NA_character_
    ),

    AOI = aoi_current,
    aoi_current = dplyr::case_when(
      missing_gaze ~ "missing",
      !is.na(aoi_current) & aoi_current != "" ~ aoi_current,
      !missing_gaze ~ "non_aoi",
      TRUE ~ NA_character_
    ),
    aoi_count = ifelse(
      !is.na(aoi_current) &
        aoi_current != "" &
        !(aoi_current %in% c("missing", "offscreen", "non_aoi", "unclassified")),
      1L,
      0L
    ),

    message = NA_character_,
    event_type = NA_character_,
    event_label = NA_character_,
    event_latency_offset_ms = event_latency_offset_ms,
    stimulus_onset_time = NA_real_,
    target_onset_time = NA_real_,
    response_time_orig = NA_real_,
    response_time = NA_real_,

    screen_width_px = screen_width_px,
    screen_height_px = screen_height_px,
    screen_width_cm = screen_width_cm,
    screen_height_cm = screen_height_cm,
    viewing_distance_cm = viewing_distance_cm,

    tracker_model = tracker_model,
    tracker_sampling_rate = tracker_sampling_rate,
    calibration_quality = NA_real_,
    validation_error_deg = NA_real_,
    drift_correction_error = NA_real_,

    response = NA_character_,
    correct_response = NA_character_,
    accuracy = NA_integer_,
    rt = NA_real_,
    choice = NA_character_,
    rating = NA_real_,
    trust_rating = NA_real_,
    risk_rating = NA_real_,
    purchase_intention = NA_real_,
    excluded_trial = FALSE,
    exclusion_reason = NA_character_
  )

  master |>
    dplyr::group_by(.data$subject, .data$MEDIA_ID) |>
    dplyr::mutate(
      sample_index = dplyr::if_else(
        is.na(.data$sample_index),
        dplyr::row_number(),
        as.integer(.data$sample_index)
      )
    ) |>
    dplyr::ungroup()
}

.gp3_get_numeric <- function(data, candidates, default = NA_real_) {
  hit <- intersect(candidates, names(data))

  if (length(hit) == 0) {
    return(rep(default, nrow(data)))
  }

  suppressWarnings(as.numeric(data[[hit[1]]]))
}

.gp3_coordinate_to_pixels <- function(x, screen_size_px = NULL) {
  if (is.null(screen_size_px)) {
    return(x)
  }

  finite_x <- x[is.finite(x)]

  if (length(finite_x) == 0) {
    return(x)
  }

  if (max(abs(finite_x), na.rm = TRUE) <= 1.5) {
    return(x * screen_size_px)
  }

  x
}

.gp3_in_window <- function(time_ms, window = NULL) {
  if (is.null(window)) {
    return(rep(FALSE, length(time_ms)))
  }

  if (!is.numeric(window) || length(window) != 2) {
    rlang::abort("Window arguments must be numeric vectors of length 2.")
  }

  time_ms >= window[1] & time_ms < window[2]
}

.gp3_detect_pupil_unit <- function(data) {
  if (any(c("LPUPILD", "RPUPILD") %in% names(data))) {
    return("diameter_meters")
  }

  if (any(c("LPMM", "RPMM") %in% names(data))) {
    return("diameter_mm")
  }

  if (any(c("LPD", "RPD") %in% names(data))) {
    return("camera_image_pixels")
  }

  "unknown"
}
