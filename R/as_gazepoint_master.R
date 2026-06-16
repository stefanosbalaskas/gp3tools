#' Convert Gazepoint all-gaze data to a master sample table
#'
#' Converts a Gazepoint all-gaze export into a standard sample-level table with
#' one row per gaze sample. The returned table keeps Gazepoint identifiers but
#' also adds analysis-friendly columns such as `subject`, `media_id`, `time_ms`,
#' `x`, `y`, `left_pupil`, `right_pupil`, `mean_pupil`, `valid_sample`,
#' `missing_gaze`, `missing_pupil`, `trackloss`, `blink`, `aoi_current`,
#' `message`, and `event_type`.
#'
#' This function is intended as a bridge between raw Gazepoint exports and more
#' advanced eye-tracking workflows. It does not require an external trial log.
#' Later, experiment-level information such as condition, trial ID, response,
#' accuracy, or reaction time can be joined to the returned table.
#'
#' @param data A Gazepoint all-gaze data frame, usually `results$all_gaze`.
#' @param screen_width_px Optional screen width in pixels. If supplied and gaze
#' coordinates are detected as normalised 0-1 coordinates, x coordinates are
#' converted to pixels.
#' @param screen_height_px Optional screen height in pixels. If supplied and gaze
#' coordinates are detected as normalised 0-1 coordinates, y coordinates are
#' converted to pixels.
#' @param source_col Column identifying the source/user file.
#' @param media_col Column identifying the Gazepoint media/stimulus.
#' @param media_name_col Column identifying the Gazepoint media/stimulus name.
#' @param time_col Gazepoint time column, usually `TIME`.
#' @param coordinate_unit One of `"auto"`, `"normalised"`, or `"pixels"`.
#' `"auto"` detects normalised coordinates when coordinate values are mostly
#' between 0 and 1.
#' @param event_latency_offset_ms Optional timing correction in milliseconds.
#' Positive values shift event/sample time forward.
#'
#' @return A tibble with one row per sample and standardised sample-level
#' eye-tracking columns.
#'
#' @examples
#' \dontrun{
#' results <- run_gazepoint_workflow(
#'   export_dir = "C:/Users/YourName/Desktop/gp3_test_exports",
#'   output_dir = "C:/Users/YourName/Desktop/gp3_outputs"
#' )
#'
#' master <- as_gazepoint_master(
#'   results$all_gaze,
#'   screen_width_px = 1920,
#'   screen_height_px = 1080
#' )
#'
#' dplyr::glimpse(master)
#' }
#'
#' @export
as_gazepoint_master <- function(
    data,
    screen_width_px = NULL,
    screen_height_px = NULL,
    source_col = "USER_FILE",
    media_col = "MEDIA_ID",
    media_name_col = "MEDIA_NAME",
    time_col = "TIME",
    coordinate_unit = c("auto", "normalised", "pixels"),
    event_latency_offset_ms = 0
) {
  coordinate_unit <- match.arg(coordinate_unit)

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  if (!is.null(screen_width_px)) {
    if (!is.numeric(screen_width_px) || length(screen_width_px) != 1) {
      rlang::abort("`screen_width_px` must be `NULL` or a single numeric value.")
    }
  }

  if (!is.null(screen_height_px)) {
    if (!is.numeric(screen_height_px) || length(screen_height_px) != 1) {
      rlang::abort("`screen_height_px` must be `NULL` or a single numeric value.")
    }
  }

  if (!is.numeric(event_latency_offset_ms) || length(event_latency_offset_ms) != 1) {
    rlang::abort("`event_latency_offset_ms` must be a single numeric value.")
  }

  data <- standardise_gazepoint_names(data)

  if (!time_col %in% names(data)) {
    rlang::abort(paste0("Column `", time_col, "` was not found."))
  }

  n <- nrow(data)

  get_numeric <- function(candidates, default = NA_real_) {
    hit <- intersect(candidates, names(data))

    if (length(hit) == 0) {
      return(rep(default, n))
    }

    suppressWarnings(as.numeric(data[[hit[1]]]))
  }

  get_character <- function(candidates, default = NA_character_) {
    hit <- intersect(candidates, names(data))

    if (length(hit) == 0) {
      return(rep(default, n))
    }

    as.character(data[[hit[1]]])
  }

  first_existing <- function(candidates) {
    hit <- intersect(candidates, names(data))

    if (length(hit) == 0) {
      return(NA_character_)
    }

    hit[1]
  }

  row_mean_safe <- function(x, y) {
    out <- rowMeans(cbind(x, y), na.rm = TRUE)
    out[is.nan(out)] <- NA_real_
    out
  }

  source_file <- get_character(source_col)
  media_id <- get_character(media_col)
  media_name <- get_character(media_name_col)

  subject <- source_file
  subject <- stringr::str_remove(subject, "_all_gaze\\.csv$")
  subject <- stringr::str_remove(subject, "\\.csv$")
  subject[is.na(source_file) | source_file == ""] <- NA_character_

  trial_global <- ifelse(
    !is.na(subject) & !is.na(media_id),
    paste(subject, media_id, sep = "_MEDIA_"),
    NA_character_
  )

  raw_time_sec <- get_numeric(time_col)
  time_ms <- (raw_time_sec * 1000) + event_latency_offset_ms

  best_x_raw <- get_numeric(c("BPOGX", "FPOGX"))
  best_y_raw <- get_numeric(c("BPOGY", "FPOGY"))

  left_x_raw <- get_numeric(c("LPOGX"))
  left_y_raw <- get_numeric(c("LPOGY"))

  right_x_raw <- get_numeric(c("RPOGX"))
  right_y_raw <- get_numeric(c("RPOGY"))

  coordinate_values <- c(
    best_x_raw,
    best_y_raw,
    left_x_raw,
    left_y_raw,
    right_x_raw,
    right_y_raw
  )

  coordinate_values <- coordinate_values[is.finite(coordinate_values)]

  detected_coordinate_unit <- coordinate_unit

  if (identical(coordinate_unit, "auto")) {
    central_coordinate_values <- coordinate_values[
      coordinate_values >= -0.25 & coordinate_values <= 1.25
    ]

    prop_central <- if (length(coordinate_values) > 0) {
      length(central_coordinate_values) / length(coordinate_values)
    } else {
      0
    }

    detected_coordinate_unit <- if (
      length(coordinate_values) > 0 &&
      prop_central >= 0.80
    ) {
      "normalised"
    } else {
      "pixels"
    }
  }

  scale_x <- function(x) {
    if (identical(detected_coordinate_unit, "normalised") && !is.null(screen_width_px)) {
      return(x * screen_width_px)
    }

    x
  }

  scale_y <- function(y) {
    if (identical(detected_coordinate_unit, "normalised") && !is.null(screen_height_px)) {
      return(y * screen_height_px)
    }

    y
  }

  x <- scale_x(best_x_raw)
  y <- scale_y(best_y_raw)

  left_x <- scale_x(left_x_raw)
  left_y <- scale_y(left_y_raw)

  right_x <- scale_x(right_x_raw)
  right_y <- scale_y(right_y_raw)

  best_valid <- get_numeric(c("BPOGV", "FPOGV"), default = 1)
  left_gaze_valid_raw <- get_numeric(c("LPOGV"))
  right_gaze_valid_raw <- get_numeric(c("RPOGV"))

  left_pupil_source <- first_existing(c("LPMM", "LPUPILD", "LPD"))
  right_pupil_source <- first_existing(c("RPMM", "RPUPILD", "RPD"))

  left_pupil <- get_numeric(c("LPMM", "LPUPILD", "LPD"))
  right_pupil <- get_numeric(c("RPMM", "RPUPILD", "RPD"))

  left_pupil_valid_raw <- get_numeric(c("LPMMV", "LPUPILV", "LPV"))
  right_pupil_valid_raw <- get_numeric(c("RPMMV", "RPUPILV", "RPV"))

  left_pupil[!is.na(left_pupil_valid_raw) & left_pupil_valid_raw == 0] <- NA_real_
  right_pupil[!is.na(right_pupil_valid_raw) & right_pupil_valid_raw == 0] <- NA_real_

  mean_pupil <- row_mean_safe(left_pupil, right_pupil)

  pupil_unit <- dplyr::case_when(
    left_pupil_source %in% c("LPMM") | right_pupil_source %in% c("RPMM") ~ "diameter_mm",
    left_pupil_source %in% c("LPUPILD") | right_pupil_source %in% c("RPUPILD") ~ "diameter_meters",
    left_pupil_source %in% c("LPD") | right_pupil_source %in% c("RPD") ~ "tracker_units",
    TRUE ~ NA_character_
  )

  gaze_unit <- if (
    identical(detected_coordinate_unit, "normalised") &&
    !is.null(screen_width_px) &&
    !is.null(screen_height_px)
  ) {
    "pixels"
  } else {
    detected_coordinate_unit
  }

  valid_sample <- !is.na(best_valid) & best_valid == 1
  missing_gaze <- !valid_sample | is.na(x) | is.na(y)
  missing_pupil <- is.na(left_pupil) & is.na(right_pupil)

  gaze_offscreen <- rep(NA, n)

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

  blink_id <- get_numeric(c("BKID"))
  blink_duration <- get_numeric(c("BKDUR"))

  has_blink_columns <- any(!is.na(blink_id)) || any(!is.na(blink_duration))

  blink <- if (has_blink_columns) {
    (!is.na(blink_id) & blink_id > 0) |
      (!is.na(blink_duration) & blink_duration > 0)
  } else {
    missing_gaze & missing_pupil
  }

  trackloss <- missing_gaze

  message <- get_character(c("USER", "USER_DATA", "MESSAGE"))

  event_type <- rep(NA_character_, n)

  has_message <- !is.na(message) & message != ""

  event_type[
    has_message & stringr::str_detect(message, "TRIAL_START")
  ] <- "trial_start"

  event_type[
    has_message & stringr::str_detect(message, "STIMULUS_ONSET")
  ] <- "stimulus_onset"

  event_type[
    has_message & stringr::str_detect(message, "TARGET_ONSET")
  ] <- "target_onset"

  event_type[
    has_message & stringr::str_detect(message, "TRIAL_END")
  ] <- "trial_end"

  aoi <- get_character(c("AOI"))

  aoi_current <- aoi
  aoi_current[is.na(aoi_current) | aoi_current == ""] <- NA_character_

  aoi_current[gaze_offscreen %in% TRUE] <- "offscreen"
  aoi_current[missing_gaze] <- "missing"

  artifact_flag <- missing_gaze | missing_pupil

  artifact_reason <- rep(NA_character_, n)
  artifact_reason[missing_gaze & missing_pupil] <- "missing_gaze_and_pupil"
  artifact_reason[missing_gaze & !missing_pupil] <- "missing_gaze"
  artifact_reason[!missing_gaze & missing_pupil] <- "missing_pupil"

  fixation_x_raw <- get_numeric(c("FPOGX"))
  fixation_y_raw <- get_numeric(c("FPOGY"))

  screen_width_value <- if (is.null(screen_width_px)) {
    NA_real_
  } else {
    screen_width_px
  }

  screen_height_value <- if (is.null(screen_height_px)) {
    NA_real_
  } else {
    screen_height_px
  }

  tibble::tibble(
    source_file = source_file,
    subject = subject,
    pID = subject,
    media_id = media_id,
    media_name = media_name,
    trial_global = trial_global,

    time = time_ms,
    time_ms = time_ms,
    time_orig_sec = raw_time_sec,
    time_orig_ms = raw_time_sec * 1000,
    sample_index = get_numeric(c("CNT")),

    time_bin_25ms = floor(time_ms / 25) * 25,
    time_bin_50ms = floor(time_ms / 50) * 50,
    time_bin_100ms = floor(time_ms / 100) * 100,

    x = x,
    y = y,
    raw_x = best_x_raw,
    raw_y = best_y_raw,
    left_x = left_x,
    left_y = left_y,
    right_x = right_x,
    right_y = right_y,

    left_pupil = left_pupil,
    right_pupil = right_pupil,
    mean_pupil = mean_pupil,
    pupil = mean_pupil,
    pupil_unit = pupil_unit,
    pupil_source_left = left_pupil_source,
    pupil_source_right = right_pupil_source,

    gaze_unit = gaze_unit,
    coordinate_unit_detected = detected_coordinate_unit,
    screen_width_px = screen_width_value,
    screen_height_px = screen_height_value,

    valid_sample = valid_sample,
    missing_gaze = missing_gaze,
    missing_pupil = missing_pupil,
    gaze_offscreen = gaze_offscreen,
    trackloss = trackloss,
    Trackloss = trackloss,
    blink = blink,

    left_gaze_valid = ifelse(is.na(left_gaze_valid_raw), NA, left_gaze_valid_raw == 1),
    right_gaze_valid = ifelse(is.na(right_gaze_valid_raw), NA, right_gaze_valid_raw == 1),
    left_pupil_valid = ifelse(is.na(left_pupil_valid_raw), NA, left_pupil_valid_raw == 1),
    right_pupil_valid = ifelse(is.na(right_pupil_valid_raw), NA, right_pupil_valid_raw == 1),

    aoi = aoi,
    AOI = aoi,
    aoi_current = aoi_current,
    aoi_count = ifelse(
      !is.na(aoi_current) & !aoi_current %in% c("missing", "offscreen"),
      1L,
      0L
    ),

    message = message,
    event_type = event_type,
    event_label = message,
    event_latency_offset_ms = event_latency_offset_ms,

    fixation_x = scale_x(fixation_x_raw),
    fixation_y = scale_y(fixation_y_raw),
    fixation_start_sec = get_numeric(c("FPOGS")),
    fixation_duration_sec = get_numeric(c("FPOGD")),
    fixation_id = get_numeric(c("FPOGID")),
    fixation_event = get_numeric(c("FPOGV")) == 1,

    artifact_flag = artifact_flag,
    artifact_reason = artifact_reason,

    tracker_model = "Gazepoint",
    tracker_sampling_rate = NA_real_
  )
}
