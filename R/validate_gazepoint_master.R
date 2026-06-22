#' Validate a Gazepoint master sample table
#'
#' Performs formal validation checks on a Gazepoint master sample-level table
#' created by [as_gazepoint_master()] or [create_gazepoint_master()]. This
#' function is intended as a gate between master-table construction and more
#' advanced steps such as pupil preprocessing, AOI modelling, or statistical
#' analysis.
#'
#' @param master A Gazepoint master sample-level table.
#' @param min_valid_sample_pct Minimum acceptable percentage of valid gaze
#'   samples. Defaults to `75`.
#' @param max_missing_gaze_pct Maximum acceptable percentage of missing gaze
#'   samples. Defaults to `25`.
#' @param max_missing_pupil_pct Maximum acceptable percentage of missing pupil
#'   samples. Defaults to `50`.
#' @param max_offscreen_gaze_pct Maximum acceptable percentage of off-screen gaze
#'   samples. Defaults to `25`.
#' @param require_pupil Logical. If `TRUE`, the validation fails when no usable
#'   pupil column is present. Defaults to `FALSE`.
#' @param require_aoi Logical. If `TRUE`, the validation fails when no real AOI
#'   samples are present. Defaults to `FALSE`.
#' @param fail_on_error Logical. If `TRUE`, the function aborts when one or more
#'   validation checks fail. Defaults to `FALSE`.
#'
#' @return A named list with:
#' \describe{
#'   \item{summary}{One-row validation summary.}
#'   \item{checks}{A tibble containing all validation checks.}
#'   \item{failed_checks}{Validation checks with status `"fail"`.}
#'   \item{warning_checks}{Validation checks with status `"warning"`.}
#'   \item{column_map}{Detected column mapping used for validation.}
#' }
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
#' validation <- validate_gazepoint_master(master)
#'
#' validation$summary
#' validation$checks
#' }
#'
#' @export
validate_gazepoint_master <- function(
    master,
    min_valid_sample_pct = 75,
    max_missing_gaze_pct = 25,
    max_missing_pupil_pct = 50,
    max_offscreen_gaze_pct = 25,
    require_pupil = FALSE,
    require_aoi = FALSE,
    fail_on_error = FALSE
) {
  if (!is.data.frame(master)) {
    rlang::abort("`master` must be a data frame.")
  }

  if (!is.numeric(min_valid_sample_pct) || length(min_valid_sample_pct) != 1) {
    rlang::abort("`min_valid_sample_pct` must be a single numeric value.")
  }

  if (!is.numeric(max_missing_gaze_pct) || length(max_missing_gaze_pct) != 1) {
    rlang::abort("`max_missing_gaze_pct` must be a single numeric value.")
  }

  if (!is.numeric(max_missing_pupil_pct) || length(max_missing_pupil_pct) != 1) {
    rlang::abort("`max_missing_pupil_pct` must be a single numeric value.")
  }

  if (!is.numeric(max_offscreen_gaze_pct) || length(max_offscreen_gaze_pct) != 1) {
    rlang::abort("`max_offscreen_gaze_pct` must be a single numeric value.")
  }

  if (!is.logical(require_pupil) || length(require_pupil) != 1) {
    rlang::abort("`require_pupil` must be `TRUE` or `FALSE`.")
  }

  if (!is.logical(require_aoi) || length(require_aoi) != 1) {
    rlang::abort("`require_aoi` must be `TRUE` or `FALSE`.")
  }

  if (!is.logical(fail_on_error) || length(fail_on_error) != 1) {
    rlang::abort("`fail_on_error` must be `TRUE` or `FALSE`.")
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
      "time",
      "x",
      "y",
      "valid_sample",
      "missing_gaze",
      "missing_pupil",
      "gaze_offscreen",
      "pupil",
      "aoi_current",
      "aoi_count",
      "screen_width_px",
      "screen_height_px"
    ),
    column = c(
      detect_col(c("subject", "pID", "participant")),
      detect_col(c("media_id", "MEDIA_ID")),
      detect_col(c("time_ms", "time", "time_orig")),
      detect_col(c("x", "gaze_x")),
      detect_col(c("y", "gaze_y")),
      detect_col(c("valid_sample")),
      detect_col(c("missing_gaze")),
      detect_col(c("missing_pupil")),
      detect_col(c("gaze_offscreen")),
      detect_col(c("mean_pupil", "pupil", "pupil_raw")),
      detect_col(c("aoi_current", "AOI")),
      detect_col(c("aoi_count")),
      detect_col(c("screen_width_px")),
      detect_col(c("screen_height_px"))
    )
  )

  get_col <- function(role) {
    col <- column_map$column[column_map$role == role]

    if (length(col) == 0 || is.na(col)) {
      return(NULL)
    }

    master[[col]]
  }

  has_role <- function(role) {
    col <- column_map$column[column_map$role == role]
    length(col) == 1 && !is.na(col)
  }

  prop_true_pct <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    x <- as.logical(x)

    if (length(x) == 0 || all(is.na(x))) {
      return(NA_real_)
    }

    mean(x, na.rm = TRUE) * 100
  }

  safe_min <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    min(x)
  }

  safe_max <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    max(x)
  }

  n_distinct_non_missing <- function(x) {
    if (is.null(x)) {
      return(NA_integer_)
    }

    length(unique(x[!is.na(x)]))
  }

  is_real_aoi <- function(x) {
    if (is.null(x)) {
      return(rep(FALSE, nrow(master)))
    }

    !is.na(x) &
      x != "" &
      !(x %in% c("missing", "offscreen", "non_aoi", "unclassified"))
  }

  check_status <- function(condition, warning_condition = FALSE) {
    if (isTRUE(condition)) {
      return("pass")
    }

    if (isTRUE(warning_condition)) {
      return("warning")
    }

    "fail"
  }

  make_check <- function(
    check_id,
    check_name,
    status,
    severity,
    value = NA_character_,
    threshold = NA_character_,
    message
  ) {
    tibble::tibble(
      check_id = check_id,
      check_name = check_name,
      status = status,
      severity = severity,
      value = as.character(value),
      threshold = as.character(threshold),
      message = message
    )
  }

  subject <- get_col("subject")
  media_id <- get_col("media_id")
  time <- get_col("time")
  x <- get_col("x")
  y <- get_col("y")
  valid_sample <- get_col("valid_sample")
  missing_gaze <- get_col("missing_gaze")
  missing_pupil <- get_col("missing_pupil")
  gaze_offscreen <- get_col("gaze_offscreen")
  pupil <- get_col("pupil")
  aoi_current <- get_col("aoi_current")
  aoi_count <- get_col("aoi_count")
  screen_width_px <- get_col("screen_width_px")
  screen_height_px <- get_col("screen_height_px")

  n_rows <- nrow(master)
  n_subjects <- n_distinct_non_missing(subject)
  n_media <- n_distinct_non_missing(media_id)

  valid_sample_pct <- prop_true_pct(valid_sample)
  missing_gaze_pct <- prop_true_pct(missing_gaze)
  missing_pupil_pct <- prop_true_pct(missing_pupil)
  offscreen_gaze_pct <- prop_true_pct(gaze_offscreen)

  has_pupil <- !is.null(pupil) && any(!is.na(pupil))
  has_aoi <- any(is_real_aoi(aoi_current))

  time_min <- safe_min(time)
  time_max <- safe_max(time)

  required_roles <- c(
    "subject",
    "media_id",
    "time",
    "x",
    "y",
    "valid_sample",
    "missing_gaze",
    "missing_pupil",
    "gaze_offscreen",
    "aoi_current",
    "aoi_count"
  )

  missing_required_roles <- required_roles[
    !vapply(required_roles, has_role, logical(1))
  ]

  checks <- list()

  checks[[length(checks) + 1]] <- make_check(
    "C001",
    "Non-empty data frame",
    check_status(n_rows > 0),
    "error",
    n_rows,
    "> 0",
    "The master table must contain at least one row."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C002",
    "Required columns detected",
    check_status(length(missing_required_roles) == 0),
    "error",
    if (length(missing_required_roles) == 0) {
      "none missing"
    } else {
      paste(missing_required_roles, collapse = ", ")
    },
    "all required roles present",
    "The master table must contain the required identifier, time, gaze, quality, and AOI columns."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C003",
    "Subject identifiers available",
    check_status(!is.na(n_subjects) && n_subjects > 0),
    "error",
    n_subjects,
    "> 0",
    "At least one non-missing subject identifier must be available."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C004",
    "Media identifiers available",
    check_status(!is.na(n_media) && n_media > 0),
    "error",
    n_media,
    "> 0",
    "At least one non-missing media/stimulus identifier must be available."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C005",
    "Time column is numeric",
    check_status(!is.null(time) && is.numeric(time)),
    "error",
    if (is.null(time)) "missing" else class(time)[1],
    "numeric",
    "The detected time column must be numeric."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C006",
    "Time span is positive",
    check_status(is.finite(time_min) && is.finite(time_max) && time_max > time_min),
    "error",
    paste0(time_min, " to ", time_max),
    "max > min",
    "The master table must contain a positive time span."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C007",
    "Gaze coordinates are numeric",
    check_status(
      !is.null(x) &&
        !is.null(y) &&
        is.numeric(x) &&
        is.numeric(y)
    ),
    "error",
    paste0(
      "x=", if (is.null(x)) "missing" else class(x)[1],
      "; y=", if (is.null(y)) "missing" else class(y)[1]
    ),
    "numeric x and y",
    "The gaze coordinate columns must be numeric."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C008",
    "Valid-sample percentage acceptable",
    check_status(
      !is.na(valid_sample_pct) &&
        valid_sample_pct >= min_valid_sample_pct
    ),
    "error",
    round(valid_sample_pct, 3),
    paste0(">= ", min_valid_sample_pct),
    "The percentage of valid samples should be above the minimum threshold."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C009",
    "Missing-gaze percentage acceptable",
    check_status(
      !is.na(missing_gaze_pct) &&
        missing_gaze_pct <= max_missing_gaze_pct
    ),
    "error",
    round(missing_gaze_pct, 3),
    paste0("<= ", max_missing_gaze_pct),
    "The percentage of missing gaze samples should be below the maximum threshold."
  )

  pupil_status <- if (require_pupil) {
    check_status(has_pupil)
  } else {
    check_status(has_pupil, warning_condition = !has_pupil)
  }

  checks[[length(checks) + 1]] <- make_check(
    "C010",
    "Pupil data available",
    pupil_status,
    if (require_pupil) "error" else "warning",
    has_pupil,
    if (require_pupil) "required" else "recommended",
    "Pupil data are required only when pupil preprocessing or pupil modelling will be performed."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C011",
    "Missing-pupil percentage acceptable",
    check_status(
      is.na(missing_pupil_pct) ||
        missing_pupil_pct <= max_missing_pupil_pct,
      warning_condition = !is.na(missing_pupil_pct) &&
        missing_pupil_pct > max_missing_pupil_pct
    ),
    "warning",
    round(missing_pupil_pct, 3),
    paste0("<= ", max_missing_pupil_pct),
    "High missing-pupil percentages may affect pupil preprocessing and pupil-based modelling."
  )

  checks[[length(checks) + 1]] <- make_check(
    "C012",
    "Off-screen gaze percentage acceptable",
    check_status(
      is.na(offscreen_gaze_pct) ||
        offscreen_gaze_pct <= max_offscreen_gaze_pct,
      warning_condition = !is.na(offscreen_gaze_pct) &&
        offscreen_gaze_pct > max_offscreen_gaze_pct
    ),
    "warning",
    round(offscreen_gaze_pct, 3),
    paste0("<= ", max_offscreen_gaze_pct),
    "High off-screen gaze percentages may indicate calibration, stimulus, or participant-quality problems."
  )

  aoi_status <- if (require_aoi) {
    check_status(has_aoi)
  } else {
    check_status(has_aoi, warning_condition = !has_aoi)
  }

  checks[[length(checks) + 1]] <- make_check(
    "C013",
    "AOI samples available",
    aoi_status,
    if (require_aoi) "error" else "warning",
    has_aoi,
    if (require_aoi) "required" else "recommended",
    "Real AOI samples are required only for AOI-based analyses."
  )

  if (!is.null(aoi_count) && !is.null(aoi_current)) {
    real_aoi_flag <- is_real_aoi(aoi_current)
    aoi_count_numeric <- suppressWarnings(as.integer(aoi_count))
    aoi_mismatch <- sum(aoi_count_numeric != as.integer(real_aoi_flag), na.rm = TRUE)
  } else {
    aoi_mismatch <- NA_integer_
  }

  checks[[length(checks) + 1]] <- make_check(
    "C014",
    "AOI count matches AOI state",
    check_status(!is.na(aoi_mismatch) && aoi_mismatch == 0),
    "error",
    aoi_mismatch,
    "0 mismatches",
    "`aoi_count` should equal 1 only for real AOI samples and 0 otherwise."
  )

  if (
    !is.null(x) &&
    !is.null(y) &&
    !is.null(missing_gaze) &&
    !is.null(gaze_offscreen) &&
    !is.null(screen_width_px) &&
    !is.null(screen_height_px)
  ) {
    screen_width_value <- unique(stats::na.omit(screen_width_px))
    screen_height_value <- unique(stats::na.omit(screen_height_px))

    if (length(screen_width_value) == 1 && length(screen_height_value) == 1) {
      expected_offscreen <- !as.logical(missing_gaze) &
        is.finite(x) &
        is.finite(y) &
        (
          x < 0 |
            x > screen_width_value |
            y < 0 |
            y > screen_height_value
        )

      offscreen_mismatch <- sum(
        as.logical(gaze_offscreen) != expected_offscreen,
        na.rm = TRUE
      )
    } else {
      offscreen_mismatch <- NA_integer_
    }
  } else {
    offscreen_mismatch <- NA_integer_
  }

  checks[[length(checks) + 1]] <- make_check(
    "C015",
    "Off-screen flag matches screen bounds",
    check_status(
      !is.na(offscreen_mismatch) &&
        offscreen_mismatch == 0,
      warning_condition = is.na(offscreen_mismatch)
    ),
    "warning",
    if (is.na(offscreen_mismatch)) "not checked" else offscreen_mismatch,
    "0 mismatches",
    "When screen dimensions are available, `gaze_offscreen` should match the x/y screen bounds."
  )

  checks <- dplyr::bind_rows(checks)

  failed_checks <- checks[checks$status == "fail", , drop = FALSE]
  warning_checks <- checks[checks$status == "warning", , drop = FALSE]

  summary <- tibble::tibble(
    validation_passed = nrow(failed_checks) == 0,
    n_checks = nrow(checks),
    n_passed = sum(checks$status == "pass"),
    n_failed = nrow(failed_checks),
    n_warnings = nrow(warning_checks),
    n_rows = n_rows,
    n_subjects = n_subjects,
    n_media = n_media,
    time_min = time_min,
    time_max = time_max,
    time_span = time_max - time_min,
    valid_sample_pct = valid_sample_pct,
    missing_gaze_pct = missing_gaze_pct,
    missing_pupil_pct = missing_pupil_pct,
    offscreen_gaze_pct = offscreen_gaze_pct,
    has_pupil = has_pupil,
    has_aoi = has_aoi
  )

  result <- list(
    summary = summary,
    checks = checks,
    failed_checks = failed_checks,
    warning_checks = warning_checks,
    column_map = column_map
  )

  if (isTRUE(fail_on_error) && nrow(failed_checks) > 0) {
    rlang::abort(
      paste0(
        "`master` failed validation with ",
        nrow(failed_checks),
        " failing check(s). Inspect `validate_gazepoint_master(master)$failed_checks`."
      )
    )
  }

  result
}
